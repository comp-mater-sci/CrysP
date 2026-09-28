#include <mkl.h>
#include <math.h>

/*
 * This library calculates finds the strain mode corresponding to a given imposed stress state using a trust-region method. This library is implemented in C because it relies on Intel MKL and the Fortran interface for the specific MKL procedures used contains bugs (as of 2025.0.1).
 *
 */

//Status code of crysp_status (src/utils/status.f90) for success. Keep it in sync.
enum
{
    CRYSP_OK = 0
};

//Alternate implementation of get_stress_state for easy C interop. Returns a status code; on failure the solver must abort and hand
//the code back to Fortran, which keeps the details of the failure itself.
extern int get_stress(const void* clusters, const double* strain_mode, double* stress_state);

const MKL_INT DIM = 5;      //Dimensionality of the problem.
const double TOL = 1.0e-9;  //Tolerance on internal calculations.

//Calculate the 2-norm of a DIM-dimensional vector of doubles.
inline double norm2(const double* vec)
{
    double norm = 0.0;
    for (int i=0; i<DIM;i++)
        norm += pow(vec[i],2);
    return sqrt(norm);
}

//Normalize a DIM-dimensional vector of doubles.
inline void normalize(double* stress_state)
{
    double norm = norm2(stress_state);
    for (int i=0;i<DIM;i++)
        stress_state[i] /= norm;
}

//Wrapper for get_stress_state specifically for use from calc_jacobi to avoid code duplication.
inline int altay_wrapper_jacobi(const void* clusters, const double* strain_rate, double* stress_mode)
{
    int stat = get_stress(clusters, strain_rate, stress_mode);
    if (stat != CRYSP_OK) return stat;
    normalize(stress_mode);

    //Multiply by -1 because the jacobi of the minimization algorithm is
    // d/dx(stress_target - stress_mode(x))
    // = 0 - d/dx(stress_mode(x))
    // = -1 * d/dx(stess_mode(x))
    for (int i=0; i<DIM;i++)
        stress_mode[i] *= -1.0;
    return stat;
}

//Calculate the jacobi of AlTay using the finite difference algorithm from MKL. The finite difference interval is provided by the caller. This procedure may recursively call itself with a larger interval. Strain rate and jacobi are both of size DIM. Note that jacobi is stored column major.
int jacobi_helper(const void* clusters, const double* strain_rate, double* jacobi, const double interval)
{
    int stat = CRYSP_OK;

    _JACOBIMATRIX_HANDLE_t handle;
    double strain_rate_buffer[DIM];
    for (int i=0;i<DIM;i++)
        strain_rate_buffer[i] = strain_rate[i];

    //See MKL documentation for details.
    MKL_INT mkl_code = djacobi_init(&handle, &DIM, &DIM, strain_rate_buffer, jacobi, &interval);
    if (mkl_code != TR_SUCCESS) return (int)mkl_code;

    //Every failure below breaks out of the loop so the handle is always released.
    double f1[DIM], f2[DIM];
    MKL_INT rci_req = 0;
    do
    {
        mkl_code = djacobi_solve(&handle, f1, f2, &rci_req);
        if (mkl_code != TR_SUCCESS)
        {
            stat = (int)mkl_code;
            break;
        }

        if (rci_req == 1)
            stat = altay_wrapper_jacobi(clusters, strain_rate_buffer, f1);
        else if (rci_req == 2)
            stat = altay_wrapper_jacobi(clusters, strain_rate_buffer, f2);
        if (stat != CRYSP_OK) break;
    } while (rci_req > 0);

    //Report the first failure, but always release the handle.
    mkl_code = djacobi_delete(&handle);
    if (stat != CRYSP_OK) return stat;
    if (mkl_code != TR_SUCCESS) return (int)mkl_code;

    //If the Jacobi contains 0-columns the solution of the minimization problem will not be unique and the trust region algorithm will fail. Because get_stress_state is 'jagged' in nature it is likely that increasing the finite difference interval fixes this problem.
    for (int i=0;i<DIM;i++)
    {
        if (norm2(jacobi+i*DIM) < TOL)
            //Doubling the interval was experimentally determined to be optimal.
            return jacobi_helper(clusters, strain_rate, jacobi, 2.0*interval);
    }
    return stat;
}

//Calculate the Jacobi. Strain_mode had dimension DIM and jacobi DIM*DIM.
int calc_jacobi(const void* clusters, const double* strain_mode, double* jacobi)
{
    //Starting interval of 0.02 was experimentally determined to be optimal.
    return jacobi_helper(clusters, strain_mode, jacobi, 0.02);
}

//Calculate the strain mode corresponding as closely as possible to the imposed stress state. All inputs and outputs must be initialized externally and are of dimension DIM, except for jacobi, which is of dimension DIM*DIM.
//Returns CRYSP_OK, the code returned by get_stress if it failed, or the error code of the MKL routine that failed.
int trust_region_solve(const void* clusters, const double* target_stress_mode, double* strain_mode, double* stress, double* residual)
{
    int stat = CRYSP_OK;

    _TRNSPBC_HANDLE_t handle;

    //Parameters for the trust region algorithm. These have experimentally been finetuned.
    const double  OBJECTIVE_THRESHOLD = 0.001;
    const double  EPSILON = 0.01 * OBJECTIVE_THRESHOLD;
    const double  EPS[] = {EPSILON, OBJECTIVE_THRESHOLD, EPSILON, EPSILON, EPSILON, EPSILON};
    const double  LOWER_BOUND[] = {-1.0, -1.0, -1.0, -1.0, -1.0};
    const double  UPPER_BOUND[] = {1.0, 1.0, 1.0, 1.0, 1.0};
    const MKL_INT ITER1 = 350;
    const MKL_INT ITER2 = 50;
    const double  INITIAL_TRUST_REGION = 0.1;

    double jacobi[5][5];

    //See MKL documentation for details.
    MKL_INT mkl_code = dtrnlspbc_init(&handle, &DIM, &DIM, strain_mode, LOWER_BOUND, UPPER_BOUND, EPS, &ITER1, &ITER2, &INITIAL_TRUST_REGION);
    if (mkl_code != TR_SUCCESS) return (int)mkl_code;

    //Every failure below breaks out of the loop so the handle and MKL buffers are always released.
    MKL_INT rci_req = 0;
    while (rci_req >= 0)
    {
        mkl_code = dtrnlspbc_solve(&handle, residual, &jacobi[0][0], &rci_req);
        if (mkl_code != TR_SUCCESS)
        {
            stat = (int)mkl_code;
            break;
        }

        if (rci_req == 1)
        {
            stat = get_stress(clusters, strain_mode, stress);
            if (stat != CRYSP_OK) break;

            double stress_mode[DIM];
            for (int i=0;i<DIM;i++)
                stress_mode[i] = stress[i];
            normalize(stress_mode);
            for (int i=0;i<DIM;i++)
                residual[i] = target_stress_mode[i] - stress_mode[i];
        }
        else if (rci_req == 2)
        {
            stat = calc_jacobi(clusters, strain_mode, &jacobi[0][0]);
            if (stat != CRYSP_OK) break;
        }
    }

    //Report the first failure, but always release the handle and MKL buffers.
    mkl_code = dtrnlspbc_delete(&handle);
    MKL_Free_Buffers();
    if (stat != CRYSP_OK) return stat;
    if (mkl_code != TR_SUCCESS) return (int)mkl_code;

    normalize(strain_mode); // The trust region algorithm may deviate from unit length.
    return stat;
}
