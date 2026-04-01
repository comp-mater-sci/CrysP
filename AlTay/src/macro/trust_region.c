#include <mkl.h>
#include <stdio.h>
#include <math.h>

/*
 * This library calculates finds the strain mode corresponding to a given imposed stress state using a trust-region method. This library is implemented in C because it relies on Intel MKL and the Fortran interface for the specific MKL procedures used contains bugs (as of 2025.0.1).
 *
 */

//Alternate implementation of get_stress_state for easy C interop
extern void get_stress(void* clusters, double* strain_mode, double* stress_state);

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
inline void altay_wrapper_jacobi(const void* clusters, const double* strain_rate, double* stress_mode)
{
    get_stress(clusters, strain_rate, stress_mode);
    normalize(stress_mode);

    //Multiply by -1 because the jacobi of the minimization algorithm is
    // d/dx(stress_target - stress_mode(x))
    // = 0 - d/dx(stress_mode(x))
    // = -1 * d/dx(stess_mode(x))
    for (int i=0; i<DIM;i++)
        stress_mode[i] *= -1.0;
}

//Calculate the jacobi of AlTay using the finite difference algorithm from MKL. The finite difference interval is provided by the caller. This procedure may recursively call itself with a larger interval. Strain rate and jacobi are both of size DIM. Note that jacobi is stored column major.
int jacobi_helper(const void* clusters, const double* strain_rate, double* jacobi, const double interval)
{
    _JACOBIMATRIX_HANDLE_t handle;
    double strain_rate_buffer[DIM];
    for (int i=0;i<DIM;i++)
        strain_rate_buffer[i] = strain_rate[i];

    //See MKL documentation for details.
    MKL_INT mkl_err = djacobi_init(&handle, &DIM, &DIM, strain_rate_buffer, jacobi, &interval);
    if (mkl_err != TR_SUCCESS) return mkl_err;


    double f1[DIM], f2[DIM];
    MKL_INT rci_req = 0;
    do
    {
        mkl_err = djacobi_solve(&handle, f1, f2, &rci_req);
        if (mkl_err != TR_SUCCESS) return mkl_err;

        switch (rci_req)
        {
            case 1:
                altay_wrapper_jacobi(clusters, strain_rate_buffer, f1);
                break;
            case 2:
                altay_wrapper_jacobi(clusters, strain_rate_buffer, f2);
                break;
        }
    } while (rci_req > 0);

    mkl_err = djacobi_delete(&handle);
    if (mkl_err != TR_SUCCESS) return mkl_err;

    //If the Jacobi contains 0-columns the solution of the minimization problem will not be unique and the trust region algorithm will fail. Because get_stress_state is 'jagged' in nature it is likely that increasing the finite difference interval fixes this problem.
    for (int i=0;i<DIM;i++)
    {
        if (norm2(jacobi+i*DIM) < TOL)
            //Doubling the interval was experimentally determined to be optimal.
            return jacobi_helper(clusters, strain_rate, jacobi, 2.0*interval);
    }
    return TR_SUCCESS;
}

//Calculate the Jacobi. Strain_mode had dimension DIM and jacobi DIM*DIM.
int calc_jacobi(const void* clusters, const double* strain_mode, double* jacobi)
{
    //Starting interval of 0.02 was experimentally determined to be optimal.
    return jacobi_helper(clusters, strain_mode, jacobi, 0.02);
}

//Calculate the strain mode corresponding as closely as possible to the imposed stress state. All inputs and outputs must be initialized externally and are of dimension DIM, except for jacobi, which is of dimension DIM*DIM.
int trust_region_solve(const void* clusters, const double* target_stress_mode, double* strain_mode, double* jacobi, double* stress, double* residual)
{

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

    //See MKL documentation for details.
    MKL_INT mkl_err = dtrnlspbc_init(&handle, &DIM, &DIM, strain_mode, LOWER_BOUND, UPPER_BOUND, EPS, &ITER1, &ITER2, &INITIAL_TRUST_REGION);
    if (mkl_err != TR_SUCCESS) return mkl_err;

    MKL_INT rci_req = 0;
    while (rci_req >= 0)
    {
        mkl_err = dtrnlspbc_solve(&handle, residual, jacobi, &rci_req);
        if (mkl_err != TR_SUCCESS) return mkl_err;

        switch (rci_req)
        {
            case 1:
            {
                double stress_mode[DIM];
                get_stress(clusters, strain_mode, stress);
                for (int i=0;i<DIM;i++)
                    stress_mode[i] = stress[i];
                normalize(stress_mode);
                for (int i=0;i<DIM;i++)
                    residual[i] = target_stress_mode[i] - stress_mode[i];
                break;
            }
            case 2:
            {
                mkl_err = calc_jacobi(clusters, strain_mode, jacobi);
                if (mkl_err != TR_SUCCESS) return mkl_err;
                break;
            }
        }
    }

    normalize(strain_mode); // The trust region algorithm may deviate from unit length.

    mkl_err =  dtrnlspbc_delete (&handle);
    MKL_Free_Buffers();
    return mkl_err;
}
