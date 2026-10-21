#include <assert.h>
#include <mkl.h>
#include <stdio.h>
#include <math.h>

/*
 * This library calculates finds the strain mode corresponding to a given imposed stress state using a trust-region method. This library is implemented in C because it relies on Intel MKL and the Fortran interface for the specific MKL procedures used contains bugs (as of 2025.0.1).
 *
 */

//C view of crysp_status::Status (src/utils/status.f90), with NAME_LEN/MSG_LEN from src/utils/base_defs.f90 and only the status codes
//used here. Keep them in sync: the struct is passed by value across the language boundary, so any mismatch corrupts it silently.
#define CRYSP_NAME_LEN 56
#define CRYSP_MSG_LEN  128

enum
{
    CRYSP_OK  = 0,  // Successful execution.
    CRYSP_ERR = -1  // General error
};

//Outcome of an operation that may fail. The name and message buffers are NUL-terminated and all-NUL when code is CRYSP_OK.
typedef struct
{
    int  code;                      // One of the status codes above.
    char mod_name[CRYSP_NAME_LEN];  // Module in which the failure was raised.
    char proc_name[CRYSP_NAME_LEN]; // Routine in which the failure was raised.
    char message[CRYSP_MSG_LEN];    // Human-readable description of the failure.
} crysp_status;

static_assert(sizeof(crysp_status) == sizeof(int) + 2*CRYSP_NAME_LEN + CRYSP_MSG_LEN, "crysp_status must not contain padding");

//Alternate implementation of get_stress_state for easy C interop. On failure the solver must abort and hand the status back to Fortran.
extern crysp_status get_stress(const void* clusters, const double* strain_mode, double* stress_state);

static const char MOD_NAME[] = "trust_region";

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

//Build a failed status for an MKL routine that did not return TR_SUCCESS. Like crysp_status::fail, all strings are truncated to fit and
//unused characters are NUL.
static crysp_status mkl_failure(const char* proc_name, const char* mkl_routine, MKL_INT mkl_code)
{
    crysp_status stat = {.code = CRYSP_ERR};
    snprintf(stat.mod_name, sizeof stat.mod_name, "%s", MOD_NAME);
    snprintf(stat.proc_name, sizeof stat.proc_name, "%s", proc_name);
    snprintf(stat.message, sizeof stat.message, "MKL %s failed with code %d", mkl_routine, (int)mkl_code);
    return stat;
}

//Wrapper for get_stress_state specifically for use from calc_jacobi to avoid code duplication.
inline crysp_status altay_wrapper_jacobi(const void* clusters, const double* strain_rate, double* stress_mode)
{
    crysp_status stat = get_stress(clusters, strain_rate, stress_mode);
    if (stat.code != CRYSP_OK) return stat;
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
crysp_status jacobi_helper(const void* clusters, const double* strain_rate, double* jacobi, const double interval)
{
    const char PROC_NAME[] = "jacobi_helper";
    crysp_status stat = {.code = CRYSP_OK};

    _JACOBIMATRIX_HANDLE_t handle;
    double strain_rate_buffer[DIM];
    for (int i=0;i<DIM;i++)
        strain_rate_buffer[i] = strain_rate[i];

    //See MKL documentation for details.
    MKL_INT mkl_code = djacobi_init(&handle, &DIM, &DIM, strain_rate_buffer, jacobi, &interval);
    if (mkl_code != TR_SUCCESS) return mkl_failure(PROC_NAME, "djacobi_init", mkl_code);

    //Every failure below breaks out of the loop so the handle is always released.
    double f1[DIM], f2[DIM];
    MKL_INT rci_req = 0;
    do
    {
        mkl_code = djacobi_solve(&handle, f1, f2, &rci_req);
        if (mkl_code != TR_SUCCESS)
        {
            stat = mkl_failure(PROC_NAME, "djacobi_solve", mkl_code);
            break;
        }

        if (rci_req == 1)
            stat = altay_wrapper_jacobi(clusters, strain_rate_buffer, f1);
        else if (rci_req == 2)
            stat = altay_wrapper_jacobi(clusters, strain_rate_buffer, f2);
        if (stat.code != CRYSP_OK) break;
    } while (rci_req > 0);

    //Report the first failure, but always release the handle.
    mkl_code = djacobi_delete(&handle);
    if (stat.code != CRYSP_OK) return stat;
    if (mkl_code != TR_SUCCESS) return mkl_failure(PROC_NAME, "djacobi_delete", mkl_code);

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
crysp_status calc_jacobi(const void* clusters, const double* strain_mode, double* jacobi)
{
    //Starting interval of 0.02 was experimentally determined to be optimal.
    return jacobi_helper(clusters, strain_mode, jacobi, 0.02);
}

//Calculate the strain mode corresponding as closely as possible to the imposed stress state. All inputs and outputs must be initialized externally and are of dimension DIM, except for jacobi, which is of dimension DIM*DIM.
crysp_status trust_region_solve(const void* clusters, const double* target_stress_mode, double* strain_mode, double* stress, double* residual)
{
    const char PROC_NAME[] = "trust_region_solve";
    crysp_status stat = {.code = CRYSP_OK};

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
    if (mkl_code != TR_SUCCESS) return mkl_failure(PROC_NAME, "dtrnlspbc_init", mkl_code);

    //Every failure below breaks out of the loop so the handle and MKL buffers are always released.
    MKL_INT rci_req = 0;
    while (rci_req >= 0)
    {
        mkl_code = dtrnlspbc_solve(&handle, residual, &jacobi[0][0], &rci_req);
        if (mkl_code != TR_SUCCESS)
        {
            stat = mkl_failure(PROC_NAME, "dtrnlspbc_solve", mkl_code);
            break;
        }

        if (rci_req == 1)
        {
            stat = get_stress(clusters, strain_mode, stress);
            if (stat.code != CRYSP_OK) break;

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
            if (stat.code != CRYSP_OK) break;
        }
    }

    //Report the first failure, but always release the handle and MKL buffers.
    mkl_code = dtrnlspbc_delete(&handle);
    MKL_Free_Buffers();
    if (stat.code != CRYSP_OK) return stat;
    if (mkl_code != TR_SUCCESS) return mkl_failure(PROC_NAME, "dtrnlspbc_delete", mkl_code);

    normalize(strain_mode); // The trust region algorithm may deviate from unit length.
    return stat;
}
