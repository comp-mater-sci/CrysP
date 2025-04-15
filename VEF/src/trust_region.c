#include <mkl.h>
#include <stdio.h>
#include <math.h>

extern void altay_get_stress_state_c(double* strain_mode, double* stress_state);

const MKL_INT DIM = 5;
const double TOL = 1.0e-9;

inline double norm2(const double* vec)
{
    double norm = 0.0;
    for (int i=0; i<DIM;i++)
        norm += pow(vec[i],2);
    return sqrt(norm);
}

inline void normalize(double* stress_state)
{
    double norm = norm2(stress_state);
    for (int i=0;i<DIM;i++)
        stress_state[i] /= norm;
}

inline void altay_wrapper_jacobi(const double* strain_rate, double* stress_mode)
{
    altay_get_stress_state_c(strain_rate, stress_mode);
    normalize(stress_mode);
    for (int i=0; i<DIM;i++)
        stress_mode[i] *= -1.0;
}

int jacobi_helper(const double* strain_rate, double* jacobi, const double interval)
{
    _JACOBIMATRIX_HANDLE_t handle;
    double strain_rate_buffer[DIM];
    for (int i=0;i<DIM;i++)
        strain_rate_buffer[i] = strain_rate[i];

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
                altay_wrapper_jacobi(strain_rate_buffer, f1);
                break;
            case 2:
                altay_wrapper_jacobi(strain_rate_buffer, f2);
                break;
        }
    } while (rci_req > 0);

    mkl_err = djacobi_delete(&handle);
    if (mkl_err != TR_SUCCESS) return mkl_err;

    for (int i=0;i<DIM;i++)
    {
        if (norm2(jacobi+i*DIM) < TOL)
            return jacobi_helper(strain_rate, jacobi, 2.0*interval);
    }
    return TR_SUCCESS;
}

int calc_jacobi(const double* strain_mode, double* jacobi)
{
    return jacobi_helper(strain_mode, jacobi, 0.02); 
}

int trust_region_solve(const double* stress_target, double* stress_state, double* strain_rate, double* jacobi, double* residual)
{
   
    _TRNSPBC_HANDLE_t handle;
    const double  OBJECTIVE_THRESHOLD = 0.01;
    const double  EPSILON = 0.01 * OBJECTIVE_THRESHOLD;
    const double  EPS[] = {EPSILON, OBJECTIVE_THRESHOLD, EPSILON, EPSILON, EPSILON, EPSILON}; 
    const double  LOWER_BOUND[] = {-1.0, -1.0, -1.0, -1.0, -1.0};
    const double  UPPER_BOUND[] = {1.0, 1.0, 1.0, 1.0, 1.0};
    const MKL_INT ITER1 = 350;
    const MKL_INT ITER2 = 50;
    const double  INITIAL_TRUST_REGION = 0.1;

    MKL_INT mkl_err = dtrnlspbc_init(&handle, &DIM, &DIM, strain_rate, LOWER_BOUND, UPPER_BOUND, EPS, &ITER1, &ITER2, &INITIAL_TRUST_REGION);
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
                altay_get_stress_state_c(strain_rate, stress_state);
                double stress_mode[DIM]; 
                for (int i=0;i<DIM;i++)
                    stress_mode[i] = stress_state[i];
                normalize(stress_mode);
                for (int i=0;i<DIM;i++)
                    residual[i] = stress_target[i] - stress_mode[i];
                break;
            }
            case 2:
            {
                mkl_err = calc_jacobi(strain_rate, jacobi);                   
                if (mkl_err != TR_SUCCESS) return mkl_err;
                break;
            }
        }
    }
    mkl_err =  dtrnlspbc_delete (&handle);
    MKL_Free_Buffers();

    return mkl_err;
}
