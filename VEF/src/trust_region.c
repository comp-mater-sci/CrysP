#include <mkl.h>
#include <stdio.h>
#include <math.h>

extern void altay_get_stress_state_c(double* strain_mode, double* stress_state);

const MKL_INT DIM = 5;
const double TOL = 1.0e-9;

double norm2(const double* vec)
{
    double norm = 0.0;
    for (int i=0; i<DIM;i++)
        norm += pow(vec[i],2);
    return sqrt(norm);
}

void normalize(double* stress_state)
{
    double norm = norm2(stress_state);
    for (int i=0;i<DIM;i++)
        stress_state[i] /= norm;
}

void altay_wrapper_jacobi(const double* strain_rate, double* stress_mode)
{
    altay_get_stress_state_c(strain_rate, stress_mode);
    normalize(stress_mode);
    for (int i=0; i<DIM;i++)
        stress_mode[i] *= -1.0;
}

void jacobi_helper(const double* strain_rate, double* jacobi, const double interval)
{
    _JACOBIMATRIX_HANDLE_t handle;
    double strain_rate_buffer[DIM];
    for (int i=0;i<DIM;i++)
        strain_rate_buffer[i] = strain_rate[i];

    MKL_INT mkl_err = djacobi_init(&handle, &DIM, &DIM, strain_rate_buffer, jacobi, &interval);

    double f1[DIM], f2[DIM];
    MKL_INT rci_req = 0;
    do
    {
        mkl_err = djacobi_solve(&handle, f1, f2, &rci_req);
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

    djacobi_delete(&handle);

    for (int i=0;i<DIM;i++)
    {
        if (norm2(jacobi+i*DIM) < TOL)
        {
            jacobi_helper(strain_rate, jacobi, 2.0*interval);
            return;
        }
    }
}

void calc_jacobi(const double* strain_mode, double* jacobi)
{
    jacobi_helper(strain_mode, jacobi, 0.02); 
}

//
//void calc_residual(double* stress_target, double* stress_state)
//{
//    double stress_mode[DIM] = stress_state;
//    normalize(stress_mode);
//    double residual[DIM];
//    for (int i=0; i<5; i++)
//        residual[i] = stress_target[i] - stress_mode[i];
//}

int trust_region_solve(double* stress_target, double* stress_mode, double* strain_mode, double* jacobi)
{
    //Von Mises guess
    strain_mode = stress_target;
    //Get stress mode at current stress target
    altay_get_stress_state_c(strain_mode, stress_mode);
    normalize(stress_mode);
    
}

int trust_region_solve_from_guess(double* stress_target, double* stress_mode, double* strain_mode, double* jacobi)
{
   
    _TRNSPBC_HANDLE_t handle;
    const double  OBJECTIVE_THRESHOLD = 0.01;
    const double  EPSILON = 0.01 * OBJECTIVE_THRESHOLD;
    const double EPS[] = {EPSILON, OBJECTIVE_THRESHOLD, EPSILON, EPSILON, EPSILON, EPSILON}; 
    const double LOWER_BOUND[] = {-1.0, -1.0, -1.0, -1.0, -1.0};
    const double UPPER_BOUND[] = {1.0, 1.0, 1.0, 1.0, 1.0};
    const MKL_INT ITER1 = 350;
    const MKL_INT ITER2 = 50;
    const double INITIAL_TRUST_REGION = 0.1;

    MKL_INT mkl_err = dtrnlspbc_init(&handle, &DIM, &DIM, strain_mode, LOWER_BOUND, UPPER_BOUND, EPS, &ITER1, &ITER2, &INITIAL_TRUST_REGION);
   
    int rci_req = 0;
    //while (rci_req >= 0)
    //{
    //    mkl_err = dtrnlspbc_solve(&handle, )
    //}


    //mkl_err =  dtrnlspbc_delete (&handle);




    //for (int i=0;i<5;i++){
    //    printf("%f\n", stress_mode[i]);
    //}


    return 5;
}

