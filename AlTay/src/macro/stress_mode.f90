include 'mkl_rci.f90'

module stress_mode
    use iso_c_binding
    use base_defs
    use conversions
    use cluster_module
    use omp_lib
    use meso
    use logging
    use mkl_rci


    implicit none

    private
    public:: simulate_stress_mode

    character(*), parameter:: MOD_NAME = "stress_mode"

    type:: ClustersWrapper
        class(Cluster), dimension(:), pointer:: clusters
    end type

    interface
        integer(C_INT) function trust_region_solve(clusters_ptr, target_stress_mode, strain_mode, jacobi, stress, residual) bind(C) result(mkl_result_code)
            import C_INT, &
                   C_DOUBLE, &
                   C_PTR

            type(C_PTR):: clusters_ptr
            real(C_DOUBLE), dimension(5)::    target_stress_mode
            real(C_DOUBLE), dimension(5)::    strain_mode
            real(C_DOUBLE), dimension(5, 5):: jacobi
            real(C_DOUBLE), dimension(5)::    stress
            real(C_DOUBLE), dimension(5)::    residual
        end function
    end interface

contains

    subroutine get_stress(clusters_wrapper_ptr, strain_rate, stress) bind(C)
        type(C_PTR), intent(in):: clusters_wrapper_ptr
        real(C_DOUBLE), dimension(5), intent(in):: strain_rate
        real(C_DOUBLE), dimension(5):: stress

        integer:: i
        real(DP):: total_weight, &
                   velocity_gradient(3,3), &
                   homogenized_stress(3,3)
        type(ClustersWrapper), pointer:: wrapper
        class(Cluster), dimension(:), pointer:: clusters

        call c_f_pointer(clusters_wrapper_ptr, wrapper)
        clusters => wrapper%clusters
        velocity_gradient = deviatoric_to_tensor(strain_rate)
        homogenized_stress = 0._DP
        total_weight = 0._DP

        !$OMP PARALLEL SHARED(clusters, velocity_gradient)
            !$OMP DO SCHEDULE(DYNAMIC, 1) REDUCTION (+:total_weight, homogenized_stress)
                do i = 1, size(clusters)
                    homogenized_stress = homogenized_stress+meso_get_stress(clusters(i), velocity_gradient) * clusters(i)%weight
                    total_weight = total_weight+clusters(i)%weight
                end do
            !$OMP END DO
        !$OMP END PARALLEL

        !Round to TOLERANCE to mitigate floating point deviations due to scheduling.
        stress = anint(tensor_to_deviatoric(homogenized_stress / total_weight)/TOLERANCE) * TOLERANCE
    end subroutine



    subroutine simulate_stress_mode(clusters, stress_mode, strain_mode, stress, residual)
        class(Cluster), dimension(:), target, intent(inout):: clusters
        real(DP), dimension(5), intent(in)::  stress_mode
        real(DP), dimension(5), intent(inout):: strain_mode

        real(DP), dimension(5), intent(out):: stress
        real(DP), dimension(5), intent(out):: residual
        type(ClustersWrapper), target:: wrapper

        real(DP):: jacobi(5,5) !! Jacobi of the nonlinear function mapping strain mode to stress mode.

        wrapper%clusters => clusters
        if(trust_region_solve(c_loc(wrapper), stress_mode, strain_mode, jacobi, stress, residual) /= TR_SUCCESS) &
              call log_error(MOD_NAME, 'simulate_stress_mode', ERR, 'Error in MKL')

    contains
            end subroutine
end module
