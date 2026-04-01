include 'mkl_rci.f90'

!> This module finds the strain mode and stress state corresponding to a given stress mode.
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

    !> Wrapper type needed to pass a reference to the clusters to C.
    !>
    !> We need to pass the clusters to C because the trust region solver itself is implemented in C because the MKL wrappers for
    !Fortran are broken as of 4/2026. However, from the solver in C we must call back to fortran to calculate the actual stress
    !response. This calculation needs the clusters, which are of polymorphic type. Passing a polymorphic type directly to C is
    !forbidden, but passing a nonpolymorphic type which contains a pointer to a polymorphic one is not. Hence the wrapper.
    type:: ClustersWrapper
        class(Cluster), dimension(:), pointer:: clusters
    end type

    !> Interface of the trust region solver in C.
    !>
    !> Needed because Fortran bindings for MKL are broken.
    interface
        integer(C_INT) function trust_region_solve(clusters_ptr, target_stress_mode, strain_mode, stress, residual) bind(C) result(mkl_result_code)
            import C_INT, &
                   C_DOUBLE, &
                   C_PTR

            type(C_PTR)::                     clusters_ptr       !! Pointer to the wrapper of the clusters so the callback in
                                                                 !! Fortran can calculate the stress state
            real(C_DOUBLE), dimension(5)::    target_stress_mode !! Stress mode we want to simulate
            real(C_DOUBLE), dimension(5)::    strain_mode        !! Strain mode matching the requested stress mode as closely as
                                                                 !! possible
            real(C_DOUBLE), dimension(5)::    stress             !! Actuall stress state found. Normalizing it should yield
                                                                 !! something very close, but not identical to the target stress mode.
            real(C_DOUBLE), dimension(5)::    residual           !! Residual of the optimization
        end function
    end interface

contains

    !> Calculate the stress state for a given strain rate
    !>
    !> Desined as a callback for the trust region optimization in C
    subroutine get_stress(clusters_wrapper_ptr, strain_rate, stress) bind(C)
        type(C_PTR), intent(in):: clusters_wrapper_ptr          !! C pointer to the clusters wrapper object needed for simulation.
        real(C_DOUBLE), dimension(5), intent(in):: strain_rate  !! Strain rate for which to calculate the stress state.
        real(C_DOUBLE), dimension(5):: stress                   !! Stress state for the provided strain rate.

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

    !> Simulate the strain mode and stress state corresponding to some stress mode
    !>
    !> Iteratively finds a strain mode that yields a stress state of which the mode closely matches the requested stress mode.
    subroutine simulate_stress_mode(clusters, stress_mode, strain_mode, stress, residual)
        class(Cluster), dimension(:), target, intent(inout):: clusters !! Material state
        real(DP), dimension(5), intent(in)::  stress_mode              !! Requested stress mode
        real(DP), dimension(5), intent(inout):: strain_mode            !! Strain mode (approx.) yielding the requested stress mode.
        real(DP), dimension(5), intent(out):: stress                   !! Actual stress state found
        real(DP), dimension(5), intent(out):: residual                 !! Residual of the iterative solver.

        type(ClustersWrapper), target:: wrapper

        wrapper%clusters => clusters
        if(trust_region_solve(c_loc(wrapper), stress_mode, strain_mode, stress, residual) /= TR_SUCCESS) &
              call log_error(MOD_NAME, 'simulate_stress_mode', ERR, 'Error in MKL')
    end subroutine
end module
