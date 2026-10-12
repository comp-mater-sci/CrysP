!> This module analyzes a particular stress or strain mode without deforming the material.
module mode
    use iso_c_binding
    use base_defs
    use conversions
    use crysp_cluster
    use omp_lib
    use meso
    use crysp_status

    implicit none

    private
    public:: simulate_stress_mode, &
             simulate_strain_mode

    character(*), parameter:: MOD_NAME = "mode"

    !> Wrapper type needed to pass a reference to the clusters and meso model to C.
    !>
    !> We need to pass the clusters and meso model to C because the trust region solver itself is implemented in C because the MKL wrappers for
    !> Fortran are broken as of 4/2026. However, from the solver in C we must call back to fortran to calculate the actual stress
    !> response. This calculation needs the clusters, which are of polymorphic type. Passing a polymorphic type directly to C is
    !> forbidden, but passing a nonpolymorphic type which contains a pointer to a polymorphic one is not. Hence the wrapper.
    type:: Wrapper
        class(MesoModel), pointer:: model
        class(Cluster), dimension(:), pointer:: clusters
    end type

    !> Interface of the trust region solver in C.
    !>
    !> Needed because Fortran bindings for MKL are broken. MKL error codes are translated into a [[crysp_status:Status]] on the C side;
    !> a failure reported by [[get_stress]] aborts the solver and is returned unchanged.
    interface
        function trust_region_solve(clusters_ptr, target_stress_mode, strain_mode, stress, residual) bind(C) result(stat)
            import C_DOUBLE, &
                   C_PTR, &
                   Status

            type(C_PTR)::                     clusters_ptr       !! Pointer to the wrapper of the clusters so the callback in
                                                                 !! Fortran can calculate the stress state
            real(C_DOUBLE), dimension(5)::    target_stress_mode !! Stress mode we want to simulate
            real(C_DOUBLE), dimension(5)::    strain_mode        !! Strain mode matching the requested stress mode as closely as
                                                                 !! possible
            real(C_DOUBLE), dimension(5)::    stress             !! Actuall stress state found. Normalizing it should yield
                                                                 !! something very close, but not identical to the target stress mode.
            real(C_DOUBLE), dimension(5)::    residual           !! Residual of the optimization
            type(Status)::                    stat
        end function
    end interface

contains

    !> Calculate the stress state for a given strain rate
    !>
    !> Designed as a callback for the trust region optimization in C, which aborts as soon as a failure is returned.
    function get_stress(wrapper_ptr, strain_rate, stress) bind(C) result(stat)
        type(C_PTR), intent(in):: wrapper_ptr          !! C pointer to the clusters wrapper object needed for simulation.
        real(C_DOUBLE), dimension(5), intent(in):: strain_rate  !! Strain rate for which to calculate the stress state.
        real(C_DOUBLE), dimension(5):: stress                   !! Stress state for the provided strain rate.
        type(Status):: stat                                     !! `CRYSP_OK`, or the failure reported by the simulation.

        type(Wrapper), pointer:: wrap
        class(MesoModel), pointer:: model
        class(Cluster), dimension(:), pointer:: clusters

        call c_f_pointer(wrapper_ptr, wrap)

        clusters => wrap%clusters
        model => wrap%model
        stat = simulate_strain_mode(model, clusters, strain_rate, stress)
    end function

    !> Calculate the stress state corresponding to a given strain mode.
    !>
    !> Does not alter the state of the material.
    !> @note
    !> A worker thread cannot branch out of an `!$OMP DO`, so a failing cluster records the first failure under a critical section
    !> and the remaining iterations short-circuit. `first_failure` is only ever written inside that critical section; the unguarded
    !> read of `any_failed` is a monotonic false-to-true flag, so a thread that observes it late merely does redundant work.
    !> @endnote
    function simulate_strain_mode(model, clusters, strain_mode, stress) result(stat)
        class(MesoModel), intent(in):: model
        class(Cluster), dimension(:), intent(inout):: clusters
        real(DP), dimension(5), intent(in):: strain_mode       !! Deviatoric strain mode to be imposed.
        real(DP), dimension(5), intent(out):: stress           !! Deviatoric stress state
        type(Status):: stat                                    !! `CRYSP_OK`, or the first failure reported by any cluster.

        integer:: i
        logical:: any_failed
        real(DP):: total_weight, &
                   velocity_gradient(3,3), &
                   homogenized_stress(3,3), &
                   stress_cluster(3,3)
        type(Status):: first_failure, &
                       cluster_stat

        stress = 0._DP
        velocity_gradient = deviatoric_to_tensor(strain_mode)
        homogenized_stress = 0._DP
        total_weight = 0._DP
        any_failed = .false.

        !$OMP PARALLEL SHARED(model, clusters, velocity_gradient, any_failed, first_failure) PRIVATE(stress_cluster, cluster_stat)
            !$OMP DO SCHEDULE(GUIDED) REDUCTION (+:total_weight, homogenized_stress)
                do i = 1, size(clusters)
                    if (any_failed) cycle

                    cluster_stat = model%get_stress(clusters(i), velocity_gradient, stress_cluster)
                    if (failed(cluster_stat)) then
                        !$OMP CRITICAL
                            if (.not. any_failed) then
                                any_failed = .true.
                                first_failure = cluster_stat
                            end if
                        !$OMP END CRITICAL
                        cycle
                    end if

                    homogenized_stress = homogenized_stress+stress_cluster * clusters(i)%weight
                    total_weight = total_weight+clusters(i)%weight
                end do
            !$OMP END DO
        !$OMP END PARALLEL

        if (any_failed) then
            stat = first_failure
            return
        end if

        !Round to TOLERANCE to mitigate floating point deviations due to scheduling.
        stress = anint(tensor_to_deviatoric(homogenized_stress / total_weight)/TOLERANCE) * TOLERANCE
    end function

    !> Simulate the strain mode and stress state corresponding to some stress mode
    !>
    !> Iteratively finds a strain mode that yields a stress state of which the mode closely matches the requested stress mode.
    !> Does not alter the state of the material
    function simulate_stress_mode(model, clusters, stress_mode, strain_mode, stress, residual) result(stat)
        class(MesoModel), target, intent(in):: model
        class(Cluster), dimension(:), target, intent(inout):: clusters !! Material state
        real(DP), dimension(5), intent(in)::  stress_mode              !! Requested stress mode
        real(DP), dimension(5), intent(inout):: strain_mode            !! Strain mode (approx.) yielding the requested stress mode.
        real(DP), dimension(5), intent(out):: stress                   !! Actual stress state found
        real(DP), dimension(5), intent(out):: residual                 !! Residual of the iterative solver.
        type(Status):: stat                                            !! `CRYSP_OK`, a failure raised inside the callback, or `CRYSP_ERR`
                                                                       !! if the MKL trust region solver itself failed.

        type(Wrapper), target:: wrap

        wrap%model => model
        wrap%clusters => clusters

        stat = trust_region_solve(c_loc(wrap), stress_mode, strain_mode, stress, residual)
    end function
end module
