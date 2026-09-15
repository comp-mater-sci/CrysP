module deformation
    use base_defs
    use conversions
    use cluster_module
    use omp_lib
    use meso
    use incrementation

    implicit none

    private
    public:: deform
contains

    function deform(model, clusters, velocity_gradient, target_vm_strain, target_stress_mode) result(increments)
        class(MesoModel), intent(inout):: model
        class(Cluster), dimension(:), intent(inout)::  clusters
        real(DP), dimension(3,3), intent(in)::         velocity_gradient
        real(DP), intent(in)::                         target_vm_strain
        real(DP), dimension(5), intent(in), optional:: target_stress_mode
        type(StrainIncrement), dimension(:), allocatable:: increments

        real(DP), parameter:: MIN_T_INC =  10 * TOLERANCE !! Minimal time increment. As small as possible because hardening can be very
                                                          !! fast at low strains but still far enough from TOLERANCE to avoid roundoff problems in the constitutive models

        integer:: i, j, &
                  prob_rem_incs
        real(DP):: total_weight, &
                   homogenized_stress(3, 3), &
                   stress_cluster(3, 3), &
                   slip_cluster, &
                   weight_cluster, &
                   t_inc, &
                   cur_vm_strain, &
                   def_grad(3,3), &
                   def_grad_inc(3,3), &
                   vm_strain_inc, &
                   next_vm_strain, &
                   next_def_grad(3,3), &
                   taylor_factor, &
                   stress_mode(5), &
                   max_stress_inc, &
                   inc_corr, &                   !Correction on the increment to get the desired accuarcy
                   max_t_inc
        type(StrainIncrementFactory):: incs

        !Limit time increment so that maximum change in deformation gradient per time step is approx. ACCURACY
        !Since this should always be in the small strain regime, linear approximation will do.
        max_t_inc = ACCURACY / norm2(velocity_gradient)
        !Pick tiny inital time interval because hardening can be very rapid for small strains.
        t_inc =  MIN_T_INC
        def_grad = UNIT_MATRIX_3X3
        cur_vm_strain = 0._DP

        do while (cur_vm_strain < target_vm_strain - TOLERANCE)
            !Determine strain at the end of the increment if we keep the current time step
            def_grad_inc = matrix_exponential(velocity_gradient*t_inc)
            next_def_grad = matmul(def_grad_inc, def_grad)
            next_vm_strain = deformation_gradient_to_von_mises_true_strain(next_def_grad)

            !If we are very close to target strain, we can linearly approximate the last step.
            if (next_vm_strain > target_vm_strain) then
                t_inc = t_inc * (target_vm_strain - cur_vm_strain) / (next_vm_strain - cur_vm_strain)
                def_grad_inc = matrix_exponential(velocity_gradient * t_inc)
                next_def_grad = matmul(def_grad_inc, def_grad)
                next_vm_strain = deformation_gradient_to_von_mises_true_strain(next_def_grad)
            end if

            !Set model state variables to correspond to end of time step so clusters can use this state to update their own state.
            call model%update(velocity_gradient, t_inc)
            total_weight = 0._DP
            homogenized_stress = 0._DP
            taylor_factor = 0._DP
            max_stress_inc = 0._DP

            !$OMP PARALLEL SHARED(model, clusters, velocity_gradient, t_inc, max_stress_inc) PRIVATE(j, stress_cluster, slip_cluster, weight_cluster)
                !$OMP DO SCHEDULE(GUIDED) REDUCTION(+:total_weight, homogenized_stress, taylor_factor)
                    do i = 1, size(clusters)
                        call model%apply_step(clusters(i), velocity_gradient, t_inc, stress_cluster, slip_cluster)
                        weight_cluster = clusters(i)%weight
                        total_weight = total_weight+weight_cluster
                        homogenized_stress = homogenized_stress+stress_cluster*weight_cluster
                        taylor_factor = taylor_factor + slip_cluster * weight_cluster
                        !$OMP CRITICAL
                            do j=1,size(clusters(i)%grains)
                                if (clusters(i)%grains(j)%stress_increment > max_stress_inc) &
                                    max_stress_inc = clusters(i)%grains(j)%stress_increment
                            end do
                        !$OMP END CRITICAL
                    end do
                !$OMP END DO
            !$OMP END PARALLEL

            !Scale increment such that the expected maximum stress change for any grain in the next iteration is ACCURACY.
            !Linear approximation is used here due to expected small time steps and large number of grains.
            !Bound such that the next time interval is in [MIN_T_INC, max_t_inc]
            inc_corr = min(max_t_inc / t_inc, max(MIN_T_INC / t_inc, ACCURACY / max_stress_inc))
            vm_strain_inc = next_vm_strain - cur_vm_strain
            def_grad = next_def_grad
            homogenized_stress = homogenized_stress / total_weight
            taylor_factor = taylor_factor * t_inc / total_weight / vm_strain_inc
            cur_vm_strain = next_vm_strain

            !Fix minimum estimated remaining increment size to 0.1% because interval likely grows due to hardening
            !Add 1 for current increment. Also makes sure we always have positive padding.
            prob_rem_incs = ceiling((target_vm_strain - cur_vm_strain) / max(.001_DP, vm_strain_inc * inc_corr)) + 1
            call incs%add(StrainIncrement(t_inc, def_grad, cur_vm_strain, homogenized_stress, taylor_factor), prob_rem_incs)

            !Force minimal amount of deformation before checking the stress mode because it is possible that the target stress mode
            !deviates quite a bit from the actual stress mode for the applied strain mode if the trust region solver gets stuck. In
            !that case, we want at least some deformation to happen so that the solver can recover.
            !After testing, the exit condition triggers roughly every 0.06 VM_strain in the non-degenerate case. This makes ACCURACY
            !a good bound.
            if (present(target_stress_mode) .and. cur_vm_strain > ACCURACY) then
                stress_mode = tensor_to_deviatoric(homogenized_stress)
                stress_mode = stress_mode / norm2(stress_mode)
                if (norm2(target_stress_mode - stress_mode) > ACCURACY) exit
            end if

            t_inc = t_inc * inc_corr
        end do

        increments = incs%get()
    end function
end module
