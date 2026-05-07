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

    function deform(clusters, velocity_gradient, target_vm_strain, target_stress_mode) result(increments)
        class(Cluster), dimension(:), intent(inout)::  clusters
        real(DP), dimension(3,3), intent(in)::         velocity_gradient
        real(DP), intent(in)::                         target_vm_strain
        real(DP), dimension(5), intent(in), optional:: target_stress_mode
        type(StrainIncrement), dimension(:), allocatable:: increments

        integer:: i
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
                   stress_mode(5)
        type(IncrementListBuilder):: incs

        def_grad = UNIT_MATRIX_3X3
        cur_vm_strain = 0._DP

        !! Pick initial time interval to be small enough.
        !! Since it is certainly less than 1%, linear approximation will do.
        t_inc = ACCURACY / norm2(velocity_gradient)
        def_grad_inc = matrix_exponential(velocity_gradient*t_inc)

        do while (cur_vm_strain < target_vm_strain - TOLERANCE)
            !Determine strain at the end of the increment if we keep the current time step
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
            call meso_update_model(velocity_gradient, t_inc)
            total_weight = 0._DP
            homogenized_stress = 0._DP
            taylor_factor = 0._DP
            !$OMP PARALLEL SHARED(clusters, velocity_gradient, t_inc) PRIVATE(stress_cluster, slip_cluster, weight_cluster)
                !$OMP DO SCHEDULE(DYNAMIC, 1) REDUCTION(+:total_weight, homogenized_stress, taylor_factor)
                    do i = 1, size(clusters)
                        call meso_apply_deformation_step(clusters(i), velocity_gradient, t_inc, stress_cluster, slip_cluster)
                        weight_cluster = clusters(i)%weight
                        total_weight = total_weight+weight_cluster
                        homogenized_stress = homogenized_stress+stress_cluster*weight_cluster
                        taylor_factor = taylor_factor + slip_cluster * weight_cluster
                    end do
                !$OMP END DO
            !$OMP END PARALLEL

            def_grad = next_def_grad
            vm_strain_inc = next_vm_strain - cur_vm_strain
            cur_vm_strain = next_vm_strain
            homogenized_stress = homogenized_stress / total_weight

            call incs%add(StrainIncrement(t_inc, &
                                          def_grad, &
                                          cur_vm_strain, &
                                          homogenized_stress, &
                                          taylor_factor / total_weight / vm_strain_inc))

            if (present(target_stress_mode)) then
                stress_mode = tensor_to_deviatoric(homogenized_stress)
                stress_mode = stress_mode / norm2(stress_mode)
                if (norm2(target_stress_mode - stress_mode) > ACCURACY) exit
            end if
        end do

        increments = incs%get_strain_increments()
    end function
end module
