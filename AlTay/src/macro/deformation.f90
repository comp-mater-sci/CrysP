module deformation
    use base_defs
    use conversions
    use cluster_module
    use omp_lib
    use meso

    implicit none

    private
    public:: Increment, &
             deform

    type:: Increment
        real(DP):: duration = 0._DP
        real(DP), dimension(3,3):: deformation_gradient !! Technically redundant but saves a lot of computation
        real(DP):: vm_strain = 0._DP                            !! Von mises true strain. Technically redundant but saves a lot of computation
        real(DP), dimension(3,3):: stress
        real(DP):: taylor_factor = 0._DP
    end type

contains

    subroutine deform(clusters, velocity_gradient, target_vm_strain, increments)
        class(Cluster), dimension(:), intent(inout)::             clusters
        real(DP), dimension(3,3), intent(in)::                    velocity_gradient
        real(DP), intent(in)::                                    target_vm_strain
        type(Increment), dimension(:), allocatable, intent(out):: increments

        integer:: i, &
                  n_incs, &
                  remaining_incs
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
                   taylor_factor
        type(Increment):: inc
        type(Increment), allocatable:: increments_buffer(:) !! Needed for when we need to increase size


        def_grad = UNIT_MATRIX_3X3
        cur_vm_strain = 0._DP
        n_incs = 0

        !! Pick initial time interval to be small enough.
        !! Since it is certainly less than 1%, linear approximation will do.
        t_inc = ACCURACY / norm2(velocity_gradient)
        def_grad_inc = matrix_exponential(velocity_gradient*t_inc)

        !Estimate the number of increments
        allocate(increments(ceiling(target_vm_strain / ACCURACY)))

        do while (cur_vm_strain < target_vm_strain - TOLERANCE)
            !Determine strain at the end of the increment if we keep the current time step
            next_def_grad = matmul(def_grad_inc, def_grad)
            next_vm_strain = deformation_gradient_to_von_mises_true_strain(next_def_grad)

            !Expand storage for increments if needed
            if (n_incs == size(increments)) then
                !Approximate new size by extrapolating the current strain increment to the total strain.
                remaining_incs = ceiling((target_vm_strain - cur_vm_strain)/(next_vm_strain-cur_vm_strain))
                allocate(increments_buffer(n_incs + remaining_incs))
                increments_buffer(1:n_incs) = increments
                call move_alloc(increments_buffer, increments)
            end if

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

            n_incs = n_incs + 1
            def_grad = next_def_grad
            vm_strain_inc = next_vm_strain - cur_vm_strain
            cur_vm_strain = next_vm_strain

            inc%duration = t_inc
            inc%deformation_gradient = def_grad
            inc%vm_strain = cur_vm_strain
            inc%stress = homogenized_stress / total_weight
            inc%taylor_factor = taylor_factor / total_weight / vm_strain_inc

            increments(n_incs) = inc
        end do

        increments = increments(1:n_incs)
    end subroutine
end module
