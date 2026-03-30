module deformation
    use base_defs
    use conversions
    use cluster_module
    use omp_lib
    use meso

    implicit none

    private
    public:: deform


contains

    subroutine deform(clusters, velocity_gradient, stress, taylor_factor)
        class(Cluster), dimension(:), intent(inout):: clusters
        real(DP), dimension(3,3), intent(in):: velocity_gradient
        real(DP), dimension(3,3), intent(out):: stress
        real(DP), intent(out):: taylor_factor

        integer:: n_clusters, &
                  i
        real(DP):: total_weight, &
                   homogenized_stress(3, 3), &
                   deformation_gradient_increment(3, 3), &
                   stress_cluster(3, 3), &
                   slip_cluster, &
                   weight_cluster, &
                   t_inc                                    ! Time increment

        !Check if the velocity gradient is purely deviatoric
        if (abs(velocity_gradient(1,1) + velocity_gradient(2,2)+ velocity_gradient(3,3)) > TOLERANCE) &
            call log_error(MOD_NAME, 'simulation_run', ERR_VAL, 'The velocity gradient must be purely deviatoric')


        n_clusters = size(clusters)
        t_inc = 1._DP

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

        stress = homogenized_stress / total_weight
        taylor_factor = taylor_factor / total_weight / strain_tensor_to_von_mises((velocity_gradient + transpose(velocity_gradient))/2._DP)
    end subroutine


end module
