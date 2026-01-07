module simulation
    use base_defs
    use conversions
    use grain_module
    use altayConfig
    use logging
    use cluster_module
    use file_io
    use omp_lib
    use meso
    use parameters

    implicit none
    private

    class(Cluster), dimension(:), allocatable:: clusters

    character(*), parameter:: MOD_NAME = 'Simul'


    public:: macro_init, &
             simulation_run, &
             simulation_finalize, &
             output_current_state, &
             get_stress
    contains

    ! initialization call
    subroutine macro_init(clstrs)
        class(Cluster), dimension(:), allocatable, intent(inout):: clstrs

        call move_alloc(clstrs, clusters)
    end subroutine

    function get_stress(velocity_gradient) result(homogenized_stress)
        real(DP), dimension(3, 3), intent(in):: velocity_gradient
        real(DP), dimension(3, 3):: homogenized_stress

        integer:: i, &
                  n_clusters, &
                  cluster_size
        real(DP):: total_weight

        n_clusters = size(clusters)
        cluster_size = size(clusters(1)%grains)
        homogenized_stress = 0._DP
        total_weight = 0._DP

        !$OMP PARALLEL SHARED(clusters, velocity_gradient, n_clusters)
            !$OMP DO SCHEDULE(DYNAMIC, 1) REDUCTION (+:total_weight, homogenized_stress)
                do i = 1, n_clusters
                    homogenized_stress = homogenized_stress+meso_get_stress(clusters(i), velocity_gradient) * clusters(i)%weight
                    total_weight = total_weight+clusters(i)%weight
                end do
            !$OMP END DO
        !$OMP END PARALLEL

        homogenized_stress = homogenized_stress/total_weight
    end function

    subroutine simulation_run(velocity_gradient, taylor_factor)
        real(DP), intent(in):: velocity_gradient(3, 3)
        real(DP), intent(out):: taylor_factor           !! Homoginized taylor factor over all grains.

        integer:: i
        real(DP):: total_weight, &
                   homogenized_stress(3, 3), &
                   deformation_gradient_increment(3, 3), &
                   stress_cluster(3, 3), &
                   slip_cluster, &
                   weight_cluster, &
                   t_inc                                    ! Time increment

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

        astate%simulCalls(astate%this)%stress = homogenized_stress / total_weight
        taylor_factor = taylor_factor / total_weight / strain_tensor_to_von_mises((velocity_gradient + transpose(velocity_gradient))/2._DP)
    end subroutine

    subroutine output_current_state()
        call cur_write_block(clusters)
    end subroutine

    subroutine simulation_finalize()
        call meso_finalize()
        deallocate(clusters)
    end subroutine
end module
