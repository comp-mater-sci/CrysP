module simulation
    use utils
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

    real(DP):: von_mises_strain
    real(DP), dimension(3, 3):: deformation_gradient = UNIT_MATRIX_3X3
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
        class(Cluster), dimension(:), allocatable, intent(inout):: clstrs !> Must be declared allocatable because only then deep copy of allocatable components is mandated by the standard.

        call move_alloc(clstrs, clusters)
        deformation_gradient = UNIT_MATRIX_3X3
        von_mises_strain = 0._DP
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

    subroutine simulation_run(velocity_gradient)
        real(DP), intent(in):: velocity_gradient(3, 3)
        integer:: cluster_size, &
                  n_clusters, &
                  i               !> Iteration variables
        real(DP):: total_weight, &
                   homogenized_stress(3, 3), &
                   homogenized_taylor_factor, &   !Volume-averaged Taylor factor
                   deformation_gradient_increment(3, 3), &
                   strain_rate(3, 3), &
                   spin(3, 3), &
                   von_mises_strain_rate, &
                   stress_cluster(3, 3), &
                   slip_cluster, &
                   weight_cluster

        n_clusters = size(clusters)
        cluster_size = acnf%simul_init%NGR

        strain_rate = symmetric_part(velocity_gradient)
        spin = antisymmetric_part(velocity_gradient)
        von_mises_strain_rate = SQR0P67*norm2(strain_rate)
        deformation_gradient_increment = matrix_exponential_small_norm(velocity_gradient)
        call meso_prepare_deformation(velocity_gradient)

        total_weight = 0._DP
        homogenized_stress = 0._DP
        homogenized_taylor_factor = 0._DP

        !$OMP PARALLEL SHARED(clusters, n_clusters, von_mises_strain_rate) PRIVATE(stress_cluster, slip_cluster, weight_cluster)
            !$OMP DO SCHEDULE(DYNAMIC, 1) REDUCTION(+:total_weight, homogenized_stress, homogenized_taylor_factor)
                do i = 1, n_clusters
                    call meso_apply_deformation_step(clusters(i), i, stress_cluster, slip_cluster)
                    weight_cluster = clusters(i)%weight
                    total_weight = total_weight+weight_cluster
                    homogenized_stress = homogenized_stress+stress_cluster*weight_cluster
                    homogenized_taylor_factor = homogenized_taylor_factor+slip_cluster/von_mises_strain_rate*weight_cluster
                end do
            !$OMP END DO
        !$OMP END PARALLEL

        homogenized_stress = homogenized_stress/total_weight
        homogenized_taylor_factor = homogenized_taylor_factor/total_weight

        ! Get the homogenized quantities:
        associate (callout => astate%simulCalls(astate%this)%output)
            callout%stress_tensor = homogenized_stress
            callout%taylor_factor = homogenized_taylor_factor
            callout%effective_stress = sqrt(3.D0/2.D0)*norm2(homogenized_stress)
            callout%effective_macro_strain_tot = von_mises_strain
            callout%effective_macro_strain_tot_end = von_mises_strain+von_mises_strain_rate
        end associate

        von_mises_strain = von_mises_strain+von_mises_strain_rate
        deformation_gradient = matmul(deformation_gradient_increment, deformation_gradient)
        call meso_update_model()
    end subroutine

    subroutine output_current_state(file_handle)
        integer, intent(in):: file_handle

        call cur_write_block(file_handle, clusters, deformation_gradient)
    end subroutine

    subroutine simulation_finalize()
        call meso_finalize()
        deallocate(clusters)
    end subroutine
end module
