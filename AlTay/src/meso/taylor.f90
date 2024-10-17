module taylor
    use utils
    use hardening
    use taylor_ambiguity
    use altayConfig, only: astate
    use logging
    use simplex
    use slip_systems
    use grain_module
    use cluster_module
    use alamel
    use full_constraints_taylor

    implicit none

    private
    public ::   taylor_init, &
                get_stress_state, &
                apply_deformation_step, &
                meso_update_model, &
                meso_prepare_deformation


    real(DP), dimension(3, 3):: deformation_gradient, &
                               deformation_gradient_during_time_step, &
                               next_deformation_gradient, &
                               deformation_gradient_increment

    character(*), parameter:: MOD_NAME = 'taylor'

contains

    function taylor_init(deformation_mechanism, cluster_size, orientations, boundaries) result(clusters)
        integer, intent(in):: cluster_size
        class(Cluster), dimension(:), allocatable, target:: clusters
        integer, dimension(:,:,:), intent(in):: deformation_mechanism
        real(DP), dimension(:,:), allocatable, intent(in):: orientations, &
                                                            boundaries
        integer:: i

        if (cluster_size == 2) then
            clusters = alamel_init(orientations, deformation_mechanism, boundaries)
        else
            clusters = full_constraints_taylor_init(orientations, deformation_mechanism)
        end if
    end function

    subroutine meso_prepare_deformation(velocity_gradient, cluster_size)
        real(DP), dimension(3, 3), intent(in):: velocity_gradient
        integer, intent(in):: cluster_size

        if (cluster_size == 2) then
            call alamel_prepare_deformation(velocity_gradient)
        else
            call full_constraints_taylor_prepare_deformation(velocity_gradient)
        end if
    end subroutine

    subroutine meso_update_model(cluster_size)
        integer, intent(in):: cluster_size

        if (cluster_size == 2) then
            call alamel_update()
        end if
    end subroutine

    !>@Brief Get stress state for a cluster
    !>@details Calculate the homogenized stress over the cluster in the global frame
    function get_stress_state(cluster_ptr, velocity_gradient) result(stress)
        class(Cluster), pointer, intent(in):: cluster_ptr
        real(DP), dimension(3, 3), intent(in):: velocity_gradient
        real(DP), dimension(3, 3):: stress

        if (size(cluster_ptr%grains) == 2) then
            stress = alamel_get_stress(cluster_ptr, velocity_gradient)
        else
            stress = full_constraints_taylor_get_stress(cluster_ptr, velocity_gradient)
        end if
    end function

    subroutine apply_deformation_step(cluster_ptr, index_cluster, stress, slip)
        class(Cluster), pointer, intent(in):: cluster_ptr
        integer, intent(in)::       index_cluster
        real(DP), dimension(3, 3), intent(out):: stress                 !> Homogenized stress over the cluster
        real(DP), intent(out):: slip                                    !> Total slip in the cluster for this time step

        if (size(cluster_ptr%grains)==2) then
            call alamel_deform(cluster_ptr, index_cluster, stress, slip)
        else
            call full_constraints_taylor_deform(cluster_ptr, index_cluster, stress, slip)
        end if
    end subroutine
end module
