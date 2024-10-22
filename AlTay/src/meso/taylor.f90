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

    !Have to use a subroutine here because IFX always copies the polymorphic function results to the stack when assigning them to a
    !variable in the calling module. Because the cluster list can be quite large this leads to segmentation faults.
    subroutine taylor_init(deformation_mechanism, cluster_size, orientations, boundaries, clusters)
        integer, intent(in):: cluster_size
        integer, dimension(:,:,:), intent(in):: deformation_mechanism
        real(DP), dimension(:,:), allocatable, intent(in):: orientations, &
                                                            boundaries
        class(Cluster), dimension(:), allocatable, intent(out):: clusters

        if (cluster_size == 2) then
            call alamel_init(orientations, deformation_mechanism, boundaries, clusters)
        else
            call full_constraints_taylor_init(orientations, deformation_mechanism, clusters)
        end if
    end subroutine

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
    function get_stress_state(cluster_, velocity_gradient) result(stress)
        class(Cluster), target, intent(inout):: cluster_
        real(DP), dimension(3, 3), intent(in):: velocity_gradient
        real(DP), dimension(3, 3):: stress

        if (size(cluster_%grains) == 2) then
            stress = alamel_get_stress(cluster_, velocity_gradient)
        else
            stress = full_constraints_taylor_get_stress(cluster_, velocity_gradient)
        end if
    end function

    subroutine apply_deformation_step(cluster_, index_cluster, stress, slip)
        class(Cluster), target, intent(inout):: cluster_
        integer, intent(in)::       index_cluster
        real(DP), dimension(3, 3), intent(out):: stress                 !> Homogenized stress over the cluster
        real(DP), intent(out):: slip                                    !> Total slip in the cluster for this time step

        if (size(cluster_%grains)==2) then
            call alamel_deform(cluster_, index_cluster, stress, slip)
        else
            call full_constraints_taylor_deform(cluster_, index_cluster, stress, slip)
        end if
    end subroutine
end module
