!> This module defines the interface every mesoscopic model must implement.

module crysp_meso_model
    use base_defs
    use conversions
    use crysp_cluster
    use mod_model

    implicit none

    private
    public:: MesoModel

    !> Base type for mesoscopic models.
    !>
    !> Declares common properties of all mesoscopic models.
    !> Concrete mesoscopic models must extend this base type.
    type, extends(Model), abstract:: MesoModel
    contains
        procedure(cp_model_init), deferred::       init                                                   !! Initialize the model.
        procedure(cp_model_get_stress), deferred:: get_stress                                             !! Get the stress state of a cluster under a certain strain condition.
        procedure(cp_model_apply_step), deferred:: apply_step                                             !! Apply a single deformation step to a single cluster.
        procedure::                                  update              => cp_model_update               !! Update the model state after a deformation step.
    end type

    abstract interface
        !> Initialize the mesoscopic model.
        !>
        !> Initializes the mesoscopic model based on a list of initialized grains.
        !> These grains are arranged into clusters, which also get initialized.
        !> @note
        !> We must make this a subroutine to avoid IFX copying the cluster list through the stack, leading to stack overflow/segfault.
        !> @endnote
        subroutine cp_model_init(this, grains, params, clusters)
            import MesoModel
            import Cluster
            import DP
            import Grain
            import Parameter

            class(MesoModel), intent(inout):: this                              !! Model instance
            type(Grain), dimension(:), intent(in):: grains                      !! List of initialized grains to be arranged into clusters.
            type(Parameter), dimension(:), intent(in):: params
            class(Cluster), dimension(:), allocatable, intent(out):: clusters   !! List of initialized clusters.
        end subroutine

        !> Get the stress state for a cluster under a certain strain condition.
        !>
        !> Returns the homogenized stress state over all of the grains of the cluster.
        function cp_model_get_stress(this, cluster_, v_grad) result(stress)
            import MesoModel
            import Cluster
            import DP

            class(MesoModel), intent(in):: this                !! Model instance
            class(Cluster), target, intent(inout):: cluster_   !! The cluster for which to calculate the homogenized stress state
            real(DP), dimension(3, 3), intent(in):: v_grad     !! Velocity gradient representing the strain condition for which to calculate the stress.
            real(DP), dimension(3, 3):: stress                 !! Homogenized stress response of the cluster
        end function

        !> Apply a deformation step to a cluster.
        !>
        !> Returns some statistics of the deformation to the caller.
        !> The cluster state is updated to the state after the deformation step.
        subroutine cp_model_apply_step(this, cluster_, velocity_gradient, time, stress, slip)
            import MesoModel
            import Cluster
            import DP

            class(MesoModel), intent(in):: this                 !! Model instance
            class(Cluster), target, intent(inout):: cluster_    !! The cluster to apply the deformation step to.
                                                                !! Upon entry, the cluster state must be consistent with the beginning of the time step.
                                                                !! Upon exit, the cluster state corresponds to the end of the time step.
            real(DP), dimension(3,3), intent(in):: velocity_gradient !! Velocity gradient to apply to the cluster
            real(DP), intent(in):: time                         !! Duration of the time step
            real(DP), dimension(3, 3), intent(out):: stress     !! Homogenized stress state of the cluster during the time step.
            real(DP), intent(out):: slip                        !! Homogenized slip that occured in the cluster to realize the deformation during this time step.
        end subroutine
    end interface

contains

    !> Update the mesoscopic model.
    !>
    !> The default implementation does nothing.
    subroutine cp_model_update(this, velocity_gradient, time)
        class(MesoModel), intent(inout):: this  !! Model instance
        real(DP), dimension(3,3), intent(in):: velocity_gradient !! Velocity gradient during the time step. Assumed constant.
        real(DP), intent(in):: time                              !! Duration of the time step.
    end subroutine

end module
