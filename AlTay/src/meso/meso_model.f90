module meso_model
    use parameters
    use utils
    use cluster_module

    implicit none

    private
    public:: MesoModel

    type, abstract:: MesoModel
        real(DP), dimension(3, 3):: velocity_gradient   !> Current Velocity gradient applied during deformation steps.
                                                        !> Note that most model state variables are derived from this.
        real(DP), dimension(3, 3):: imposed_spin_rate   !> Current imposed spin rate derived from the velocity gradient.
    contains
        procedure, nopass:: get_parameters => meso_model_get_parameters
        procedure(meso_model_init), deferred:: init
        procedure(meso_model_get_stress), deferred:: get_stress
        procedure(meso_model_apply_step), deferred:: apply_step
        procedure:: prepare_deformation => meso_model_prepare_deformation
        procedure:: update => meso_model_update
        procedure:: finalize => meso_model_finalize
    end type

    interface
        !>@Brief initialize the mesoscopic model.
        !>@Details Initializes the mesoscopic model based on a list of initialized grains. These grains are arranged into clusters,
        !which also get initialized.
        !Note that we must make this a subroutine to avoid IFX copying the cluster list through the stack.
        subroutine meso_model_init(this, grains, params, clusters)
            import MesoModel
            import Parameter
            import Cluster
            import DP
            import Grain

            class(MesoModel), intent(inout):: this                          !> The mesoscopic model
            type(Grain), dimension(:), allocatable, intent(in):: grains          !>List of initialized grains to be arranged into clusters. Must be declared allocatable to ensure deep copy of allocatable components.
            type(Parameter), dimension(:), intent(in):: params !> List of parameters to initialize the model. These are
                                                                            !> defined by the model itself and can be retrieved by calling get_parameters on the model instance.
            class(Cluster), dimension(:), allocatable, intent(out):: clusters                !> List of initialized clusters with each a
        end subroutine

        !> @Brief Get the stress state for a cluster given some velocity gradient.
        !> @Details Returns the homogenized stress state over all of the grains of the cluster.
        function meso_model_get_stress(this, cluster_, v_grad) result(stress)
            import MesoModel
            import Cluster
            import DP

            class(MesoModel), intent(in):: this                     !> The mesoscopic model
            class(Cluster), target, intent(inout):: cluster_   !> Pointer to the cluster
            real(DP), dimension(3, 3), intent(in):: v_grad !> Velocity gradient for which to calculate the stress.
            real(DP), dimension(3, 3):: stress   !> Homogenized stress response of the cluster
        end function

        !> @Brief Apply a deformation step to a cluster
        !> @Details Applies a deformation step to a cluster and returns useful data about the deformation. Upon return, the cluster state is updated to the state after the
        !deformation step.
        subroutine meso_model_apply_step(this, cluster_, index_cluster, stress, slip)
            import MesoModel
            import Cluster
            import DP

            class(MesoModel), intent(in):: this !> The mesoscopic model
            class(Cluster), target, intent(inout):: cluster_ !> The cluster to apply the deformation step to. Upon entry, the
                                                                 !> cluster state must be consistent with the beginning of the time step. Upon exit, the cluster state corresponds to the
                                                                 !> end of the time step.
            integer, intent(in):: index_cluster                  !> Index of the cluster in the cluster list. To be removed.
            real(DP), dimension(3, 3), intent(out):: stress           !> Homogenized stress state of the cluster during the time step.
            real(DP), intent(out):: slip                        !> Total slip that occured in the cluster to realize the
                                                                !> deformation during this time step.
        end subroutine
    end interface

contains

    !>@Brief returns the list of parameters for the mesoscopic model.
    !>@Details The default implementation returns an empty (unallocated) list.
    function meso_model_get_parameters() result(params)
        type(Parameter), dimension(:), allocatable:: params !> List of parameters.

        allocate(params(0))
    end function

    !>@Brief Update the mesoscopic model
    !>@Details The default implementation does nothing.
    subroutine meso_model_update(this)
        class(MesoModel), intent(inout):: this !> The mesoscopic model
    end subroutine

    !>@Brief Finalize the mesoscopic model
    !>@Details The default implementation does nothing.
    subroutine meso_model_finalize(this)
        class(MesoModel), intent(in):: this !> The mesoscopic model.
    end subroutine

    !>@Brief Prepare the model for a deformation
    !>@Brief The default implementation sets the model velocity gradient and the imposed spin rate based on the provided velocity gradient.
    subroutine meso_model_prepare_deformation(this, v_grad)
        class(MesoModel), intent(inout):: this          !> The mesoscopic model to prepare for deformation
        real(DP), dimension(3, 3), intent(in):: v_grad !> Velocity gradient for the deformation being prepared

        this%velocity_gradient = v_grad
        this%imposed_spin_rate = antisymmetric_part(this%velocity_gradient)
    end subroutine
end module
