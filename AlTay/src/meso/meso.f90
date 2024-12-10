module meso
    use utils
    use parameters
    use cluster_module
    use meso_model
    use grain_module

    implicit none

    public

    !Supported mesoscopic models
    !Constants set to correspond to the number of grains in a cluster
    enum, bind(C)
        enumerator  ::  MESO_MODEL_FCTAYLOR       = 1,  &
                        MESO_MODEL_ALAMEL         = 2
    end enum

    interface

        !>@Brief Gets the parameters correspondding to a certain mesoscopic model
        module function meso_get_parameters(model_id) result(params)
            integer, intent(in):: model_id                         !> ID of the model for which to return the parameters
            type(Parameter), dimension(:), allocatable:: params    !> The list of parametes
        end function

        !> @brief Initialize a meso model
        !> @details Set the mesoscopic model to an instance of the hardening model defined by model_id and initialize it using a
        !list of parameters.
        !Note that we must make this a subroutine to avoid creations of temporaries passed through the stack by IFX.
        module subroutine meso_init(model_id, grains, params, clusters)
            integer, intent(in):: model_id          !> ID of the model to be initialized
            type(Grain), dimension(:), intent(in):: grains  !> Initialized grains to be distributed among the clusters
            type(Parameter), dimension(:), allocatable, intent(in):: params   !> List of parameters with which to initialize the model. Must correspond
                                                                 ! to the parameter list obtained by calling meso_get_parameters(model_id)
            class(Cluster), dimension(:), allocatable, intent(out):: clusters !> Innitialized clusters
        end subroutine

        !> @Brief prepares the model for a deformation according to a given velocity gradient
        !> @Details Updates model state variables to correspond to the selected velocity gradient. Useful because often several
        ! quantities are derived from the velocity gradient, which remains constant over all clusters and often many time steps.
        !Note to be called only when preparing for an actual deformation (not when probing stress state).
        module subroutine meso_prepare_deformation(velocity_gradient)
            real(DP), dimension(3, 3), intent(in):: velocity_gradient !> Velocity gradient for the current deformation.
        end subroutine

        !>@brief Get the stress state of a cluster when a certain velocity gradient is applied
        !>@details The stress state is homogenized over all of the grains of the cluster and presented in the macroscopic frame.
        module function meso_get_stress(cluster_, velocity_gradient) result(stress)
            class(Cluster), intent(inout):: cluster_   !> Pointer to cluster for which to calculate the stress state.
            real(DP), dimension(3, 3), intent(in):: velocity_gradient !> Velocity gradient to probe
            real(DP), dimension(3, 3):: stress                        !> Homogenized stress state of the cluster in the macroscopic
                                                                      !frame when deforming according to the given velocity gradient.
        end function

        !>@Brief Apply a deformation step to a cluster
        !>@Details Calculates and outputs the stress state and slip rates of the cluster during the time step and updates the
        !cluster state to that at the end of the time step.
        module subroutine meso_apply_deformation_step(cluster_, index_cluster, stress, slip)
            class(Cluster), intent(inout):: cluster_ !> Cluster to which to apply the deformation step
            integer, intent(in):: index_cluster               !> Index of the cluster in the cluster list (to be removed)
            real(DP), dimension(3, 3), intent(out):: stress              !> Homogenized stress state of the cluster during the time step
            real(DP), intent(out):: slip !> Total slip which occured in the cluster during the time step
        end subroutine

        !>@Brief Update the model state
        !>@Details Increments the model state after a time step has elapsed.
        module subroutine meso_update_model()
        end subroutine

        !>@Brief finalize the mesoscopic model.
        !>@Details Deallocate any pointers or allocatable data structures at the meso level.
        module subroutine meso_finalize()
        end subroutine
    end interface
end module

submodule(meso) meso_imp
    use logging
    use meso_model
    use full_constraints_taylor
    use alamel

    implicit none

    character(*), parameter:: MOD_NAME = "Meso"

    class(MesoModel), allocatable:: model

contains

    !> @Brief Return a list of IDs of all the supported mesoscopic models
    !> @Details The main purpose of this function is to avoid duplication of the hardcoded list of supported mesoscopic models.
    pure function get_model_ids() result(ids)
        integer, dimension(:), allocatable:: ids

        ids = [MESO_MODEL_FCTAYLOR, MESO_MODEL_ALAMEL]
    end function

    !>@Brief returns an instance of a mesoscopic model with the provided ID.
    !>@Details The main purpose of this function is to avoid duplication of the hard-coded link between model IDs and their types.
    function get_model_instance(id) result(m)
        integer, intent(in):: id            !> Numerical ID of the model. Must be contained in MESO_MODELS enum.
        class(MesoModel), allocatable:: m   !> The model instance

        select case (id)
            case (MESO_MODEL_FCTaylor)
                allocate(TaylorModel:: m)
            case (MESO_MODEL_ALAMEL)
                allocate(AlamelModel:: m)
            case default
                call log_error(MOD_NAME, "get_model_instance", ERR_VAL, "Invalid model ID")
        end select
    end function

    module procedure meso_get_parameters
        class(MesoModel), allocatable:: m

        !We must get an instance of the model if we want to exploit polymorphism in Fortran.
        m = get_model_instance(model_id)
        params = m%get_parameters()
    end procedure

    module procedure meso_init
        model = get_model_instance(model_id)
        call model%init(grains, params, clusters)
    end procedure

    module procedure meso_get_stress
        stress = model%get_stress(cluster_, velocity_gradient)
    end procedure

    module procedure meso_prepare_deformation
        call model%prepare_deformation(velocity_gradient)
    end procedure

    module procedure meso_apply_deformation_step
        call model%apply_step(cluster_, index_cluster, stress, slip)
    end procedure

    module procedure meso_update_model
        call model%update()
    end procedure

    module procedure meso_finalize
        call model%finalize()
        deallocate(model)
    end procedure
end submodule
