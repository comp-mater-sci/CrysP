!> Top-level module at the meso scale.
!>
!> The mesoscopic scale refers in our case to a small, local cluster of grains. This allows the simulation of interactions between
!> neighboring grains under deformation, which defines the local material response to the deformation.
!>
!> This module merely serves as a generic interface between the mesoscopic and macroscopic layer and does not contain any implementation.
!> It does however define which mesoscopic models are supported. These models implement the logic needed for the interface defined here.

module meso
    use utils
    use parameters
    use cluster_module
    use meso_model
    use grain_module

    implicit none

    public

    !> Supported mesoscopic models
    !> Constants set to correspond to the number of grains in the clusters the models use.
    enum, bind(C)
        enumerator  ::  MESO_MODEL_FCTAYLOR       = 1,  & !! Traditional full-constraints Taylor model.
                        MESO_MODEL_ALAMEL         = 2     !! ALAMEL model developed at KU Leuven.
    end enum

    interface

        !> Gets the parameters corresponding to a certain mesoscopic model
        !>
        !> The ID must exist in the enum defined in this module. If not, this routine crashes the program.
        module function meso_get_parameters(model_id) result(params)
            integer, intent(in):: model_id                         !! ID of the model for which to return the parameters.
            type(Parameter), dimension(:), allocatable:: params    !! The list of parameters for the specified model.
        end function

        !> Initialize the mesoscopic level of the simulation.
        !>
        !> To be called only after the microscopic level has been initialized.
        !>
        !> Set the mesoscopic model for the simulation and initialize it. Also transform the unstructured list of grains resulting
        !> from the initialization of the micro level into a list of local clusters, which are also initialized.
        !>
        !> If the provided model ID is not contained in the enum provided in this module or the list of parameters is not properly
        !> initialized, this routine crashes the program.
        !>
        !> @note
        !> We must make this a subroutine to avoid creations of temporaries passed through the stack by IFX, which may lead to a
        !> stack overflow/segfault.
        !> @endnote
        module subroutine meso_init(model_id, grains, params, clusters)
            integer, intent(in):: model_id                               !! ID of the model to be initialized. Must exist in the enum defined in this module.
            type(Grain), dimension(:), intent(in):: grains  !! Initialized grains to be distributed among the clusters.
            type(Parameter), dimension(:), intent(in):: params   !! List of parameters with which to initialize the model. Must correspond
                                                                              !! to the parameter list obtained by calling
                                                                              !! meso_get_parameters(model_id) and be properly initialized.
            class(Cluster), dimension(:), allocatable, intent(out):: clusters !! Initialized clusters which form the unit of
                                                                              !! simulation at the mesoscopic level.
        end subroutine

        !> Prepare for a deformation according to a given velocity gradient.
        !>
        !> Allows meso models to update state variables to correspond to the selected velocity gradient. Useful because often several
        !> quantities are derived from the velocity gradient, which remains constant over all clusters and often many time steps.
        module subroutine meso_prepare_deformation(velocity_gradient)
            real(DP), dimension(3, 3), intent(in):: velocity_gradient !! Velocity gradient for the upcoming deformation.
        end subroutine

        !> Get the stress state of a cluster when a certain velocity gradient is applied.
        !>
        !> The cluster is not deformed.
        !> The stress state is homogenized over all of the grains of the cluster and presented in the macroscopic frame.
        module function meso_get_stress(cluster_, velocity_gradient) result(stress)
            class(Cluster), intent(inout):: cluster_                  !! Cluster for which to calculate the stress state.
            real(DP), dimension(3, 3), intent(in):: velocity_gradient !! Velocity gradient to probe.
            real(DP), dimension(3, 3):: stress                        !! Homogenized stress state of the cluster in the macroscopic
                                                                      !! frame when deforming according to the given velocity gradient.
        end function

        !> Apply a deformation step to a cluster.
        !>
        !> Calculates and outputs the stress state and slip rates of the cluster during the time step.
        !> All quantities are assumed constant during the time step.
        !> On return, the cluster state is updated to correspond to the end of the time step.
        module subroutine meso_apply_deformation_step(cluster_, stress, slip)
            class(Cluster), intent(inout):: cluster_        !! Cluster to deform
            real(DP), dimension(3, 3), intent(out):: stress !! Homogenized stress state of the cluster during the time step in the
                                                            !! macroscopic frame.
            real(DP), intent(out):: slip                    !! Total slip which occured in the cluster during the time step
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
        call model%apply_step(cluster_, stress, slip)
    end procedure

    module procedure meso_update_model
        call model%update()
    end procedure

    module procedure meso_finalize
        call model%finalize()
        deallocate(model)
    end procedure
end submodule
