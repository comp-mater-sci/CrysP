!> Top-level module at the meso scale.
!>
!> The mesoscopic scale refers in our case to a small, local cluster of grains. This allows the simulation of interactions between
!> neighboring grains under deformation, which defines the local material response to the deformation.
!>
!> This module merely serves as a generic interface between the mesoscopic and macroscopic layer and does not contain any implementation.
!> It does however define which mesoscopic models are supported. These models implement the logic needed for the interface defined here.
!> Higher-level modules can query this module for all of the information they need regarding the supported mesoscopic models, which
!> parameters they use etc. In this way, no hard-coded information about the available models needs to be kept at all at higher
!> levels.

module meso
    use base_defs
    use parameters
    use cluster_module
    use meso_model
    use grain_module

    implicit none

    public

    !> Supported mesoscopic models.
    !>
    !> Refer to the documentation of the implementation each model for details on the model itself as well as its parameters.
    enum, bind(C)
        enumerator::  MESO_MODEL_FCTAYLOR       = 1 !! Full-Constraints Taylor.
        enumerator::  MESO_MODEL_ALAMEL         = 2 !! Advanced LAMEL model.
    end enum

    interface
!
!        !> Gets the parameters corresponding to a certain mesoscopic model
!        !>
!        !> The ID must exist in the enum defined in this module. If not, this routine crashes the program.
!        module function meso_get_parameters(model_id) result(params)
!            integer, intent(in):: model_id                         !! ID of the model for which to return the parameters.
!            type(Parameter), dimension(:), allocatable:: params    !! The list of parameters for the specified model.
!        end function
!
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
        module subroutine meso_init(model_id, grains, params, model, clusters)
            integer, intent(in):: model_id                               !! ID of the model to be initialized. Must exist in the enum defined in this module.
            type(Grain), dimension(:), intent(in):: grains  !! Initialized grains to be distributed among the clusters.
            character(*), intent(in):: params   !! List of parameters with which to initialize the model. Must correspond
                                                                              !! to the parameter list obtained by calling
                                                                              !! meso_get_parameters(model_id) and be properly initialized.
            class(MesoModel), allocatable, intent(out):: model                !! The initialized mesoscopic model.
            class(Cluster), dimension(:), allocatable, intent(out):: clusters !! Initialized clusters which form the unit of
                                                                              !! simulation at the mesoscopic level.
        end subroutine
    end interface
end module

!> Implementation of the interface declared in the meso module.
!>
!> Links the different model IDs to specific mesoscopic models and keeps a reference to the particular model currently in use.
submodule(meso) meso_imp
    use logging
    use meso_model
    use full_constraints_taylor
    use alamel

    implicit none

contains

    !> Return a list of IDs of all the supported mesoscopic models
    pure function get_model_ids() result(ids)
        integer, dimension(:), allocatable:: ids !! List of supported mesoscopic model IDs.

        ids = [MESO_MODEL_FCTAYLOR, MESO_MODEL_ALAMEL]
    end function

    !> Returns an instance of a mesoscopic model with the provided ID.
    !>
    !> Avoids duplication of the hard-coded link between model IDs and their types.
    function get_model_instance(id) result(m)
        integer, intent(in):: id            !! Numerical ID of the model. Must be contained in MESO_MODELS enum.
        class(MesoModel), allocatable:: m   !! The model instance

        select case (id)
            case (MESO_MODEL_FCTaylor)
                allocate(TaylorModel:: m)
            case (MESO_MODEL_ALAMEL)
                allocate(AlamelModel:: m)
            case default
                call log_error("Meso", "get_model_instance", ERR_VAL, "Invalid model ID")
        end select
    end function

    !> See interface definition in meso module.
    module procedure meso_get_parameters
        class(MesoModel), allocatable:: m

        !We must get an instance of the model if we want to exploit polymorphism in Fortran.
        m = get_model_instance(model_id)
        params = m%get_parameters()
    end procedure

    !> See interface definition in meso module.
    module procedure meso_init
        model = get_model_instance(model_id)
        call model%init(grains, params, clusters)
    end procedure
end submodule
