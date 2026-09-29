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
    use crysp_cluster
    use crysp_meso_model
    use crysp_grain
    use crysp_model
    use crysp_parameters
    use crysp_input
    use crysp_status
    use conversions

    implicit none

    public

    !> Supported mesoscopic models.
    !>
    !> Refer to the documentation of the implementation each model for details on the model itself as well as its parameters.
    enum, bind(C)
        enumerator::  MESO_MODEL_FCTAYLOR       = 1 !! Full-Constraints Taylor.
        enumerator::  MESO_MODEL_ALAMEL         = 2 !! Advanced LAMEL model.
    end enum

    !> All supported mesoscopic models. Must list every enumerator above.
    integer, dimension(*), parameter:: SUPPORTED_MESO_MODELS = [MESO_MODEL_FCTAYLOR, MESO_MODEL_ALAMEL]

    interface

        !> Initialize the mesoscopic level of the simulation.
        !>
        !> To be called only after the microscopic level has been initialized.
        !>
        !> Set the mesoscopic model for the simulation and initialize it. Also transform the unstructured list of grains resulting
        !> from the initialization of the micro level into a list of local clusters, which are also initialized.
        !>
        !> The model ID comes from the caller, so an unknown one is reported as `CRYSP_ERR_VAL` rather than terminating.
        !>
        !> @note
        !> The clusters and the model are returned through `intent(out)` arguments rather than as the function result, to avoid
        !> creation of temporaries passed through the stack by IFX, which may lead to a stack overflow/segfault. Only the
        !> [[crysp_status:Status]] is returned by value.
        !> @endnote
        module function meso_init(model_data, grains, model, clusters) result(stat)
            type(ModelData), intent(in):: model_data                               !! ID of the model to be initialized. Must exist in the enum defined in this module.
            type(Grain), dimension(:), intent(in):: grains  !! Initialized grains to be distributed among the clusters.
            class(MesoModel), allocatable, intent(out):: model                !! The initialized mesoscopic model.
            class(Cluster), dimension(:), allocatable, intent(out):: clusters !! Initialized clusters which form the unit of
                                                                              !! simulation at the mesoscopic level.
            type(Status):: stat                                               !! `CRYSP_OK`, or `CRYSP_ERR_VAL` if the model ID is unknown.
        end function

        module function meso_get_model(id, model) result(stat)
            integer, intent(in):: id
            class(MesoModel), allocatable, intent(out):: model
            type(Status):: stat
        end function
    end interface
end module

!> Implementation of the interface declared in the meso module.
!>
!> Links the different model IDs to specific mesoscopic models and keeps a reference to the particular model currently in use.
submodule(meso) meso_imp
    use crysp_meso_model
    use full_constraints_taylor
    use alamel

    implicit none

    character(*), parameter:: MOD_NAME = 'meso'

contains

    !> Returns an instance of a mesoscopic model with the provided ID.
    !>
    !> Avoids duplication of the hard-coded link between model IDs and their types.
    module procedure meso_get_model
        select case (id)
            case (MESO_MODEL_FCTaylor)
                allocate(TaylorModel:: model)
            case (MESO_MODEL_ALAMEL)
                allocate(AlamelModel:: model)
            case default
                stat = fail(MOD_NAME, 'meso_get_model', CRYSP_ERR_VAL, 'Invalid crystal plasticity model ID: '// int_to_str(id))
                return
        end select

        model%id = id
    end procedure

    !> See interface definition in meso module.
    module procedure meso_init
        stat = meso_get_model(model_data%id, model)
        if (failed(stat)) return

        call model%init(grains, model_data%parameters, clusters)
    end procedure
end submodule
