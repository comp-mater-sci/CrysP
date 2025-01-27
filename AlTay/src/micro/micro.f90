!> Dispatcher of hardening models
module micro
    use utils
    use parameters
    use logging
    use grain_module

    implicit none
    public

    !> Supported hardening models.
    enum, bind(C)
        !> Constants set for backwards compatibility with input file format.
        enumerator:: HARDENING_NONE       = 0,  &
                     HARDENING_VOCE       = 1,  &
                     HARDENING_SWIFT      = 3,  &
                     HARDENING_BP         = 11, &
                     HARDENING_PEBP_SCREW = 12, &
                     HARDENING_PEBP_LOOP  = 13
    end enum

    interface
        !>@Brief Returns the parameter list for a particular hardening model.
        !>@Details The parameters are used to initialize the hardening model. If no valid ID is provided, the procedure crashes the program.
        module function micro_get_parameters(model_id) result(params)
            integer, intent(in)::          model_id !> ID of the hardening model. Valid IDs are listed above.
            type(Parameter), allocatable:: params(:) !> List of parameters for the hardening model corresponding to the provided ID.
        end function

        !> @Brief Check if a list of initialized parameters is valid for a given hardening model.
        !> @Details If the parameters do not meet the specified constraints, this routine crashes the program.
        !!          Each hardening model is free do define additional constraints on the parameters. Refer to the documentation of the
        !!          individual hardening models for details.
        module subroutine micro_validate_parameters(model_id, params)
            integer, intent(in):: model_id                             !> ID of the hardening model to be initialized. Must
                                                                       !! exist in the enum list provided in this module.
            type(Parameter), dimension(:), target, intent(in):: params !> List of initialized parameters to be validated. \
                                                                       !! Must correspond to the parameter list
                                                                       !! obtained by calling micro_get_parameters(model_id)
        end subroutine

        !>@Brief Initialize the micro-level entities of the simulation: The hardening model and the grains.
        !>@Details If the parameters do not meet the specified constraints, this procedure crashes the program.
        !!         Must be subroutine to avoid problems with IFX copying too much to the stack.
        module subroutine  micro_init(model_id, orientations, deformation_mechanism, params, grains)
            integer, intent(in):: model_id                                      !> ID of the hardening model to be initialized. Must
                                                                                !! exist in the enum list provided in this module.
            real(DP), dimension(:,:), intent(in):: orientations                 !> List of Euler angle triplets in Bunge convention
                                                                                !> in the macroscopic frame representing grain orientations.
            integer, dimension(:,:,:), intent(in):: deformation_mechanism       !> Deformation mechanism to be employed for all grains.
            type(Parameter), dimension(:), target, intent(in):: params          !> Parameters used to initialize the hardening
                                                                                !! model. Must correspond to the parameter list
                                                                                !! obtained by calling micro_get_parameters(model_id)
                                                                                !! and must pass
                                                                                !! micro_validate_parameters(model_id).
            type(Grain), dimension(:), allocatable, intent(out):: grains        !> List of initialized grain objects.
        end subroutine


        !>@Brief Update the critical resolved shear stresses (CRSS) of the grain.
        !>@Details Calculates the evolution of the CRSS on each slip system given the slip rate on each slip system and the elapsed
        !time since the last update of the CRSS. The slip rates are assumed constant during the time interval.
        module subroutine micro_update_crss(grain_, time, slip_rates)
            type(Grain), intent(in)::  grain_        !> Grain for which to update the CRSS.
            real(DP), intent(in)::     time, &       !> Elapsed time since last update of the CRSS for this grain.
                                       slip_rates(:) !> Slip rate for each slip system of the grain. Size(slip_rates) must equal
                                                     !> the number of slip systems in the grain.
        end subroutine
    end interface
end module

submodule(micro) micro_imp
    use hardening_model

    implicit none

    class(HardeningModel), allocatable:: model

contains

    !>@Brief Retrieve an unitialized instance of a given hardening model.
    !>@Details Workaround to be able to call type-bound overriden procedures.
    !>@return Uninitialized instance of the requested hardening model.
    function get_model_instance(model_id) result(instance)
        use hardening_model_none
        use hardening_model_swift
        use hardening_model_voce
        use hardening_model_bp
        use hardening_model_pebp_screw
        use hardening_model_pebp_loop

        integer, intent(in):: model_id                              !> ID of the hardening model. Must be contained in the list provided in this
                                                                    !!  module. If not, this routine crashes the program.
        class(HardeningModel), allocatable:: instance               !> Uninitialized instance of the requested hardening model.

        select case(model_id)
            case(HARDENING_NONE)
                allocate(HardeningModelNone:: instance)
            case(HARDENING_VOCE)
                allocate(HardeningModelVoce:: instance)
            case(HARDENING_SWIFT)
                allocate(HardeningModelSwift:: instance)
            case(HARDENING_BP)
                allocate(HardeningModelBP:: instance)
            case(HARDENING_PEBP_SCREW)
                allocate(HardeningModelPEBPScrew:: instance)
            case(HARDENING_PEBP_LOOP)
                allocate(HardeningModelPEBPLoop:: instance)
            case default
                call log_error('micro', 'micro_get_parameters', ERR_VAL, 'Invalid hardening model ID')
        end select
    end function

    module procedure micro_get_parameters
        class(HardeningModel), allocatable:: dummy_instance

        dummy_instance = get_model_instance(model_id)
        params = dummy_instance%get_parameters()
    end procedure

    module procedure micro_validate_parameters
        class(HardeningModel), allocatable:: dummy_instance

        dummy_instance = get_model_instance(model_id)
        params = dummy_instance%validate_parameters()
    end procedure

    module procedure micro_init
        model = get_model_instance(model_id)
        call model%init(params)
    end procedure


    module procedure micro_init
        integer:: i, &
                  n_grains

        n_grains = size(orientations, 2)

        allocate(grains(n_grains))

        do i = 1, n_grains
            call grains(i)%init(deformation_mechanism, orientations(:,i))
        end do

        call model%init(grains, params)

        do i = 1, n_grains
            call grains(i)%set_crss(micro_get_crss(i, 0._DP))
        end do
    end procedure

    module procedure micro_update_crss
        call model%update_crss(grain, time, slip_rates)
    end procedure
end submodule
