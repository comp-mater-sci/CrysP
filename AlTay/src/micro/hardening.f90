!> Dispatcher of hardening models
module Hardening
    use utils
    use parameters
    use logging

    implicit none
    public

    enum, bind(C)
        !> Constants set for backwards compatibility with input file format.
        enumerator  ::  HARDENING_NONE       = 0,  &
                        HARDENING_VOCE       = 1,  &
                        HARDENING_SWIFT      = 3,  &
                        HARDENING_BP         = 11, &
                        HARDENING_PEBP_SCREW = 12, &
                        HARDENING_PEBP_LOOP  = 13
    end enum

    interface
        !> Returns the parameter list for a particular hardening model
        !> Also allocates the back-end hardening model
        !> Must be called before initialization
        module function hardening_get_parameters(model_id) result(params)
            integer, intent(in)             :: model_id
            type(Parameter), allocatable    :: params(:)
        end function hardening_get_parameters

        !> Initialize module from config data object
        module subroutine  hardening_init(params)
            type(Parameter), allocatable, intent(in)  :: params(:)
        end subroutine

        module subroutine hardening_finalize()
        end subroutine hardening_finalize

        module function hardening_get_crss(grain, strain) result(crss)
            integer, intent(in):: grain
            real(DP), intent(in):: strain
            real(DP), allocatable:: crss(:,:)
        end function hardening_get_crss

        module subroutine hardening_update_state(grain, time, slip_rates)
            integer, intent(in)      ::  grain
            real(DP), intent(in)    ::  time, &
                                        slip_rates(:)
        end subroutine hardening_update_state
    end interface
end module hardening

submodule(hardening) hardening_imp
    use hardening_model
    use hardening_model_swift
    use hardening_model_voce
    use hardening_model_bp
    use hardening_model_pebp_screw
    use hardening_model_pebp_loop

    implicit none

    class(HardeningModel), allocatable:: model

contains

    module procedure hardening_get_parameters
        if (allocated(model)) deallocate(model)

        select case(model_id)
            case(HARDENING_NONE)
                allocate(HardeningModel:: model)
            case(HARDENING_VOCE)
                allocate(HardeningModelVoce:: model)
            case(HARDENING_SWIFT)
                allocate(HardeningModelSwift:: model)
            case(HARDENING_BP)
                allocate(HardeningModelBP:: model)
            case(HARDENING_PEBP_SCREW)
                allocate(HardeningModelPEBPScrew:: model)
            case(HARDENING_PEBP_LOOP)
                allocate(HardeningModelPEBPLoop:: model)
            case default
                call log_error('hardening', 'get_parameters', ERR_VAL, 'Invalid hardening model ID')
        end select

        params = model%get_parameters()
    end procedure hardening_get_parameters

    !> Initialize module from config data object
    module procedure hardening_init
        call model%init(params)
    end procedure hardening_init

    module procedure hardening_finalize
        call model%finalize()
    end procedure hardening_finalize

    module procedure hardening_get_crss
        crss = model%get_crss(grain, strain)
    end procedure

    module procedure hardening_update_state
        call model%update_state(grain, time, slip_rates)
    end procedure
end submodule Hardening_Imp
