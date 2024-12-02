!> Dispatcher of hardening models
module micro
    use utils
    use parameters
    use logging

    implicit none
    public

    enum, bind(C)
        !> Constants set for backwards compatibility with input file format.
        enumerator  ::  HARDENING_NONE      = 0,  &
                        HARDENING_VOCE      = 1,  &
                        HARDENING_SWIFT     = 3,  &
                        HARDENING_DSH_EDGE  = 11, &
                        HARDENING_DSH_SCREW = 12, &
                        HARDENING_DSH_LOOP  = 13
    end enum

    interface
        !> Returns the parameter list for a particular hardening model
        !> Also allocates the back-end hardening model
        !> Must be called before initialization
        module function micro_get_parameters(model_id) result(params)
            integer, intent(in)             :: model_id
            type(Parameter), allocatable    :: params(:)
        end function

        !> Initialize module from config data object
        module subroutine  micro_init(params)
            type(Parameter), allocatable, intent(in)  :: params(:)
        end subroutine

        module subroutine micro_finalize()
        end subroutine

        module function micro_get_crss(grain, sum_slip) result(crss)
            integer, intent(in):: grain
            real(DP), intent(in):: sum_slip
            real(DP), allocatable:: crss(:,:)
        end function

        module subroutine micro_update_state(grain, time, slip_rates)
            integer, intent(in)      ::  grain
            real(DP), intent(in)    ::  time, &
                                        slip_rates(:)
        end subroutine
    end interface
end module

submodule(micro) micro_imp
    use hardening_model
    use swift
    use voce
    use dsh_edge
    use dsh_screw
    use dsh_loop

    implicit none

    class(HardeningModel), allocatable:: model

contains

    module procedure micro_get_parameters
        if (allocated(model)) deallocate(model)

        select case(model_id)
            case(HARDENING_NONE)
                allocate(HardeningModel:: model)
            case(HARDENING_VOCE)
                allocate(HardeningModelVoce:: model)
            case(HARDENING_SWIFT)
                allocate(HardeningModelSwift:: model)
            case(HARDENING_DSH_EDGE)
                allocate(HardeningModelDSHEdge:: model)
            case(HARDENING_DSH_SCREW)
                allocate(HardeningModelDSHScrew:: model)
            case(HARDENING_DSH_LOOP)
                allocate(HardeningModelDSHLoop:: model)
            case default
                call log_error('hardening', 'get_parameters', ERR_VAL, 'Invalid hardening model ID')
        end select

        params = model%get_parameters()
    end procedure

    !> Initialize module from config data object
    module procedure micro_init
        call model%init(params)
    end procedure

    module procedure micro_finalize
        call model%finalize()
    end procedure

    module procedure micro_get_crss
        crss = model%get_crss(grain, sum_slip)
    end procedure

    module procedure micro_update_state
        call model%update_state(grain, time, slip_rates)
    end procedure
end submodule
