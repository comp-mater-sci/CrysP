module hardening_model
    use definitions, only: dp
    use altayConfig
    use parameters
    use logging
    
    implicit none
    private

    !>Basic hardening model implementation.
    !>No hardening occurs.
    !>All other hardening models extend this type.   
    type, public :: HardeningModel
        integer :: nss
    contains
        procedure :: get_parameters      => hardening_model_get_parameters
        procedure :: validate_parameters => hardening_model_validate_parameters 
        procedure :: init                => hardening_model_init
        procedure :: get_crss            => hardening_model_get_crss
        procedure :: finalize            => hardening_model_finalize
        procedure :: update_state            => hardening_model_update_state
    end type

    public ::   hardening_model_validate_parameters,    &   
                hardening_model_init

contains
    
    function hardening_model_get_parameters(this) result(params)
        class(HardeningModel), intent(in)       :: this
        type(Parameter), allocatable    :: params(:)
    
        params = [parameter_init('n_slip_systems', TYPE_STRING)]
    end function hardening_model_get_parameters

    subroutine hardening_model_validate_parameters(this, params)
        class(HardeningModel), intent(in)   :: this
        type(Parameter), allocatable, intent(in) :: params(:)
        character(:), allocatable :: n_slip_systems

        n_slip_systems = params .find. 'n_slip_systems'

        select case(n_slip_systems)
            case('fcc12', 'bcc24', 'bcc48') 
                continue
            case default
                call log_error('hardening_model', 'validate_parameters', ERR_VAL, 'Number of slip systems must be 12, 24 or 48')
        end select    
    end subroutine hardening_model_validate_parameters

    subroutine hardening_model_init(this, params)
        class(HardeningModel), intent(inout)     ::  this
        type(Parameter), allocatable, intent(in) :: params(:)
        character(:), allocatable                :: slip_system

        slip_system = params .find. 'n_slip_systems'
        
        select case(slip_system)
            case('fcc12')
                this%nss = 12
            case('bcc24')
                this%nss = 24
            case('bcc48')
                this%nss = 48
        end select
    end subroutine hardening_model_init

    function hardening_model_get_crss(this, grain, strain) result(crss)
        class(HardeningModel), intent(in)                ::  this
        integer, intent(in)                                 ::  grain
        real(DP), intent(in) ::                            strain
        real(DP), dimension(2, this%nss)                :: crss 

        crss = 1._DP
    end function

    !>Do nothing
    subroutine hardening_model_update_state(this, grain, time, slip_rates)
        class(HardeningModel), intent(inout)    :: this
        integer, intent(in)                                 ::  grain
        real(DP), intent(in)                                ::  time
        real(DP), dimension(this%nss), intent(in) ::  slip_rates
    end subroutine 

    !>Do nothing
    subroutine hardening_model_finalize(this)
        class(HardeningModel), intent(inout)    :: this
    end subroutine 
end module hardening_model
