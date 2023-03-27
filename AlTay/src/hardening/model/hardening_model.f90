module hardening_model
    use altay_definitions, only: dp
    use altayConfig
    use parameters
    use altay_log
    
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
        procedure :: update              => hardening_model_update
        procedure :: finalize            => hardening_model_finalize
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
            case('FCC12', 'BCC24', 'BCC48') 
                continue
            case default
                call vef_exception('hardening_model', 'validate_parameters', VEF_BADVAL, 'Number of slip systems must be 12, 24 or 48')
        end select    
    end subroutine hardening_model_validate_parameters

    !>Initialize nss using params(1)
    !>@return the number of parameters used by this class
    subroutine hardening_model_init(this, config)
        class(HardeningModel), intent(inout)    ::  this
        type(HardeningData), intent(in)             ::  config

        this%nss = 96
    end subroutine hardening_model_init

    !>Do nothing
    subroutine hardening_model_update(this, grain, time, strain, slip_rates)
        class(HardeningModel), intent(inout)                ::  this
        integer, intent(in)                                 ::  grain
        real(dp), intent(in)                                ::  time, &
                                                                strain
        real(dp), dimension(this%nss), intent(in) ::  slip_rates
    end subroutine

    !>All values 1.0
    function hardening_model_get_crss(this, grain) result(crss)
        class(HardeningModel), intent(in)               :: this
        integer, intent(in)                             :: grain
        real(dp), dimension(2, this%nss)                :: crss 

        crss = 1.D0
    end function

    !>Do nothing
    subroutine hardening_model_finalize(this)
        class(HardeningModel), intent(inout)    :: this
    end subroutine 
end module hardening_model
