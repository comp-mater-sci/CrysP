module hardening_model_swift
use altayMiscutils, only: terminate, stopcode_runtimeerror
use altay_definitions, only: dp
use hardening_types
use altayConfig
use hardening_model_isotropic
use hardening_model
use altay_log
use parameters

    implicit none
    private
      
    type, public, extends(HardeningModelIsotropic) :: HardeningModelSwift
        real(dp)    ::  k,      &
                        gamma0, &   
                        n               
    contains
        procedure :: get_parameters => swift_get_parameters
        procedure :: validate_parameters => swift_validate_parameters
        procedure :: init       => swift_init
        procedure :: update     => swift_update
    end type

    character(*), parameter :: MODULE_NAME = 'altay_hardening_swift'

contains

    function swift_get_parameters(this) result(params)
        class(HardeningModelSwift), intent(in)  :: this
        type(Parameter), allocatable    :: params(:)
    
        params = [parameter_init('n_slip_systems', TYPE_STRING),  &
                  parameter_init('crss0', TYPE_REAL),             &   
                  parameter_init('gamma0', TYPE_REAL),            &   
                  parameter_init('n', TYPE_REAL)]   
    end function swift_get_parameters

    subroutine swift_validate_parameters(this, params)
        class(HardeningModelSwift), intent(in)  :: this
        type(Parameter), allocatable, intent(in)    :: params(:)
        character(*), parameter :: PROC_NAME = 'validate_parameters'    

        call hardening_model_validate_parameters(this, params)

        if ((params .find. 'gamma0') <= 0._dp)  &
            call vef_exception(MODULE_NAME, PROC_NAME, VEF_BADVAL, 'Initial strain must be greater than 0')
        if ((params .find. 'n') <= 0._dp)       &   
            call vef_exception(MODULE_NAME, PROC_NAME, VEF_BADVAL, 'N must be greater than 0')
        if ((params .find. 'crss0') <= 0._dp)   &   
            call vef_exception(MODULE_NAME, PROC_NAME, VEF_BADVAL, 'CRSS0 must be greater than 0')
    end subroutine swift_validate_parameters

    subroutine swift_init(this, params)
        class(HardeningModelSwift),   intent(inout)   :: this
        type(Parameter), allocatable, intent(in) :: params(:)
        real(dp)    :: crss0

        call hardening_model_init(this, params)

        this%gamma0 = params .find. 'gamma0'
        this%n = params .find. 'n'
        crss0 = params .find. 'crss0'
        this%k = crss0 / (this%gamma0**this%n)
    end subroutine swift_init

    subroutine swift_update(this, grain, time, strain, slip_rates)
        class(HardeningModelSwift), intent(inout)           ::  this
        integer,                    intent(in)              ::  grain
        real(dp),                   intent(in)              ::  time, &
                                                                strain
        real(dp), dimension(this%nss), intent(in) ::  slip_rates

        this%crss = this%k * (strain + this%gamma0)**(this%n)    
    end subroutine swift_update

end module hardening_model_swift
