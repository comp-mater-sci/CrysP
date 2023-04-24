module hardening_model_swift
use definitions, only: dp
use hardening_types
use altayConfig
use hardening_model_isotropic
use hardening_model
use logging
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
    
<<<<<<< HEAD
        params = [parameter_init('n_slip_systems', TYPE_STRING),  &
=======
        params = [parameter_init('n_slip_systems', TYPE_STRING), &
>>>>>>> master
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
            call log_error(MODULE_NAME, PROC_NAME, ERR_VAL, 'Initial strain must be greater than 0')
        if ((params .find. 'n') <= 0._dp)       &   
            call log_error(MODULE_NAME, PROC_NAME, ERR_VAL, 'N must be greater than 0')
        if ((params .find. 'crss0') <= 0._dp)   &   
            call log_error(MODULE_NAME, PROC_NAME, ERR_VAL, 'CRSS0 must be greater than 0')
    end subroutine swift_validate_parameters

    subroutine swift_init(this, params)
        class(HardeningModelSwift),   intent(inout)   :: this
        type(Parameter), allocatable, intent(in) :: params(:)
        real(dp)    :: crss0

        call hardening_model_init(this, params)

<<<<<<< HEAD
        this%gamma0 = params .find. 'gamma0'
        this%n = params .find. 'n'
        crss0 = params .find. 'crss0'
        this%k = crss0 / (this%gamma0**this%n)
=======
        this%gamma0 = config%swiftScnf%gamma0
        this%n = config%swiftScnf%n
        this%k = config%swiftScnf%crss0 / (this%gamma0**this%n)

        if (this%gamma0 <= 0 .or. this%n <= 0 .or. this%k <=0) call log_error(MODULE_NAME, 'swift_init', ERR_VAL, 'Swift params must be greater than 0')
>>>>>>> master
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
