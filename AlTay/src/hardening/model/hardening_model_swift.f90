module hardening_model_swift
    use utils, only: dp
    use altayConfig
    use hardening_model
    use logging
    use parameters

    implicit none
    private
      
    type, public, extends(HardeningModel) :: HardeningModelSwift
        real(DP)    ::  k,      &
                        gamma0, &   
                        n               
    contains
        procedure :: get_parameters => swift_get_parameters
        procedure :: validate_parameters => swift_validate_parameters
        procedure :: init       => swift_init
        procedure :: get_crss     => swift_get_crss
    end type

    character(*), parameter :: MODULE_NAME = 'altay_hardening_swift'

contains

    function swift_get_parameters(this) result(params)
        class(HardeningModelSwift), intent(in)  :: this
        type(Parameter), allocatable    :: params(:)
    
        params = [hardening_model_get_parameters(this), &
                 [parameter_init('crss0', TYPE_REAL),             &   
                  parameter_init('gamma0', TYPE_REAL),            &   
                  parameter_init('n', TYPE_REAL)]]   
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
        real(DP)    :: crss0

        call hardening_model_init(this, params)

        this%gamma0 = params .find. 'gamma0'
        this%n = params .find. 'n'
        crss0 = params .find. 'crss0'
        this%k = crss0 / (this%gamma0**this%n)
    end subroutine swift_init

    function swift_get_crss(this, grain, strain) result(crss)
        class(HardeningModelSwift), intent(in)           ::  this
        integer,                    intent(in)              ::  grain
        real(DP),                   intent(in)              ::  strain
        real(DP), dimension(2, this%nss) :: crss

        crss = this%k * (strain + this%gamma0)**(this%n)    
    end function swift_get_crss
end module hardening_model_swift
