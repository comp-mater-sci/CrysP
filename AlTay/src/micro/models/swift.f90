module swift
    use utils, only: dp
    use altayConfig
    use hardening_model
    use logging
    use parameters

    implicit none
    private

    type, public, extends(HardeningModel):: HardeningModelSwift
        real(DP)    ::  k,      &
                        gamma0, &
                        n
    contains
        procedure:: get_parameters => swift_get_parameters
        procedure:: validate_parameters => swift_validate_parameters
        procedure:: init       => swift_init
        procedure:: get_crss     => swift_get_crss
    end type

    character(*), parameter:: MODULE_NAME = 'altay_hardening_swift'

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
        type(Parameter), dimension(:), intent(in):: params

        call hardening_model_validate_parameters(this, params)

        call parameter_check_bounds(params .find. 'gamma0', lower=0._DP, lower_inclusive=.false.)
        call parameter_check_bounds(params .find. 'n',      lower=0._DP, lower_inclusive=.false.)
        call parameter_check_bounds(params .find. 'crss0',  lower=0._DP, lower_inclusive=.false.)
    end subroutine swift_validate_parameters

    subroutine swift_init(this, params)
        class(HardeningModelSwift),   intent(inout)   :: this
        type(Parameter), allocatable, intent(in):: params(:)

        call hardening_model_init(this, params)

        this%gamma0 = params .find. 'gamma0'
        this%n = params .find. 'n'
        this%k = (params .find. 'crss0') / (this%gamma0**this%n)
    end subroutine swift_init

    function swift_get_crss(this, grain, sum_slip) result(crss)
        class(HardeningModelSwift), intent(in):: this
        integer,                    intent(in):: grain
        real(DP),                   intent(in):: sum_slip
        real(DP), dimension(2, this%nss):: crss

        crss = this%k * (sum_slip+this%gamma0)**(this%n)
    end function swift_get_crss
end module swift
