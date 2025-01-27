module swift
    use utils, only: dp
    use altayConfig
    use hardening_model
    use logging
    use parameters

    implicit none

    private
    public:: HardeningModelSwift

    !> Classic isotropic SWIFT hardening model.
    type, extends(HardeningModel):: HardeningModelSwift
        real(DP)    ::  k,      &
                        gamma0, &
                        n
    contains
        procedure:: get_parameters      => swift_get_parameters
        procedure:: validate_parameters => swift_validate_parameters
        procedure:: init                => swift_init
        procedure:: update_crss         => swift_update_crss
    end type

contains

    !> @Brief See hardening_model_get_parameters
    function swift_get_parameters() result(params)
        type(Parameter), allocatable    :: params(:)

        params = [parameter_init('crss0', TYPE_REAL),             &
                  parameter_init('gamma0', TYPE_REAL),            &
                  parameter_init('n', TYPE_REAL)]
    end function swift_get_parameters

    !> @Brief See hardening_model_validate_parameters
    subroutine swift_validate_parameters(params)
        type(Parameter), dimension(:), target, intent(in):: params

        call parameter_check_bounds(params .find. 'gamma0', lower = 0._DP, lower_inclusive=.false.)
        call parameter_check_bounds(params .find. 'n',      lower = 0._DP, lower_inclusive=.false.)
        call parameter_check_bounds(params .find. 'crss0',  lower = 0._DP, lower_inclusive=.false.)
    end subroutine swift_validate_parameters

    !> @Brief See hardening_model_init
    subroutine swift_init(this, params)
        class(HardeningModelSwift),   intent(inout)   :: this
        type(Parameter), allocatable, target, intent(in):: params(:)

        this%gamma0 = params .find. 'gamma0'
        this%n = params .find. 'n'
        this%k = (params .find. 'crss0') / (this%gamma0**this%n)
    end subroutine swift_init

    !> @Brief See hardening_model_update_crss
    subroutine swift_update_crss(this, grain_, time, slip_rates)
        class(HardeningModelSwift), intent(in)::                     this
        class(Grain),               intent(inout)::                  grain_
        real(DP), intent(in)::                                       time
        real(DP), dimension(size(grain_%slip_systems)), intent(in):: slip_rates

        integer:: i
        real(DP):: crss

        crss = this%k * (sum(abs(slip_rates))+this%gamma0)**(this%n)

        do i = 1, size(grain_%slip_systems)
            grain_%slip_systems(i)%crss = crss
        end do
    end subroutine
end module swift
