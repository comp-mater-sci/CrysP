module swift
    use utils, only: dp
    use altayConfig
    use hardening_model
    use logging
    use parameters
    use grain_module

    implicit none

    private
    public:: HardeningModelSwift


    !> Grain-specific hardening state data needed by the SWIFT hardening law.
    type, extends(HardeningState):: SwiftState
        real(DP):: total_slip = 0._DP !> Sum of all the slip on all the slip systems of the grain.
    end type

    !> Classic isotropic SWIFT hardening model.
    type, extends(HardeningModel):: HardeningModelSwift
        !Model parameters
        real(DP)    ::  k,      &
                        gamma0, &
                        n
    contains
        procedure, nopass:: get_parameters      => swift_get_parameters
        procedure, nopass:: validate_parameters => swift_validate_parameters
        procedure:: init                => swift_init
        procedure:: deform              => swift_deform
    end type

contains

    !> @Brief Convert a generic HardeningState to a pointer to a SwiftState object
    !> @Details Closest Fortran comes to type casting
    !!          If the provided state is not of type swift_state, the program crashes.
    function to_swift_state(state) result(swift_state_ptr)
        class(HardeningState), target, intent(in):: state   !> HardeningState to be converted. Must be of type SwiftState
        type(SwiftState), pointer:: swift_state_ptr         !> Pointer of type SwiftState to the HardeningState

        select type (state)
            type is (SwiftState)
                swift_state_ptr => state
            class default
                call log_error(ERR_TYPE)
        end select
    end function

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
    subroutine swift_init(this, grains, params)
        class(HardeningModelSwift),   intent(inout):: this
        type(Grain), dimension(:), intent(inout)::    grains
        type(Parameter), dimension(:), target, intent(in):: params

        integer:: i, j  !> Iterators
        real(DP):: crss !> Buffer for crss so that we do not have to recalculate it for every slip system of every grain.

        this%gamma0 = params .find. 'gamma0'
        this%n = params .find. 'n'
        this%k = (params .find. 'crss0') / (this%gamma0**this%n)

        crss = this%k*this%gamma0**this%n

        !Initialize grain-specific state
        do i = 1, size(grains)
            allocate(SwiftState:: grains(i)%hardening_state)
            do j = 1, size(grains(i)%slip_systems)
                grains(i)%slip_systems(j)%crss = crss
            end do
        end do
    end subroutine swift_init

    !> @Brief See hardening_model_update_crss
    subroutine swift_deform(this, grain_, time, slip_rates)
        class(HardeningModelSwift), intent(inout)::                  this
        type(Grain), target, intent(inout)::                        grain_
        real(DP), intent(in)::                                       time
        real(DP), dimension(size(grain_%slip_systems)), intent(in):: slip_rates

        integer:: i
        real(DP):: crss
        type(SwiftState), pointer:: state_ptr

        state_ptr => to_swift_state(grain_%hardening_state)
        state_ptr%total_slip = state_ptr%total_slip+sum(abs(slip_rates)) * time

        crss = this%k * (state_ptr%total_slip+this%gamma0)**(this%n)

        do i = 1, size(grain_%slip_systems)
            grain_%slip_systems(i)%crss = crss
        end do
    end subroutine
end module swift
