!> Implementation of the isotropic phenomenological SWIFT hardening law.

module hockett_sherby
    use base_defs, only: dp
    use constitutive_model
    use logging
    use serialization
    use grain_module

    implicit none

    private
    public:: ConstitutiveModelHockettSherby

    !> Grain-specific hardening state data needed by the Hockett-Sherby hardening law.
    type, extends(HardeningState):: HockettSherbyState
        real(DP):: total_slip = 0._DP           !! Sum of all the slip on all the slip systems of the grain.
    end type

    !> Classic isotropic Hockett-Sherby hardening model.
    type, extends(ConstitutiveModel):: ConstitutiveModelHockettSherby
        real(DP):: tau_0    !! Initial critical resoved shear stress
        real(DP):: tau_sat  !! Saturated critical resoved shear stress
        real(DP):: b        !! Hardening exponent
        real(DP):: n        !! Hardening exponent on slip
    contains
        procedure, nopass:: get_parameters      => hs_get_parameters      !! Inherited from [[ConstitutiveModel]]
!        procedure, nopass:: validate_parameters => hs_validate_parameters !! Inherited from [[ConstitutiveModel]]
        procedure:: init                        => hs_init                !! Inherited from [[ConstitutiveModel]]
        procedure:: deform                      => hs_deform              !! Inherited from [[ConstitutiveModel]]
    end type

contains

    !> Convert a generic HardeningState to a pointer to a HockettSherbyState object
    !>
    !> Closest Fortran comes to type casting
    !> If the provided state is not of type HockettSherbyState, the program crashes.
    function to_hockett_sherby_state(state) result(hockett_sherby_state_ptr)
        class(HardeningState), target, intent(in):: state   !! HardeningState to be converted. Must be of type SwiftState
        type(HockettSherbyState), pointer:: hockett_sherby_state_ptr         !! Pointer of type SwiftState to the HardeningState

        select type (state)
            type is (HockettSherbyState)
                hockett_sherby_state_ptr => state
            class default
                call log_error(ERR_TYPE)
        end select
    end function

    !> See [[ConstitutiveModel:get_parameters]]
    function hs_get_parameters() result(params)
        type(Parameter), allocatable    :: params(:)    !! - **tau_0**: Initial critical resoved shear stress
                                                        !! - **tau_sat**: Final critical resolved shear stress
                                                        !! - **b**: Hardening exponent
                                                        !! - **n**: Hardening exponent on slip

        params = [Parameter('tau_0', TYPE_REAL),   &
                  Parameter('tau_sat', TYPE_REAL), &
                  Parameter('b', TYPE_REAL),       &
                  Parameter('n', TYPE_REAL)]
    end function

!    !> See [[ConstitutiveModel:validate_parameters]]
!    subroutine hs_validate_parameters(params)
!        type(Parameter), dimension(:), target, intent(in):: params
!
!        type(Parameter), pointer:: buffer
!
!        call parameter_check_bounds(params .find. 'tau_0', lower = TOLERANCE, lower_inclusive=.true.)
!        buffer => params .find. 'tau_0'
!        call parameter_check_bounds(params .find. 'tau_sat',  buffer, lower_inclusive=.false.)
!    end subroutine

    !> See [[ConstitutiveModel:init]]
    function hs_init(this, miller_indices, params) result(initial_state)
        class(ConstitutiveModelHockettSherby),   intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        character(*), target, intent(in):: params
        class(HardeningState), allocatable:: initial_state

        type(JSONParser):: parser

        allocate(HockettSherbyState:: initial_state)
        call this%base_init(miller_indices, initial_state)

        parser = params
        this%tau_0 = parser
        this%tau_sat = parser
        this%b = parser
        this%n = parser

        initial_state%crss = this%tau_0
    end function

    !> See [[ConstitutiveModel:deform]]
    subroutine hs_deform(this, state, time, slip_rates)
        class(ConstitutiveModelHockettSherby), intent(inout)::               this
        class(HardeningState), target, intent(inout)::               state
        real(DP), intent(in)::                                       time
        real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in):: slip_rates

        type(HockettSherbyState), pointer:: state_ptr

        state_ptr => to_hockett_sherby_state(state)
        state_ptr%total_slip = state_ptr%total_slip+sum(abs(slip_rates)) * time

        state_ptr%crss = this%tau_sat - (this%tau_sat - this%tau_0) * exp(-this%b*state_ptr%total_slip**this%n)
    end subroutine
end module hockett_sherby
