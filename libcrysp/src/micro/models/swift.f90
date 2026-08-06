!> Implementation of the isotropic phenomenological SWIFT hardening law.

module swift
    use base_defs, only: dp
    use constitutive_model
    use logging
    use parameters
    use grain_module
    use serialization

    implicit none

    private
    public:: ConstitutiveModelSwift

    !> Grain-specific hardening state data needed by the SWIFT hardening law.
    type, extends(HardeningState):: SwiftState
        real(DP):: total_slip = 0._DP           !! Sum of all the slip on all the slip systems of the grain.
    end type

    !> Classic isotropic SWIFT hardening model.
    type, extends(ConstitutiveModel):: ConstitutiveModelSwift
        real(DP):: k      !! Scaling factor
        real(DP):: gamma0 !! Initial sum of slip across all slip systems
        real(DP):: n      !! Exponent
    contains
        procedure, nopass:: get_parameters      => swift_get_parameters      !! Inherited from [[ConstitutiveModel]]
        procedure:: init                        => swift_init                !! Inherited from [[ConstitutiveModel]]
        procedure:: deform                      => swift_deform              !! Inherited from [[ConstitutiveModel]]
    end type

contains

    !> Convert a generic HardeningState to a pointer to a SwiftState object
    !>
    !> Closest Fortran comes to type casting
    !> If the provided state is not of type swift_state, the program crashes.
    function to_swift_state(state) result(swift_state_ptr)
        class(HardeningState), target, intent(in):: state   !! HardeningState to be converted. Must be of type SwiftState
        type(SwiftState), pointer:: swift_state_ptr         !! Pointer of type SwiftState to the HardeningState

        select type (state)
            type is (SwiftState)
                swift_state_ptr => state
            class default
                call log_error(ERR_TYPE)
        end select
    end function

    !> See [[ConstitutiveModel:get_parameters]]
    function swift_get_parameters() result(params)
        type(Parameter), allocatable:: params(:)    !! - **crss0**: Initial critical resoved shear stress
                                                    !! - **gamma0**: Initial sum of slip across all slip systems
                                                    !! - **n**: Hardening exponent

        params = [Parameter('crss0', TYPE_REAL, lower_bound = "0"),  &
                  Parameter('gamma0', TYPE_REAL, lower_bound = "0"), &
                  Parameter('n', TYPE_REAL, lower_bound = "0")]
    end function swift_get_parameters

    !> See [[ConstitutiveModel:init]]
    function swift_init(this, miller_indices, params) result(initial_state)
        class(ConstitutiveModelSwift),   intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        character(*), target, intent(in):: params
        class(HardeningState), allocatable:: initial_state

        real(DP):: crss0
        type(JSONParser):: parser

        allocate(SwiftState:: initial_state)
        call this%base_init(miller_indices, initial_state)

        call parser%init(params)
        crss0       = parser%parse_real()
        this%gamma0 = parser%parse_real()
        this%n      = parser%parse_real()
        this%k = crss0 / (this%gamma0**this%n)

        initial_state%crss = crss0
   end function

    !> See [[ConstitutiveModel:deform]]
    subroutine swift_deform(this, state, time, slip_rates)
        class(ConstitutiveModelSwift), intent(inout)::               this
        class(HardeningState), target, intent(inout)::               state
        real(DP), intent(in)::                                       time
        real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in):: slip_rates

        type(SwiftState), pointer:: state_ptr

        state_ptr => to_swift_state(state)
        state_ptr%total_slip = state_ptr%total_slip+sum(abs(slip_rates)) * time

        state_ptr%crss = this%k * (state_ptr%total_slip+this%gamma0)**(this%n)
    end subroutine
end module swift
