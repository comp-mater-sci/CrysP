!> Implementation of the isotropic phenomenological SWIFT hardening law.

module hockett_sherby
    use base_defs
    use constitutive_model
    use logging
    use grain_module
    use crysp_serialization
    use crysp_input
    use conversions

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
        procedure, nopass:: get_name => hs_get_name
        procedure, nopass:: get_description => hs_get_description
        procedure, nopass:: get_signature  => hs_get_signature      !! Inherited from [[ConstitutiveModel]]
        procedure, nopass:: get_input => hs_get_input             !! Inherited from [[ConstitutiveModel]]
        procedure:: init                   => hs_init                !! Inherited from [[ConstitutiveModel]]
        procedure:: deform                 => hs_deform              !! Inherited from [[ConstitutiveModel]]
        procedure:: serialize => hs_serialize
        procedure:: deserialize => hs_deserialize
    end type

contains

    pure function hs_get_name() result(name)
        character(:), allocatable:: name

        name = "Hockett-Sherby"
    end function

    pure function hs_get_description() result(description)
        character(:), allocatable:: description
        description = "Hockett-Sherby hardening law"
    end function

    pure function hs_get_signature() result(signature)
        integer, dimension(:), allocatable:: signature

        allocate(signature(4), source=INPUT_REAL)
    end function

    !> See [[ConstitutiveModel:get_parameters]]
    pure function hs_get_input() result(inputs)
        type(Input), dimension(:), allocatable:: inputs    !! - **tau_0**: Initial critical resoved shear stress
                                                        !! - **tau_sat**: Final critical resolved shear stress
                                                        !! - **b**: Hardening exponent
                                                        !! - **n**: Hardening exponent on slip

        inputs = [Input(to_c_string('tau_0',NAME_LEN), INPUT_REAL, lower_bound=serialize(0._C_DOUBLE)),   &
                  Input(to_c_string('tau_sat',NAME_LEN), INPUT_REAL, lower_bound=serialize("tau_0"), lower_bound_inclusive=.true.), &
                  Input(to_c_string('b',NAME_LEN), INPUT_REAL),       &
                  Input(to_c_string('n',NAME_LEN), INPUT_REAL)]
    end function

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

    !> See [[ConstitutiveModel:init]]
    function hs_init(this, miller_indices, params) result(initial_state)
        class(ConstitutiveModelHockettSherby),   intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        type(Parameter), dimension(:), intent(in):: params
        class(HardeningState), allocatable:: initial_state

        allocate(HockettSherbyState:: initial_state)
        call this%base_init(miller_indices, initial_state)

        this%tau_0 =   params(1)
        this%tau_sat = params(2)
        this%b =       params(3)
        this%n =       params(4)

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

    pure function serialize(this) result(params)
        class(ConstitutiveModelHockettSherby), intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        params = this%ConstitutiveModel%serialize()
        params = params .add. [this%tau_0, &
                               this%tau_sat, &
                               this%b, &
                               this%n]
    end function

    function deserialize(this, params) result(params_)
        class(ConstitutiveModelHockettSherby), intent(inout):: this
        type(Parameter), dimension(:), intent(in):: params
        type(Parameter), dimension(:), allocatable:: params_

        params_ = this%ConstitutiveModel%deserialize(params)
        this%tau_0 = params_(1)
        this%tau_sat = params_(2)
        this%b = params_(3)
        this%n = params_(4)

        params_ = params_ .pop. 4
    end function
end module hockett_sherby
