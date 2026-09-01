!> Implementation of the isotropic phenomenological Hockett-Sherby hardening law.

module hockett_sherby
    use base_defs
    use constitutive_model
    use logging
    use crysp_serialization
    use crysp_input
    use conversions
    use crysp_model
    use crysp_isotropic_state

    implicit none

    private
    public:: ConstitutiveModelHockettSherby

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
        procedure:: init                    => hs_init                !! Inherited from [[ConstitutiveModel]]
        procedure:: deform                  => hs_deform              !! Inherited from [[ConstitutiveModel]]
        procedure, nopass:: make_hardening_state => hs_make_state
        procedure:: init_hardening_state => hs_init_state
        procedure:: size => hs_size
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

    !> See [[ConstitutiveModel:init]]
    subroutine hs_init(this, miller_indices, params)
        class(ConstitutiveModelHockettSherby), intent(out):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        type(Parameter), dimension(:), intent(in):: params

        call this%ConstitutiveModel%init(miller_indices, params)

        this%tau_0 =   params(1)
        this%tau_sat = params(2)
        this%b =       params(3)
        this%n =       params(4)
    end subroutine

    !> See [[ConstitutiveModel:deform]]
    subroutine hs_deform(this, state, time, slip_rates)
        class(ConstitutiveModelHockettSherby), intent(inout)::               this
        class(HardeningState), target, intent(inout)::               state
        real(DP), intent(in)::                                       time
        real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in):: slip_rates

        type(IsotropicState), pointer:: state_ptr

        state_ptr => to_isotropic_state(state)
        state_ptr%total_slip = state_ptr%total_slip+sum(abs(slip_rates)) * time

        state_ptr%crss = this%tau_sat - (this%tau_sat - this%tau_0) * exp(-this%b*state_ptr%total_slip**this%n)
    end subroutine

    function hs_make_state() result(state)
        class(HardeningState), allocatable:: state

        allocate(IsotropicState::state)
    end function

    subroutine hs_init_state(this, state)
        class(ConstitutiveModelHockettSherby), intent(in):: this
        class(HardeningState), intent(out):: state

        call this%ConstitutiveModel%init_hardening_state(state)
        state%crss = this%tau_0
    end subroutine

    pure function hs_size(this) result(size)
        class(ConstitutiveModelHockettSherby), intent(in):: this
        integer:: size

        size = this%ConstitutiveModel%size() + 4
    end function

    pure function hs_serialize(this) result(params)
        class(ConstitutiveModelHockettSherby), target, intent(in):: this
        type(Parameter), dimension(this%size()):: params

        integer:: offset

        params(:this%ConstitutiveModel%size()) = this%ConstitutiveModel%serialize()
        offset = this%ConstitutiveModel%size()

        params(offset+1) = this%tau_0
        params(offset+2) = this%tau_sat
        params(offset+3) = this%b
        params(offset+4) = this%n
    end function

    subroutine hs_deserialize(this, params)
        class(ConstitutiveModelHockettSherby), target, intent(out):: this
        type(Parameter), dimension(:), intent(in):: params

        integer:: offset

        call this%ConstitutiveModel%deserialize(params)
        offset = this%ConstitutiveModel%size()

        this%tau_0 = params(offset+1)
        this%tau_sat = params(offset+2)
        this%b = params(offset+3)
        this%n = params(offset+4)
    end subroutine
end module hockett_sherby
