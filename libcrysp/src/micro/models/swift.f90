!> Implementation of the isotropic phenomenological SWIFT hardening law.

module swift
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
    public:: ConstitutiveModelSwift

    !> Classic isotropic SWIFT hardening model.
    type, extends(ConstitutiveModel):: ConstitutiveModelSwift
        real(DP):: k      !! Scaling factor
        real(DP):: gamma0 !! Initial sum of slip across all slip systems
        real(DP):: n      !! Exponent
    contains
        procedure, nopass:: get_name       => swift_get_name
        procedure, nopass:: get_description => swift_get_description
        procedure, nopass:: get_signature  => swift_get_signature      !! Inherited from [[ConstitutiveModel]]
        procedure, nopass:: get_input       => swift_get_input       !! Inherited from [[ConstitutiveModel]]
        procedure:: init                   => swift_init                !! Inherited from [[ConstitutiveModel]]
        procedure:: deform                 => swift_deform              !! Inherited from [[ConstitutiveModel]]
        procedure, nopass:: make_hardening_state => swift_make_state
        procedure:: init_hardening_state => swift_init_state
        procedure:: size => swift_size
        procedure:: serialize => swift_serialize
        procedure:: deserialize => swift_deserialize
    end type

contains


    pure function swift_get_name() result(name)
        character(:), allocatable:: name

        name = "Swift"
    end function

    pure function swift_get_description() result(description)
        character(:), allocatable:: description

        description = "Swift hardening model."
    end function

    pure function swift_get_signature() result(signature)
        integer, dimension(:), allocatable:: signature

        allocate(signature(3), source=INPUT_REAL)
    end function

    !> See [[ConstitutiveModel:get_parameters]]
    pure function swift_get_input() result(inputs)
        type(Input), allocatable:: inputs(:)    !! - **crss0**: Initial critical resoved shear stress
                                                    !! - **gamma0**: Initial sum of slip across all slip systems
                                                    !! - **n**: Hardening exponent

        inputs = [Input(to_c_string('crss0',NAME_LEN), INPUT_REAL, lower_bound = serialize(0._C_DOUBLE)),  &
                  Input(to_c_string('gamma0',NAME_LEN), INPUT_REAL, lower_bound = serialize(0._C_DOUBLE)), &
                  Input(to_c_string('n',NAME_LEN), INPUT_REAL, lower_bound = serialize(0._C_DOUBLE))]
    end function

    !> See [[ConstitutiveModel:init]]
    subroutine swift_init(this, miller_indices, params)
        class(ConstitutiveModelSwift), intent(out):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        type(Parameter), dimension(:), intent(in):: params

        real(DP):: crss0

        call this%ConstitutiveModel%init(miller_indices, params)

        crss0       = params(1)
        this%gamma0 = params(2)
        this%n      = params(3)
        this%k = crss0 / (this%gamma0**this%n)
    end subroutine

    !> See [[ConstitutiveModel:deform]]
    subroutine swift_deform(this, state, time, slip_rates)
        class(ConstitutiveModelSwift), intent(inout)::               this
        class(HardeningState), target, intent(inout)::               state
        real(DP), intent(in)::                                       time
        real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in):: slip_rates

        type(IsotropicState), pointer:: state_ptr

        state_ptr => to_isotropic_state(state)
        state_ptr%total_slip = state_ptr%total_slip+sum(abs(slip_rates)) * time

        state_ptr%crss = this%k * (state_ptr%total_slip+this%gamma0)**(this%n)
    end subroutine

    function swift_make_state() result(state)
        class(HardeningState), allocatable:: state

        allocate(IsotropicState::state)
    end function

    subroutine swift_init_state(this, state)
        class(ConstitutiveModelSwift), intent(in):: this
        class(HardeningState), intent(out):: state

        call this%ConstitutiveModel%init_hardening_state(state)
        state%crss = this%k * (this%gamma0**this%n)
    end subroutine

    pure function swift_size(this) result(size)
        class(ConstitutiveModelSwift), intent(in):: this
        integer:: size

        size = this%ConstitutiveModel%size() + 3
    end function

    pure function swift_serialize(this) result(params)
        class(ConstitutiveModelSwift), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        integer:: offset
        type(Parameter), dimension(:), allocatable:: base

        allocate(params(this%size()))

        base = this%ConstitutiveModel%serialize()
        offset = size(base)
        params(:offset) = base

        params(offset+1) = this%k
        params(offset+2) = this%gamma0
        params(offset+3) = this%n
    end function

    subroutine swift_deserialize(this, params)
        class(ConstitutiveModelSwift), target, intent(out):: this
        type(Parameter), dimension(:), intent(in):: params

        integer:: offset

        call this%ConstitutiveModel%deserialize(params)
        offset = this%ConstitutiveModel%size()

        this%k = params(offset+1)
        this%gamma0 = params(offset+2)
        this%n = params(offset+3)
    end subroutine
end module swift
