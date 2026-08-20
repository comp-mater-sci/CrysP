!> Implementation of the isotropic phenomenological SWIFT hardening law.

module swift
    use base_defs
    use constitutive_model
    use logging
    use crysp_serialization
    use crysp_input
    use conversions
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
        procedure, nopass:: get_grain_state => swift_get_state
        procedure:: init                   => swift_init                !! Inherited from [[ConstitutiveModel]]
        procedure:: deform                 => swift_deform              !! Inherited from [[ConstitutiveModel]]
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

    pure function swift_get_state() result(state)
        class(GrainState), allocatable:: state

        allocate(IsotropicState:: state)
    end function

    !> See [[ConstitutiveModel:init]]
    function swift_init(this, miller_indices, params) result(initial_state)
        class(ConstitutiveModelSwift),   intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        type(Parameter), dimension(:), intent(in):: params
        class(GrainState), allocatable:: initial_state

        real(DP):: crss0

        allocate(IsotropicState:: initial_state)
        call this%base_init(miller_indices, initial_state)

        crss0       = params(1)
        this%gamma0 = params(2)
        this%n      = params(3)
        this%k = crss0 / (this%gamma0**this%n)

        initial_state%crss = crss0
   end function

    !> See [[ConstitutiveModel:deform]]
    subroutine swift_deform(this, state, time, slip_rates)
        class(ConstitutiveModelSwift), intent(inout)::               this
        class(GrainState), target, intent(inout)::               state
        real(DP), intent(in)::                                       time
        real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in):: slip_rates

        type(IsotropicState), pointer:: state_ptr

        state_ptr => to_isotropic_state(state)
        state_ptr%total_slip = state_ptr%total_slip+sum(abs(slip_rates)) * time

        state_ptr%crss = this%k * (state_ptr%total_slip+this%gamma0)**(this%n)
    end subroutine

    pure function serialize(this) result(params)
        class(ConstitutiveModelSwift), intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        params = this%ConstitutiveModel%serialize()
        params = params .add. [serialize(this%k), &
                               serialize(this%gamma0), &
                               serialize(this%n)]
    end function

    function deserialize(this, params) result(params_)
        class(ConstitutiveModelSwift), intent(inout):: this
        type(Parameter), dimension(:), intent(in):: params
        type(Parameter), dimension(:), allocatable:: params_

        params_ = this%ConstitutiveModel%deserialize(params)
        this%k = params_(1)
        this%gamma0 = params_(2)
        this%n = params_(3)
        params_ = params_ .pop. 3
    end function
end module swift
