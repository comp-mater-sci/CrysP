!> Implementation of the isotropic phenomenological SWIFT hardening law.

module swift
    use base_defs
    use constitutive_model
    use crysp_status
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
        procedure:: describe => swift_describe
        procedure:: init                   => swift_init                !! Inherited from [[ConstitutiveModel]]
        procedure:: deform                 => swift_deform              !! Inherited from [[ConstitutiveModel]]
        procedure, nopass:: make_hardening_state => swift_make_state
        procedure:: init_hardening_state => swift_init_state
        procedure:: size => swift_size
        procedure:: serialize => swift_serialize
        procedure:: deserialize => swift_deserialize
    end type

contains

    function swift_describe(this) result(desc)
        class(ConstitutiveModelSwift), intent(in):: this
        type(ModelDescriptor):: desc

        desc = this%ConstitutiveModel%describe()
        desc%name = "Swift"
        desc%description = "Swift hardening model."
        desc%inputs = [Input('crss0', &
                             "Critical resolved shear stress on all slip systems in undeformed state.", &
                             INPUT_REAL, &
                             lower_bound = serialize(0._C_DOUBLE)),  &
                       Input('gamma0', &
                              "Pre-existing accumulated slip over all slip systems", &
                              INPUT_REAL, &
                              lower_bound = serialize(0._C_DOUBLE)), &
                       Input('n', &
                             "Hardening exponent", &
                             INPUT_REAL, &
                             lower_bound = serialize(0._C_DOUBLE))]
        desc%model_state = [desc%model_state, &
                            StateDescriptor("k", &
                                            "Scaling factor", &
                                            TYPE_REAL), &
                            StateDescriptor("gammma_0", &
                                            "Pre-existing accumulated slip over all slip systems", &
                                            TYPE_REAL), &
                            StateDescriptor("n", &
                                            "Hardening exponent", &
                                            TYPE_REAL)]
        desc%element_state = [desc%element_state, &
                              StateDescriptor("Total slip", &
                                              "Sum of total slip across all slip systems", &
                                              TYPE_REAL)]

    end function

    !> See [[ConstitutiveModel:init]]
    subroutine swift_init(this, miller_indices, params)
        class(ConstitutiveModelSwift), intent(inout):: this
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
    function swift_deform(this, state, time, slip_rates) result(stat)
        class(ConstitutiveModelSwift), intent(inout)::               this
        class(HardeningState), target, intent(inout)::               state
        real(DP), intent(in)::                                       time
        real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in):: slip_rates
        type(Status):: stat

        type(IsotropicState), pointer:: state_ptr

        stat = to_isotropic_state(state, state_ptr)
        if (failed(stat)) return

        state_ptr%total_slip = state_ptr%total_slip+sum(abs(slip_rates)) * time

        state_ptr%crss = this%k * (state_ptr%total_slip+this%gamma0)**(this%n)
    end function

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

    function swift_serialize(this) result(params)
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
        class(ConstitutiveModelSwift), target, intent(inout):: this
        type(Parameter), dimension(:), intent(in):: params

        integer:: offset

        call this%ConstitutiveModel%deserialize(params)

        offset = this%ConstitutiveModel%size()

        this%k = params(offset+1)
        this%gamma0 = params(offset+2)
        this%n = params(offset+3)
    end subroutine
end module swift
