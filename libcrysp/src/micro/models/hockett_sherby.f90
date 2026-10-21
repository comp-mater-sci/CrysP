!> Implementation of the isotropic phenomenological Hockett-Sherby hardening law.

module hockett_sherby
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
    public:: ConstitutiveModelHockettSherby

    !> Classic isotropic Hockett-Sherby hardening model.
    type, extends(ConstitutiveModel):: ConstitutiveModelHockettSherby
        real(DP):: tau_0    !! Initial critical resoved shear stress
        real(DP):: tau_sat  !! Saturated critical resoved shear stress
        real(DP):: b        !! Hardening exponent
        real(DP):: n        !! Hardening exponent on slip
    contains
        procedure:: describe => hs_describe
        procedure:: init                    => hs_init                !! Inherited from [[ConstitutiveModel]]
        procedure:: deform                  => hs_deform              !! Inherited from [[ConstitutiveModel]]
        procedure, nopass:: make_hardening_state => hs_make_state
        procedure:: init_hardening_state => hs_init_state
        procedure:: size => hs_size
        procedure:: serialize => hs_serialize
        procedure:: deserialize => hs_deserialize
    end type

contains

    function hs_describe(this) result(desc)
        class(ConstitutiveModelHockettSherby), intent(in):: this
        type(ModelDescriptor):: desc

        desc = this%ConstitutiveModel%describe()
        desc%name = "Hockett-Sherby"
        desc%description = "Hockett-Sherby hardening law"
        desc%inputs = [Input('tau_0', &
                            "Initial critical resolved shear stress on all slip systems", &
                            INPUT_REAL, &
                            lower_bound=serialize(0._C_DOUBLE)),   &
                       Input('tau_sat', &
                             "Maximum critical resolved shear stress on all slip systems", &
                             INPUT_REAL, &
                             lower_bound=serialize("tau_0"), &
                             lower_bound_inclusive=.true.), &
                       Input('b', &
                             "Hardening exponent", &
                             INPUT_REAL),       &
                       Input('n', &
                             "Hardening exponent on slip", &
                             INPUT_REAL)]
        desc%model_state = [desc%model_state, &
                            StateDescriptor("tau_0", &
                                            "Initial critical resolved shear stress on all slip systems", &
                                            TYPE_REAL), &
                            StateDescriptor("tau_sat", &
                                            "Maximum critical resolved shear stress on all slip systems", &
                                            TYPE_REAL), &
                            StateDescriptor("b", &
                                            "Hardening exponent", &
                                            TYPE_REAL), &
                            StateDescriptor("n", &
                                            "Hardening exponent on slip", &
                                            TYPE_REAL)]
        desc%element_state = [desc%element_state, &
                              StateDescriptor("Total slip", &
                                              "Sum of total slip across all slip systems", &
                                              TYPE_REAL)]
    end function

    !> See [[ConstitutiveModel:init]]
    subroutine hs_init(this, miller_indices, params)
        class(ConstitutiveModelHockettSherby), intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        type(Parameter), dimension(:), intent(in):: params

        call this%ConstitutiveModel%init(miller_indices, params)

        this%tau_0 =   params(1)
        this%tau_sat = params(2)
        this%b =       params(3)
        this%n =       params(4)
    end subroutine

    !> See [[ConstitutiveModel:deform]]
    function hs_deform(this, state, time, slip_rates) result(stat)
        class(ConstitutiveModelHockettSherby), intent(inout)::               this
        class(HardeningState), target, intent(inout)::               state
        real(DP), intent(in)::                                       time
        real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in):: slip_rates
        type(Status):: stat

        type(IsotropicState), pointer:: state_ptr

        stat = to_isotropic_state(state, state_ptr)
        if (failed(stat)) return

        state_ptr%total_slip = state_ptr%total_slip+sum(abs(slip_rates)) * time

        state_ptr%crss = this%tau_sat - (this%tau_sat - this%tau_0) * exp(-this%b*state_ptr%total_slip**this%n)
    end function

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

    function hs_serialize(this) result(params)
        class(ConstitutiveModelHockettSherby), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        integer:: offset
        type(Parameter), dimension(:), allocatable:: base

        allocate(params(this%size()))

        base = this%ConstitutiveModel%serialize()
        offset = size(base)
        params(:offset) = base

        params(offset+1) = this%tau_0
        params(offset+2) = this%tau_sat
        params(offset+3) = this%b
        params(offset+4) = this%n
    end function

    subroutine hs_deserialize(this, params)
        class(ConstitutiveModelHockettSherby), target, intent(inout):: this
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
