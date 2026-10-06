!> This module implements a two-stage isotropic Voce hardening law.

module voce
    use base_defs, only: dp
    use constitutive_model
    use logging
    use crysp_serialization
    use crysp_input
    use conversions
    use crysp_model
    use crysp_isotropic_state

    implicit none

    private
    public:: ConstitutiveModelVoce

    !> Type wrapping parameters associated to a particular stage of the Voce hardening law.
    type:: Stage
        real(DP):: TS !! Saturation flow stress
        real(DP):: T1 !! Initial flow stress
        real(DP):: TH !! Initial hardening rate
    end type

    !> Voce hardening model
    type, extends(ConstitutiveModel):: ConstitutiveModelVoce
        real(DP)   ::  transition_slip = 0._DP !! Value for total slip at which to transition from stage 1 to stage 2.
        type(Stage), dimension(2):: stages
    contains
        procedure, nopass:: get_name        => voce_get_name
        procedure, nopass:: get_description => voce_get_description
        procedure, nopass:: get_signature   => voce_get_signature
        procedure, nopass:: get_input       => voce_get_input          !! Inherited from [[ConstitutiveModel]]
        procedure:: init                    => voce_init                    !! Inherited from [[ConstitutiveModel]]
        procedure:: deform                  => voce_deform                  !! Inherited from [[ConstitutiveModel]]
        procedure, nopass:: make_hardening_state => voce_make_state
        procedure:: init_hardening_state => voce_init_state
        procedure:: size => voce_size
        procedure:: serialize => voce_serialize
        procedure:: deserialize => voce_deserialize
    end type

contains


    pure function voce_get_name() result(name)
        character(:), allocatable:: name

        name = "VOCE"
    end function

    pure function voce_get_description() result(description)
        character(:), allocatable:: description

        description = "2-stage VOCE hardening law. Can also be used as 1-stage VOCE by setting the saturation stress of stage 1 equal to the initial stress of stage 1."
    end function

    pure function voce_get_signature() result(signature)
        integer, dimension(:), allocatable:: signature

        allocate(signature(5), source=INPUT_REAL)
    end function

    !> See [[ConstitutiveModel:get_parameters]]
    function voce_get_input() result(inputs)
        type(Input), dimension(:), allocatable:: inputs !! - **TIII1**: Initial flow stress.
                                                        !! - **TIIIS**: Saturation flow stress for the first stage.
                                                        !! - **TIVS**: Saturation flow stress for the second stage.
                                                        !! - **THIII1**: Initial hardening rate.
                                                        !! - **THT**: Hardening rate at which to transition from stage 1 to stage 2.

        inputs = [Input(to_c_string('TIII1',NAME_LEN),  INPUT_REAL, lower_bound=serialize(0._C_DOUBLE), upper_bound=serialize("TIIIS")), &
                  Input(to_c_string('TIIIS',NAME_LEN),  INPUT_REAL, upper_bound=serialize("TIVS"), upper_bound_inclusive=.true.), &
                  Input(to_c_string('TIVS',NAME_LEN),   INPUT_REAL, lower_bound=serialize("TIIIS"), lower_bound_inclusive=.true.), &
                  Input(to_c_string('THIII1',NAME_LEN), INPUT_REAL), &
                  Input(to_c_string('THT',NAME_LEN),    INPUT_REAL, lower_bound=serialize(0._C_DOUBLE), upper_bound=serialize("THIII1"))]
    end function

    !> See [[ConstitutiveModel:init]]
    subroutine voce_init(this, miller_indices, params)
        class(ConstitutiveModelVoce), intent(out):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        type(Parameter), dimension(:), intent(in):: params

        !Local variables
        real(DP):: THT, &
                   ETA, &
                   TAUT, &
                   THIII1

        call this%ConstitutiveModel%init(miller_indices, params)

        !Parse the hardening parameters
        this%stages(1)%T1 = params(1)
        this%stages(1)%TS = params(2)
        this%stages(2)%TS = params(3)
        THIII1          = params(4)
        THT             = params(5)

        this%stages(1)%TH = THIII1 / (1.D0-this%stages(1)%T1/this%stages(1)%TS)
        ETA = THT/this%stages(1)%TH
        this%transition_slip = -this%stages(1)%TS*log(ETA*this%stages(1)%TS / (this%stages(1)%TS-this%stages(1)%T1)) / this%stages(1)%TH
        TAUT = this%stages(1)%TS - (this%stages(1)%TS-this%stages(1)%T1) * exp(-this%stages(1)%TH*this%transition_slip/this%stages(1)%TS)
        this%stages(2)%TH = THT / (1.D0-TAUT/this%stages(2)%TS)
        this%stages(2)%T1 = this%stages(2)%TS + (TAUT-this%stages(2)%TS) * exp(this%stages(2)%TH*this%transition_slip/this%stages(2)%TS)
    end subroutine

    !> See [[ConstitutiveModel:deform]]
    subroutine voce_deform(this, state, time, slip_rates)
        class(ConstitutiveModelVoce),  intent(inout):: this
        class(HardeningState), target, intent(inout):: state
        real(DP),                      intent(in)::    time
        real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in)::    slip_rates

        type(Stage):: current_stage          !Current stage in the Voce hardening process
        type(IsotropicState), pointer:: state_ptr !Pointer to hardening_state of type VoceState for easy access to model-specific fields

        state_ptr => to_isotropic_state(state)

        state_ptr%total_slip = state_ptr%total_slip+sum(abs(slip_rates)) * time

        current_stage = merge(this%stages(1), &
                              this%stages(2), &
                              state_ptr%total_slip <= this%transition_slip)

        state_ptr%crss = current_stage%TS - (current_stage%TS-current_stage%T1) * exp(-current_stage%TH*state_ptr%total_slip/current_stage%TS)
    end subroutine

    function voce_make_state() result(state)
        class(HardeningState), allocatable:: state

        allocate(IsotropicState::state)
    end function

    subroutine voce_init_state(this, state)
        class(ConstitutiveModelVoce), intent(in):: this
        class(HardeningState), intent(out):: state

        call this%ConstitutiveModel%init_hardening_state(state)
        state%crss = this%stages(1)%T1
    end subroutine

    pure function voce_size(this) result(size)
        class(ConstitutiveModelVoce), intent(in):: this
        integer:: size

        size = this%ConstitutiveModel%size() + 7
    end function

    function voce_serialize(this) result(params)
        class(ConstitutiveModelVoce), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        integer:: i, &
                  offset
        type(Parameter), dimension(:), allocatable:: base

        allocate(params(this%size()))

        base = this%ConstitutiveModel%serialize()
        offset = size(base)
        params(:offset) = base

        params(offset+1) = this%transition_slip
        offset = offset+1
        do i=1,2
            offset = offset + 3*(i-1)
            params(offset+1) = this%stages(i)%TS
            params(offset+2) = this%stages(i)%T1
            params(offset+3) = this%stages(i)%TH
        end do
    end function

    subroutine voce_deserialize(this, params)
        class(ConstitutiveModelVoce), target, intent(out):: this
        type(Parameter), dimension(:), intent(in):: params

        integer:: i, &
                  offset

        call this%ConstitutiveModel%deserialize(params)
        offset = this%ConstitutiveModel%size()

        this%transition_slip = params(offset+1)
        offset = offset+1

        do i=1,2
            this%stages(i)%TS = params(offset+1)
            this%stages(i)%T1 = params(offset+2)
            this%stages(i)%TH = params(offset+3)
            offset = offset + 3
        end do
    end subroutine
end module voce
