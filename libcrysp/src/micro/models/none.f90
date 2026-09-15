module none
    use base_defs
    use constitutive_model
    use grain_module
    use parameters
    use mod_model

    implicit none

    private
    public:: ConstitutiveModelNone

    type, extends(HardeningState):: HardeningStateNone
    end type

    !> @Brief Hardening model representing no hardening.
    type, extends(ConstitutiveModel):: ConstitutiveModelNone
    contains
        procedure, nopass:: get_name            => none_get_name
        procedure, nopass:: get_description     => none_get_description
        procedure, nopass:: get_signature       => none_get_signature
        procedure, nopass:: get_parameters      => none_get_parameters
        procedure:: init                        => none_init
        procedure:: deform                      => none_deform
    end type

contains

    pure function none_get_name() result(name)
        character(:), allocatable:: name

        name = "No Hardening"
    end function

    pure function none_get_description() result(description)
        character(:), allocatable:: description

        description = "Critical resolved shear stress on all slip systems is kept constant at 1."
    end function

    pure function none_get_signature() result(signature)
        integer, dimension(:), allocatable:: signature

        allocate(signature(0))
    end function

    !!> See [[hardening_model_get_parameters]]
    pure function none_get_parameters() result(params)
        type(ParameterDescriptor), dimension(:), allocatable:: params

        allocate(params(0))
    end function

    !> @Brief See hardening_model_init
    !> @Details All slip systems get a CRSS of 1 in both directions to make all slip systems equally hard. Note that this
    !! yields an unrealistic value for the amount of plastic work and that this trick only works if all phases have no hardening.
    function none_init(this, miller_indices, params) result(initial_state)
        class(ConstitutiveModelNone), intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        type(Parameter), dimension(:), intent(in):: params
        class(HardeningState), allocatable:: initial_state

        allocate(HardeningStateNone:: initial_state)
        call this%base_init(miller_indices, initial_state)

        initial_state%crss = 1._DP
    end function

    !> @Brief See hardening_model_deform
    subroutine none_deform(this, state, time, slip_rates)
        class(ConstitutiveModelNone), intent(inout):: this
        class(HardeningState), target, intent(inout)      :: state
        real(DP), intent(in)                    :: time
        real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in):: slip_rates
    end subroutine
end module
