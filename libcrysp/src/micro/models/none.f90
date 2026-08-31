module none
    use base_defs
    use constitutive_model
    use crysp_grain
    use crysp_serialization
    use crysp_input
    use mod_model

    implicit none

    private
    public:: ConstitutiveModelNone

    !> @Brief Hardening model representing no hardening.
    type, extends(ConstitutiveModel):: ConstitutiveModelNone
    contains
        procedure, nopass:: get_name        => none_get_name
        procedure, nopass:: get_description => none_get_description
        procedure:: deform                  => none_deform
        procedure:: make_hardening_state    => none_make_hardening_state
        procedure:: init_hardening_state    => none_init_hardening_state
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

    !> @Brief See hardening_model_deform
    subroutine none_deform(this, state, time, slip_rates)
        class(ConstitutiveModelNone), intent(inout):: this
        class(HardeningState), target, intent(inout)      :: state
        real(DP), intent(in)                    :: time
        real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in):: slip_rates
    end subroutine

    pure function none_make_hardening_state() result(state)
        class(HardeningState), allocatable:: state

        allocate(state)
    end function

    pure subroutine none_init_hardening_state(this, state)
        class(ConstitutiveModelNone), intent(in):: this
        class(HardeningState), intent(out):: state

        call this%ConstitutiveModel%init_hardening_state(state)
        state%crss = 1._DP
    end subroutine
end module
