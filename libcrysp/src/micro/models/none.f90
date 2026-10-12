module none
    use base_defs
    use crysp_status
    use constitutive_model
    use crysp_grain
    use crysp_serialization
    use crysp_input
    use crysp_model

    implicit none

    private
    public:: ConstitutiveModelNone

    !> @Brief Hardening model representing no hardening.
    type, extends(ConstitutiveModel):: ConstitutiveModelNone
    contains
        procedure, nopass:: get_name        => none_get_name
        procedure, nopass:: get_description => none_get_description
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

    subroutine none_init_hardening_state(this, state)
        class(ConstitutiveModelNone), intent(in):: this
        class(HardeningState), intent(out):: state

        call this%ConstitutiveModel%init_hardening_state(state)
        state%crss = 1._DP
    end subroutine
end module
