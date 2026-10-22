module none
    use base_defs
    use crysp_status
    use crysp_constitutive_model
    use crysp_grain
    use crysp_parameters
    use crysp_input
    use crysp_model

    implicit none

    private
    public:: ConstitutiveModelNone

    !> @Brief Hardening model representing no hardening.
    type, extends(ConstitutiveModel):: ConstitutiveModelNone
    contains
        procedure:: describe => none_describe
        procedure:: init_hardening_state    => none_init_hardening_state
    end type

contains

    function none_describe(this) result(desc)
        class(ConstitutiveModelNone), intent(in):: this
        type(ModelDescriptor):: desc

        desc = this%ConstitutiveModel%describe()
        desc%name = "No hardening"
        desc%description = "Critical resolved shear stress on all slip systems is kept constant at 1."
    end function

    subroutine none_init_hardening_state(this, state)
        class(ConstitutiveModelNone), intent(in):: this
        class(HardeningState), intent(out):: state

        call this%ConstitutiveModel%init_hardening_state(state)
        state%crss = 1._DP
    end subroutine
end module
