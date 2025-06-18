module grain_module
    use base_defs
    use conversions
    use logging
    use constitutive_model

    implicit none

    private
    public  ::  Grain

    !>Texture-related state variables for single grain
    type:: Grain
        real(DP), dimension(3, 3):: orientation
        class(ConstitutiveModel), pointer:: model
        class(HardeningState), allocatable:: state
    contains
        procedure:: init              => grain_init
    end type grain

contains

    !Initialize grain.
    subroutine grain_init(this, orientation, model, state)
        class(Grain), intent(out):: this
        real(DP), dimension(:), intent(in):: orientation
        class(ConstitutiveModel), target, intent(in):: model
        class(HardeningState), intent(in):: state

        this%orientation = euler_to_tensor(orientation)
        this%model => model
        this%state = state
    end subroutine
end module
