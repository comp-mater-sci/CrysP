module altay_hardening_model_isotropic
    use altay_definitions, only: dp
    use altay_hardening_model

    implicit none

    !>Superclass for simple hardening models yielding the same CRSS for all slip systems.
    type, abstract, extends(BaseHardeningModel) :: HardeningModelIsotropic
        real(dp)    ::  crss
    contains
        procedure :: get_crss => hardening_model_isotropic_get_crss
        procedure :: init => hardening_model_isotropic_init
    end type

contains

    function hardening_model_isotropic_init(this, variant, params) result(params_left)
        class(HardeningModelIsotropic),         intent(inout)   :: this
        integer,                                intent(in)      :: variant
        real(dp), dimension(:), allocatable,    intent(inout)   :: params
        real(dp), dimension(:), allocatable                     :: params_left

        params_left = hardening_model_init(this, variant, params)
    end function

    function hardening_model_isotropic_get_crss(this, grain) result(crss)
        class(HardeningModelIsotropic), intent(in) :: this
        integer, intent(in)                     :: grain
        real(dp), dimension(2,this%nss)         :: crss

        crss = this%crss
    end function
end module
