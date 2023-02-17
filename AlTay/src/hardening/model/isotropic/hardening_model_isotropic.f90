module hardening_model_isotropic
    use altay_definitions, only: dp
    use hardening_model

    implicit none

    !>Superclass for simple hardening models yielding the same CRSS for all slip systems.
    type, abstract, extends(HardeningModel) :: HardeningModelIsotropic
        real(dp)    ::  crss
    contains
        procedure :: get_crss => hardening_model_isotropic_get_crss
    end type
    
    private
    public :: HardeningModelIsotropic

contains

    function hardening_model_isotropic_get_crss(this, grain) result(crss)
        class(HardeningModelIsotropic), intent(in) :: this
        integer, intent(in)                     :: grain
        real(dp), dimension(2,this%nss)         :: crss

        crss = this%crss
    end function
end module hardening_model_isotropic
