module altay_hardening_model
    use altay_definitions, only: dp
    use altayConfig
    
    implicit none

    !>Basic hardening model implementation.
    !>No hardening occurs.
    !>All other hardening models extend this type.   
    type :: BaseHardeningModel
        integer :: nss
    contains
        procedure, pass(this)  :: init         => hardening_model_init
        procedure, pass(this)  :: get_crss     => hardening_model_get_crss
        procedure, pass(this)  :: update       => hardening_model_update
        procedure, pass(this)  :: finalize     => hardening_model_finalize
    end type

contains

    !>Initialize nss using params(1)
    !>@return the number of parameters used by this class
    subroutine hardening_model_init(this, config)
        class(BaseHardeningModel), intent(inout)    ::  this
        type(HardeningData), intent(in)             ::  config

        this%nss = 96
    end subroutine hardening_model_init

    !>Do nothing
    subroutine hardening_model_update(this, grain, time, strain, slip_rates)
        class(BaseHardeningModel), intent(inout)                ::  this
        integer, intent(in)                                 ::  grain
        real(dp), intent(in)                                ::  time, &
                                                                strain
        real(dp), dimension(this%nss), intent(in) ::  slip_rates
    end subroutine

    !>All values 1.0
    function hardening_model_get_crss(this, grain) result(crss)
        class(BaseHardeningModel), intent(in)               :: this
        integer, intent(in)                             :: grain
        real(dp), dimension(2, this%nss)                :: crss 

        crss = 1.D0
    end function

    !>Do nothing
    subroutine hardening_model_finalize(this)
        class(BaseHardeningModel), intent(inout)    :: this
    end subroutine 
end module
