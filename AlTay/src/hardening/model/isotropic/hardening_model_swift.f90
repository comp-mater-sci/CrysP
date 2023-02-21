module hardening_model_swift
use altayMiscutils, only: terminate, stopcode_runtimeerror
use altay_definitions, only: dp
use hardening_types
use altayConfig
use hardening_model_isotropic
use hardening_model
use altay_log

implicit none
      
    type, extends(HardeningModelIsotropic) :: HardeningModelSwift
        real(dp)    ::  initial_crss,     &
                        initial_strain,      &   
                        n               
    contains
        procedure :: init       => swift_init
        procedure :: update     => swift_update
    end type

    character(*), parameter :: MODULE_NAME = 'altay_hardening_swift'

    private
    public :: HardeningModelSwift

contains

    subroutine swift_init(this, config)
        class(HardeningModelSwift),             intent(inout)   :: this
        type(HardeningData), intent(in) :: config

        call hardening_model_init(this, config)

        this%initial_strain = config%swiftScnf%gamma0
        this%n = config%swiftScnf%n
        this%initial_crss = config%swiftScnf%crss0

        if (this%initial_strain < 0 .or. this%n < 0) call vef_exception(MODULE_NAME, 'swift_init', VEF_BADVAL, 'Swift params must be greater than 0')
    end subroutine swift_init

    subroutine swift_update(this, grain, time, strain, slip_rates)
        class(HardeningModelSwift), intent(inout)           ::  this
        integer,                    intent(in)              ::  grain
        real(dp),                   intent(in)              ::  time, &
                                                                strain
        real(dp), dimension(this%nss), intent(in) ::  slip_rates

        this%crss = this%initial_crss * (strain / this%initial_strain + 1)**this%n
    end subroutine swift_update

end module hardening_model_swift
