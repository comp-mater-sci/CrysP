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
        real(dp)    ::  initial_crss    = 398.1D0,  &
                        initial_strain  = 1.D-3,    &   
                        n               = 0.2D0
    contains
        procedure :: init       => swift_init
        procedure :: update     => swift_update
    end type

    character(*), parameter :: MODULE_NAME = 'altay_hardening_swift'

    private
    public :: HardeningModelSwift

contains

      subroutine readSwiftSConfig(inunit,c,info)
      use altayIOConfig
      integer,intent(in)                  :: inunit
      type(SwiftSConfig),intent(out)      :: c
      integer,intent(out)                 :: info
      !
      info = -1
      ! Read the parameters of the work hardening model:
      read (inunit,99,iostat=info) c%crss0
      if (info /= 0) return
      read (inunit,99,iostat=info) c%gamma0
      if (info /= 0) return
      read (inunit,99,iostat=info) c%n
      if (info /= 0) return
  99        format (3f10.0)
      if(NLIST == 1) write (IMP,200) c%crss0,c%gamma0,c%n
 200        format(' Work hardening model = SWIFT-S model',/, &
                   ' crss0  = ',f20.8,/, &
                   ' gamma0 = ',f20.8,/, &
                   ' n      = ',f20.8)
      info = 0
      !
      end subroutine


    subroutine swift_init(this, config)
        class(HardeningModelSwift),             intent(inout)   :: this
        type(HardeningData), intent(in) :: config

        call hardening_model_init(this, config)

        this%initial_strain = config%swiftScnf%gamma0
        this%n = config%swiftScnf%n
        this%initial_crss = config%swiftScnf%crss0 

        if (this%initial_strain <= 0 .or. this%n <= 0 .or. this%initial_crss <= 0) call vef_exception(MODULE_NAME, 'swift_init', VEF_BADVAL, 'Swift params must be greater than 0')
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
