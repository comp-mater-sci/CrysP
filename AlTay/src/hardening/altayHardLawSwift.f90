!> Implementation of 'simple' hardening laws TAU(GAMMA), i.e. with only 1 internal variable: accumulated slip in grain GAMMA.
!> Available laws:
!>  - DoubleVoce
!>  - SwiftK: Swift law with K-factor     :: TAU = K * (gamma0+GAMMA)**n
!>  - SwiftS: Swift law with initial crsS :: TAU = crss0 * (1.+GAMMA/gammaA0)**n
module altayHardLaw_swift
use altayMiscutils, only: terminate, stopcode_runtimeerror
use altay_definitions, only: dp
use altayHardTypes
implicit none

      !> Configuration parameters of SwiftK hardening law.
      !> Some 'reasonable' defaults are used.
      type :: SwiftKConfig
            real(dp)  :: K      = 398.1D0 !-> crss0 = 100.
            real(dp)  :: gamma0 = 1.D-3
            real(dp)  :: n      = 0.2D0
      end type

      !> Configuration parameters of SwiftS hardening law.
      !> Some 'reasonable' defaults are used.
      type :: SwiftSConfig
            real(dp)  :: crss0  = 100.0D0
            real(dp)  :: gamma0 = 1.D-3
            real(dp)  :: n      = 0.2D0
      end type

      
    type, extends(HardeningModelIsotropic) :: HardeningModelSwift
        real(dp)    ::  k           = 398.1D0,  &
                        gamma0      = 1.D-3,    &   
                        n           = 0.2D0
    contains
        procedure :: init       => swift_init
        procedure :: update     => swift_update
    end type

    character(*), parameter :: MODULE_NAME = 'altay_hardening_swift'

contains

      subroutine readSwiftKConfig(inunit,c,info)
      use altayIOConfig
      integer,intent(in)                  :: inunit
      type(SwiftKConfig),intent(out)      :: c
      integer,intent(out)                 :: info
      !
      info = -1
      ! Read the parameters of the work hardening model:
      read (inunit,99,iostat=info) c%K
      if (info /= 0) return
      read (inunit,99,iostat=info) c%gamma0
      if (info /= 0) return
      read (inunit,99,iostat=info) c%n
      if (info /= 0) return
  99        format (3f10.0)
      if(NLIST == 1) write (IMP,200) c%K,c%gamma0,c%n
 200        format(' Work hardening model = SWIFT-K model',/, &
                   ' K      = ',f20.8,/, &
                   ' gamma0 = ',f20.8,/, &
                   ' n      = ',f20.8)
      info = 0
      !
      end subroutine

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

        base_hardening_model_init(this, config)

        select case(config%hardlawid)
            case(hard_swifts)
                this%k = config%swiftkcnf%k
                this%gamma0 = config%swiftkcnf%gamma0
                this%n = config%swiftkcnf%n
            case(hard_swiftk)
                this%gamma0 = config%swiftScnf%gamma0
                this%n = config%swiftScnf%n
                this%k = config%swiftScnf%crss0 / (this%gamma0**this%n)
        end select

        if (this%gamma0 <= 0 .or. this%n <= 0 .or. this%k <= 0) call vef_exception(MODULE_NAME, 'swift_init', VEF_BADVAL, 'Swift params must be greater than 0')
    end subroutine

    subroutine swift_update(this, grain, time, strain, slip_rates)
        class(HardeningModelSwift), intent(inout)           ::  this
        integer,                    intent(in)              ::  grain
        real(dp),                   intent(in)              ::  time, &
                                                                strain
        real(dp), dimension(this%nss), intent(in) ::  slip_rates

        this%crss = this%k * (strain + this%gamma0)**(this%n)    
    end subroutine

end module
