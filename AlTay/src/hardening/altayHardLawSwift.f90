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

      !> Parameters of Swift hardening law, private to this module.
      type :: SwiftParams
            real(dp)  :: K      = 0.D0
            real(dp)  :: gamma0 = 0.D0
            real(dp)  :: n      = 0.D0
      end type
      !
      type(SwiftParams),private,save    :: swiftPar

      interface InitModuleAltayHardLaw_swift !Generic Interface
            module procedure init_swiftK, init_swiftS
      end interface

      integer,save,private :: configured_law_id = hard_invalid

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

      subroutine init_swiftK(c,info)
      use altayIOConfig
      type(swiftKConfig),intent(in)       :: c
      integer,intent(out)                 :: info
      !
      type(SwiftParams) :: p
      info = -1
      ! Check validity of inputs:
      if (.not.(c%K > 0.D0 .and. c%gamma0 > 0.D0 .and. c%n > 0.D0)) then
#ifdef ALTAY_SUBROUTINE
            return
#else
            if(NLIST == 1) write (IMP,101)
       101      format ('All 3 input parameters of SwiftK must be greater than 0.')
            call terminate(stopcode_runtimeerror)
#endif
      endif
      ! Assign Swift parameters
      p%K=c%K
      p%gamma0=c%gamma0
      p%n=c%n
      !
      if(NLIST == 1) write (IMP,103) p%K,p%gamma0,p%n
       103  format ('Swift: K, gamma0, n: ',/,3d15.5)
      configured_law_id = hard_swiftK
      ! Save the trial parameter set p
      SwiftPar=p
      ! Succesful initialization:
      info = 0
      end subroutine

      subroutine init_swiftS(c,info)
      use altayIOConfig
      type(swiftSConfig),intent(in)       :: c
      integer,intent(out)                 :: info
      !
      type(SwiftParams) :: p
      info = -1
      ! Check validity of inputs:
      if (.not.(c%crss0 > 0.D0 .and. c%gamma0 > 0.D0 .and. c%n > 0.D0)) then
#ifdef ALTAY_SUBROUTINE
            return
#else
            if(NLIST == 1) write (IMP,101)
       101      format ('All 3 input parameters of SwiftS must be greater than 0.')
            call terminate(stopcode_runtimeerror)
#endif
      endif
      ! Assign Swift parameters
      p%K=c%crss0/(c%gamma0**c%n)
      p%gamma0=c%gamma0
      p%n=c%n
      !
      if(NLIST == 1) write (IMP,103) p%K,p%gamma0,p%n
       103  format ('Swift: K, gamma0, n: ',/,3d15.5)
      configured_law_id = hard_swiftS
      ! Save the trial parameter set p
      SwiftPar=p
      ! Succesful initialization:
      info = 0
      end subroutine

      subroutine getRefTau(hardID,gamma,RefTau,info)
      integer,intent(in)                  :: hardID
      real(dp),intent(in)         :: gamma
      real(dp),intent(out)        :: RefTau
      integer,intent(out)                 :: info
     
            info = -1
            if (hardID /= configured_law_id) return
            info = 0
            RefTau = swiftPar%K * (swiftPar%GAMMA0+gamma)**(swiftPar%n)
      
      end subroutine

end module
