
!> Implementation of 'simple' hardening laws TAU(GAMMA), i.e. with only 1 internal variable: accumulated slip in grain GAMMA.
!> Available laws:
!>  - DoubleVoce 
!>  - SwiftK: Swift law with K-factor     :: TAU = K * (gamma0+GAMMA)**n
!>  - SwiftS: Swift law with initial crsS :: TAU = crss0 * (1.+GAMMA/gammaA0)**n
module altayHardLaw_Simple
use altayMiscutils, only: terminate, stopcode_runtimeerror
implicit none
      
      integer, private, parameter :: &
            IDdoublevoce  =      0,  &
            IDswift =            1

      
      integer, private, save :: hardID = -1

      !> Configuration parameters of DoubleVoce hardening law. 
      !> Some 'reasonable' defaults are used.
      type :: VoceConfig
            double precision  :: TIII1  = 1.486     
            double precision  :: TIIIS  = 2.476     
            double precision  :: TIVS   = 8.357 
            double precision  :: THIII1 = 2.75      
            double precision  :: THT    = 0.55
      end type

      !> Configuration parameters of SwiftK hardening law. 
      !> Some 'reasonable' defaults are used.
      type :: SwiftKConfig
            double precision  :: K      = 398.1D0 !-> crss0 = 100.     
            double precision  :: gamma0 = 1.D-3     
            double precision  :: n      = 0.2D0 
      end type

      !> Configuration parameters of SwiftS hardening law. 
      !> Some 'reasonable' defaults are used.
      type :: SwiftSConfig
            double precision  :: crss0  = 100.0D0      
            double precision  :: gamma0 = 1.D-3     
            double precision  :: n      = 0.2D0 
      end type
      
      !> Parameters of DoubleVoce hardening law, private to this module.
      type :: VoceParams
            double precision  :: TIII1  = 0.D0     
            double precision  :: TIIIS  = 0.D0     
            double precision  :: TIVS   = 0.D0 
            double precision  :: GAMMAT = 0.D0
            double precision  :: THIII  = 0.D0
            double precision  :: ETA    = 0.D0
            double precision  :: TAUT   = 0.D0
            double precision  :: THIV   = 0.D0
            double precision  :: TIV0   = 0.D0
      end type

      !> Parameters of Swift hardening law, private to this module.
      type :: SwiftParams
            double precision  :: K      = 0.D0     
            double precision  :: gamma0 = 0.D0     
            double precision  :: n      = 0.D0
      end type

      ! Instances of the model configurations/parameters:
      type(VoceConfig),public,save      :: voceCnf
      type(SwiftKConfig),public,save    :: swiftKCnf
      type(SwiftSConfig),public,save    :: swiftSCnf
      !
      type(VoceParams),private,save     :: vocePar
      type(SwiftParams),private,save    :: swiftPar
      
      interface InitModuleAltayHardLaw_Simple !Generic Interface
        module procedure init_voce, init_swiftK, init_swiftS
      end interface      
      
contains

      subroutine readVoceConfig(inunit,c,info)
      use altayIOConfig
      implicit none
      integer,intent(in)                  :: inunit
      type(VoceConfig),intent(out)        :: c
      integer,intent(out)                 :: info
      !
            info = -1
            ! Read the parameters of the work hardening model:
            read (inunit,99,iostat=info) c%TIII1,c%TIIIS,c%TIVS
            if (info /= 0) return
            read (inunit,99,iostat=info) c%THIII1,c%THT
            if (info /= 0) return
  99        format (3f10.0)
            if(NLIST.eq.1) write (IMP,100) c%TIII1,c%TIIIS,c%TIVS,c%THIII1,c%THT
 100        format(' Work hardening model = DOUBLE VOCE-model',/, &
                   ' TAU-III-1=  ',f20.8,/, &
                   ' TAU-III-S = ',F20.8,/, &
                   ' TAU-IV-S =  ',f20.8,/, &
                   ' THETA-III-1=',f20.8,/, &
                   ' THETA-T=    ',f20.8)
            info = 0
      !
      end subroutine

      subroutine readSwiftKConfig(inunit,c,info)
      use altayIOConfig
      implicit none
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
      if(NLIST.eq.1) write (IMP,200) c%K,c%gamma0,c%n
 200        format(' Work hardening model = SWIFT-K model',/, &
                   ' K      = ',f20.8,/, &
                   ' gamma0 = ',f20.8,/, &
                   ' n      = ',f20.8)
      info = 0
      !
      end subroutine

      subroutine readSwiftSConfig(inunit,c,info)
      use altayIOConfig
      implicit none
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
      if(NLIST.eq.1) write (IMP,200) c%crss0,c%gamma0,c%n
 200        format(' Work hardening model = SWIFT-S model',/, &
                   ' crss0  = ',f20.8,/, &
                   ' gamma0 = ',f20.8,/, &
                   ' n      = ',f20.8)
      info = 0
      !
      end subroutine

      subroutine init_voce(c,info)
      use altayIOConfig
      implicit none
      type(VoceConfig),intent(in)         :: c
      integer,intent(out)                 :: info
      !
      type(VoceParams) :: p !trial parameter set
      info = -1
      ! Check validity of inputs:
            if (.not.(c%TIIIS.gt.c%TIII1.and.c%THIII1.gt.c%THT)) then
#ifdef ALTAY_SUBROUTINE
                  return
#else
                  if(NLIST.eq.1) write (IMP,101)
       101  format ('TAU-III-S must be larger than TAU-III-1',/, &
                    'also, THETA-III-1 must be larger than THETA-T')
                  call terminate(stopcode_runtimeerror)
#endif
            endif
            p%THIII=c%THIII1/(1.D0-c%TIII1/c%TIIIS)
            if ((abs(p%THIII) < epsilon(0.D0)) .or. (abs(c%TIIIS) < epsilon(0.D0))) return      
            p%ETA=c%THT/p%THIII
            ! Calculation of transition-gamma
            p%GAMMAT=-c%TIIIS*LOG(p%ETA*c%TIIIS/(c%TIIIS-c%TIII1))/p%THIII
            ! Calculation of transition TAU
            p%TAUT=c%TIIIS-(c%TIIIS-c%TIII1)*exp(-p%THIII*p%GAMMAT/c%TIIIS)
            ! Calculation of theta-IV-0
            p%THIV=c%THT/(1.D0-p%TAUT/c%TIVS)
            ! Calculation of TAU-IV-0
            p%TIV0=c%TIVS+(p%TAUT-c%TIVS)*exp(p%THIV*p%GAMMAT/c%TIVS)
            ! 
            p%TIII1= c%TIII1     
            p%TIIIS= c%TIIIS     
            p%TIVS = c%TIVS
            !      
            if(NLIST.eq.1) write (IMP,102) p%GAMMAT,p%TAUT,p%THIV,p%TIV0
       102  format (' GAMMA-T, TAU-T, THETA-IV-0, TAU-IV-0',/,4d15.5)
      ! Set local identifier for hardening law:
      hardID = IDdoublevoce
      ! Save the trial parameter set p
      vocePar=p
      ! Succesful initialization:
      info = 0
      end subroutine

      subroutine init_swiftK(c,info)
      use altayIOConfig
      implicit none
      type(swiftKConfig),intent(in)       :: c
      integer,intent(out)                 :: info
      !
      type(SwiftParams) :: p
      info = -1
      ! Check validity of inputs:
      if (.not.(c%K.gt.0.D0 .and. c%gamma0.gt.0.D0 .and. c%n.gt.0.D0)) then
#ifdef ALTAY_SUBROUTINE
            return
#else
            if(NLIST.eq.1) write (IMP,101)
       101      format ('All 3 input parameters of SwiftK must be greater than 0.')
            call terminate(stopcode_runtimeerror)
#endif
      endif
      ! Assign Swift parameters
      p%K=c%K
      p%gamma0=c%gamma0
      p%n=c%n
      ! 
      if(NLIST.eq.1) write (IMP,103) p%K,p%gamma0,p%n
       103  format ('Swift: K, gamma0, n: ',/,3d15.5)
      ! Set local identifier for hardening law:
      hardID = IDswift
      ! Save the trial parameter set p
      SwiftPar=p
      ! Succesful initialization:
      info = 0
      end subroutine

      subroutine init_swiftS(c,info)
      use altayIOConfig
      implicit none
      type(swiftSConfig),intent(in)       :: c
      integer,intent(out)                 :: info
      !
      type(SwiftParams) :: p
      info = -1
      ! Check validity of inputs:
      if (.not.(c%crss0.gt.0.D0 .and. c%gamma0.gt.0.D0 .and. c%n.gt.0.D0)) then
#ifdef ALTAY_SUBROUTINE
            return
#else
            if(NLIST.eq.1) write (IMP,101)
       101      format ('All 3 input parameters of SwiftS must be greater than 0.')
            call terminate(stopcode_runtimeerror)
#endif
      endif
      ! Assign Swift parameters
      p%K=c%crss0/(c%gamma0**c%n)
      p%gamma0=c%gamma0
      p%n=c%n
      ! 
      if(NLIST.eq.1) write (IMP,103) p%K,p%gamma0,p%n
       103  format ('Swift: K, gamma0, n: ',/,3d15.5)
      ! Set local identifier for hardening law:
      hardID = IDswift
      ! Save the trial parameter set p
      SwiftPar=p
      ! Succesful initialization:
      info = 0
      end subroutine
      
      double precision function hardFtau(GAMMA) result(TAU)
      implicit none
      double precision,intent(in)         :: GAMMA
      select case (hardID)
      case (IDdoublevoce)
        ! Implementation of the Double-Voce-model
        if (GAMMA.le.vocePar%GAMMAT) then
          TAU=vocePar%TIIIS-(vocePar%TIIIS-vocePar%TIII1)*EXP(-vocePar%THIII*GAMMA/vocePar%TIIIS)
        else
          TAU=vocePar%TIVS-(vocePar%TIVS-vocePar%TIV0)*EXP(-vocePar%THIV*GAMMA/vocePar%TIVS)
        endif
      case (IDswift)
        TAU = swiftPar%K * (swiftPar%GAMMA0+GAMMA)**(swiftPar%n)
      end select
      end function
      
end module