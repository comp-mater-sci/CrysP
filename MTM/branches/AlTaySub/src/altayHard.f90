!> Dispatcher of hardening models
module altayHard
use altayHardTypes
use altayIOConfig, only: LEC
use altayHardLaw_Simple
#ifdef ALTAY_SUBROUTINE
use altayConfig
#endif
#ifdef PEBP_ENABLED
use altayHardLaw_DSH
use AltayDSHstate
#endif
implicit none

      !Following identifier sets CRSS == 1.0 for all slip systems (independent of the inputted "crss_ratios"):
      integer,parameter :: hard_none =          0 
      !Following identifiers invoke module altayHardLaw_Simple for the reference-crss "RefTau".
      ! CRSS for each individual slip system is multiplied with inputted "crss_ratios". 
      ! Note that "RefTau" is work-equivalent to the total slip rate ONLY IF all "crss_ratios" == 1.
      integer,parameter :: hard_voce =          1, &
                           hard_swiftK =        2, &
                           hard_swiftS =        3
      !Following identifiers invoke module altayHardLaw_DSH, resulting in generally different CRSS for the slip systems.
      ! The reference-crss "RefTau" is arbitrarily set to 1.
      integer,parameter :: hard_BP =           11, &
                           hard_PEBPscrew =    12, &
                           hard_PEBPloop =     13
      
      !Hardening law identifier of the initialized module
      integer,save :: HardLawID = 0
      
      
      type(CRSS),private,save :: crss_ratios
      
contains
      
      
      subroutine InitModuleAltayHard(inunit,KOST,crss_init,info)
      implicit none
      integer,intent(in)      :: inunit
      integer,intent(in)      :: KOST
      type(CRSS),intent(in)   :: crss_init
      integer,intent(out)     :: info
      !
      info = -1
      !
      select case(KOST)
      !
      case(hard_none,hard_voce)
            ! Just for non-hardening and isotropic, Voce-type hardening
#ifdef ALTAY_SUBROUTINE
            voceCnf = acnf%hardening%VoceCnf
#else
            call readVoceConfig(inunit,voceCnf,info)
            if (info /= 0) return
#endif
            call InitModuleAltayHardLaw_Simple(voceCnf,info)
      !
      case(hard_swiftK)
            ! Swift-K hardening
#ifdef ALTAY_SUBROUTINE
            !swiftKCnf = ??? <-------------- 
#else
            call readSwiftKConfig(inunit,swiftKCnf,info)
            if (info /= 0) return
#endif
            call InitModuleAltayHardLaw_Simple(swiftKCnf,info)
      !
      case(hard_swiftS)
            ! Swift-S hardening
#ifdef ALTAY_SUBROUTINE
            !swiftSCnf = ??? <-------------- 
#else
            call readSwiftSConfig(inunit,swiftSCnf,info)
            if (info /= 0) return
#endif
            call InitModuleAltayHardLaw_Simple(swiftSCnf,info)
      !
#ifdef PEBP_ENABLED     
      case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
#ifdef ALTAY_SUBROUTINE
            info = InitModuleAltayHardLaw_DSH(acnf%hardening%PEBPCnf%params,KOST,LEC)
#else            
            info = InitModuleAltayHardLaw_DSH(inunit,KOST,LEC)
#endif
#endif
      !
      case default
            ! Unsupported hardening model is requested
            info = -1
      !      
      end select
      !
      HardLawID = KOST
      !
#ifdef ALTAY_SUBROUTINE
      !following line of code yet to be tested!!!
      !crss_ratios = acnf%slipsystem%crss_ratios 
#else            
      crss_ratios = crss_init
#endif      
      !
      end subroutine
      
      double precision function FTAU(GAMMA)
      use altayHardLaw_Simple
      implicit none
      double precision,intent(in)   :: GAMMA
      !
      select case(HardLawID)
      case(hard_none,hard_BP,hard_PEBPscrew,hard_PEBPloop)
            FTAU = 1.D0
      case(hard_voce,hard_swiftK,hard_swiftS)
            FTAU = RefTau(GAMMA)
      case default
            FTAU = 1.D0
      end select
      !
      end function
      
      subroutine getCRSS(ior,gamma,CRSSmatrix,info)
      implicit none
      integer,intent(in)                           :: ior
      double precision,intent(in)                  :: gamma         
      type(CRSS),intent(out)                       :: CRSSmatrix
      integer, intent(out)                         :: info
      !
      select case(HardLawID)
      case(hard_none)
            CRSSmatrix%crss = 1.D0 ! CRSS of all slip systems equal to 1. (& not dependent on crss_ratios)         
      case(hard_voce,hard_swiftK,hard_swiftS)
            CRSSmatrix%crss = crss_ratios%crss * FTAU(gamma)
#ifdef PEBP_ENABLED
      case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
            call KS_getCRSS(ior,CRSSmatrix,info)
#endif
      case default
        info = -1
      end select
      end subroutine
      
end module
      
