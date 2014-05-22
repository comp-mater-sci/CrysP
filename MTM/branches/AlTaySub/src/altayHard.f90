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
use altayDSHstate
#endif
implicit none
      
      !Hardening law identifier of the initialized module
      integer,save :: HardLawID = hard_None
      
      type(CRSS),private,save :: crss_ratios
      
      interface InitModuleAltayHard
#ifdef ALTAY_SUBROUTINE
            module procedure InitModuleAltayHard_file, InitModuleAltayHard_config
#else
            module procedure InitModuleAltayHard_file
#endif
      end interface

contains
      
      
      subroutine InitModuleAltayHard_file(inunit,KOST,crss_init,info)
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
            call readVoceConfig(inunit,voceCnf,info)
            if (info /= 0) return
            call InitModuleAltayHardLaw_Simple(voceCnf,info)
      !
      case(hard_swiftK)
            ! Swift-K hardening
            call readSwiftKConfig(inunit,swiftKCnf,info)
            if (info /= 0) return
            call InitModuleAltayHardLaw_Simple(swiftKCnf,info)
      !
      case(hard_swiftS)
            ! Swift-S hardening
            call readSwiftSConfig(inunit,swiftSCnf,info)
            if (info /= 0) return
            call InitModuleAltayHardLaw_Simple(swiftSCnf,info)
      !
#ifdef PEBP_ENABLED     
      case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
            info = InitModuleAltayHardLaw_DSH(inunit,KOST,LEC)
#endif
      !
      case default
            ! Unsupported hardening model is requested
            info = -1
      !      
      end select
      !
      HardLawID = KOST
      crss_ratios = crss_init
      !
      end subroutine

#ifdef ALTAY_SUBROUTINE
      !> Initialize module from config data object
      subroutine InitModuleAltayHard_config(config,info)
      implicit none
      type(hardeningData),intent(in)      :: config
      integer,intent(out)                 :: info
      !
      info = -1
      ! Set the module members
      HardLawID = config%HardLawID
      crss_ratios = config%crss_ratios 
      !
      select case(config%HardLawID)
      !
      case(hard_none,hard_voce)
            ! Just for non-hardening and isotropic, Voce-type hardening
            call InitModuleAltayHardLaw_Simple(config%VoceCnf,info)
      !
      case(hard_swiftK)
            ! Swift-K hardening
            call InitModuleAltayHardLaw_Simple(config%swiftKCnf,info)
      !
      case(hard_swiftS)
            ! Swift-S hardening
            call InitModuleAltayHardLaw_Simple(config%swiftSCnf,info)
      !
#ifdef PEBP_ENABLED     
      case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
            info = InitModuleAltayHardLaw_DSH(config%PEBPCnf%params,&
                                              config%HardLawID,LEC)
#endif
      !
      case default
            ! Unsupported hardening model is requested
            info = -1
      !
      end select
      !
      end subroutine
#endif
      
      
      
      double precision function FTAU(GAMMA)
      use altayHardLaw_Simple
      implicit none
      double precision,intent(in)   :: GAMMA
      !
      select case(HardLawID)
      case(hard_none,hard_BP,hard_PEBPscrew,hard_PEBPloop)
            FTAU = 1.D0
      case(hard_voce,hard_swiftK,hard_swiftS)
            FTAU = RefTau(HardLawID,GAMMA)
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
      
