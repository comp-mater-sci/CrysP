      !> Dispatcher of hardening models
      module altayHard
      implicit none
      
      integer,parameter ::          &
            hard_none =          0, & 
            hard_voce =          1, &
            hard_BP =           11, &
            hard_PEBPscrew =    12, &
            hard_PEBPloop =     13
      
      ! Workaround: KOST that is not accessible other ways
      integer,save :: KOST_global = 0
      
      contains
      
      
      subroutine InitModuleAltayHard(inunit,KOST,info)
      use altayIOConfig
      use altayHardLaw_Simple
#ifdef ALTAY_SUBROUTINE
      use altayConfig
#endif
#ifdef PEBP_ENABLED
      use altayHardLaw_DSH
#endif
      implicit none
      integer,intent(in)      :: inunit
      integer,intent(in)      :: KOST
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
      KOST_global = KOST
      !
      end subroutine
      

      
      
      double precision function FTAU(GAMMA,KOST)
      use altayHardLaw_Simple
      implicit none
      double precision,intent(in)   :: GAMMA
      integer,intent(in)            :: KOST
      !
      select case(KOST)
      case(hard_none,hard_BP,hard_PEBPscrew,hard_PEBPloop)
            FTAU = 1.D0
      case(hard_Voce)
            FTAU = hardFtau(GAMMA)
      case default
            FTAU = 1.D0
      end select
      !
      end function
      
      end module
      
