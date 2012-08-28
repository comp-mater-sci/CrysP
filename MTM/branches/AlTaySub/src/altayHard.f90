      !> Dispatcher of hardening models
      module altayHard
      implicit none
      
      
      integer,parameter :: hard_none = 0, hard_voce = 1, hard_pebp =11
      
      ! Workaround: KOST that is not accessible other ways
      integer,save :: KOST_global = 0
      
      contains
      
      
      subroutine readHardParams(inunit,KOST,info)
      use IOConfig
      use hardVoce
#ifdef ALTAY_SUBROUTINE
      use altayConfig
#endif
#ifdef PEBP_ENABLED
      use KOST1x
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
            call precalculateVoceParams(voceCnf,vocePar,info)
      !
#ifdef PEBP_ENABLED     
      case(hard_pebp)
#ifdef ALTAY_SUBROUTINE
            info = InitModuleKOST1x(acnf%hardening%PEBPCnf,KOST,LEC)
#else            
            info = InitModuleKOST1x(inunit,KOST,LEC)
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
      use hardVoce
      implicit none
      double precision,intent(in)   :: GAMMA
      integer,intent(in)            :: KOST
      !
      select case(KOST)
      case(hard_none,hard_PEBP)
            FTAU = 1.D0
      case(hard_Voce)
            FTAU = hardVoceFtau(GAMMA)
      case default
            FTAU = 1.D0
      end select
      !
      end function
      
      end module
      
