!> Dispatcher of hardening models
module altayHard
    use altayHardTypes
    use altayIOConfig, only: LEC
    use altayHardLaw_Simple
    use altayConfig
    use altayHardLaw_DSH
    use altayDSHState
    use altay_definitions, only: dp
    
    implicit none
    
    integer, save :: HardLawID = hard_invalid !<Hardening law identifier of the initialized module
    type(CRSS), save :: crss_ratios
    
    interface InitModuleAltayHard
        module procedure InitModuleAltayHard_file, InitModuleAltayHard_config
    end interface

    private
    public  ::  hard_BP,                &
                hard_PEBPscrew,         &
                hard_PEBPloop,          &
                initModuleAltayHard,    &
                hard_none,              &
                hard_voce,              &
                hard_swifts,            &
                hard_swiftK,            &
                gettau,                 &
                getcrss    
    
contains

    subroutine InitModuleAltayHard_file(inunit,HardLaw,crss_init,info)
        integer,intent(in)      :: inunit
        integer,intent(in)      :: HardLaw
        type(CRSS),intent(in)   :: crss_init
        integer,intent(out)     :: info
        
        ! Instances of the model configurations/parameters:
        type(VoceConfig)   :: voceCnf
        type(SwiftKConfig) :: swiftKCnf
        type(SwiftSConfig) :: swiftSCnf
        
        info = -1
        
        select case(HardLaw)
            case(hard_none,hard_voce)
                ! Just for non-hardening and isotropic, Voce-type hardening
                call readVoceConfig(inunit,voceCnf,info)
                if (info /= 0) return
                if (HardLaw == hard_voce) call InitModuleAltayHardLaw_Simple(voceCnf,info)
            case(hard_swiftK)
                ! Swift-K hardening
                call readSwiftKConfig(inunit,swiftKCnf,info)
                if (info /= 0) return
                call InitModuleAltayHardLaw_Simple(swiftKCnf,info)
            case(hard_swiftS)
                ! Swift-S hardening
                call readSwiftSConfig(inunit,swiftSCnf,info)
                if (info /= 0) return
                call InitModuleAltayHardLaw_Simple(swiftSCnf,info)
            case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
                info = InitModuleAltayHardLaw_DSH(inunit,HardLaw,LEC)
            case default
                info = -1
        end select
        
        HardLawID = HardLaw
        crss_ratios = crss_init
    end subroutine

    !> Initialize module from config data object
    subroutine InitModuleAltayHard_config(config,info)
        type(hardeningData),intent(in)      :: config
        integer,intent(out)                 :: info
        
        info = -1
        
        select case(config%HardLawID)
            case(hard_none)
                  info = 0
            case(hard_voce)
                  ! Just for non-hardening and isotropic, Voce-type hardening
                  call InitModuleAltayHardLaw_Simple(config%VoceCnf,info)
            case(hard_swiftK)
                  ! Swift-K hardening
                  call InitModuleAltayHardLaw_Simple(config%swiftKCnf,info)
            case(hard_swiftS)
                  ! Swift-S hardening
                  call InitModuleAltayHardLaw_Simple(config%swiftSCnf,info)
            case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
                  info = InitModuleAltayHardLaw_DSH(config%PEBPCnf%params, config%HardLawID,LEC)
            case default
                  info = -1
        end select

        if (info /= 0) return

        HardLawID = config%HardLawID
        crss_ratios = config%crss_ratios
    end subroutine


    subroutine getTau(gamma, tau, info)
        use altayHardLaw_Simple

        real(dp),intent(in)   :: gamma
        real(dp),intent(out)  :: tau
        integer,intent(out)           :: info
        
        info = 0

        select case(HardLawID)
            case(hard_none,hard_BP,hard_PEBPscrew,hard_PEBPloop)
                  tau = 1.D0
            case(hard_voce,hard_swiftK,hard_swiftS)
                  call getRefTau(HardLawID, gamma, tau, info)
            case default
                  tau = 1.D0
                  info = -1
        end select
    end subroutine

    subroutine getCRSS(ior, gamma, CRSSmatrix, info)
        integer, intent(in)             :: ior
        real(dp), intent(in)    :: gamma
        type(CRSS), intent(out)         :: CRSSmatrix
        integer, intent(out)            :: info
        real(dp)                :: tau

        select case(HardLawID)
            case(hard_none)
                ! CRSS of all slip systems equal to 1. (& not dependent on crss_ratios)
                CRSSmatrix%crss = 1.D0 
            case(hard_voce,hard_swiftK,hard_swiftS)
                call getTau(gamma, tau, info)
                if (info == 0) CRSSmatrix%crss = crss_ratios%crss * tau
            case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
                call KS_getCRSS(ior,CRSSmatrix,info)
            case default
                info = -1
        end select
    end subroutine

end module
