!
! $Id$
!

!> Dispatcher of hardening models
module altayHard
use criErrcodes
use altayHardConstants
use altayCRSSTypes
use altayStateTypes
use altayConfig
!
#ifdef PEBP_ENABLED
use altayIOConfig, only: LEC
use altayHardLaw_DSH
use altayDSHstate
#endif
implicit none
      
    interface HardeningModelParams_init
        module procedure :: HardeningModelParams_initFromConfig
    end interface
    
    contains
      
    !> \todo change intent of config to `out` and reinstate the line that reads 
    !       config%hardLawID
    subroutine altayHard_readConfig(inunit, config, info)
    implicit none
    integer,intent(in)                  :: inunit
    type(HardeningConfig),intent(inout)   :: config
    integer,intent(out)     :: info
    !
    integer :: ierr
    !
        info = criErr_IORead
        !> \todo see todo above the subroutine, uncoment the lines -->>
        !> read(inunit,fmt=*,iostat=ierr) config%hardLawID
        !> if (ierr /= 0) return
        !> <<--
        select case(config%hardLawID)
        !
        case(hard_none,hard_voce)
            ! Just for non-hardening and isotropic, Voce-type hardening
            call readVoceConfig(inunit,config%voceCnf,info)
        !
        case(hard_swiftK)
            ! Swift-K hardening
            call readSwiftKConfig(inunit,config%swiftKCnf,info)
        !
        case(hard_swiftS)
            ! Swift-S hardening
            call readSwiftSConfig(inunit,config%swiftSCnf,info)
        !
        case(hard_KM)
            ! Kocks-Mecking hardening
            info = KMConfig_read(config%kmCnf, inunit)

#ifdef PEBP_ENABLED     
        case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
            info = InitModuleAltayHardLaw_DSH(inunit,HardLaw,LEC)
#endif
        !
        case default
            ! Unsupported hardening model is requested
            info = criErr_IORead
        !      
        end select
    !
    end subroutine

    !
    !> Initialize module from config data object
    subroutine HardeningModelParams_initFromConfig(this,config, info)
    implicit none
    type(HardeningModelParams),intent(out)       :: this
    type(HardeningConfig),intent(in)        :: config
    integer,intent(out)                     :: info
    !
        info = criError
        !
        select case(config%hardLawID)
        !
        case(hard_none)
            info = criSuccess
        case(hard_voce)
            ! Just for non-hardening and isotropic, Voce-type hardening
            call VoceParams_init(config%voceCnf, this%voceParams, info)
        !
        case(hard_swiftK)
            ! Swift-K hardening
            call SwiftParams_init(config%swiftKCnf, this%swiftParams, info)
        !
        case(hard_swiftS)
            ! Swift-S hardening
            call SwiftParams_init(config%swiftSCnf, this%swiftParams, info)
        case(hard_KM)
            ! Kocks-Mecking hardening
            info = KMParameters_init(this%kmParams, config%kmCnf)
        !
#ifdef PEBP_ENABLED
        case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
            info = InitModuleAltayHardLaw_DSH(config%PEBPCnf%this,&
                                                config%hardLawID,LEC)
#endif
        !
        case default
            ! Unsupported hardening model is requested
            info = criErr_BadArgs
        !
        end select
        ! Other actions, common for several hardening models
        select case(config%hardLawID)
        case(hard_voce,hard_swiftK,hard_swiftS)
            this%crss_ratios = config%crss_ratios
        end select
        !
        if (info == criSuccess) this%hardLawID = config%hardLawID
    !
    end subroutine
      
      
    subroutine altayHard_getTau(hardparams, gamma, tau, info)
    implicit none
    type(HardeningModelParams),intent(in)    :: hardparams
    double precision,intent(in)         :: gamma
    double precision,intent(out)        :: tau
    integer,intent(out)                 :: info
    !
        select case(hardparams%hardLawID)
        case(hard_none)
            tau = 1.D0
        !
        case(hard_voce)
            call getTau_Voce(hardparams%voceParams, gamma, tau, info)
        !
        case(hard_swiftK,hard_swiftS)
            call getTau_Swift(hardparams%swiftParams, gamma, tau, info)
        !
#ifdef PEBP_ENABLED
        case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
            tau = 1.D0
#endif
        case default
            tau = 1.D0
            info = criErr_BadArgs
        end select
    !
    end subroutine
      
    subroutine altayHard_getCRSS(hardparams, state, grain_id, crss,info)
    implicit none
    type(HardeningModelParams),intent(in)    :: hardparams
    type(altayStateVariables),intent(in):: state
    integer,intent(in)                  :: grain_id
    type(CRSSData),intent(inout)        :: crss
    integer, intent(out)                :: info
    !
    double precision :: gamma    
    double precision :: tau
    select case(hardparams%hardLawID)
    case(hard_none)
        ! CRSS of all slip systems equal to 1. (& not dependent on crss_ratios)
        crss%crss = 1.D0
    !
    case(hard_voce,hard_swiftK,hard_swiftS)
        gamma = state%grainstates%grainstate(grain_id)%accumulatedshear
        call altayHard_getTau(hardparams,gamma, tau, info)
        if (info == criSuccess) crss%crss = hardparams%crss_ratios%crss * tau
    case(hard_KM)
        !> todo: the 1st argument to be replaced with the good component of new datastructure
        !call KMStateVariables_getCRSS(state%km_state(grain_id), &
        !                              hardparams%kmParams, crss, info)
    !
#ifdef PEBP_ENABLED
    case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
        call KS_getCRSS(ior,CRSSmatrix,info)
#endif
    case default
        info = -1
    end select
    end subroutine
      
end module
      
