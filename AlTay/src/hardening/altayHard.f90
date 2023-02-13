!> Dispatcher of hardening models
module altayHard
    use altayHardTypes
    use altayIOConfig, only: LEC
    use altayConfig
    use altayHardLaw_DSH
    use altayHardLaw_voce
    use altayHardLaw_swift
    use altay_definitions, only: dp
    use altay_hardening_model
    use altay_hardening_model_voce
    use altay_hardening_model_swift
    use altay_hardening_model_dsh
    
    implicit none
    
    class(BaseHardeningModel), allocatable :: model
    integer, save :: HardLawID = hard_invalid !<Hardening law identifier of the initialized module
    
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
                KS_updatestate,         &
                getcrss    
    
contains

    !> Initialize module from config data object
    subroutine InitModuleAltayHard(config,info)
        type(hardeningData),intent(in)      :: config
        integer,intent(out)                 :: info
        
        info = -1
        
        select case(config%HardLawID)
            case(hard_none)
                info = 0
                allocate(BaseHardeningModel::model)
            case(hard_voce)
                  ! Just for non-hardening and isotropic, Voce-type hardening
                allocate(HardeningModelVoce::model)
            case(hard_swiftK, swiftS)
                  ! Swift-K hardening
                allocate(HardeningModelSwift::model)
            case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
                allocate(HardeningModelDSH::model)
            case default
                  info = -1
        end select

        if (info /= 0) return
        model%init(config%hardlawid, config)

        HardLawID = config%HardLawID
        crss_ratios = config%crss_ratios
    end subroutine

   
    subroutine getTau(gamma, tau, info)
        real(dp),intent(in)   :: gamma
        real(dp),intent(out)  :: tau 
        integer,intent(out)           :: info
        real(dp), dimension(48) :: slip_rates
        real(dp), dimension(2,48) :: crss_buffer
    
        info = 0 

        select case(HardLawID)
            case(hard_none,hard_BP,hard_PEBPscrew,hard_PEBPloop)
                  tau = 1.D0
            case(hard_voce,hard_swiftK,hard_swiftS)
                    call model%update(1, 0.D0, gamma, slip_rates)
                    crss_buffer = model%get_crss(1)
                    tau = crss_buffer(1,1)
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
                CRSSmatrix%crss = model%get_crss(ior)
            case default
                info = -1
        end select
    end subroutine


    subroutine KS_updateState(i,sliprate,deltaT,info)
      integer,intent(in)                              :: i        !< Grain identifier
      double precision,intent(in), dimension(24)      :: sliprate !< slip rates on 2*12 slip systems
      double precision,intent(in)                     :: deltaT   !< Time increment
      integer,intent(out)                             :: info
      !   
        call model%update(i, deltaT, 0.D0, sliprate)
        info = VEF_OK   
      end subroutine




end module
