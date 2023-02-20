!> Dispatcher of hardening models
module hardening
    use hardening_types
    use altayIOConfig, only: LEC
    use altayConfig
    use altay_definitions
    use hardening_model
    use hardening_model_swift
    use hardening_model_voce
    use hardening_model_bp
    use hardening_model_pebp_screw
    use hardening_model_pebp_loop
    
    implicit none
    
    class(HardeningModel), allocatable :: model
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
        
        info = 0
        
        if (.not. allocated(model)) then
        select case(config%HardLawID)
            case(hard_none)
                allocate(HardeningModel::model)
            case(hard_voce)
                  ! Just for non-hardening and isotropic, Voce-type hardening
                allocate(HardeningModelVoce::model)
            case(hard_swiftK, hard_swiftS)
                  ! Swift-K hardening
                allocate(HardeningModelSwift::model)
            case(hard_BP)
                allocate(HardeningModelBP::model)
            case(hard_PEBPscrew)
                allocate(HardeningModelPEBPScrew::model)
            case(hard_PEBPloop)
                allocate(HardeningModelPEBPLoop::model)
            case default
                  info = -11
        end select
        end if

        call model%init(config)

        HardLawID = config%HardLawID
    end subroutine

   
    subroutine getTau(gamma, tau, info)
        real(dp),intent(in)   :: gamma
        real(dp),intent(out)  :: tau 
        integer,intent(out)           :: info
        real(dp), dimension(48) :: slip_rates
        real(dp), dimension(2,96) :: crss_buffer
    
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
                  info = -5
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
                if (info == 0) CRSSmatrix%crss = tau
            case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
                CRSSmatrix%crss = model%get_crss(ior)
            case default
                info = -10
        end select
    end subroutine


    subroutine KS_updateState(i,sliprate,deltaT,info)
      integer,intent(in)                              :: i        !< Grain identifier
      real(dp),intent(in), dimension(24)      :: sliprate !< slip rates on 2*12 slip systems
      real(dp),intent(in)                     :: deltaT   !< Time increment
      integer,intent(out)                             :: info
      ! 
        call model%update(i, deltaT, 0.D0, sliprate)
        info = VEF_OK   
    end subroutine




end module
