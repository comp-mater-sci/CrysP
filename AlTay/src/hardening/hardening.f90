!> Dispatcher of hardening modelsR
module Hardening
    use hardening_types
    use altayIOConfig, only: LEC
    use altayConfig, only: hardeningData
    use altay_definitions

    implicit none
    public
    
    enum, bind(C)
        !> Constants set for backwards compatibility with input file format.
        enumerator  ::  HARDENING_NONE       = 0,  &
                        HARDENING_VOCE       = 1,  &
                        HARDENING_SWIFT      = 3,  &
                        HARDENING_BP         = 11, &
                        HARDENING_PEBP_SCREW = 12, &
                        HARDENING_PEBP_LOOP  = 13     
    end enum

    interface
        !> Initialize module from config data object
        module subroutine  InitModuleAltayHard(config,info)
            type(hardeningData),intent(in)      :: config
            integer,intent(out)                 :: info
        end subroutine 

        module subroutine hardening_finalize()
        end subroutine hardening_finalize
        
        module subroutine getTau(gamma, tau, info)
            real(dp),intent(in)         :: gamma
            real(dp),intent(out)        :: tau 
            integer,intent(out)         :: info
        end subroutine getTau
        
        module subroutine getCRSS(ior, gamma, CRSSmatrix, info)
            integer, intent(in)                     :: ior
            real(dp), intent(in)                    :: gamma
            type(CRSS), intent(out)                 :: CRSSmatrix
            integer, intent(out)                    :: info
        end subroutine getCRSS
        
        module subroutine KS_updateState(i,sliprate,deltaT,info)
            integer,intent(in)                  :: i        
            real(dp),intent(in), dimension(24)  :: sliprate 
            real(dp),intent(in)                 :: deltaT   
            integer,intent(out)                 :: info
        end subroutine KS_updateState
    end interface
end module Hardening

submodule(Hardening) Hardening_Imp
    use hardening_model
    use hardening_model_swift
    use hardening_model_voce
    use hardening_model_bp
    use hardening_model_pebp_screw
    use hardening_model_pebp_loop

    implicit none
    
    class(HardeningModel), allocatable :: model
    integer :: HardLawID !<Hardening law identifier of the initialized module

contains

    !> Initialize module from config data object
    module procedure InitModuleAltayHard
        
        info = 0
        
        if (.not. allocated(model)) then
        select case(config%HardLawID)
            case(HARDENING_NONE)
                allocate(HardeningModel::model)
            case(HARDENING_VOCE)
                  ! Just for non-hardening and isotropic, Voce-type hardening
                allocate(HardeningModelVoce::model)
            case(HARDENING_SWIFT)
                  ! Swift-K hardening
                allocate(HardeningModelSwift::model)
            case(HARDENING_BP)
                allocate(HardeningModelBP::model)
            case(HARDENING_PEBP_SCREW)
                allocate(HardeningModelPEBPScrew::model)
            case(HARDENING_PEBP_LOOP)
                allocate(HardeningModelPEBPLoop::model)
            case default
                  info = -11
        end select
        end if

        call model%init(config)

        HardLawID = config%HardLawID
    end procedure

    module procedure hardening_finalize
        call model%finalize()
    end procedure hardening_finalize
   
    module procedure getTau
        real(dp) :: slip_rates(48), &
                    crss_buffer(2,96)
        
        info = 0 

        select case(HardLawID)
            case(HARDENING_NONE,HARDENING_BP,HARDENING_PEBP_SCREW,HARDENING_PEBP_LOOP)
                  tau = 1.D0
            case(HARDENING_VOCE, HARDENING_SWIFT)
                    call model%update(1, 0.D0, gamma, slip_rates)
                    crss_buffer = model%get_crss(1)
                    tau = crss_buffer(1,1)
            case default
                  tau = 1.D0
                  info = -5
        end select
    end procedure getTau

    module procedure getCRSS
        real(dp) :: tau
        
        select case(HardLawID)
            case(HARDENING_NONE)
                ! CRSS of all slip systems equal to 1. (& not dependent on crss_ratios)
                CRSSmatrix%crss = 1.D0 
            case(HARDENING_VOCE, HARDENING_SWIFT)
                call getTau(gamma, tau, info)
                if (info == 0) CRSSmatrix%crss = tau
            case(HARDENING_BP, HARDENING_PEBP_SCREW, HARDENING_PEBP_LOOP)
                CRSSmatrix%crss = model%get_crss(ior)
            case default
                info = -10
        end select
    end procedure getCRSS

    module procedure KS_updateState
        call model%update(i, deltaT, 0.D0, sliprate)
        info = VEF_OK   
    end procedure KS_updateState
end submodule Hardening_Imp

