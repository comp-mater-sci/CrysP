!> Dispatcher of hardening modelsR
module hardening
    use hardening_types
    use altayIOConfig, only: LEC
    use definitions
    use parameters

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
        !> Returns the parameter list for a particular hardening model
        !> Also allocates the back-end hardening model
        !> Must be called before initialization
        module function hardening_get_parameters(model_id) result(params) 
            integer, intent(in)                     :: model_id
            type(Parameter), allocatable    :: params(:)
        end function hardening_get_parameters

        !> Initialize module from config data object
        module subroutine hardening_init(params)
            type(Parameter), allocatable, intent(in)    :: params(:)
        end subroutine hardening_init

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
        
        !> Allocate memory to the KS_state array.
        module integer function KS_initState(norient) result(info)
            integer,intent(in)  :: norient 
        end function KS_initState

        module subroutine KS_updateState(i,sliprate,deltaT,info)
            integer,intent(in)                  :: i        
            real(dp),intent(in), dimension(24)  :: sliprate 
            real(dp),intent(in)                 :: deltaT   
            integer,intent(out)                 :: info
        end subroutine KS_updateState
    end interface
end module hardening

submodule(hardening) hardening_imp
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

    module procedure hardening_get_parameters
        if (allocated(model)) deallocate(model)

        select case(model_id)
            case(HARDENING_NONE)
                allocate(HardeningModel::model)
            case(HARDENING_VOCE)
                allocate(HardeningModelVoce::model)
            case(HARDENING_SWIFT)
                allocate(HardeningModelSwift::model)
            case(HARDENING_BP)
                allocate(HardeningModelBP::model)
            case(HARDENING_PEBP_SCREW)
                allocate(HardeningModelPEBPScrew::model)
            case(HARDENING_PEBP_LOOP)
                allocate(HardeningModelPEBPLoop::model)
        end select

        HardLawID = model_id
        params = model%get_parameters()
    end procedure hardening_get_parameters

    module procedure hardening_init
        call model%validate_parameters(params)
        call model%init(params)
    end procedure hardening_init

    module procedure hardening_finalize
        call model%finalize()
    end procedure hardening_finalize
   
    module procedure getTau
        real(dp) :: slip_rates(48)
        real(dp), dimension(:,:), allocatable :: crss_buffer
        
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
        real(dp), allocatable, dimension(:,:) :: tmp
        select case(HardLawID)
            case(HARDENING_NONE)
                ! CRSS of all slip systems equal to 1. (& not dependent on crss_ratios)
                CRSSmatrix%crss = 1.D0 
            case(HARDENING_VOCE, HARDENING_SWIFT)
                call getTau(gamma, tau, info)
                if (info == 0) CRSSmatrix%crss = tau
            case(HARDENING_BP, HARDENING_PEBP_SCREW, HARDENING_PEBP_LOOP)
                tmp = model%get_crss(ior)
                CRSSmatrix%crss(1:2,1:size(tmp,2)) = tmp
            case default
                info = -10
        end select
    end procedure getCRSS

    module procedure KS_updateState
        call model%update(i, deltaT, 0.D0, sliprate)
        info = VEF_OK   
    end procedure KS_updateState
end submodule hardening_imp
