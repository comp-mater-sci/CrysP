module hardening_model_voce
use altayMiscutils, only: terminate, stopcode_runtimeerror
use altay_definitions, only: dp
use hardening_types
use hardening_model
use hardening_model_isotropic
use altayConfig
use altay_log
implicit none

    type :: stage
        real(dp) :: TS, &
                    T1, &
                    TH  
    end type

    type, extends(HardeningModelIsotropic)   :: HardeningModelVoce
        real(dp)    ::  transition_strain = 0.D0
        type(stage) ::  stage_1,    &   
                        stage_2
    contains
        procedure :: init           => voce_init
        procedure :: update         => voce_update
    end type

    character(*), parameter :: MODULE_NAME = 'altayHardLaw_voce'

    private
    public :: HardeningModelVoce

contains

      subroutine readVoceConfig(inunit,c,info)
      use altayIOConfig
      integer,intent(in)                  :: inunit
      type(VoceConfig),intent(out)        :: c
      integer,intent(out)                 :: info
      !
            info = -1
            ! Read the parameters of the work hardening model:
            read (inunit,99,iostat=info) c%TIII1,c%TIIIS,c%TIVS
            if (info /= 0) return
            read (inunit,99,iostat=info) c%THIII1,c%THT
            if (info /= 0) return
  99        format (3f10.0)
            if(NLIST == 1) write (IMP,100) c%TIII1,c%TIIIS,c%TIVS,c%THIII1,c%THT
 100        format(' Work hardening model = DOUBLE VOCE-model',/, &
                   ' TAU-III-1=  ',f20.8,/, &
                   ' TAU-III-S = ',F20.8,/, &
                   ' TAU-IV-S =  ',f20.8,/, &
                   ' THETA-III-1=',f20.8,/, &
                   ' THETA-T=    ',f20.8)
            info = 0
      !
      end subroutine

   subroutine voce_init(this, config)
        class(HardeningModelVoce),            intent(inout)   ::  this 
        type(HardeningData), intent(in) :: config
        integer                                                 ::  info
        real(dp)                                                ::  THIII1, &
                                                                    THT,    &   
                                                                    ETA,    &   
                                                                    TAUT

        call hardening_model_init(this, config)
        this%crss = 0.0_dp

        

        this%stage_1%T1 = config%vocecnf%TIII1 
        this%stage_1%TS = config%vocecnf%TIIIS
        this%stage_2%TS = config%vocecnf%TIVS
        THIII1          = config%vocecnf%THIII1
        THT             = config%vocecnf%THT
    
        !>Check validity of inputs:
        if (.not. (this%stage_1%TS > this%stage_1%T1 .and. THIII1 > THT)) call vef_exception(MODULE_NAME, 'voce_init', VEF_BADVAL, 'TAU-III-S must be larger than TAU-III-1 and THETA-III-1 must be larger than THETA-T')

        this%stage_1%TH = THIII1 / (1.D0 - this%stage_1%T1 / this%stage_1%TS)
        ETA = THT / this%stage_1%TH
        this%transition_strain = -this%stage_1%TS * log(ETA * this%stage_1%TS / (this%stage_1%TS - this%stage_1%T1)) / this%stage_1%TH 
        TAUT = this%stage_1%TS - (this%stage_1%TS - this%stage_1%T1) * exp(-this%stage_1%TH * this%transition_strain / this%stage_1%TS) 
        this%stage_2%TH = THT / (1.D0 - TAUT / this%stage_2%TS)
        this%stage_2%T1 = this%stage_2%TS + (TAUT - this%stage_2%TS) * exp(this%stage_2%TH * this%transition_strain / this%stage_2%TS)
    end subroutine

    subroutine voce_update(this, grain, time, strain, slip_rates)
        class(HardeningModelVoce), intent(inout)            ::  this
        integer, intent(in)                                 :: grain
        real(dp), intent(in)                                ::  time, &
                                                                strain
        real(dp), dimension(this%nss), intent(in) ::  slip_rates
        type(stage)                                         :: current_stage    

        if (strain <= this%transition_strain) then
            current_stage = this%stage_1
        else
            current_stage = this%stage_2
        end if
        this%crss = current_stage%TS - (current_stage%TS - current_stage%T1) * exp(-current_stage%TH * strain / current_stage%TS)
    end subroutine
end module hardening_model_voce
