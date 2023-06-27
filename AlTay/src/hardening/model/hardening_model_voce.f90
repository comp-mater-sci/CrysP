module hardening_model_voce
    use definitions, only: dp
    use hardening_model
    use altayConfig
    use logging
    use parameters
    
    implicit none
    private

    type :: Stage
        real(dp) :: TS, &
                    T1, &
                    TH  
    end type

    type, public, extends(HardeningModel) :: HardeningModelVoce
        real(dp)    ::  transition_strain = 0.D0
        type(stage) ::  stage_1,    &   
                        stage_2
    contains
        procedure :: get_parameters      => voce_get_parameters
        procedure :: validate_parameters => voce_validate_parameters
        procedure :: init                => voce_init
        procedure :: get_crss              => voce_get_crss
    end type

    character(*), parameter :: MODULE_NAME = 'altayHardLaw_voce'

contains

    function voce_get_parameters(this) result(params)
        class(HardeningModelVoce), intent(in)    :: this
        type(Parameter), allocatable    :: params(:)
    
        params = [parameter_init('n_slip_systems', TYPE_STRING), &
                  parameter_init('TIII1', TYPE_REAL),             &   
                  parameter_init('TIIIS', TYPE_REAL),             &   
                  parameter_init('TIVS', TYPE_REAL),              &   
                  parameter_init('THIII1', TYPE_REAL),            &   
                  parameter_init('THT', TYPE_REAL)]
    end function voce_get_parameters

    subroutine voce_validate_parameters(this, params) 
        class(HardeningModelVoce), intent(in) :: this
        type(Parameter), allocatable, intent(in) :: params(:)
        character(*), parameter :: PROC_NAME = 'validate_parameters'

        call hardening_model_validate_parameters(this, params)    

        if ((params .find. 'TIIIS') <= (params .find. 'TIII1')) & 
            call log_error(MODULE_NAME, PROC_NAME, ERR_VAL, 'TAU-III-S must be larger than TAU-III-1')
        if ((params .find. 'THIII1') <= (params .find. 'THT'))  &
            call log_error(MODULE_NAME, PROC_NAME, ERR_VAL, 'THETA-III-1 must be larger than THETA-T')
    end subroutine voce_validate_parameters

    subroutine voce_init(this, params)
        class(HardeningModelVoce), intent(inout) :: this 
        type(Parameter), allocatable, intent(in) :: params(:)
        real(dp)                                 :: THIII1,     &
                                                    THT,        &   
                                                    ETA,        &   
                                                    TAUT

        call hardening_model_init(this, params)

        this%stage_1%T1 = params .find. 'TIII1'
        this%stage_1%TS = params .find. 'TIIIS'
        this%stage_2%TS = params .find. 'TIVS'
        THIII1          = params .find. 'THIII1'
        THT             = params .find. 'THT'

        this%stage_1%TH = THIII1 / (1.D0 - this%stage_1%T1 / this%stage_1%TS)
        ETA = THT / this%stage_1%TH
        this%transition_strain = -this%stage_1%TS * log(ETA * this%stage_1%TS / (this%stage_1%TS - this%stage_1%T1)) / this%stage_1%TH 
        TAUT = this%stage_1%TS - (this%stage_1%TS - this%stage_1%T1) * exp(-this%stage_1%TH * this%transition_strain / this%stage_1%TS) 
        this%stage_2%TH = THT / (1.D0 - TAUT / this%stage_2%TS)
        this%stage_2%T1 = this%stage_2%TS + (TAUT - this%stage_2%TS) * exp(this%stage_2%TH * this%transition_strain / this%stage_2%TS)
    end subroutine

    function voce_get_crss(this, grain, strain) result(crss)
        class(HardeningModelVoce), intent(in)            ::  this
        integer, intent(in)                                 :: grain
        real(dp), intent(in)                                :: strain
        real(DP), dimension(2,this%nss) :: crss
        type(stage)                                         :: current_stage    

        current_stage = merge(this%stage_1, &
                              this%stage_2, &
                              strain <= this%transition_strain)

        crss = current_stage%TS - (current_stage%TS - current_stage%T1) * exp(-current_stage%TH * strain / current_stage%TS)
    end function
end module hardening_model_voce
