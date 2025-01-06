module voce
    use utils, only: dp
    use hardening_model
    use altayConfig
    use logging
    use parameters

    implicit none
    private

    type:: Stage
        real(DP):: TS, &
                    T1, &
                    TH
    end type

    type, public, extends(HardeningModel):: HardeningModelVoce
        real(DP)    ::  transition_slip = 0.D0
        type(stage) ::  stage_1,    &
                        stage_2
    contains
        procedure:: get_parameters      => voce_get_parameters
        procedure:: validate_parameters => voce_validate_parameters
        procedure:: init                => voce_init
        procedure:: get_crss              => voce_get_crss
    end type

    character(*), parameter:: MODULE_NAME = 'altayHardLaw_voce'

contains

    function voce_get_parameters(this) result(params)
        class(HardeningModelVoce), intent(in)    :: this
        type(Parameter), allocatable    :: params(:)

        params = [hardening_model_get_parameters(this), &
                  [parameter_init('TIII1', TYPE_REAL),             &
                  parameter_init('TIIIS', TYPE_REAL),             &
                  parameter_init('TIVS', TYPE_REAL),              &
                  parameter_init('THIII1', TYPE_REAL),            &
                  parameter_init('THT', TYPE_REAL)]]
    end function voce_get_parameters

    subroutine voce_validate_parameters(this, params)
        class(HardeningModelVoce), intent(in):: this
        type(Parameter), dimension(:), target, intent(in):: params
        character(*), parameter:: PROC_NAME = 'validate_parameters'

        call hardening_model_validate_parameters(this, params)

        call parameter_check_bounds(params .find. 'TIIIS', lower=(params .find. 'TIII1'), lower_inclusive=.false.)
        call parameter_check_bounds(params .find. 'THIII1', lower=(params .find. 'THT'),  lower_inclusive=.false.)
    end subroutine voce_validate_parameters

    subroutine voce_init(this, params)
        class(HardeningModelVoce), intent(inout):: this
        type(Parameter), allocatable, target, intent(in):: params(:)
        real(DP)                                 :: THT,        &
                                                    ETA,        &
                                                    TAUT

        call hardening_model_init(this, params)

        this%stage_1%T1 = params .find. 'TIII1'
        this%stage_1%TS = params .find. 'TIIIS'
        this%stage_2%TS = params .find. 'TIVS'
        THT             = params .find. 'THT'

        this%stage_1%TH = (params .find. 'THIII1') / (1.D0-this%stage_1%T1/this%stage_1%TS)
        ETA = THT/this%stage_1%TH
        this%transition_slip = -this%stage_1%TS*log(ETA*this%stage_1%TS / (this%stage_1%TS-this%stage_1%T1)) / this%stage_1%TH
        TAUT = this%stage_1%TS - (this%stage_1%TS-this%stage_1%T1) * exp(-this%stage_1%TH*this%transition_slip/this%stage_1%TS)
        this%stage_2%TH = THT / (1.D0-TAUT/this%stage_2%TS)
        this%stage_2%T1 = this%stage_2%TS + (TAUT-this%stage_2%TS) * exp(this%stage_2%TH*this%transition_slip/this%stage_2%TS)
    end subroutine

    function voce_get_crss(this, grain, sum_slip) result(crss)
        class(HardeningModelVoce), intent(in):: this
        integer, intent(in)                  :: grain
        real(DP), intent(in)                 :: sum_slip
        real(DP), dimension(2, this%nss)      :: crss
        type(stage)                          :: current_stage

        current_stage = merge(this%stage_1, &
                              this%stage_2, &
                              sum_slip <= this%transition_slip)

        crss = current_stage%TS - (current_stage%TS-current_stage%T1) * exp(-current_stage%TH*sum_slip/current_stage%TS)
    end function
end module voce
