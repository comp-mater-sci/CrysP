!> This module implements a two-stage isotropic Voce hardening law.

module voce
    use utils, only: dp
    use constitutive_model
    use altayConfig
    use logging
    use parameters

    implicit none

    private
    public:: ConstitutiveModelVoce

    !> Grain-specific hardening state data needed by the Voce hardening law.
    type, extends(HardeningState):: VoceState
        real(DP):: total_slip = 0._DP !! Sum of all the slip on all the slip systems of the grain.
    end type

    !> Type wrapping parameters associated to a particular stage of the Voce hardening law.
    type:: Stage
        real(DP):: TS !! Saturation flow stress
        real(DP):: T1 !! Initial flow stress
        real(DP):: TH !! Initial hardening rate
    end type

    !> Voce hardening model
    type, extends(ConstitutiveModel):: ConstitutiveModelVoce
        real(DP)   ::  transition_slip = 0._DP !! Value for total slip at which to transition from stage 1 to stage 2.
        type(Stage)::  stage_1                 !! Initial hardening behavior.
        type(Stage)::  stage_2                 !! Hardening behavior after the total slip surpasses the transition slip.
    contains
        procedure, nopass:: get_parameters      => voce_get_parameters          !! Inherited from ConstitutiveModel
        procedure, nopass:: validate_parameters => voce_validate_parameters     !! Inherited from ConstitutiveModel
        procedure:: init                        => voce_init                    !! Inherited from ConstitutiveModel
        procedure:: deform                      => voce_deform                  !! Inherited from ConstitutiveModel
    end type

contains

    !> Convert a generic HardeningState to a pointer to a VoceState object
    !> 
    !> Closest Fortran comes to type casting
    !> If the provided state is not of type voce_state, the program crashes.
    function to_voce_state(state) result(voce_state_ptr)
        class(HardeningState), target, intent(in):: state   !! HardeningState to be converted. Must have dynamic type VoceState.
        type(VoceState), pointer:: voce_state_ptr           !! Pointer of type VoceState to the HardeningState

        select type (state)
            type is (VoceState)
                voce_state_ptr => state
            class default
                call log_error(ERR_TYPE)
        end select
    end function

    !> See cm_get_parameters
    !>
    !> Parameters:   
    !> **TIII1**: Initial flow stress.  
    !> **TIIIS**: Saturation flow stress for the first stage.  
    !> **TIVS**: Saturation flow stress for the second stage.  
    !> **THIII1**: Initial hardening rate.
    !> **THT**: Hardening rate at which to transition from stage 1 to stage 2.
    function voce_get_parameters() result(params)
        type(Parameter), dimension(:), allocatable:: params

        params = [parameter_init('TIII1',  TYPE_REAL), &
                  parameter_init('TIIIS',  TYPE_REAL), &
                  parameter_init('TIVS',   TYPE_REAL), &
                  parameter_init('THIII1', TYPE_REAL), &
                  parameter_init('THT',    TYPE_REAL)]
    end function

    !> See cm_validate_parameters
    !> 
    !> Constraints on parameters:   
    !> 0 < TIII1 < TIIIS < TIVS
    !> 0 < THT < THIII1
    subroutine voce_validate_parameters(params)
        type(Parameter), dimension(:), target, intent(in):: params

        type(Parameter), pointer:: buffer !Buffer for bounds in calls to parameter_check_bounds. Needed due to a bug in gfortran.

        buffer => params .find. 'TIIIS'
        call parameter_check_bounds(params .find. 'TIII1', 0._DP, buffer, .false., .false.)
        buffer => params .find. 'TIVS'
        call parameter_check_bounds(params .find. 'TIIIS', upper = buffer)
        buffer => params .find. 'THIII1'
        call parameter_check_bounds(params .find. 'THT', 0._DP, buffer, .false., .false.)
    end subroutine

    !> See cm_init
    function voce_init(this, miller_indices, params) result(initial_state)
        class(ConstitutiveModelVoce), intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        type(Parameter), target, intent(in):: params(:)
        class(HardeningState), allocatable:: initial_state

        !Local variables
        real(DP):: THT, &
                   ETA, &
                   TAUT

        allocate(VoceState:: initial_state)
        call this%base_init(miller_indices, initial_state)

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

        initial_state%crss = this%stage_1%T1
    end function

    !> See cm_deform
    subroutine voce_deform(this, state, time, slip_rates)
        class(ConstitutiveModelVoce),                      intent(inout):: this
        class(HardeningState), target,                  intent(inout):: state
        real(DP),                                       intent(in)::    time
        real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in)::    slip_rates

        type(Stage):: current_stage          !Current stage in the Voce hardening process
        type(VoceState), pointer:: state_ptr !Pointer to hardening_state of type VoceState for easy access to model-specific fields

        state_ptr => to_voce_state(state)

        state_ptr%total_slip = state_ptr%total_slip+sum(abs(slip_rates)) * time

        current_stage = merge(this%stage_1, &
                              this%stage_2, &
                              state_ptr%total_slip <= this%transition_slip)

        state_ptr%crss = current_stage%TS - (current_stage%TS-current_stage%T1) * exp(-current_stage%TH*state_ptr%total_slip/current_stage%TS)
    end subroutine
end module voce
