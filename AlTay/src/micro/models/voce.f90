module voce
    use utils, only: dp
    use hardening_model
    use altayConfig
    use logging
    use parameters
    use grain_module

    implicit none

    private
    public:: HardeningModelVoce

    !> Grain-specific hardening state data needed by the Voce hardening law.
    type, extends(HardeningState):: VoceState
        real(DP):: total_slip = 0._DP !> Sum of all the slip on all the slip systems of the grain.
    end type

    !> Type wrapping parameters associated to a particular stage of the Voce hardening law.
    type:: Stage
        real(DP):: TS, &
                   T1, &
                   TH
    end type

    !> Voce hardening model
    type, extends(HardeningModel):: HardeningModelVoce
        real(DP)    ::  transition_slip = 0._DP !> Value for total slip at which to transition from stage 1 to stage 2.
        type(stage) ::  stage_1,    &           !> Parameters to use for small strain.
                        stage_2                 !> Parameters to use for large strain.
    contains
        procedure, nopass:: get_parameters      => voce_get_parameters
        procedure, nopass:: validate_parameters => voce_validate_parameters
        procedure:: init                => voce_init
        procedure:: deform              => voce_deform
    end type

contains

    !> @Brief Convert a generic HardeningState to a pointer to a VoceState object
    !> @Details Closest Fortran comes to type casting
    !!          If the provided state is not of type voce_state, the program crashes.
    function to_voce_state(state) result(voce_state_ptr)
        class(HardeningState), target, intent(in):: state   !> HardeningState to be converted. Must have dynamic type VoceState.
        type(VoceState), pointer:: voce_state_ptr         !> Pointer of type VoceState to the HardeningState

        select type (state)
            type is (VoceState)
                voce_state_ptr => state
            class default
                call log_error(ERR_TYPE)
        end select
    end function

    function voce_get_parameters() result(params)
        type(Parameter), dimension(:), allocatable:: params

        params = [parameter_init('TIII1',  TYPE_REAL), &
                  parameter_init('TIIIS',  TYPE_REAL), &
                  parameter_init('TIVS',   TYPE_REAL), &
                  parameter_init('THIII1', TYPE_REAL), &
                  parameter_init('THT',    TYPE_REAL)]
    end function

    !> @Brief See hardening_model_validate_parameters
    subroutine voce_validate_parameters(params)
        type(Parameter), dimension(:), target, intent(in):: params

        type(Parameter), pointer:: buffer !> Buffer for bounds in calls to parameter_check_bounds. Needed due to a bug in gfortran.

        buffer => params .find. 'TIIIS'
        call parameter_check_bounds(params .find. 'TIII1', 0._DP, buffer, .false., .false.)
        buffer => params .find. 'TIVS'
        call parameter_check_bounds(params .find. 'TIIIS', upper = buffer)
        buffer => params .find. 'THIII1'
        call parameter_check_bounds(params .find. 'THT', 0._DP, buffer, .false., .false.)
    end subroutine

    !> @Brief See hardening_model_init
    subroutine voce_init(this, grains, params)
        class(HardeningModelVoce), intent(inout):: this
        type(Grain), dimension(:), intent(inout):: grains
        type(Parameter), target, intent(in):: params(:)

        !Local variables
        integer::  i, j     !> Iterators
        real(DP):: THT, &
                   ETA, &
                   TAUT

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

        do i = 1, size(grains)
            allocate(VoceState:: grains(i)%hardening_state)
            do j = 1, size(grains(i)%slip_systems)
                grains(i)%slip_systems(j)%crss = this%stage_1%T1
            end do
        end do
    end subroutine

    subroutine voce_deform(this, grain_, time, slip_rates)
        class(HardeningModelVoce),                      intent(inout):: this
        type(Grain), target,                            intent(inout):: grain_
        real(DP),                                       intent(in)::    time
        real(DP), dimension(size(grain_%slip_systems)), intent(in)::    slip_rates

        integer:: i                          !> Iterator
        real(DP):: crss                      !> Buffer for CRSS so that it needs to be calculated only once.
        type(Stage):: current_stage          !> Current stage in the Voce hardening process
        type(VoceState), pointer:: state_ptr !> Pointer to hardening_state of type VoceState for easy access to model-specific fields

        state_ptr => to_voce_state(grain_%hardening_state)

        state_ptr%total_slip = state_ptr%total_slip+sum(abs(slip_rates)) * time

        current_stage = merge(this%stage_1, &
                              this%stage_2, &
                              state_ptr%total_slip <= this%transition_slip)

        crss = current_stage%TS - (current_stage%TS-current_stage%T1) * exp(-current_stage%TH*state_ptr%total_slip/current_stage%TS)

        do i = 1, size(grain_%slip_systems)
            grain_%slip_systems(i)%crss = crss
        end do
    end subroutine
end module voce
