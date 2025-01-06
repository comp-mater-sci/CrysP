module hardening_model
    use utils, only: dp
    use altayConfig
    use parameters
    use logging

    implicit none
    private

    !>Basic hardening model implementation.
    !>No hardening occurs.
    !>All other hardening models extend this type.
    type, public:: HardeningModel
        integer:: nss
    contains
        procedure:: get_parameters      => hardening_model_get_parameters
        procedure:: validate_parameters => hardening_model_validate_parameters
        procedure:: init                => hardening_model_init
        procedure:: get_crss            => hardening_model_get_crss
        procedure:: finalize            => hardening_model_finalize
        procedure:: update_state            => hardening_model_update_state
    end type

    public ::   hardening_model_get_parameters, &
                hardening_model_validate_parameters,    &
                hardening_model_init

contains

    function hardening_model_get_parameters(this) result(params)
        class(HardeningModel), intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        allocate(params(1))
        params(1) = parameter_init('n_slip_systems', TYPE_INTEGER)
    end function hardening_model_get_parameters

    subroutine hardening_model_validate_parameters(this, params)
        class(HardeningModel), intent(in)   :: this
        type(Parameter), dimension(:), target, intent(in):: params
        character(:), allocatable:: n_slip_systems

    end subroutine hardening_model_validate_parameters

    subroutine hardening_model_init(this, params)
        class(HardeningModel), intent(inout)     ::  this
        type(Parameter), allocatable, target, intent(in):: params(:)

        this%nss = params .find. 'n_slip_systems'

        end subroutine hardening_model_init

    function hardening_model_get_crss(this, grain, sum_slip) result(crss)
        class(HardeningModel), intent(in):: this
        integer, intent(in)              :: grain
        real(DP), intent(in)             :: sum_slip
        real(DP), dimension(2, this%nss):: crss

        crss = 1._DP
    end function

    !>Do nothing
    subroutine hardening_model_update_state(this, grain, time, slip_rates)
        class(HardeningModel), intent(inout)     :: this
        integer, intent(in)                      :: grain
        real(DP), intent(in)                     :: time
        real(DP), dimension(this%nss), intent(in):: slip_rates
    end subroutine

    !>Do nothing
    subroutine hardening_model_finalize(this)
        class(HardeningModel), intent(inout):: this
    end subroutine
end module hardening_model
