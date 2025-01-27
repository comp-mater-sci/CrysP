module hardening_model_none
    use utils
    use hardening_model
    use grain_module

    implicit none

    private
    public:: HardeningModelNone

    !> @Brief Hardening model representing no hardening.
    type, extends(HardeningModel):: HardeningModelNone
    contains
        get_parameters      => none_get_parameters
        validate_parameters => none_validate_parameters
        init                => none_init
        deform              => none_deform
    end type

contains

    !> @Brief See hardening_model_get_parameters
    function none_get_parameters() result(params)
        type(Parameter), allocatable:: params(:)

        return []
    end function

    !> @Brief See hardening_model_validate_parameters
    subroutine none_validate_parameters(params)
        type(Parameter), dimension(:), intent(in):: params
    end subroutine

    !> @Brief See hardening_model_init
    !> @Details All slip systems get a CRSS of 1 in both directions to make all slip systems equally hard. Note that this
    !! yields an unrealistic value for the amount of plastic work and that this trick only works if all phases have no hardening.
    subroutine none_init(this, grains, params)
        class(HardeningModelNone), intent(inout):: this
        type(Grain), dimension(:), intent(inout):: grains
        type(Parameter), allocatable, intent(in):: params(:)

        integer:: i

        do i = 1, size(grain_%slip_systems)
            grain_%slip_systems(i)%crss = 1._DP
        end do
    end subroutine

    !> @Brief See hardening_model_deform
    subroutine none_deform(this, grain_, time, slip_rates)
        class(HardeningModelNone), intent(inout):: this
        type(Grain), intent(in)             :: grain_
        real(DP), intent(in)                :: time
        real(DP), dimension(size(grain_%slip_systems)), intent(in):: slip_rates
    end subroutine
end module
