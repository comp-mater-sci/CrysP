module crysp_isotropic_state
    use crysp_serialization
    use crysp_grain

    implicit none

    private
    public:: IsotropicState, &
             to_isotropic_state

    !> Hardening state used by many simple isotropic hardening models
    type, extends(HardeningState):: IsotropicState
        real(DP):: total_slip = 0._DP !! Sum of all the slip on all the slip systems of the grain.
    contains
        procedure:: size => isotropic_state_size
        procedure:: serialize => isotropic_state_serialize
        procedure:: deserialize => isotropic_state_deserialize
    end type

contains

    !> Convert a generic HardeningState to a pointer to a IsotropicState object
    !>
    !> Closest Fortran comes to type casting
    !> If the provided state is not of type swift_state, the program crashes.
    function to_isotropic_state(state) result(isotropic_state_ptr)
        class(HardeningState), target, intent(in):: state   !! HardeningState to be converted.
        type(IsotropicState), pointer:: isotropic_state_ptr !! Pointer of type IsotropicState to the HardeningState

        select type (state)
            type is (IsotropicState)
                isotropic_state_ptr => state
            class default
                call log_error(ERR_TYPE)
        end select
    end function

    pure function isotropic_state_size(this) result(size)
        class(IsotropicState), intent(in):: this
        integer:: size

        size = this%HardeningState%size() + 1
    end function

    pure function isotropic_state_serialize(this) result(params)
        class(IsotropicState), intent(in):: this
        type(Parameter), dimension(this%size()):: params

        params = this%HardeningState%serialize()
        params(this%size()) = serialize(this%total_slip)
    end function

    subroutine isotropic_state_deserialize(this, params)
        class(IsotropicState), intent(out):: this
        type(Parameter), dimension(:), intent(in):: params

        call this%HardeningState%deserialize(params)
        this%total_slip = params(this%size())
    end subroutine
end module
