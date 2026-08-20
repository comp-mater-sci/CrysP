module crysp_isotropic_state
    use crysp_serialization
    use crysp_grain

    implicit none

    private
    public:: IsotropicState, &
             to_isotropic_state

    !> Hardening state used by many simple isotropic hardening models
    type, extends(GrainState):: IsotropicState
        real(DP):: total_slip = 0._DP !! Sum of all the slip on all the slip systems of the grain.
    contains
        procedure:: serialize => isotropic_state_serialize
        procedure:: deserialize => isotropic_state_deserialize
    end type

contains

    !> Convert a generic GrainState to a pointer to a IsotropicState object
    !>
    !> Closest Fortran comes to type casting
    !> If the provided state is not of type swift_state, the program crashes.
    function to_isotropic_state(state) result(isotropic_state_ptr)
        class(GrainState), target, intent(in):: state   !! GrainState to be converted.
        type(IsotropicState), pointer:: isotropic_state_ptr !! Pointer of type IsotropicState to the GrainState

        select type (state)
            type is (IsotropicState)
                isotropic_state_ptr => state
            class default
                call log_error(ERR_TYPE)
        end select
    end function

    pure function isotropic_state_serialize(this) result(params)
        class(IsotropicState), intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        params = this%GrainState%serialize()
        params = params .add. serialize(this%total_slip)
    end function

    function isotropic_state_deserialize(this, params) result(params_)
        class(IsotropicState), intent(out):: this
        type(Parameter), dimension(:), intent(in):: params
        type(Parameter), dimension(:), allocatable:: params_

        params_ = this%GrainState%deserialize(params)
        this%total_slip = params_(1)
        params_ = params_ .pop. 1
    end function
end module
