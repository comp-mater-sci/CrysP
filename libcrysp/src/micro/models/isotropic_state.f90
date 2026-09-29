module crysp_isotropic_state
    use base_defs
    use crysp_status
    use crysp_parameters
    use crysp_grain
    use crysp_constitutive_model

    implicit none

    private
    public:: IsotropicState, &
             to_isotropic_state

    character(*), parameter:: MOD_NAME = 'isotropic_state'

    integer, parameter:: STATE_SIZE = 1

    !> Hardening state used by many simple isotropic hardening models
    type, extends(HardeningState):: IsotropicState
        real(DP):: total_slip = 0._DP !! Sum of all the slip on all the slip systems of the grain.
    contains
        procedure:: serialize => isotropic_state_serialize
        procedure:: deserialize => isotropic_state_deserialize
    end type

contains

    !> Convert a generic HardeningState to a pointer to a IsotropicState object
    !>
    !> Closest Fortran comes to type casting.
    !> A state of any other dynamic type means the model and the state it was handed disagree, which is a defect in libCrysP rather
    !> than anything the caller did. It is still reported rather than fatal: a library must not take its host down.
    function to_isotropic_state(state, isotropic_state_ptr) result(stat)
        class(HardeningState), target, intent(in):: state   !! HardeningState to be converted.
        type(IsotropicState), pointer, intent(out):: isotropic_state_ptr !! Pointer of type IsotropicState to the HardeningState
        type(Status):: stat                                 !! `CRYSP_OK`, or `CRYSP_ERR_TYPE` if the state is not an `IsotropicState`.

        select type (state)
            type is (IsotropicState)
                isotropic_state_ptr => state
            class default
                isotropic_state_ptr => null()
                stat = fail(MOD_NAME, 'to_isotropic_state', CRYSP_ERR_TYPE, 'Hardening state is not an IsotropicState')
        end select
    end function

    function isotropic_state_serialize(this) result(params)
        class(IsotropicState), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        integer:: offset
        type(Parameter), dimension(:), allocatable:: base

        base = this%HardeningState%serialize()
        offset = size(base)

        allocate(params(offset+STATE_SIZE))
        params(:offset) = base

        params(offset+1) = serialize(this%total_slip)
    end function

    subroutine isotropic_state_deserialize(this, params)
        class(IsotropicState), target, intent(inout):: this
        type(Parameter), dimension(:), intent(in):: params

        integer:: offset

        offset = size(params)-STATE_SIZE
        call this%HardeningState%deserialize(params(:offset))

        this%total_slip = params(offset+1)
    end subroutine
end module
