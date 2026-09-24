module crysp_isotropic_state
    use base_defs
    use crysp_status
    use crysp_serialization
    use crysp_grain
    use constitutive_model

    implicit none

    private
    public:: IsotropicState, &
             to_isotropic_state

    character(*), parameter:: MOD_NAME = 'isotropic_state'

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

    pure function isotropic_state_size(this) result(size)
        class(IsotropicState), intent(in):: this
        integer:: size

        size = this%HardeningState%size() + 1
    end function

    function isotropic_state_serialize(this) result(params)
        class(IsotropicState), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        type(Parameter), dimension(:), allocatable:: base

        allocate(params(this%size()))

        base = this%HardeningState%serialize()
        params(:size(base)) = base
        params(this%size()) = serialize(this%total_slip)
    end function

    subroutine isotropic_state_deserialize(this, params)
        class(IsotropicState), target, intent(inout):: this
        type(Parameter), dimension(:), intent(in):: params

        call this%HardeningState%deserialize(params)

        this%total_slip = params(this%size())
    end subroutine
end module
