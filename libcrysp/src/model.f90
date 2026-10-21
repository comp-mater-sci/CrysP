module crysp_model
    use crysp_serialization
    use crysp_input
    implicit none

    !> Base type for all models (micro-, meso- and macro-scopic).
    !>
    !> @note
    !> This type is deliberately NOT abstract, even though @c describe only yields a complete descriptor when extended by a
    !> concrete model. This is because subclasses call their parent's procedures via `this%Model%...` (and transitively
    !> `this%ConstitutiveModel%...`), and the Fortran standard only allows such a type-bound procedure reference if the parent
    !> part-name is not of abstract type (F2018 19.4.5: if the data-ref preceding the procedure name is of abstract type it must
    !> be polymorphic, which an inherited-component reference is not).
    !> @endnote
    type, extends(State):: Model
        integer:: id
    contains
        procedure:: describe => model_describe
        procedure:: size => model_size
        procedure:: serialize => model_serialize
        procedure:: deserialize => model_deserialize
    end type

    type:: StateDescriptor
        character(:), allocatable:: name
        character(:), allocatable:: description
        integer:: type
    end type

    type:: ModelDescriptor
        integer:: id
        character(:), allocatable:: name
        character(:), allocatable:: description
        type(Input), dimension(:), allocatable:: Inputs
        type(StateDescriptor), dimension(:), allocatable:: model_state
        type(StateDescriptor), dimension(:), allocatable:: element_state
    end type

contains

    !> Base of the descriptor chain. The state lists are allocated empty so that every level below can append to them
    !> unconditionally, without having to know whether it is the first one to contribute. The inputs are allocated empty so that
    !> models without parameters report zero inputs rather than an unallocated list.
    function model_describe(this) result(desc)
        class(Model), intent(in):: this
        type(ModelDescriptor):: desc

        desc%id = this%id
        allocate(desc%inputs(0))
        allocate(desc%model_state(0))
        allocate(desc%element_state(0))
    end function

    pure function model_size(this) result(size)
        class(Model), intent(in):: this
        integer:: size

        size = 0
    end function

    function model_serialize(this) result(params)
        class(Model), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        allocate(params(this%size()))
    end function

    subroutine model_deserialize(this, params)
        class(Model), target, intent(inout):: this
        type(Parameter), dimension(:), intent(in):: params
    end subroutine
end module
