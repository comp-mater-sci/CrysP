module crysp_model
    use crysp_parameters
    use crysp_input

    implicit none

    !> Interface implemented by every serializable type.
    !>
    !> @note
    !> `serialize` returns an allocatable array rather than an explicit-shape result `dimension(this%size())`. With the latter,
    !> the shape of the result is a specification expression that the *caller* evaluates, and IFX (as of 2026.1) evaluates
    !> `this%size()` there using the declared rather than the dynamic type of `this`. A call through a polymorphic object then
    !> yields an array of the parent type's size, silently truncating the state. Letting the callee allocate the result avoids any
    !> dependence on how the compiler resolves the spec expression, and also avoids walking the state tree twice per call.
    !> @endnote
    type, abstract:: State
    contains
        procedure(state_serialize), deferred:: serialize
        procedure(state_deserialize), deferred:: deserialize
    end type

    abstract interface
        function state_serialize(this) result(params)
            import State
            import Parameter

            class(State), target, intent(in):: this
            type(Parameter), dimension(:), allocatable:: params !! Allocated to this%size() by the callee.
        end function
        subroutine state_deserialize(this, params)
            import State
            import Parameter

            class(State), target, intent(inout):: this
            type(Parameter), dimension(:), intent(in):: params
        end subroutine
    end interface

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
        procedure:: serialize => model_serialize
        procedure:: deserialize => model_deserialize
    end type

    type:: StateDescriptor
        character(:), allocatable:: name
        character(:), allocatable:: description
        integer:: type  !! Element type, one of the `TYPE_*` constants in [[crysp_parameters]].
        integer:: rank  !! 0 for scalars and strings, 1 for arrays, 2 for matrices, as in [[crysp_parameters:Parameter]].
    end type

    type:: ModelDescriptor
        integer:: id
        character(:), allocatable:: name
        character(:), allocatable:: description
        type(Input), dimension(:), allocatable:: Inputs
        type(StateDescriptor), dimension(:), allocatable:: model_state
        type(StateDescriptor), dimension(:), allocatable:: element_state
    end type

    type:: ModelData
        integer:: id
        type(Parameter), dimension(:), allocatable:: parameters
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

    function model_serialize(this) result(params)
        class(Model), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        allocate(params(0))
    end function

    subroutine model_deserialize(this, params)
        class(Model), target, intent(inout):: this
        type(Parameter), dimension(:), intent(in):: params
    end subroutine
 end module
