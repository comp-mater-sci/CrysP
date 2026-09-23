module crysp_model
    use crysp_serialization
    use crysp_input
    implicit none

    !> Base type for all models (micro-, meso- and macro-scopic).
    !>
    !> @note
    !> This type is deliberately NOT abstract, even though its @c get_name / @c get_description operations only make sense when
    !> overridden by a concrete model. This is because subclasses call their parent's non-deferred procedures via
    !> `this%Model%...` (and transitively `this%ConstitutiveModel%...`), and the Fortran standard only allows such a type-bound
    !> procedure reference if the parent part-name is not of abstract type (F2018 19.4.5: if the data-ref preceding the procedure
    !> name is of abstract type it must be polymorphic, which an inherited-component reference is not).
    !> The default implementations below therefore terminate with an error if ever invoked on an unextended instance.
    !>
    !> These defaults are among the few places in libCrysP that still terminate rather than return a [[crysp_status:Status]],
    !> because each is a function whose result is the object itself, so there is no room for a status. `get_name` and `get_description` are moreover `pure`, which rules out `intent(out)` subroutines. None of
    !> them sits on a failure path a caller can reach: they are only ever dispatched on concrete models.
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
        type(StateDescriptor), dimension(:), allocatable:: model_state_fields
        type(StateDescriptor), dimension(:), allocatable:: element_state_fields
    end type

contains

    pure function describe() result(desc)
        type(ModelDescriptor):: desc

        desc%id = this%id
    end function

    !> The base model contributes exactly one serialized field: its ID.
    pure function model_size(this) result(size)
        class(Model), intent(in):: this
        integer:: size

        size = 1
    end function

    function model_serialize(this) result(params)
        class(Model), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        allocate(params(this%size()))

        params(1) = this%id
    end function

    subroutine model_deserialize(this, params)
        class(Model), target, intent(out):: this
        type(Parameter), dimension(:), intent(in):: params

        this%id = params(1)
    end subroutine
end module
