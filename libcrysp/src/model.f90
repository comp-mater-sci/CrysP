module crysp_model
    use crysp_serialization
    use crysp_input
    use logging

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
    !> @endnote
    type, extends(State):: Model
    contains
        procedure, nopass:: get_name => model_get_name_default
        procedure, nopass:: get_description => model_get_description_default
        procedure, nopass:: get_signature => model_get_signature
        procedure, nopass:: get_input => model_get_input
        procedure:: size => model_size
        procedure:: serialize => model_serialize
        procedure:: deserialize => model_deserialize
    end type

contains

    !> Default @c get_name: the base model has no name, so this should never be called on an unextended instance.
    pure function model_get_name_default() result(name)
        character(:), allocatable:: name

        call log_error(ERR_TYPE)
        name = ''
    end function

    !> Default @c get_description: see [[model_get_name_default]].
    pure function model_get_description_default() result(description)
        character(:), allocatable:: description

        call log_error(ERR_TYPE)
        description = ''
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
        class(Model), target, intent(out):: this
        type(Parameter), dimension(:), intent(in):: params
    end subroutine

    pure function model_get_signature() result(signature)
        integer, dimension(:), allocatable:: signature

        allocate(signature(0))
    end function

    function model_get_input() result(inputs)
        type(Input), dimension(:), allocatable:: inputs

        allocate(inputs(0))
    end function
end module
