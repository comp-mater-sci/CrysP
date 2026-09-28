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
        integer:: type
    end type

    !> C-compatible counterpart of [[StateDescriptor]]. Name and description are NUL-terminated and truncated to fit.
    type, bind(C):: StateDescriptor_C
        character(C_CHAR), dimension(NAME_LEN):: name
        character(C_CHAR), dimension(DESC_LEN):: description
        integer(C_INT):: type
    end type


    type:: ModelDescriptor
        integer:: id
        character(:), allocatable:: name
        character(:), allocatable:: description
        type(Input), dimension(:), allocatable:: Inputs
        type(StateDescriptor), dimension(:), allocatable:: model_state
        type(StateDescriptor), dimension(:), allocatable:: element_state
    end type

    !> C-compatible counterpart of [[ModelDescriptor]].
    !>
    !> Each list is a malloc'ed array of `n_...` elements. A `ModelDescriptor_C` produced by libCrysP owns these lists and, through
    !> the inputs, the data of their bounds and default values: C releases those with `free()`, then the lists themselves.
    type, bind(C):: ModelDescriptor_C
        integer(C_INT):: id
        character(C_CHAR), dimension(NAME_LEN):: name
        character(C_CHAR), dimension(DESC_LEN):: description
        integer(C_INT):: n_inputs
        type(C_PTR):: inputs             !! n_inputs * type(Input_C)
        integer(C_INT):: n_model_state
        type(C_PTR):: model_state        !! n_model_state * type(StateDescriptor_C)
        integer(C_INT):: n_element_state
        type(C_PTR):: element_state      !! n_element_state * type(StateDescriptor_C)
    end type

    type:: ModelData
        integer:: id
        type(Parameter), dimension(:), allocatable:: parameters
    end type

    !> C-compatible counterpart of [[ModelData]].
    type, bind(C):: ModelData_C
        integer(C_INT):: id
        integer(C_INT):: n_parameters
        type(C_PTR):: parameters         !! n_parameters * type(Parameter_C)
    end type

    interface assignment(=)
        module procedure state_descriptor_to_c, model_descriptor_to_c, model_data_to_c, model_data_from_c
    end interface

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

        allocate(params(this%size()))
    end function

    subroutine model_deserialize(this, params)
        class(Model), target, intent(inout):: this
        type(Parameter), dimension(:), intent(in):: params
    end subroutine

    !> Convert a state descriptor to its C-compatible counterpart.
    subroutine state_descriptor_to_c(desc_c, desc)
        type(StateDescriptor_C), intent(out):: desc_c
        type(StateDescriptor), intent(in):: desc

        desc_c%name = string_to_chars(desc%name, size(desc_c%name))
        desc_c%description = string_to_chars(desc%description, size(desc_c%description))
        desc_c%type = int(desc%type, C_INT)
    end subroutine

    !> Convert a model descriptor to its C-compatible counterpart.
    !>
    !> The caller takes ownership of the pointers.
    subroutine model_descriptor_to_c(desc_c, desc)
        type(ModelDescriptor_C), intent(out):: desc_c
        type(ModelDescriptor), intent(in):: desc

        integer:: i
        type(Input_C):: input_mold
        type(StateDescriptor_C):: state_mold
        type(Input_C), dimension(:), pointer:: inputs
        type(StateDescriptor_C), dimension(:), pointer:: model_state, &
                                                         element_state

        desc_c%id = int(desc%id, C_INT)
        desc_c%name = string_to_chars(desc%name, size(desc_c%name))
        desc_c%description = string_to_chars(desc%description, size(desc_c%description))

        desc_c%n_inputs = int(size(desc%inputs), C_INT)
        desc_c%inputs = malloc(int(size(desc%inputs), C_SIZE_T) * c_sizeof(input_mold))
        call c_f_pointer(desc_c%inputs, inputs, [size(desc%inputs)])
        do i = 1, size(desc%inputs)
            inputs(i) = desc%inputs(i)
        end do

        desc_c%n_model_state = int(size(desc%model_state), C_INT)
        desc_c%model_state = malloc(int(size(desc%model_state), C_SIZE_T) * c_sizeof(state_mold))
        call c_f_pointer(desc_c%model_state, model_state, [size(desc%model_state)])
        do i = 1, size(desc%model_state)
            model_state(i) = desc%model_state(i)
        end do

        desc_c%n_element_state = int(size(desc%element_state), C_INT)
        desc_c%element_state = malloc(int(size(desc%element_state), C_SIZE_T) * c_sizeof(state_mold))
        call c_f_pointer(desc_c%element_state, element_state, [size(desc%element_state)])
        do i = 1, size(desc%element_state)
            element_state(i) = desc%element_state(i)
        end do
    end subroutine

    !> Convert model data to its C-compatible counterpart. The caller takes ownership of the parameters, see [[ModelData_C]].
    subroutine model_data_to_c(model_data_c, model_data)
        type(ModelData_C), intent(out):: model_data_c
        type(ModelData), intent(in):: model_data

        integer:: i
        type(Parameter_C):: mold
        type(Parameter_C), dimension(:), pointer:: params

        model_data_c%id = int(model_data%id, C_INT)
        model_data_c%n_parameters = int(size(model_data%parameters), C_INT)
        model_data_c%parameters = malloc(int(size(model_data%parameters), C_SIZE_T) * c_sizeof(mold))
        call c_f_pointer(model_data_c%parameters, params, [size(model_data%parameters)])
        do i = 1, size(model_data%parameters)
            params(i) = model_data%parameters(i)
        end do
    end subroutine

    !> Convert C-compatible model data to model data. The parameters are copied and then freed, see [[parameter_from_c]], and so is
    !> the list holding them: `model_data_c%parameters` must have been malloc'ed and is dangling afterwards.
    !>
    !> `model_data_c` is assumed to be well-formed: it is not validated here.
    subroutine model_data_from_c(model_data, model_data_c)
        type(ModelData), intent(out):: model_data
        type(ModelData_C), intent(in):: model_data_c

        integer:: i
        type(Parameter_C), dimension(:), pointer:: params

        model_data%id = int(model_data_c%id)
        allocate(model_data%parameters(model_data_c%n_parameters))

        call c_f_pointer(model_data_c%parameters, params, [int(model_data_c%n_parameters)])
        do i = 1, size(params)
            model_data%parameters(i) = params(i)
        end do
        call free(model_data_c%parameters)
    end subroutine
end module
