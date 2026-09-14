module crysp_serialization
    use iso_c_binding
    use base_defs
    use logging

    implicit none

    private
    public:: TYPE_INTEGER, &
             TYPE_INT_ARRAY, &
             TYPE_REAL, &
             TYPE_REAL_ARRAY, &
             TYPE_REAL_MATRIX, &
             TYPE_STRING, &
             Parameter, &
             assignment(=), &
             serialize, &
             type_of, &
             shape_of, &
             State, &
             deserialize_real_matrix

    character(*), parameter:: MOD_NAME = 'serialization'

    !> Supported types for serialization.
    !>
    !> Each type represents a unique C-compatible Fortran type
    enum, bind(C)
        enumerator:: TYPE_INTEGER       !! Integer(C_INT)
        enumerator:: TYPE_INT_ARRAY     !! integer(C_INT), dimension(:)
        enumerator:: TYPE_REAL          !! Real(C_DOUBLE)
        enumerator:: TYPE_REAL_ARRAY    !! real(C_DOUBLE), dimension(:)
        enumerator:: TYPE_REAL_MATRIX   !! Real(C_DOUBLE), dimension(:,:)
        enumerator:: TYPE_STRING        !! Character(C_CHAR,:)
    end enum

    !> Opaque C-compatible wrapper for parameters.
    !>
    !> Needed because the internal representation uses polymorphism and is therefore not C-compatible.
    type, bind(C):: Parameter
        type(C_PTR):: handle
    end type

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
        procedure(state_get_size), deferred:: size
        procedure(state_serialize), deferred:: serialize
        procedure(state_deserialize), deferred:: deserialize
    end type

    abstract interface
        !> Get the amount of Parameters needed to serialize the current object.
        !>
        !> Note that for many objects, this is only known at runtime due to e.g. varying-size list members.
        pure function state_get_size(this) result(size)
            import State

            class(State), intent(in):: this
            integer:: size
        end function
        !> Transform a state into a list of parameters
        pure function state_serialize(this) result(params)
            import State
            import Parameter

            class(State), target, intent(in):: this
            type(Parameter), dimension(:), allocatable:: params !! Allocated to this%size() by the callee.
        end function
        !> Transform a list of parameters into an object of State (sub)type
        subroutine state_deserialize(this, params)
            import State
            import Parameter

            class(State), target, intent(out):: this
            type(Parameter), dimension(:), intent(in):: params
        end subroutine
    end interface

    !> Wrapper type for storing parameter values.
    !>
    !> Needed because the values may be of any type, kind, length and rank.
    !> Earlier designs use byte arrays or class(*), but none of them can leverage the Fortran type system while maintaining support
    !> for both dynamic type and rank.
    type, abstract:: Value
    end type
    !> Wrapper for integers. Corresponds to TYPE_INTEGER
    type, extends(Value):: IntValue
        integer(C_INT):: buffer
    end type
    !> Wrapper for integer arrays. Corresponds to TYPE_INT_ARRAY
    type, extends(Value):: IntArrayValue
        integer(C_INT), dimension(:), allocatable:: buffer
    end type
    !> Wrapper for doubles. Corresponds to TYPE_REAL
    type, extends(Value):: RealValue
        real(C_DOUBLE):: buffer
    end type
    !> Wrapper for real arrays. Corresponds to TYPE_REAL_ARRAY.
    type, extends(Value):: RealArrayValue
        real(C_DOUBLE), dimension(:), allocatable:: buffer
    end type

    !> Wrapper real matrices. Corresponds to TYPE_REAL_MATRIX
    type, extends(Value):: RealMatrixValue
        real(C_DOUBLE), dimension(:,:), allocatable:: buffer
    end type

    !> Wrapper for strings. Corresponds to TYPE_STRING
    type, extends(Value):: StringValue
        character(kind=C_CHAR,len=:), allocatable:: buffer
    end type

    !> Generic container to pass values between program units in an opaque, yet type-safe way
    !>
    !> Enforces type safety through the wrapper types declared above.
    !> If we skip this wrapper for the value we can not enforce type safety with the regular Fortran type system
    type:: ParameterValue
        class(Value), allocatable:: value
    end type

    !> From within fortran, creating and destroying parameters is done using intrinsic assignment to/from the types described in the
    !> enum at the top of this module.
    interface assignment(=)
        module procedure int_to_param, int_array_to_param, real_to_param, real_array_to_param, real_matrix_to_param, string_to_param, &
                         param_to_int, param_to_int_array, param_to_real, param_to_real_array, param_to_real_matrix, param_to_string
    end interface

    !> Convenience functions wrapping the setter subroutines above.
    !>
    !> Useful for creating anonymous Parameter instances not bound to a local variable.
    !> No serialize_real_matrix due to IFX compiler bug as of 2026.1.1.19
    interface serialize
        module procedure serialize_int, serialize_int_array, serialize_real, serialize_real_array, serialize_real_matrix, serialize_string
    end interface

contains

    !> Get the type of a parameter
    !>
    !> Returns one of the types declared in the enum at the top of this module.
    function type_of(param) result(t) bind(C)
        type(Parameter), intent(in):: param
        integer(C_INT):: t

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        select type (value => val%value)
            type is (IntValue)
                t = TYPE_INTEGER
            type is (IntArrayValue)
                t = TYPE_INT_ARRAY
            type is (RealValue)
                t = TYPE_REAL
            type is (RealArrayValue)
                t = TYPE_REAL_ARRAY
            type is (RealMatrixValue)
                t = TYPE_REAL_MATRIX
            type is (StringValue)
                t = TYPE_STRING
        end select
    end function

    !> Get the shape of the value held by a parameter.
    !>
    !> The shape is always reported as two extents, so that the interface is interoperable with C. Matrices fill both; scalars
    !> ([1, 0]), arrays ([size, 0]) and strings ([length, 0]) leave the second extent zero.
    !> Useful for allocating a receiving buffer of the right size before reading the value. Does not consume the parameter.
    subroutine shape_of(param,s) bind(C)
        type(Parameter), intent(in):: param
        integer(C_INT), dimension(2), intent(out):: s

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        select type (value => val%value)
            type is (IntValue)
                s = [1,0]
            type is (IntArrayValue)
                s = [size(value%buffer),0]
            type is (RealValue)
                s = [1,0]
            type is (RealArrayValue)
                s = [size(value%buffer),0]
            type is (RealMatrixValue)
                s = shape(value%buffer)
            type is (StringValue)
                s = [len(value%buffer),0]
            class default
                call log_error(MOD_NAME, 'shape_of', ERR_TYPE, 'Unknown parameter type')
        end select
    end subroutine

    !> Initialize a parameter based on a Value object of any type.
    pure function to_parameter(data) result(param)
        class(Value), intent(in):: data
        type(Parameter):: param

        type(ParameterValue), pointer:: val

        allocate(val)
        val%value = data
        param%handle = c_loc(val)
    end function

    pure subroutine int_to_param(param, data) bind(C)
        type(Parameter), intent(out):: param
        integer(C_INT), intent(in):: data

        param = to_parameter(IntValue(data))
    end subroutine
    pure subroutine int_array_to_param(param, data) bind(C)
        type(Parameter), intent(out):: param
        integer(C_INT), dimension(:), intent(in):: data

        type(IntArrayValue):: val

        !Manually copy over data due to bug in IFX as of 2026.1.1.19
        val%buffer = data
        param = to_parameter(val)
    end subroutine
    pure subroutine real_to_param(param, data) bind(C)
        type(Parameter), intent(out):: param
        real(C_DOUBLE), intent(in):: data

        param = to_parameter(RealValue(data))
    end subroutine
    pure subroutine real_array_to_param(param, data) bind(C)
        type(Parameter), intent(out):: param
        real(C_DOUBLE), dimension(:), intent(in):: data

        type(RealArrayValue):: val

        !Manually copy over data due to bug in IFX as of 2026.1.1.19
        val%buffer = data
        param = to_parameter(val)
    end subroutine
    pure subroutine real_matrix_to_param(param, data) bind(C)
        type(Parameter), intent(out):: param
        real(C_DOUBLE), dimension(:,:), intent(in):: data

        type(RealMatrixValue):: val

        !Manually copy over data due to bug in IFX as of 2026.1.1.19
        val%buffer = data
        param = to_parameter(val)
    end subroutine

    pure subroutine string_to_param(param, data) bind(C)
        type(Parameter), intent(out):: param
        character(kind=C_CHAR,len=*), intent(in):: data

        param = to_parameter(StringValue(data))
    end subroutine

    subroutine param_to_int(data, param) bind(C)
        integer(C_INT), intent(out):: data
        type(Parameter), intent(in):: param

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        select type (value => val%value)
            type is (IntValue)
                data = value%buffer
            class default
                call log_error(MOD_NAME, 'parameter_to_int', ERR_TYPE, 'Parameter is not of correct type')
        end select

        deallocate(val)
    end subroutine
    subroutine param_to_int_array(data, param) bind(C)
        integer(C_INT), dimension(:), intent(out):: data
        type(Parameter), intent(in):: param

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        select type (value => val%value)
            type is (IntArrayValue)
                if (size(data) /= size(value%buffer)) &
                    call log_error(MOD_NAME, 'parameter_to_int_array', ERR_DIMS, 'Parameter size does not match destination')
                data = value%buffer
            class default
                call log_error(MOD_NAME, 'parameter_to_int_array', ERR_TYPE, 'Parameter is not of correct type')
        end select

        deallocate(val)
    end subroutine
    subroutine param_to_real(data, param) bind(C)
        real(C_DOUBLE), intent(out):: data
        type(Parameter), intent(in):: param

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        select type (value => val%value)
            type is (RealValue)
                data = value%buffer
            class default
                call log_error(MOD_NAME, 'parameter_to_real', ERR_TYPE, 'Parameter is not of correct type')
        end select

        deallocate(val)
    end subroutine
    subroutine param_to_real_array(data, param) bind(C)
        real(C_DOUBLE), dimension(:), intent(out):: data
        type(Parameter), intent(in):: param

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        select type (value => val%value)
            type is (RealArrayValue)
                if (size(data) /= size(value%buffer)) &
                    call log_error(MOD_NAME, 'parameter_to_real_array', ERR_DIMS, 'Parameter size does not match destination')
                data = value%buffer
            class default
                call log_error(MOD_NAME, 'parameter_to_real_array', ERR_TYPE, 'Parameter is not of correct type')
        end select

        deallocate(val)
    end subroutine
    subroutine param_to_real_matrix(data, param) bind(C)
        real(C_DOUBLE), dimension(:,:), intent(out):: data
        type(Parameter), intent(in):: param

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        select type (value => val%value)
            type is (RealMatrixValue)
                if (any(shape(data) /= shape(value%buffer))) &
                    call log_error(MOD_NAME, 'parameter_to_real_matrix', ERR_DIMS, 'Parameter shape does not match destination')
                data = value%buffer
            class default
                call log_error(MOD_NAME, 'parameter_to_real_matrix', ERR_TYPE, 'Parameter is not of correct type')
        end select

        deallocate(val)
    end subroutine
    subroutine param_to_string(data, param) bind(C)
        character(kind=C_CHAR,len=:), allocatable, intent(out):: data
        type(Parameter), intent(in):: param

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        select type (value => val%value)
            type is (StringValue)
                data = value%buffer
            class default
                call log_error(MOD_NAME, 'parameter_to_string', ERR_TYPE, 'Parameter is not of correct type')
        end select

        deallocate(val)
    end subroutine

    pure function serialize_int(data) result(param) bind(C)
        integer(C_INT), intent(in):: data
        type(Parameter):: param

        param = data
    end function

    !!Longer notation due to bug in IFX as of 2026.1.1.19
    pure function serialize_int_array(data) result(param) bind(C)
        integer(C_INT), dimension(:), intent(in):: data
        type(Parameter):: param

        type(IntArrayValue):: val

        val%buffer = data
        param = to_parameter(val)
    end function
    pure function serialize_real(data) result(param) bind(C)
        real(C_DOUBLE), intent(in):: data
        type(Parameter):: param

        param = data
    end function
    !!Longer notation due to bug in IFX as of 2026.1.1.19
    pure function serialize_real_array(data) result(param) bind(C)
        real(C_DOUBLE), dimension(:), intent(in):: data
        type(Parameter):: param

        type(RealArrayValue):: val

        val%buffer = data
        param = to_parameter(val)
    end function
    !!Longer notation due to bug in IFX as of 2026.1.1.19
    pure function serialize_real_matrix(data) result(param) bind(C)
        real(C_DOUBLE), dimension(:,:), intent(in):: data
        type(Parameter):: param

        type(RealMatrixValue):: val

        val%buffer = data
        param = to_parameter(val)
    end function
    pure function serialize_string(data) result(param) bind(C)
        character(kind=C_CHAR,len=*), intent(in):: data
        type(Parameter):: param

        param = data
    end function

    function deserialize_real_matrix(param) result(data)
        type(Parameter), intent(in):: param
        real(DP), dimension(:,:), allocatable:: data

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        select type (value => val%value)
            type is (RealMatrixValue)
                data = value%buffer
            class default
                call log_error(MOD_NAME, 'deserialize_real_matrix', ERR_TYPE, 'Parameter is not of correct type')
        end select

        deallocate(val)
    end function

end module
