module parameters
    use iso_c_binding
    use base_defs
    use logging

    implicit none

    private
    public:: TYPE_INTEGER, &
             TYPE_REAL, &
             TYPE_STRING, &
             TYPE_ANGLES_LIST, &
             Parameter, &
             ParameterDescriptor, &
             assignment(=), &
             serialize, &
             typeof


    character(*), parameter:: MOD_NAME = 'parameters'

    !> Supported types for serialization.
    !>
    !> Each type represents a unique C-compatible Fortran type
    enum, bind(C)
        enumerator:: TYPE_INTEGER       !! Integer(C_INT)
        enumerator:: TYPE_REAL          !! Real(C_DOUBLE)
        enumerator:: TYPE_STRING        !! Character(C_CHAR,:)
        enumerator:: TYPE_ANGLES_LIST   !! Real(C_DOUBLE), dimension(2,:)
    end enum

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
    !> Wrapper for doubles. Corresponds to TYPE_REAL
    type, extends(Value):: RealValue
        real(C_DOUBLE):: buffer
    end type
    !> Wrapper for strings. Corresponds to TYPE_STRING
    type, extends(Value):: StringValue
        character(kind=C_CHAR,len=:), allocatable:: buffer
    end type
    !> Wrapper for angle lists. Corresponds to TYPE_ANGLES_LIST
    type, extends(Value):: AnglesListValue
        real(C_DOUBLE), dimension(:,:), allocatable:: buffer
    end type

    !> Generic container to pass values between program units in an opaque, yet type-safe way
    !>
    !> Enforces type safety through the wrapper types declared above.
    !> If we skip this wrapper for the value we can not enforce type safety with the regular Fortran type system
    type:: ParameterValue
        class(Value), allocatable:: value
    end type

    !> Opaque C-compatible wrapper for parameters.
    !>
    !> Needed because the internal representation uses polymorphism and is therefore not C-compatible.
    type, bind(C):: Parameter
        type(C_PTR):: handle = C_NULL_PTR
    end type

    !> Descriptor of a parameter requested from the user.
    !>
    !> Contains all needed information to enforce sanitization at the front-end.
    !> Any parameters needed by models should be described using this type.
    type, bind(C):: ParameterDescriptor
        character(kind=C_CHAR), dimension(32)  :: name                    !! Name of the parameter
        integer(C_INT)                         :: type                    !! Type of the parameter. Must exist in enum list in serialization.
        character(kind=C_CHAR), dimension(1024):: description           = ""      !! Description of the parameter
        logical(C_BOOL)                        :: optional              = .false. !! Indicate if the parameter is optional
        type(Parameter)       :: lower_bound
        logical(C_BOOL)                        :: lower_bound_inclusive = .false. !! Ignored if lower_bound == ""
        type(Parameter)       :: upper_bound
        logical(C_BOOL)                        :: upper_bound_inclusive = .false. !! Ignored if upper_bound == ""
        type(Parameter):: default_value
    end type

    !> From within fortran, creating and destroying parameters is done using intrinsic assignment to/from the types described in the
    !> enum at the top of this module.
    interface assignment(=)
        module procedure int_to_parameter, real_to_parameter, string_to_parameter, angles_list_to_parameter, &
                         parameter_to_int, parameter_to_real, parameter_to_string, parameter_to_angles_list
    end interface

    !> Convenience functions wrapping the setter subroutines above.
    !>
    !> Useful for creating anonymous Parameter instances not bound to a local variable.
    !> No serialize_angles_list due to IFX compiler bug as of 2026.1.1.19
    interface serialize
        module procedure serialize_int, serialize_real, serialize_string
    end interface

contains

    !> Get the type of a parameter
    !>
    !> Returns one of the types declared in the enum at the top of this module.
    function typeof(param) result(t)
        type(Parameter), intent(in):: param
        integer(C_INT):: t

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        select type (value => val%value)
            type is (IntValue)
                t = TYPE_INTEGER
            type is (RealValue)
                t = TYPE_REAL
            type is (StringValue)
                t = TYPE_STRING
            type is (AnglesListValue)
                t = TYPE_ANGLES_LIST
        end select
    end function

    !> Initialize a parameter based on a Value object of any type.
    function to_parameter(data) result(param)
        class(Value), intent(in):: data
        type(Parameter):: param

        type(ParameterValue), pointer:: val

        allocate(val)
        val%value = data
        param%handle = c_loc(val)
    end function

    subroutine int_to_parameter(param, data) bind(C)
        type(Parameter), intent(out):: param
        integer(C_INT), intent(in):: data

        param = to_parameter(IntValue(data))
    end subroutine
    subroutine real_to_parameter(param, data) bind(C)
        type(Parameter), intent(out):: param
        real(C_DOUBLE), intent(in):: data

        param = to_parameter(RealValue(data))
    end subroutine
    subroutine string_to_parameter(param, data) bind(C)
        type(Parameter), intent(out):: param
        character(kind=C_CHAR,len=*), intent(in):: data

        param = to_parameter(StringValue(data))
    end subroutine
    subroutine angles_list_to_parameter(param, data) bind(C)
        type(Parameter), intent(out):: param
        real(C_DOUBLE), dimension(:,:), intent(in):: data

        type(AnglesListValue):: val

        !Manually copy over data due to bug in IFX as of 2026.1.1.19
        val%buffer = data

        param = to_parameter(val)
    end subroutine

    subroutine parameter_to_int(data, param) bind(C)
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
    subroutine parameter_to_real(data, param) bind(C)
        real(C_DOUBLE), intent(out):: data
        type(Parameter), intent(in):: param

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        select type (value => val%value)
            type is (RealValue)
                data = value%buffer
            class default
                call log_error(MOD_NAME, 'parameter_to_int', ERR_TYPE, 'Parameter is not of correct type')
        end select

        deallocate(val)
    end subroutine
   subroutine parameter_to_string(data, param) bind(C)
        character(kind=C_CHAR,len=:), allocatable, intent(out):: data
        type(Parameter), intent(in):: param

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        select type (value => val%value)
            type is (StringValue)
                data = value%buffer
            class default
                call log_error(MOD_NAME, 'parameter_to_int', ERR_TYPE, 'Parameter is not of correct type')
        end select

        deallocate(val)
    end subroutine
   subroutine parameter_to_angles_list(data, param) bind(C)
        real(C_DOUBLE), dimension(:,:), allocatable, intent(out):: data
        type(Parameter), intent(in):: param

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        select type (value => val%value)
            type is (AnglesListValue)
                data = value%buffer
            class default
                call log_error(MOD_NAME, 'parameter_to_int', ERR_TYPE, 'Parameter is not of correct type')
        end select

        deallocate(val)
    end subroutine

    function serialize_int(data) result(param) bind(C)
        integer(C_INT), intent(in):: data
        type(Parameter):: param

        param = data
    end function
    function serialize_real(data) result(param) bind(C)
        real(C_DOUBLE), intent(in):: data
        type(Parameter):: param

        param = data
    end function
    function serialize_string(data) result(param) bind(C)
        character(kind=C_CHAR,len=*), intent(in):: data
        type(Parameter):: param

        param = data
    end function
end module
