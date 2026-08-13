module parameters
    use iso_c_binding
    use base_defs
    use logging

    implicit none

    public

    !> Supported types for serialization.
    !>
    !> Each type has a unique Fortran and JSON representation.
    enum, bind(C)
        enumerator:: TYPE_INTEGER       !! Integer(C_INT)
        enumerator:: TYPE_REAL          !! Real(C_DOUBLE)
        enumerator:: TYPE_STRING        !! Character(C_CHAR,:)
        enumerator:: TYPE_ANGLES_LIST   !! Real(C_DOUBLE), dimension(2,:)
    end enum

    type:: ParameterValue
        private
        integer(BYTE), dimension(:), allocatable:: value
        integer:: type
    end type

    type, bind(C):: Parameter
        type(C_PTR):: handle = C_NULL_PTR
    end type

    !> Generic container for configuration settings that can be passed between program units in an opaque way.
    !>
    !> Fields can only be modified through dedicated procedures.
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

    interface assignment(=)
        module procedure int_to_parameter, real_to_parameter, string_to_parameter, angles_list_to_parameter, &
                         parameter_to_int, parameter_to_real, parameter_to_string, parameter_to_angles_list
    end interface

    !No serialize_angles_list tue o IFX compiler bug as of 2026/8/12
    interface serialize
        module procedure serialize_int, serialize_real, serialize_string
    end interface

contains

    pure function to_c_string(fortran_string, length) result(c_string)
        character(*), intent(in):: fortran_string
        integer, intent(in):: length
        character(C_CHAR), dimension(length):: c_string

        integer:: i

        do i=1,len(fortran_string)
            c_string(i) = char(iachar(fortran_string(i:i)),kind=C_CHAR)
        end do
        c_string(i) = C_NULL_CHAR
    end function

    subroutine check_type(param, type)
        type(ParameterValue), intent(in):: param
        integer, intent(in):: type

        if (param%type /= type) &
            call log_error('parameters', 'check_type', ERR_TYPE, 'Parameter is not of the correct type')
    end subroutine

    subroutine int_to_parameter(param, data) bind(C)
        type(Parameter), intent(out):: param
        integer(C_INT), intent(in):: data

        type(ParameterValue), pointer:: val

        allocate(val)
        val%value = transfer(data,val%value)
        val%type=TYPE_INTEGER

        param%handle = c_loc(val)
    end subroutine
    subroutine real_to_parameter(param, data) bind(C)
        type(Parameter), intent(out):: param
        real(C_DOUBLE), intent(in):: data

        type(ParameterValue), pointer:: val

        allocate(val)
        val%value = transfer(data,val%value)
        val%type=TYPE_REAL

        param%handle = c_loc(val)
    end subroutine
    subroutine string_to_parameter(param, data) bind(C)
        type(Parameter), intent(out):: param
        character(kind=C_CHAR,len=*), intent(in):: data

        type(ParameterValue), pointer:: val

        allocate(val)
        val%value = transfer(data,val%value)
        val%type=TYPE_STRING

        param%handle = c_loc(val)
    end subroutine
    subroutine angles_list_to_parameter(param, data) bind(C)
        type(Parameter), intent(out):: param
        real(C_DOUBLE), dimension(:,:), intent(in):: data

        type(ParameterValue), pointer:: val

        allocate(val)
        val%value = transfer(data,val%value)
        val%type=TYPE_ANGLES_LIST

        param%handle = c_loc(val)
    end subroutine

    subroutine parameter_to_int(data, param) bind(C)
        integer(C_INT), intent(out):: data
        type(Parameter), intent(in):: param

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        call check_type(val, TYPE_INTEGER)
        data = transfer(val%value, data)
        deallocate(val)
    end subroutine
    subroutine parameter_to_real(data, param) bind(C)
        real(C_DOUBLE), intent(out):: data
        type(Parameter), intent(in):: param

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        call check_type(val, TYPE_REAL)
        data = transfer(val%value, data)
        deallocate(val)
    end subroutine
    subroutine parameter_to_string(data, param) bind(C)
        character(kind=C_CHAR,len=:), allocatable, intent(out):: data
        type(Parameter), intent(in):: param

        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        call check_type(val, TYPE_STRING)
        allocate(character(kind=C_CHAR,len=size(val%value)*8/storage_size(C_NULL_CHAR))::data)
        data = transfer(val%value, data)
        deallocate(val)
    end subroutine
    subroutine parameter_to_angles_list(data, param) bind(C)
        real(C_DOUBLE), dimension(:,:), allocatable, intent(out):: data
        type(Parameter), intent(in):: param

        integer:: n_dirs
        type(ParameterValue), pointer:: val

        call c_f_pointer(param%handle, val)

        call check_type(val, TYPE_ANGLES_LIST)
        n_dirs = size(val%value)*8/storage_size(.0_C_DOUBLE)/2
        allocate(data(2,n_dirs))
        data = reshape(transfer(val%value, data, 2*n_dirs), [2,n_dirs])
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
