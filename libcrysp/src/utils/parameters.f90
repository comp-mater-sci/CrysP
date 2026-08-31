!> This module defines a generic, type-safe parameter type.
!>
!> Parameters can be used to store key-value pairs of many different types,
!> which is very useful for model configuration because higher-level modules need no knowledge of the parameters required by concrete models.
!> The key is always a character(*). The value is one of several predefined types.
!> Many operators are defined to make working with parameters as easy as possible.

module parameters
    use base_defs

    implicit none
    public

    !> Supported parameter types
    enum, bind(C)
        enumerator:: TYPE_INTEGER       !! Integer
        enumerator:: TYPE_REAL          !! Real(DP)
        enumerator:: TYPE_STRING        !! Character(:)
        enumerator:: TYPE_ANGLES_LIST   !! Real(DP), dimension(3,:)
    end enum

    !> Generic container for configuration settings that can be passed between program units in an opaque way.
    !>
    !> Fields can only be modified through dedicated procedures.
    !> Parameters are type-safe in the sense that the type of their content must correspond to the declared type of the paramater.
    !> This is enforced for all procedurures acting on Parameters.
    type Parameter
        private
        character(:), allocatable:: name  !! Name of the parameter
        integer:: type                    !! Type of the parameter. Must exist in enum list at the top of this module.
        character(:), allocatable:: value !! Buffer for the data carried by this parameter.
    end type Parameter

    interface
        !> Initialize a parameter.
        !>
        !> If the type is invalid, this procedure crashes the program.
        !> A default value may be provided.
        !> If its type does not correspond to the declared type of the parameter, the routine crashes the program..
        module type(Parameter) function parameter_init(param_name, param_type, default_value) result(param)
            character(*), intent(in):: param_name                         !! Name to give the new parameter
            integer, intent(in):: param_type                              !! Type of the new parameter. Must exist in the list of types at the top of this module.
            class(*), dimension(..), intent(in), optional:: default_value !! Optional default value for the parameter. If omitted, the content of param%value is undefined.
                                                                          !! The type must correspond to param_type.
        end function parameter_init

        !> Find the number of elements stored in a parameter.
        !>
        !> The exact interpretation of 'number of elements' is defined for each parameter type individually:
        !> - **TYPE_INTEGER**: 1
        !> - **TYPE_REAL**: 1
        !> - **TYPE_STRING**: The number of characters in the string
        !> - **TYPE_ANGLES_LIST**: The number of Euler angle triplets stored in the list
        module integer function parameter_size(param) result(size)
            type(Parameter):: param     !! Input parameter
        end function

        !> Validate if a parameter lies within a specified range.
        !>
        !> Only makes sense for parameters of numerical type (TYPE_INTEGER or TYPE_REAL).
        !> May be called with only lower bound, only upper bound, or both.
        !> By default the bounds are treated as inclusive, but through optional arguments they can be treated as exclusive.
        !>
        !> Crashes the program if the parameter is not of numerical type or its value lies outside the given bounds.
        module subroutine parameter_check_bounds(param, lower, upper, lower_inclusive, upper_inclusive)
            type(Parameter), intent(in):: param             !! Parameter of numerical type
            class(*), intent(in), optional:: lower          !! Lower bound. Inclusive by default
            class(*), intent(in), optional:: upper          !! Upper bound. Inclusive by default
            logical, intent(in), optional:: lower_inclusive !! Treat lower bound as inclusive.
            logical, intent(in), optional:: upper_inclusive !! Treat upper bound as inclusive.
        end subroutine

    end interface

    !> Set the value of a parameter with a specific name from a parameter list
    !>
    !> If no parameter of the provided name is present in the list, the procedure crashes the program.
    !> If the type of the provided value does not correspond to the parameter type, the procedure crashes the program.
    interface parameter_set
        !> Set parameter to integer value
        module subroutine parameter_set_int(params, name, val)
            type(Parameter), dimension(:), target, intent(inout):: params !! Parameter list
            character(*), intent(in):: name                               !! Name of the parameter to set
            integer, intent(in):: val                                     !! Value to set the parameter to
        end subroutine
        !> Analogous to [[parameter_set_int]]
        module subroutine parameter_set_real(params, name, val)
            type(Parameter), dimension(:), target, intent(inout):: params
            character(*), intent(in):: name
            real(DP), intent(in):: val
        end subroutine
        !> Analogous to [[parameter_set_int]]
        module subroutine parameter_set_string(params, name, val)
            type(Parameter), dimension(:), target, intent(inout):: params
            character(*), intent(in):: name
            character(*), intent(in):: val
        end subroutine
        !> Analogous to [[parameter_set_int]]
        module subroutine parameter_set_angles_list(params, name, val)
            type(Parameter), dimension(:), target, intent(inout):: params
            character(*), intent(in):: name
            real(DP), dimension(:,:), intent(in):: val
        end subroutine
    end interface

    interface assignment(=)
        !> Assign the value of a parameter to an integer.
        !>
        !> Allows a clean and type-safe way to extract the value of a parameter.
        !> The parameter must be of TYPE_INTEGER. If not, the routine crashes the program.
        module subroutine get_val_int(val, param)
            integer, intent(out):: val             !! Value to which to assign the content of the parameter
            type(Parameter), intent(in):: param    !! The input parameter
        end subroutine
        !> Analogous to [[get_val_int]]
        module subroutine get_val_real(val, param)
            real(DP), intent(out):: val
            type(Parameter), intent(in):: param
        end subroutine
        !> Analogous to [[get_val_int]]
        module subroutine get_val_string(val, param)
            character(:), allocatable, intent(out):: val
            type(Parameter), intent(in):: param
        end subroutine
        !> Analogous to [[get_val_int]]
        module subroutine get_val_angles_list(val, param)
            real(DP), dimension(:,:), allocatable, intent(out):: val
            type(Parameter), intent(in):: param
        end subroutine

        !> Set the value of a parameter to some provided integer.
        !>
        !> Allows a clean and type-safe way to set the value of a parameter.
        !> The parameter must be of TYPE_INTEGER. If not, the routine crashes the program.
        module subroutine set_val_int(param, val)
            type(Parameter), intent(inout):: param !! TYPE_INTEGER parameter to assign a value to
            integer, intent(in):: val                          !! The input value
        end subroutine
        !> Analogous to [[set_val_int]]
        module subroutine set_val_real(param, val)
            type(Parameter), intent(inout):: param
            real(DP), intent(in):: val
        end subroutine
        !> Analogous to [[set_val_int]]
        module subroutine set_val_string(param, val)
            type(Parameter), intent(inout):: param
            character(*), intent(in):: val
        end subroutine
        !> Analogous to [[set_val_int]]
        module subroutine set_val_angles_list(param, val)
            type(Parameter), intent(inout):: param
            real(DP), dimension(:,:), intent(in):: val
        end subroutine
    end interface


    interface operator(.includes.)
        !> Check if a parameter list includes a parameter by a given name.
        module pure logical function parameter_includes(params, name) result(inc)
            type(Parameter), dimension(:), intent(in):: params !! Parameter list
            character(*), intent(in):: name                            !! Name of the parameter to find.
        end function
    end interface


    interface operator(.find.)
        !> Find the parameter with a specific name in a list of parameters.
        !>
        !> If the list does not contain a parameter with the requested name, the routine crashes the program.
        module function parameter_find_by_name(params, name) result(param)
            type(Parameter), dimension(:), target, intent(in):: params !! Parameter list
            character(*), intent(in):: name                            !! Name of the parameter to find.
            type(Parameter), pointer:: param                           !! Pointer to the parameter with the requested name.
        end function
    end interface

    interface operator(-)
        !> Calculate the numerical difference between a parameter and some other value.
        !>
        !> Both input arguments must represent numerical values. If not, the routine crashes the program.
        module pure real(DP) function parameter_difference(param, arg) result(difference)
            type(Parameter), intent(in):: param !! Parameter of numerical type (TYPE_INTEGER or TYPE_REAL).
            class(*), intent(in)       :: arg   !! Value to compare the parameter to. Must be an integer, real
                                                !! or parameter of TYPE_INTEGER or TYPE_REAL.
        end function
    end interface

    interface operator(*)
        !> Analogous to parameter_difference
        module pure real(DP) function parameter_mult(param, arg) result(prod)
            type(Parameter), intent(in):: param
            real(DP), intent(in):: arg
        end function
    end interface

    interface operator(/)
        !> Analogous to parameter_difference
        module pure real(DP) function parameter_div(param, arg) result(quot)
            type(Parameter), intent(in):: param
            real(DP), intent(in):: arg
        end function
    end interface

    interface operator(==)
        !> Check if a parameter holds a value equivalent to some provided value or the value held by another parameter.
        !>
        !> What exactly it means to be equivalent depends on the type of the parameter/value:
        !> A parameter is equivalent to a value if the parameter type corresponds to the type of the value as defined in the interface definition above
        !> and the parameter value is equal to the value as per intrinsic == operator.
        !> 2 parameters are equivalent if their types and values are identical.
        !> @note
        !> As of 15/1/2025, only works on scalars due to a bug in IFX.
        !> @endnote
        module pure logical function parameter_equals(param, arg) result(eq)
            type(Parameter), intent(in):: param !! The parameter holding the value to be tested
            class(*), intent(in)        :: arg  !! The value to which to compare the value held by the parameter.
        end function
    end interface
    interface operator(/=)
        !> Inverse operation of parameter_equals.
        module pure logical function parameter_nequals(param, arg) result(neq)
            type(Parameter), intent(in):: param
            class(*), intent(in)        :: arg
        end function
    end interface

    interface operator(>)
        !> Determine if the value held by a parameter is greater than some other value or the value held by another parameter.
        !>
        !> Both input arguments must represent numerical values (see argument list for details). If not, the routine crashes the program.
        module pure logical function parameter_gt(param, arg) result(gt)
            type(Parameter), intent(in):: param !! The parameter holding the value to be tested. Must be of TYPE_INTEGER or TYPE_REAL.
            class(*), intent(in)        :: arg  !! The value to compare the parameter to. Must be an integer, a real
                                                !! or a Parameter of type TYPE_INTEGER or TYPE_REAL.
        end function
    end interface
    interface operator(<)
        !> Analogous to parameter_gt
        module pure logical function parameter_lt(param, arg) result(lt)
            type(Parameter), intent(in):: param
            class(*), intent(in)        :: arg
        end function
    end interface
    interface operator(>=)
        !> Analogous to parameter_gt
        module pure logical function parameter_gteq(param, arg) result(gteq)
            type(Parameter), intent(in):: param
            class(*), intent(in)        :: arg
        end function
    end interface
    interface operator(<=)
        !> Analogous to parameter_gt
        module pure logical function parameter_lteq(param, arg) result(lteq)
            type(Parameter), intent(in):: param
            class(*), intent(in)        :: arg
        end function
    end interface
end module parameters


!> Implementation of the parameters module.
submodule (parameters) parameters_imp
    use logging

    implicit none

    character(*), parameter:: MOD_NAME = 'parameter'

contains

    module procedure parameter_check_bounds
        logical:: invalid

        invalid = .false.

        if (present(lower)) then
            if (param < lower) &
                invalid = .true.
            if (present(lower_inclusive)) then
                if ((.not. lower_inclusive) .and. (param == lower)) &
                    invalid = .true.
            end if
        end if
        if (present(upper)) then
            if (param > upper) &
                invalid = .true.
            if (present(upper_inclusive)) then
                if ((.not. upper_inclusive) .and. (param == upper)) &
                    invalid = .true.
            end if
        end if

        if (invalid) &
            call log_error(MOD_NAME, 'check_bounds', ERR_VAL, 'Value of parameter ' // param%name // ' is out of bounds.')
    end procedure

    !> Check if a parameter has the correct type.
    !>
    !> If the parameter type does not correspond to the provided value, the routine crashes the program.
    pure subroutine check_type(param, type)
        type(Parameter), intent(in):: param !! The parameter for which the type must be checked.
        integer, intent(in):: type          !! Expected type of the parameter. Must exist in the enum provided at the top of this module file.

        if (param%type /= type) &
            call log_error(ERR_TYPE)
    endsubroutine

    module procedure get_val_int
        call check_type(param, TYPE_INTEGER)
        val = transfer(param%value, val)
    end procedure
    module procedure get_val_real
        call check_type(param, TYPE_REAL)
        val = transfer(param%value, val)
    end procedure
    module procedure get_val_string
        call check_type(param, TYPE_STRING)
        val = param%value
    end procedure
    module procedure get_val_angles_list
        call check_type(param, TYPE_ANGLES_LIST)

        if (.not. allocated(val)) &
            allocate(val(2, parameter_size(param)))
        val = reshape(transfer(param%value, val), [2, parameter_size(param)])
    end procedure

    module procedure set_val_int
        character(:), allocatable:: buffer

        call check_type(param, TYPE_INTEGER)
        allocate(character(storage_size(val)/8):: buffer)
        param%value = transfer(val, buffer)
    end procedure
    module procedure set_val_real
        character(:), allocatable:: buffer

        call check_type(param, TYPE_REAL)
        allocate(character(storage_size(val)/8):: buffer)
        param%value = transfer(val, buffer)
    end procedure
    module procedure set_val_string
        call check_type(param, TYPE_STRING)
        param%value = val
    end procedure
    module procedure set_val_angles_list
        character(:), allocatable:: buffer

        call check_type(param, TYPE_ANGLES_LIST)
        allocate(character(storage_size(val)/8*2*size(val, 2)):: buffer)
        param%value = transfer(val, buffer)
    end procedure

    module procedure parameter_init
        character(*), parameter:: PROC_NAME = 'parameter_init'

        if (param_type /= TYPE_INTEGER .and. param_type /= TYPE_REAL .and. param_type /= TYPE_STRING .and. param_type /= TYPE_ANGLES_LIST) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Unsupported parameter type.')

        param%name = param_name
        param%type = param_type

        !Must be implemented using infinite polymorphism because the argument that may vary in type is optional.
        if (present(default_value)) then
            select rank (default_value)
                rank (0)
                    select type (default_value)
                        type is (integer)
                            param = default_value
                        type is (real(DP))
                            param = default_value
                        type is (character(*))
                            param = default_value
                        class default
                            call log_error(ERR_TYPE)
                    end select
                rank (2)
                    select type (default_value)
                        type is (real(DP))
                            param = default_value
                        class default
                            call log_error(ERR_TYPE)
                    end select
                rank default
                    call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Unsupported rank for default value')
            end select
        end if
    end procedure parameter_init

    module procedure parameter_size
        select case(param%type)
        case (TYPE_INTEGER)
            size = 1
        case (TYPE_REAL)
            size = 1
        case (TYPE_STRING)
            size = storage_size(param%value)/storage_size('c')
        case (TYPE_ANGLES_LIST)
            size = storage_size(param%value)/(2*storage_size(0._DP))
        end select
    end procedure


    module procedure parameter_includes
        integer:: i

        inc = .false.
        do i = 1, size(params)
            if (params(i)%name == name) then
                inc = .true.
                return
            end if
        end do
    end procedure

    module procedure parameter_find_by_name
        integer:: i

        do i = 1, size(params)
            if (params(i)%name == name) then
                param => params(i)
                return
            end if
        end do

        call log_error(MOD_NAME, 'parameter_find_by_name', ERR_VAL, 'No parameter with name ' // name)
    end procedure parameter_find_by_name

    module procedure parameter_set_int
        type(Parameter), pointer:: param_ptr

        param_ptr => params .find. name
        param_ptr = val
    end procedure
    module procedure parameter_set_real
        type(Parameter), pointer:: param_ptr

        param_ptr => params .find. name
        param_ptr = val
    end procedure
    module procedure parameter_set_string
        type(Parameter), pointer:: param_ptr

        param_ptr => params .find. name
        param_ptr = val
    end procedure
    module procedure parameter_set_angles_list
        type(Parameter), pointer:: param_ptr

        param_ptr => params .find. name
        param_ptr = val
    end procedure


    !> Get the numerical representation of some value.
    !>
    !> Attempts to convert a value to its floating point representation.
    !> If such a representation does not exist, the routine crashes the program.
    pure real(DP) function get_numerical_value(val) result(num)
        class(*), intent(in):: val !! The value to convert to a floating point.
                                   !! Must be integer, real or Parameter of type TYPE_INTEGER or TYPE_REAL

        select type (val)
            type is (integer)
                num = real(val, DP)
            type is (real(DP))
                num = val
            type is (Parameter)
                !The numerical representation of a parameter is the numerical representation of its value.
                select case(val%type)
                    case (TYPE_INTEGER)
                        num = real(transfer(val%value, 0), DP)
                    case (TYPE_REAL)
                        num = transfer(val%value, 0._dp)
                    case default
                        call log_error(ERR_VAL)
                end select
            class default
                call log_error(ERR_VAL)
        end select
    end function get_numerical_value

    module procedure parameter_difference
        difference = get_numerical_value(param) - get_numerical_value(arg)
    end procedure

    module procedure parameter_mult
        prod = get_numerical_value(param) * get_numerical_value(arg)
    end procedure
    module procedure parameter_div
        quot = get_numerical_value(param) / get_numerical_value(arg)
    end procedure

    module procedure parameter_equals
        select type (arg)
            type is (integer)
                call check_type(param, TYPE_INTEGER)
                eq = param%value == transfer(arg, param%value)
            type is (real(DP))
                call check_type(param, TYPE_REAL)
                eq = param%value == transfer(arg, param%value)
            type is (character(*))
                call check_type(param, TYPE_STRING)
                eq = param%value == transfer(arg, param%value)
            type is (Parameter)
                call check_type(param, arg%type)
                eq = param%value == arg%value
            class default
                call log_error(ERR_TYPE)
        end select
    end procedure
    module procedure parameter_nequals
        neq = .not. (param == arg)
    end procedure

    module procedure parameter_gt
        gt = param-arg > 0._DP
    end procedure
    module procedure parameter_lt
        lt = param-arg < 0._DP
    end procedure
    module procedure parameter_gteq
        gteq = param-arg >= 0._DP
    end procedure
    module procedure parameter_lteq
        lteq = param-arg <= 0._DP
    end procedure

end submodule parameters_imp
