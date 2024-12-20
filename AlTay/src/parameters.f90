module parameters
    use utils

    implicit none
    public

    !Supported parameter types
    enum, bind(C)
        enumerator ::   TYPE_INTEGER, &
                        TYPE_REAL,    &
                        TYPE_STRING, &
                        TYPE_ANGLES_LIST
    end enum

    type Parameter
        private
        character(:), allocatable:: name
        integer:: type
        character(:), allocatable:: value
    end type Parameter

    interface
        !Initialize parameter
        module type(Parameter) function parameter_init(param_name, param_type, param_value) result(param)
            character(*), intent(in):: param_name
            integer, intent(in):: param_type
            character(*), intent(in), optional:: param_value
        end function parameter_init

        !Measure of the number of elements stored in the parameter. The exact interpretation of an 'element' is defined for each
        !parameter type individually. See implementation.
        module function parameter_size(param) result(size)
            type(Parameter):: param
            integer:: size
        end function
    end interface

    !Intrinsic assignment can not be made polymorphic using class(*) because this leads to infinite recursion
    interface assignment(=)
        !Getter for value buffer
        module subroutine get_val_int(val, param)
            integer, intent(out):: val
            type(Parameter), intent(in):: param
        end subroutine
        module subroutine get_val_real(val, param)
            real(DP), intent(out):: val
            type(Parameter), intent(in):: param
        end subroutine
        module subroutine get_val_string(val, param)
            character(:), allocatable, intent(out):: val
            type(Parameter), intent(in):: param
        end subroutine
        module subroutine get_val_angles_list(val, param)
            real(DP), dimension(:,:), allocatable, intent(out):: val
            type(Parameter), intent(in):: param
        end subroutine

        module subroutine set_val_int(param, val)
            type(Parameter), intent(inout):: param
            integer:: val
        end subroutine
        module subroutine set_val_real(param, val)
            type(Parameter), intent(inout):: param
            real(DP):: val
        end subroutine
        module subroutine set_val_string(param, val)
            type(Parameter), intent(inout):: param
            character(*):: val
        end subroutine
        module subroutine set_val_angles_list(param, val)
            type(Parameter), intent(inout):: param
            real(DP), dimension(:,:), intent(in):: val
        end subroutine
    end interface

    interface operator(.find.)
        module function parameter_find_by_name(params, name) result(param)
            type(Parameter), dimension(:), target, intent(in):: params
            character(*), intent(in):: name
            type(Parameter), pointer:: param
        end function
    end interface

    interface operator(-)
        !Calculate difference between value buffers of 2 parameters or a parameter and a (real) constant
        !Throws exception if at least 1 of the given parameters is not of numeric type.
        module pure real(DP) function parameter_difference(param, arg) result(difference)
            type(Parameter), intent(in):: param
            class(*), intent(in)        :: arg
        end function
    end interface

    interface operator(*)
        module pure real(DP) function parameter_mult(param, arg) result(prod)
            type(Parameter), intent(in):: param
            real(DP), intent(in):: arg
        end function
    end interface

    interface operator(>)
        !Determine if parameter is greater than another parameter or a constant.
        !Throws exception if at least 1 of the given parameters is not of numeric type.
        module pure logical function parameter_gt(param, arg) result(gt)
            type(Parameter), intent(in):: param
            class(*), intent(in)        :: arg
        end function
    end interface
    interface operator(<)
        !Analogous to parameter_gt
        module pure logical function parameter_lt(param, arg) result(lt)
            type(Parameter), intent(in):: param
            class(*), intent(in)        :: arg
        end function
    end interface
    interface operator(>=)
        !Analogous to parameter_gt
        module pure logical function parameter_gteq(param, arg) result(gteq)
            type(Parameter), intent(in):: param
            class(*), intent(in)        :: arg
        end function
    end interface
    interface operator(<=)
        !Analogous to parameter_gt
        module pure logical function parameter_lteq(param, arg) result(lteq)
            type(Parameter), intent(in):: param
            class(*), intent(in)        :: arg
        end function
    end interface
end module parameters

submodule (parameters) parameters_imp
    use logging

    implicit none

    character(*), parameter:: MOD_NAME = 'parameter'

contains

    subroutine check_type(param, type)
        type(Parameter), intent(in):: param
        integer:: type

        if (param%type /= type) &
            call log_error(MOD_NAME, 'check_type', ERR_VAL, 'Incorrect parameter type.')
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
            allocate(val(3, parameter_size(param)))
        val = reshape(transfer(param%value, val), [3, parameter_size(param)])
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
        allocate(character(storage_size(val)/8*3*size(val, 2)):: buffer)
        param%value = transfer(val, buffer)
    end procedure

    module procedure parameter_init
        character(*), parameter:: PROC_NAME = 'parameter_init'
        character(:), allocatable:: buffer

        if (present(param_value)) buffer = param_value

        if (param_type /= TYPE_INTEGER .and. param_type /= TYPE_REAL .and. param_type /= TYPE_STRING .and. param_type /= TYPE_ANGLES_LIST) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Unsupported parameter type.')

        param%name = param_name
        param%type = param_type

        !param%value = buffer
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
            size = storage_size(param%value)/(3*storage_size(0._DP))
        end select
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

    pure real(DP) function get_numerical_value(val) result(num)
        class(*), intent(in):: val

        select type (val)
            type is (integer)
                num = real(val, DP)
            type is (real(DP))
                num = val
            type is (Parameter)
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
