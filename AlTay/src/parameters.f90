module parameters
    use definitions

    implicit none
    public

    !Supported parameter types
    enum, bind(C)
        enumerator ::   TYPE_INTEGER, &
                        TYPE_REAL,    &
                        TYPE_STRING
    end enum

    type Parameter
        private
        character(:), allocatable :: name
        integer :: type
        character(:), allocatable :: value
    end type Parameter

    !Getter for value buffer
    interface assignment(=)
        module procedure get_val_int,  &
                         get_val_real, &
                         get_val_string
    end interface assignment(=)

    !Setter for value buffer, called with a list of parameters and the name and value of the parameter to be set.
    interface parameter_set
        module procedure set_val_int,  &
                         set_val_real, &
                         set_val_string
    end interface parameter_set

    !Find the parameter matching a given name in a list of parameters.
    interface operator(.find.)
        module procedure parameter_find_by_name 
    end interface operator(.find.)

    !Calculate difference between value buffers of 2 parameters or a parameter and a (real) constant
    !Throws exception if at least 1 of the given parameters is not of numeric type.
    interface operator(-)
        module procedure parameter_difference,     &
                         parameter_difference_real
    end interface operator(-)

    !Determine if parameter is greater than another parameter or a constant.
    !Throws exception if at least 1 of the given parameters is not of numeric type.
    interface operator(>)
        module procedure parameter_gt,     &
                         parameter_gt_real
    end interface operator(>)
    
    !Analogous to operator(>)
    interface operator(<)
        module procedure parameter_lt,     &
                         parameter_lt_real
    end interface operator(<)

    !Analogous to operator(>)
    interface operator(>=)
        module procedure parameter_gteq,     &
                         parameter_gteq_real
    end interface operator(>=)

    !Analogous to operator(>)
    interface operator(<=)
        module procedure parameter_lteq,     &
                         parameter_lteq_real
    end interface operator(<=)

    interface
        !Initialize parameter
        module type(Parameter) function parameter_init(param_name, param_type, param_value) result(param)
            character(*), intent(in) :: param_name
            integer, intent(in) :: param_type
            character(*), intent(in), optional :: param_value
        end function parameter_init
    
        !assignment(=)
        module subroutine get_val_int(val, param)
            integer, intent(out) :: val 
            type(Parameter), intent(in) :: param
        end subroutine 
        module subroutine get_val_real(val, param)
            real(dp), intent(out) :: val 
            type(Parameter), intent(in) :: param
        end subroutine 
        module subroutine get_val_string(val, param)
            character(:), allocatable, intent(out) :: val 
            type(Parameter), intent(in) :: param
        end subroutine 
        
        !parameter_set
        module subroutine set_val_int(params, name, val)
            type(Parameter), allocatable, intent(inout) :: params(:)
            character(*), intent(in) :: name
            integer, intent(in) :: val 
        end subroutine 
        module subroutine set_val_real(params, name, val)
            type(Parameter), allocatable, intent(inout) :: params(:)
            character(*), intent(in) :: name
            real(dp), intent(in) :: val 
        end subroutine 
        module subroutine set_val_string(params, name, val)
            type(Parameter), allocatable, intent(inout) :: params(:)
            character(*), intent(in)                    :: name
            character(:), allocatable, intent(in)       :: val
        end subroutine 

        !operator(.find.)
        module function parameter_find_by_name(params, name) result(param)
            type(Parameter), allocatable, intent(in) :: params(:)
            character(*), intent(in) :: name
            type(Parameter) :: param
        end function 

        !operator(-)
        module pure real(dp) function parameter_difference(param1, param2) result(difference)
            type(Parameter), intent(in) ::  param1, &
                                            param2
        end function 
        module pure real(dp) function parameter_difference_real(param, num) result(difference)
            type(Parameter), intent(in) :: param
            real(dp), intent(in)        :: num
        end function 

        !operator(>)
        module pure logical function parameter_gt(param1, param2) result(gt)
            type(Parameter), intent(in) ::  param1,  &
                                            param2
        end function 
        module pure logical function parameter_gt_real(param, num) result(gt)
            type(Parameter), intent(in) ::  param
            real(dp), intent(in)        ::  num
        end function 
        
        !operator(<)
        module pure logical function parameter_lt(param1, param2) result(lt)
            type(Parameter), intent(in) ::  param1,  &
                                            param2
        end function parameter_lt
        module pure logical function parameter_lt_real(param, num) result(lt)
            type(Parameter), intent(in) ::  param
            real(dp), intent(in)        ::  num
        end function 
        
        !operator(>=)
        module pure logical function parameter_gteq(param1, param2) result(gteq)
            type(Parameter), intent(in) ::  param1,  &
                                            param2
        end function 
        module pure logical function parameter_gteq_real(param, num) result(gteq)
            type(Parameter), intent(in) ::  param
            real(dp), intent(in)        ::  num
        end function 

        !operator(<=)
        module pure logical function parameter_lteq(param1, param2) result(lteq)
            type(Parameter), intent(in) ::  param1,  &
                                            param2
        end function 
        module pure logical function parameter_lteq_real(param, num) result(lteq)
            type(Parameter), intent(in) ::  param
            real(dp), intent(in)        ::  num
        end function
    end interface
end module parameters

submodule (parameters) parameters_imp
    use logging

    implicit none

    character(*), parameter :: MOD_NAME = 'parameter'

contains

    module procedure parameter_init
        character(*), parameter :: PROC_NAME = 'parameter_init'
        character(:), allocatable :: buffer 

        if (present(param_value)) buffer = param_value

        if (param_type /= TYPE_INTEGER .and. param_type /= TYPE_REAL .and. param_type /= TYPE_STRING) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Unsupported parameter type.')
       
        param = Parameter(param_name, param_type, buffer)
    end procedure parameter_init

    !>Throw an exception if someone wants to assign or read a value of the wrong type
    subroutine check_type(param, t)
        type(Parameter), intent(in) :: param
        integer, intent(in) :: t
        
        if (param%type /= t) &
            call log_error(MOD_NAME, 'check_type', ERR_VAL, 'Value does not conform with parameter type')
    end subroutine check_type

    subroutine search_parameter_list(params, name, param, ind)
        type(Parameter), allocatable, intent(in) :: params(:)
        character(*), intent(in) :: name
        type(Parameter), intent(out) :: param
        integer, intent(out) :: ind
        
        integer :: i
        
        ind = 0 
        do i=1,size(params)
            if (params(i)%name == name) then
                param = params(i)
                ind = i
                exit
            end if
        end do

        if (ind == 0) &
            call log_error(MOD_NAME, 'parameter_find_by_name', ERR_VAL, 'No parameter with name ' // name)
    end subroutine search_parameter_list

    integer function find_index(params, name) result(ind)
        type(Parameter), allocatable, intent(in) :: params(:)
        character(*), intent(in) :: name
        type(Parameter) :: param

        call search_parameter_list(params, name, param, ind)
    end function find_index 


    module procedure parameter_find_by_name
        integer :: ind

        call search_parameter_list(params, name, param, ind)
    end procedure parameter_find_by_name


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

    module procedure set_val_int
        integer :: ind
        character(4), parameter :: mold = '1234'
        
        ind = find_index(params, name)
        call check_type(params(ind), TYPE_INTEGER)
        params(ind)%value = transfer(val, mold)
    end procedure 
    module procedure set_val_real
        integer :: ind
        character(8), parameter :: mold = '12345678'
        
        ind = find_index(params, name)
        call check_type(params(ind), TYPE_REAL)
        params(ind)%value = transfer(val, mold)
    end procedure 
    module procedure set_val_string
        integer :: ind
        
        ind = find_index(params, name)
        call check_type(params(ind), TYPE_STRING)
        params(ind)%value = val
    end procedure 

    pure real(dp) function get_numerical_value(param) result(num)
        type(Parameter), intent(in) :: param
        integer :: buffer

        select case(param%type)
            case (TYPE_INTEGER)
                buffer = transfer(param%value, 0)
                num = real(buffer)
            case (TYPE_REAL)
                num = transfer(param%value, 0._dp)
            case default
                call log_error(ERR_VAL)
        end select
    end function get_numerical_value

    module procedure parameter_difference
        difference = get_numerical_value(param1) - get_numerical_value(param2)
    end procedure 
    module procedure parameter_difference_real
        difference = get_numerical_value(param) - num
    end procedure

    module procedure parameter_gt
        gt = ((param1 - param2) > 0._dp)
    end procedure     
    module procedure parameter_lt
        lt = ((param1 - param2) < 0._dp)
    end procedure     
    module procedure parameter_gteq
        gteq = ((param1 - param2) >= 0._dp)
    end procedure     
    module procedure parameter_lteq
        lteq = ((param1 - param2) <= 0._dp)
    end procedure    
    module procedure parameter_gt_real
        gt = ((param - num) > 0._dp)
    end procedure    
    module procedure parameter_lt_real
        lt = ((param - num) < 0._dp)
    end procedure    
    module procedure parameter_gteq_real
        gteq = ((param - num) >= 0._dp)
    end procedure    
    module procedure parameter_lteq_real
        lteq = ((param - num) <= 0._dp)
    end procedure    
end submodule parameters_imp

