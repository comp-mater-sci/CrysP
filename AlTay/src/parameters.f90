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
        character, allocatable :: value(:)
    end type Parameter

    interface
        !Initialize parameter
        module type(Parameter) function parameter_init(param_name, param_type, param_value) result(param)
            character(*), intent(in) :: param_name
            integer, intent(in) :: param_type
            character(*), intent(in), optional :: param_value
        end function parameter_init
    
        !Getter for value buffer
        module subroutine get_val(val, param)
            class(*), intent(inout) :: val 
            type(Parameter), intent(in) :: param
        end subroutine 
                
        !Setter for value buffer, called with a list of parameters and the name and value of the parameter to be set.
        module subroutine set_val(params, name, val)
            type(Parameter), allocatable, intent(inout) :: params(:)
            character(*), intent(in) :: name
            class(*), intent(in) :: val 
        end subroutine 
        
        !Find the parameter matching a given name in a list of parameters.
        module function parameter_find_by_name(params, name) result(param)
            type(Parameter), allocatable, intent(in) :: params(:)
            character(*), intent(in) :: name
            type(Parameter) :: param
        end function 

        !Calculate difference between value buffers of 2 parameters or a parameter and a (real) constant
        !Throws exception if at least 1 of the given parameters is not of numeric type.
        module pure real(dp) function parameter_difference(param, arg) result(difference)
            type(Parameter), intent(in) :: param
            class(*), intent(in)        :: arg
        end function 
        
        !Determine if parameter is greater than another parameter or a constant.
        !Throws exception if at least 1 of the given parameters is not of numeric type.
        module pure logical function parameter_gt(param, arg) result(gt)
            type(Parameter), intent(in) :: param
            class(*), intent(in)        :: arg
        end function 
        !Analogous to parameter_gt
        module pure logical function parameter_gteq(param, arg) result(gteq)
            type(Parameter), intent(in) :: param
            class(*), intent(in)        :: arg
        end function 
        !Analogous to parameter_gt
        module pure logical function parameter_lt(param, arg) result(lt)
            type(Parameter), intent(in) :: param
            class(*), intent(in)        :: arg
        end function 
        !Analogous to parameter_gt
        module pure logical function parameter_lteq(param, arg) result(lteq)
            type(Parameter), intent(in) :: param
            class(*), intent(in)        :: arg
        end function 
    end interface

    interface assignment(=)
        module procedure get_val
    end interface

    interface operator(.find.)
        module procedure parameter_find_by_name 
    end interface

    interface operator(-)
        module procedure parameter_difference
    end interface

    interface operator(>)
        module procedure parameter_gt
    end interface
    interface operator(<)
        module procedure parameter_lt
    end interface
    interface operator(>=)
        module procedure parameter_gteq
    end interface
    interface operator(<=)
        module procedure parameter_lteq
    end interface
end module parameters

submodule (parameters) parameters_imp
    use logging

    implicit none

    character(*), parameter :: MOD_NAME = 'parameter'

contains

    module procedure parameter_init
        character(*), parameter :: PROC_NAME = 'parameter_init'
        character, allocatable :: buffer(:) 

        if (present(param_value)) buffer = param_value

        if (param_type /= TYPE_INTEGER .and. param_type /= TYPE_REAL .and. param_type /= TYPE_STRING) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Unsupported parameter type.')
       
        param = Parameter(param_name, param_type, buffer)
    end procedure parameter_init

   subroutine check_type(param, val)
        type(Parameter), intent(in) :: param
        class(*), intent(in) :: val

        select type(val)
            type is (integer)
                if (param%type == TYPE_INTEGER) return
            type is (real(DP))
                if (param%type == TYPE_REAL) return
            type is (character(*))
                if (param%type == TYPE_STRING) return
        end select

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

    module procedure get_val
        call check_type(param, val)

        !Overly verbose, but necessary workaround due to bug in gfortran.
        select type(val)
            type is (integer)
                val = transfer(param%value, val)
            type is (real(DP))
                val = transfer(param%value, val)
            type is (character(*))
                val = transfer(param%value, val)
        end select
    end procedure 
    
    module procedure set_val
        integer :: ind
        character, allocatable :: buffer(:)
        
        ind = find_index(params, name)
        call check_type(params(ind), val)

        allocate(buffer(sizeof(val)))
        params(ind)%value = transfer(val, buffer)
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
        real(DP) real_val

        select type(arg)
            type is (integer)
                real_val = real(arg)
            type is (real)
                real_val = arg
            type is (Parameter)
                real_val = get_numerical_value(arg)
        end select

        difference = get_numerical_value(param) - real_val
    end procedure 
    
    module procedure parameter_gt
        gt = ((param - arg) > 0._DP)
    end procedure     
    module procedure parameter_lt
        lt = ((param - arg) < 0._DP)
    end procedure     
    module procedure parameter_gteq
        gteq = ((param - arg) >= 0._DP)
    end procedure     
    module procedure parameter_lteq
        lteq = ((param - arg) <= 0._DP)
    end procedure     
end submodule parameters_imp
