module logging
    use definitions

    implicit none
    public

    !Supported error codes
    !Explicitly numbered for easy reference
    enum, bind(C)
        enumerator :: ERR      = 1, & !< General error
                      ERR_DIMS = 2, & !< Out of bounds
                      ERR_VAL  = 3, & !< Unacceptable value
                      ERR_IO   = 4, & !< Error during an IO operation
                      ERR_INIT = 5   !< Procedure call without proper initialization
    end enum

    !Print trace message when preprocessor flag TRACE is set
    interface log_trace
        module procedure log_trace_str, log_trace_tensor
    end interface

    !Log exception if possible and terminate
    interface log_error
        module procedure log_error, log_error_pure
    end interface

    interface
        !log_trace
        module subroutine log_trace_str(caller_module, caller_routine, message)
        character(len=*), intent(in) :: caller_module,  &
                                        caller_routine, & 
                                        message
        end subroutine
        module subroutine log_trace_tensor(caller_module, caller_routine, tensor)
            character(len=*), intent(in)        :: caller_module, &
                                                   caller_routine
            real(dp), dimension(..), intent(in) :: tensor
        end subroutine

        !log_error
        module subroutine log_error(caller_module, caller_routine, code, message)
            character(*),   intent(in)           :: caller_module, &
                                                    caller_routine
            integer,        intent(in)           :: code
            character(*),   intent(in), optional :: message
        end subroutine
        module pure subroutine log_error_pure(code)
            integer, intent(in) :: code
        end subroutine
    end interface 
end module logging

submodule(logging) log_imp
    implicit none

    contains

    module procedure log_trace_str
#ifdef TRACE
        print *, 'TRACE ', caller_module, ', ', caller_routine, ': ', message
#endif
    end procedure

    module procedure log_trace_tensor
        real(dp) :: buffer

#ifdef TRACE
        select rank(tensor)
            rank(0)
                buffer = tensor
            rank(1)
                buffer = sum(tensor)
            rank(2)
                buffer = sum(tensor)
            rank(3)
                buffer = sum(tensor)
            rank default
                call log_error('log', 'trace_tensor', ERR_DIMS, 'Maximum supported rank is 3.') 
        end select
        print *, 'TRACE ', caller_module, ', ', caller_routine, ': ', buffer
#endif
    end procedure

    module procedure log_error
        print *, 'EXCEPTION ', caller_module, ', ', caller_routine, ', ', code, ' ', message
        error stop code
    end procedure

    module procedure log_error_pure
        error stop code
    end procedure
end submodule log_imp
