module logging
    use base_defs

    implicit none
    public

    !Supported error codes
    !Explicitly numbered for easy reference
    enum, bind(C)
        enumerator:: ERR      = 1, & !! General error
                     ERR_DIMS = 2, & !! Out of bounds
                     ERR_VAL  = 3, & !! Unacceptable value
                     ERR_IO   = 4, & !! Error during an IO operation
                     ERR_INIT = 5, & !! Procedure call without proper initialization
                     ERR_TYPE = 6, & !! Erroneous type provided.
                     ERR_ARG  = 7    !! Invalid argument provided by caller.
    end enum

    !Log exception if possible and terminate
    interface log_error
        module procedure log_error, log_error_pure
    end interface

    interface
        module subroutine log_error(module, routine, code, message)
            character(*), intent(in)::           module
            character(*), intent(in)::           routine
            integer, intent(in)::                code
            character(*), intent(in), optional:: message
        end subroutine
        module pure subroutine log_error_pure(code)
            integer, intent(in):: code
        end subroutine
    end interface
end module logging

submodule(logging) log_imp

    implicit none

contains

    module procedure log_error
        print *, 'EXCEPTION ', module, ', ', routine, ', ', code, ' ', message
        error stop code
    end procedure

    module procedure log_error_pure
        error stop code
    end procedure
end submodule log_imp
