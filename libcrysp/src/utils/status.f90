!> Status reporting for libCrysP.
!>
!> libCrysP is a library, and may be embedded in a long-lived host such as a web server or an API backend. A bad request must
!> therefore never take the host process down with it. Every operation that can fail returns a [[Status]] describing what happened
!> instead of terminating, and the caller decides what to do about it.
module crysp_status
    use base_defs

    implicit none

    public

    !> Supported status codes.
    !>
    !> Explicitly numbered for easy reference from a caller in another language. `CRYSP_OK` is zero, so `if (stat%code)` reads
    !> naturally as "did it fail" from C.
    enum, bind(C)
        enumerator:: CRYSP_OK       = 0, & !! Successful execution.
                     CRYSP_ERR      = -1, & !! General error
                     CRYSP_ERR_DIMS = -2, & !! Out of bounds
                     CRYSP_ERR_VAL  = -3, & !! Unacceptable value
                     CRYSP_ERR_IO   = -4, & !! Error during an IO operation
                     CRYSP_ERR_INIT = -5, & !! Procedure call without proper initialization
                     CRYSP_ERR_TYPE = -6, & !! Erroneous type provided.
                     CRYSP_ERR_ARG  = -7    !! Invalid argument provided by caller.
    end enum

    !> Outcome of an operation that may fail.
    !>
    !> `code` is `CRYSP_OK` on success. Any oof the CRYSP_ERR... error enums above on failure.
    !> The strings are unallocated when `code` is `CRYSP_OK`, and all allocated on a failure built by [[fail]].
    !>
    !> @note
    !> Default-initialized to indicate success. C callers get a [[crysp_utils_c:Status_C]] instead.
    !> @endnote
    type:: Status
        integer::                   code = CRYSP_OK !! One of the status codes above.
        character(:), allocatable:: mod_name        !! Module in which the failure was raised.
        character(:), allocatable:: proc_name       !! Routine in which the failure was raised.
        character(:), allocatable:: message         !! Human-readable description of the failure. Empty if none was given.
    end type

contains

    !> Build a failed [[Status]].
    pure function fail(mod_name, proc_name, code, message) result(stat)
        character(*), intent(in)::           mod_name  !! Module raising the failure.
        character(*), intent(in)::           proc_name !! Routine raising the failure.
        integer, intent(in)::                code      !! Status code. Must not be `CRYSP_OK`.
        character(*), intent(in), optional:: message   !! Description of what went wrong.
        type(Status):: stat

        stat%code = code
        stat%mod_name = mod_name
        stat%proc_name = proc_name
        stat%message = ''
        if (present(message)) stat%message = message
    end function

    !> Did an operation fail?
    pure function failed(stat) result(f)
        type(Status), intent(in):: stat
        logical:: f

        f = stat%code /= CRYSP_OK
    end function
end module
