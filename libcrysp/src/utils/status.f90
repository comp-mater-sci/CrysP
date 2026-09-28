!> Status reporting for libCrysP.
!>
!> libCrysP is a library, and may be embedded in a long-lived host such as a web server or an API backend. A bad request must
!> therefore never take the host process down with it. Every operation that can fail returns a [[Status]] describing what happened
!> instead of terminating, and the caller decides what to do about it.
module crysp_status
    use base_defs
    use conversions, only: string_to_chars, chars_to_string

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
    !> Default-initialized to indicate success. C callers get a [[libcrysp_c_binding:Status_C]] instead.
    !> @endnote
    type:: Status
        integer::                   code = CRYSP_OK !! One of the status codes above.
        character(:), allocatable:: mod_name        !! Module in which the failure was raised.
        character(:), allocatable:: proc_name       !! Routine in which the failure was raised.
        character(:), allocatable:: message         !! Human-readable description of the failure. Empty if none was given.
    end type

    !> C-compatible counterpart of [[Status]], returned by value by every entry point of [[libcrysp_c_binding]].
    !>
    !> `code` is `CRYSP_OK` on success. The three buffers are NUL-terminated C strings, truncated to fit, and all-NUL when `code` is
    !> `CRYSP_OK`.
    !>
    !> @note
    !> Returned by value, so keep it small.
    !> @endnote
    type, bind(C):: Status_C
        integer(C_INT)::         code                = CRYSP_OK    !! One of the status codes in [[crysp_status]].
        character(kind=C_CHAR):: mod_name(NAME_LEN)  = C_NULL_CHAR !! Module in which the failure was raised.
        character(kind=C_CHAR):: proc_name(NAME_LEN) = C_NULL_CHAR !! Routine in which the failure was raised.
        character(kind=C_CHAR):: message(MSG_LEN)    = C_NULL_CHAR !! Human-readable description of the failure.
    end type

    interface assignment(=)
        module procedure status_to_c, status_from_c
    end interface

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

    !> Convert a status to its C-compatible counterpart.
    subroutine status_to_c(stat_c, stat)
        type(Status_C), intent(out):: stat_c
        type(Status), intent(in):: stat

        stat_c%code = int(stat%code, C_INT)
        if (stat%code /= CRYSP_OK) then
            stat_c%mod_name = string_to_chars(stat%mod_name, size(stat_c%mod_name))
            stat_c%proc_name = string_to_chars(stat%proc_name, size(stat_c%proc_name))
            stat_c%message = string_to_chars(stat%message, size(stat_c%message))
        end if
    end subroutine

    !> Convert a C-compatible status to a status. Strings end at the first NUL of their buffer.
    subroutine status_from_c(stat, stat_c)
        type(Status), intent(out):: stat
        type(Status_C), intent(in):: stat_c

        stat%code = int(stat_c%code)
        if (stat%code /= CRYSP_OK) then
            stat%mod_name = chars_to_string(stat_c%mod_name)
            stat%proc_name = chars_to_string(stat_c%proc_name)
            stat%message = chars_to_string(stat_c%message)
        end if
    end subroutine

end module
