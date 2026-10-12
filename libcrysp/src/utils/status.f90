!> Status reporting for libCrysP.
!>
!> libCrysP is a library, and may be embedded in a long-lived host such as a web server or an API backend. A bad request must
!> therefore never take the host process down with it. Every operation that can fail returns a [[Status]] describing what happened
!> instead of terminating, and the caller decides what to do about it.
module crysp_status
    use iso_c_binding
    use base_defs
    use conversions, only: to_c_string

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
    !> The three name buffers are NUL-terminated C strings. they are all-NUL when `code` is `CRYSP_OK`.
    !>
    !> @note
    !> Default-initialized to indicate success.
    !> @endnote
    !> @warning
    !> Mirrored as `crysp_status` in src/macro/trust_region.c, together with `NAME_LEN`/`MSG_LEN` and the codes it uses. Keep them in sync.
    !> @endwarning
    type, bind(C):: Status
        integer(C_INT)::         code                = CRYSP_OK    !! One of the status codes above.
        character(kind=C_CHAR):: mod_name(NAME_LEN)  = C_NULL_CHAR !! Module in which the failure was raised.
        character(kind=C_CHAR):: proc_name(NAME_LEN) = C_NULL_CHAR !! Routine in which the failure was raised.
        character(kind=C_CHAR):: message(MSG_LEN)    = C_NULL_CHAR !! Human-readable description of the failure.
    end type

contains

    !> Build a failed [[Status]].
    !>
    !> Names and message are truncated to the capacity of their buffer and always NUL-terminated.
    pure function fail(mod_name, proc_name, code, message) result(stat)
        character(*), intent(in)::           mod_name  !! Module raising the failure.
        character(*), intent(in)::           proc_name !! Routine raising the failure.
        integer, intent(in)::                code      !! Status code. Must not be `CRYSP_OK`.
        character(*), intent(in), optional:: message   !! Description of what went wrong.
        type(Status):: stat

        stat%code = code
        stat%mod_name = to_c_string(mod_name, NAME_LEN)
        stat%proc_name = to_c_string(proc_name, NAME_LEN)
        if (present(message)) stat%message = to_c_string(message, MSG_LEN)
    end function

    !> Did an operation fail?
    pure function failed(stat) result(f)
        type(Status), intent(in):: stat
        logical:: f

        f = stat%code /= CRYSP_OK
    end function
end module
