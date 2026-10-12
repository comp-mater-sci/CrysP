!> Terminal error reporting for the CLI.
!>
!> @note
!> Library code does not belong here. libCrysP may be embedded in a long-lived host, so an operation that can fail returns a
!> [[crysp_status:Status]] rather than terminating. The CLI can afford to quit: it reports its own errors through [[log_error]]
!> and checks the result of every library call with [[handle_crysp_status]].
!> @endnote
module logging
    use base_defs
    use conversions, only: from_c_string
    use crysp_status

    implicit none
    public

    interface
        !> Log an exception and terminate.
        module subroutine log_error(module, routine, code, message)
            character(*), intent(in)::           module
            character(*), intent(in)::           routine
            integer, intent(in)::                code
            character(*), intent(in), optional:: message
        end subroutine

        !> Log and terminate if a libCrysP call failed; do nothing if it succeeded.
        !>
        !> Meant to wrap a library call directly: `call handle_crysp_status(crysp_...(...))`.
        module subroutine handle_crysp_status(stat)
            type(Status), intent(in):: stat
        end subroutine
    end interface
end module logging

submodule(logging) log_imp

    implicit none

contains

    module procedure log_error
        !Explicit format so that a long message is not wrapped onto a new line as list-directed output would.
        if (present(message)) then
            print '(5A,I0,2A)', 'EXCEPTION ', module, ', ', routine, ', ', code, ', ', message
        else
            print '(5A,I0)', 'EXCEPTION ', module, ', ', routine, ', ', code
        end if
        error stop code
    end procedure

    module procedure handle_crysp_status
        if (.not. failed(stat)) return

        call log_error(from_c_string(stat%mod_name), from_c_string(stat%proc_name), stat%code, from_c_string(stat%message))
    end procedure
end submodule log_imp
