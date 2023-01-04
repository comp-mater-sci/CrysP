module altay_log
    use altay_definitions

    implicit none
    private

    !>\name Exit codes from altayHardLaw_DSH subroutines and functions:
    !>@{
    integer, parameter, public :: VEF_OK            = 0     !< OK
    integer, parameter, public :: VEF_ERROR         = -1    !< General error (not covered by any specific error code).
    integer, parameter, public :: VEF_BADDIMS       = -2    !< At least one parameter out of boundaries
    integer, parameter, public :: VEF_BADVAL        = -5    !< At least one input parameter has unacceptable value
    integer, parameter, public :: VEF_OutOfRange    = -6    !< At least one input parameter has a value outside acceptable range
    integer, parameter, public :: VEF_IO            = -15   !< Error during an IO operation
    integer, parameter, public :: VEF_Nss           = -16   !< Unsupported number of slip systems proposed. Supported values are: 12, 24
    integer, parameter, public :: VEF_Uninitialized = -50   !< Call to module procedures without proper initialization of the module
    !>@}

    interface vef_trace
        module procedure vef_trace_str, vef_trace_tensor
    end interface

    public  ::  vef_trace,          &
                vef_exception

contains

    subroutine vef_trace_str(caller_module, caller_routine, message)
        character(len=*), intent(in)    :: caller_module, caller_routine, message
#ifdef TRACE
        print *, 'TRACE ', caller_module, ', ', caller_routine, ': ', message
#endif
    end subroutine

    subroutine vef_trace_tensor(caller_module, caller_routine, tensor)
        character(len=*), intent(in)                :: caller_module, caller_routine
        real(dp), dimension(..), intent(in) :: tensor
        real(dp)                            :: buffer

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
                print *, "TRACE Invalid rank!"
                call exit(1)
        end select
        print *, 'TRACE ', caller_module, ', ', caller_routine, ': ', buffer
#endif
    end subroutine

    subroutine vef_exception(caller_module, caller_routine, code, message)
        character(*),   intent(in)              ::    caller_module, caller_routine
        integer,        intent(in)              ::    code
        character(*),   intent(in), optional    ::    message

        print *, 'EXCEPTION ', caller_module, ', ', caller_routine, ', ', code, ' ', message
    end subroutine
end module
