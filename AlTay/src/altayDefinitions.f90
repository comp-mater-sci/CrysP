!>Global definitions used in multiple modules within AlTay
module altay_definitions
    implicit none

    !>Standard real(dp)
    integer, parameter :: dp = selected_real_kind(15,307)

    integer, parameter :: HARDENING_MODEL_NONE      = 0
    integer, parameter :: HARDENING_MODEL_VOCE      = 1
    integer, parameter :: HARDENING_MODEL_SWIFT_K   = 2
    integer, parameter :: HARDENING_MODEL_SWIFT_S   = 3
    integer, parameter :: HARDENING_MODEL_BP        = 11
    integer, parameter :: HARDENING_MODEL_PEBP_SCREW  = 12
    integer, parameter :: HARDENING_MODEL_PEBP_LOOP   = 13

    !>\name Exit codes from altayHardLaw_DSH subroutines and functions:
    !>@{
    integer, parameter, public :: VEF_OK            = 0     !< OK
    integer, parameter, public :: VEF_ERROR         = -1    !< General error (not covered by any specific error code).
    integer, parameter, public :: VEF_BADDIMS       = -2    !< At least one parameter out of boundaries
    integer, parameter, public :: VEF_FAIL          = 1
    integer, parameter, public :: VEF_BADVAL        = -5    !< At least one input parameter has unacceptable value
    integer, parameter, public :: VEF_OutOfRange    = -6    !< At least one input parameter has a value outside acceptable range
    integer, parameter, public :: VEF_IO            = -15   !< Error during an IO operation
    integer, parameter, public :: VEF_Nss           = -16   !< Unsupported number of slip systems proposed.
    integer, parameter, public :: VEF_Uninitialized = -50   !< Call to module procedures without proper initialization of the module
    !>@}

contains

      !> Returns .true. if errcode represents an error condition, otherwise .false.
    elemental logical function is_error(errcode)
        integer,value,intent(in)    :: errcode
      
        is_error = (errcode /= VEF_OK) .and. (errcode /= VEF_FAIL)
    end function
end module 
