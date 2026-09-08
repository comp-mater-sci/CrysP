!> This module contains the most basic definitions used by almost any other module in the project.
module base_defs
    use, intrinsic:: iso_fortran_env, only: output_unit
    use iso_c_binding

    implicit none

    public

    integer, parameter:: DP = selected_real_kind(15, 307)  !! Kind for reals corresponding to the classic notion of a double precision floating point number of 8 bytes.
    integer, parameter:: BYTE = c_int8_t                   !! Guaranteed by the standard to be 1 byte. Useful for generating bytestrings.
    integer, parameter:: MAX_PATHLEN = 2048                !! Maximum file path length.
    integer, parameter:: FNAME_LEN = 512                   !< Length of filenames
    integer, parameter:: DISPLAY_UNIT = output_unit        !! Identifier for stdout. Used in write statements.
    real(DP), parameter:: TOLERANCE  = 1.E-9_DP            !! Default tolerance on floating point calculations to compensate for inherent inaccuracy of floating point arithmetic,
    integer, parameter:: NAME_LEN = 32
    integer, parameter:: DESCRIPTION_LEN = 1024
                                                           !! especially for multithreaded computations.
    real(DP), parameter:: ACCURACY = 1.E-2_DP              !! Accuracy on linear approximations.

    !> Status codes. Used to communicate information on the completion of a procedure to the caller.
    enum, bind(C)
        enumerator:: VEF_OK      !! Sucessful execution
        enumerator:: VEF_FAIL    !! No errors occured, but the routine did not accomplish its main goal.
        enumerator:: VEF_ERROR   !! Errors occured during exection.
    end enum
end module
