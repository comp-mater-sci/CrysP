!>Global definitions used in multiple modules within AlTay
module definitions
    use,intrinsic :: iso_fortran_env, only: output_unit
    implicit none
    public

    integer, parameter :: DP = selected_real_kind(15,307)
    REAL(DP), parameter :: TOLERANCE = 1.E-9_DP 

    !Status codes
    enum, bind(C)
        enumerator :: VEF_OK, &
                      VEF_FAIL, &
                      VEF_ERROR
    end enum
    integer,parameter       :: display_unit = output_unit

    integer :: LEC = 4   !< data set with slip systems
    integer :: IMP1 = 7  !< output-file with successive "current situations"
    integer :: IMP5 = 111     !< output of stress-strain or slip-stress

    !> Maximal length of path acceptable by the filesystem
    integer,parameter       :: MAX_PATHLEN = 2048


end module definitions
