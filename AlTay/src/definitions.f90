!>Global definitions used in multiple modules within AlTay
module definitions
    use,intrinsic :: iso_fortran_env, only: output_unit
    implicit none
    public

    !>Standard real(dp)
    integer, parameter :: dp = selected_real_kind(15,307)

    !Status codes
    enum, bind(C)
        enumerator :: VEF_OK, &
                      VEF_FAIL, &
                      VEF_ERROR
    end enum
    integer,parameter       :: display_unit = output_unit

end module definitions
