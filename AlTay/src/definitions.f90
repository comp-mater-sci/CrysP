!>Global definitions used in multiple modules within AlTay
module definitions
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
end module definitions 
