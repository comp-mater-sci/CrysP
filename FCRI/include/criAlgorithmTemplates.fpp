!>    \file criAlgorithm.fpp Algorithms of criAlgorithm.f90 parametrized with
!>          different types.

    ! Parametrization of the functions in this template file:

    ! _TYPE_NAME : Fortran type name that paramertizes the functions
    ! _LOWER_BOUND_FX_NAME

    !> Find the first element in array not before val, using operator <
    integer pure function _LOWER_BOUND_FX_NAME(array,val) result(res)
    implicit none
    _TYPE_NAME,dimension(:),intent(in)     :: array
    _TYPE_NAME,intent(in)                  :: val
    !
    integer :: first,dist,cnt,mid
    !
        first = lbound(array,dim=1)
        dist = ubound(array,dim=1)     ! full range: [first:first+dist-1]
        !
        do while (dist > 0)
            cnt = dist / 2
            mid = first + cnt
            if (array(mid) < val) then
                first = mid + 1
                dist = dist - (cnt + 1)
            else
                dist = cnt
            endif
        enddo
        res = first
    !
    end function

    ! Clean-up the parametrization
#undef _TYPE_NAME
#undef _LOWER_BOUND_FX_NAME
