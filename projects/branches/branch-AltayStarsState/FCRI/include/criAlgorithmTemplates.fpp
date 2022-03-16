!
! $Id: criAlgorithmTemplates.fpp 2389 2015-11-09 10:59:21Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2015-11-07, based on the content
!>          of criAlgorithm.f90
!>    $Revision: 2389 $
!>    $Date: 2015-11-09 11:59:21 +0100 (Mon, 09 Nov 2015) $
!>
!>    History of modifications: (see svn log)
!>
!>    \file criAlgorithm.fpp Algorithms of criAlgorithm.f90 parametrized with 
!>          different types.
    
    ! Parametrization of the functions in this template file:
    
    ! _TYPE_NAME : Fortran type name that paramertizes the functions
    ! _LOWER_BOUND_FX_NAME
    ! _UPPER_BOUND_FX_NAME
    !_ BINARY_SEARCH_FX_NAME
    ! _BINARY_SEARCH2_FX_NAME
    
    
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

    !> Find the last element in array not before val, using operator <
    integer pure function _UPPER_BOUND_FX_NAME(array,val) result(res)
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
            if (.not.(val < array(mid))) then
                first = mid + 1
                dist = dist - (cnt + 1)
            else
                dist = cnt
            endif
        enddo
        res = first
    !    
    end function
      

    !> Find out if val appears in a sorted array, using operator < for equivalence.
    logical pure function _BINARY_SEARCH_FX_NAME(array,val) result(res)
    implicit none
    _TYPE_NAME,dimension(:),intent(in)     :: array
    _TYPE_NAME,intent(in)                  :: val
    !
    integer :: first,last,tmp
    !
        res = .false.
        first = lbound(array,dim=1)
        last = ubound(array,dim=1)
        ! The element cannot appear in a zero-lenght array
        if (first < last) then
            tmp = _LOWER_BOUND_FX_NAME(array,val)
            if (tmp <= last) res = .not.(val < array(tmp))
        endif
    !
    end function
      
     
    !> Find out if val appears in a sorted array, using operator < for equivalence. 
    !> The function is optimized for datasets where the searched value is the most frequently
    !> outside the range of values in the array.
    logical pure function _BINARY_SEARCH2_FX_NAME(array,val) result(res)
    implicit none
    _TYPE_NAME,dimension(:),intent(in)     :: array
    _TYPE_NAME,intent(in)                  :: val
    !
    integer :: first,last,tmp
    !
        res = .false.
        first = lbound(array,dim=1)
        last = ubound(array,dim=1)
        ! The element cannot appear in a zero-lenght array
        if (first < last) then
            ! For cases when the value is outside the the bounds:
            if (.not. ( (array(last) < val) .or. (val < array(first))) ) then
                tmp = _LOWER_BOUND_FX_NAME(array,val)
                if (tmp <= last) res = .not.(val < array(tmp))
            endif
        endif
    !
    end function

    ! Clean-up the parametrization
#undef _TYPE_NAME
#undef _LOWER_BOUND_FX_NAME
#undef _UPPER_BOUND_FX_NAME
#undef _BINARY_SEARCH_FX_NAME
#undef _BINARY_SEARCH2_FX_NAME



 