!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven


!> The template requires two user-supplied preprocessor macros:
!> * _VALUE_TYPE that gives the actual value type, e.g. integer, double precision, type(mytype)
!> * _VALUE_NAME that gives a simple name for the value type, e.g. integer, double, mytype

#ifndef _VALUE_TYPE
#error Instantization of criExpandableVector requires _VALUE_TYPE macro to be defined.
#endif
#ifndef _VALUE_NAME
#error Instantization of criExpandableVector requires _VALUE_NAME macro to be defined.
#endif

! The preprocessor does not expands the macros recursively in a context 
! where operator ## is applied. So, we have to use an extra layer of indirection.
#define _CAT(x, y) x ## y
#define _MERGE(x, y) _CAT(x, y)

#define __FX(name, _T) _MERGE(_CAT(name, _), _T)
#define __TYPE_NAME(_T) _MERGE(_CAT(xVector, _), _T)



#ifndef DEFINITIONS_ONLY
    
    type :: __TYPE_NAME(_VALUE_NAME)
        !> Private data storage
        _VALUE_TYPE,dimension(:),allocatable,private  :: xdata
            
        !> Provides view on contents of the vector. 
        _VALUE_TYPE,dimension(:),pointer        :: values => null()
    end type


    !> Append element at the end of the vector
    interface xVector_push
        module procedure __FX(xVector_push, _VALUE_NAME)
    end interface


    !> Expands the xVector
    interface xVector_expand
        module procedure __FX(xVector_expand, _VALUE_NAME)
    end interface


    !> Returns the number of elements in the xVector
    interface xVector_size
        module procedure __FX(xVector_size, _VALUE_NAME)
    end interface


    !> Returns the number of elements in the xVector 
    !> (modelled after generic size)
    interface size
        module procedure __FX(xVector_size, _VALUE_NAME)
    end interface


    !> Returns the size of allocated storage capacity in terms of
    !> elements.
    interface xVector_capacity
        module procedure __FX(xVector_capacity, _VALUE_NAME)
    end interface
    

    
#endif ! of DEFINITIONS_ONLY

#if .not. (defined(DEFINITIONS_ONLY) .or. defined(DECLARATIONS_ONLY))
contains
#endif

#ifndef DECLARATIONS_ONLY
    
    integer function __FX(xVector_push, _VALUE_NAME)(v, item) result(info)
    implicit none
    type(__TYPE_NAME(_VALUE_NAME)),intent(inout),target :: v
    _VALUE_TYPE,intent(in) :: item
    !
    integer :: idx_last
    !
        call xVector_expand(v, 1, info)
        if (info /= criSuccess) return
        idx_last = xVector_size(v) + 1 !< Move the index
        ! ... and place the item
        v%xdata(idx_last) = item
        v%values => v%xdata(1:idx_last)
        info = criSuccess
    !
    end function


    !> Expands data storage to fit at least nelem new elements
    subroutine __FX(xVector_expand, _VALUE_NAME)(v, nelem, info)
    implicit none
    type(__TYPE_NAME(_VALUE_NAME)),intent(inout),target :: v
    integer,intent(in)  :: nelem
    integer,intent(out) :: info
    !
    _VALUE_TYPE,dimension(:),allocatable :: tmp
    integer :: ierr, idx_last
    !
        if (nelem <= 0) then
            ! Expand by 0 is legal, but ignored
            info = criErr_BadArgs
            if (nelem == 0) info = criSuccess
            return
        endif
        info = criErr_MemAlloc
        if (.not. allocated(v%xdata)) then
            v%values => null()
            allocate(v%xdata(nelem), stat=ierr)
            if (ierr /= 0) return
        else
            idx_last = xVector_size(v)
            if (idx_last + nelem > size(v%xdata)) then
                allocate(tmp(2*(idx_last + nelem)))
                tmp(1:idx_last) = v%xdata(1:idx_last)
                deallocate(v%xdata)
                call move_alloc(tmp,v%xdata)
                v%values => v%xdata(1:idx_last)
            endif
        endif
        info = criSuccess
    !
    end subroutine
    
    
    !> Returns the number of elements currently stored in the vector
    integer function __FX(xVector_size, _VALUE_NAME)(v) result(res)
    implicit none
    type(__TYPE_NAME(_VALUE_NAME)),intent(in) :: v
    !
        res = 0
        if (associated(v%values)) res = size(v%values)
    !
    end function
    
    
    !> Returns the size of the storage space currently allocated for the vector, 
    !> expressed in terms of elements.
    integer function __FX(xVector_capacity, _VALUE_NAME)(v) result(res)
    implicit none
    type(__TYPE_NAME(_VALUE_NAME)),intent(in) :: v
    !
        res = 0
        if (allocated(v%xdata)) res = size(v%xdata)
    !
    end function

#endif ! of DECLARATIONS_ONLY


