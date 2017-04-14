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
    integer :: ierr, idx_last, new_size, i, new_min_size
    !
        if (nelem <= 0) then
            ! Expand by 0 is legal, but ignored
            info = criErr_BadArgs
            if (nelem == 0) info = criSuccess
            return
        endif
        if (.not. allocated(v%xdata)) then
            v%values => null()
            allocate(v%xdata(nelem), stat=ierr)
            if (ierr /= 0) then
                info = criErr_MemAlloc
                return
            endif
        else
            info = criErr_BadArgs
            idx_last = xVector_size(v)
            new_min_size = idx_last + nelem
            ! Check for integer overflow
            if (new_min_size < idx_last) return
            if (new_min_size > size(v%xdata)) then
                new_size = 2*(new_min_size)
                ! Check for integer overflow
                if (new_size < idx_last) then
                    ! new_size is bigger than huge(), so let's limit new_size
                    ! to that bound.
                    new_size = huge(idx_last)
                    ! We need to guarantee that the storage is expanded to 
                    ! make space for at least nelem objects.
                    if (new_size < new_min_size) return
                endif
                allocate(tmp(new_size), stat=ierr)
                if (ierr /= 0) then
                    info = criErr_MemAlloc
                    return
                endif
                ! Copy data from the old storage to the newly allocated
                ! storate.
                ! Note: normally this operation should be just an assignment
                ! statement as below. However, for some unclear reason Intel
                ! Fortran compiler makes an temporary array (even though there
                ! are neither overlaps, nor storage aliasing) on the stack.
                ! This makes the code crash due to stack corruption.
                ! tmp(1:idx_last) = v%xdata(1:idx_last)
                ! DO CONCURRENT loop does not involve temporary array, but is
                ! slower.
                do concurrent (i=1:idx_last)
                    tmp(i) = v%xdata(i)
                enddo
                deallocate(v%xdata)
                call move_alloc(tmp,v%xdata)
                v%values => v%xdata(1:idx_last)
            endif
        endif
        info = criSuccess
    !
    end subroutine
    
    
    !> Returns the number of elements currently stored in the vector
    pure integer function __FX(xVector_size, _VALUE_NAME)(v) result(res)
    implicit none
    type(__TYPE_NAME(_VALUE_NAME)),intent(in) :: v
    !
        res = 0
        if (associated(v%values)) res = size(v%values)
    !
    end function
    
    
    !> Returns the size of the storage space currently allocated for the vector, 
    !> expressed in terms of elements.
    pure integer function __FX(xVector_capacity, _VALUE_NAME)(v) result(res)
    implicit none
    type(__TYPE_NAME(_VALUE_NAME)),intent(in) :: v
    !
        res = 0
        if (allocated(v%xdata)) res = size(v%xdata)
    !
    end function

#endif ! of DECLARATIONS_ONLY


