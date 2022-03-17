!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2017-04-07
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

#include "criMacros.fpp"

!> In-memory cache/table of the recent results from the multi-level model.
module dmcResultTable
use criErrcodes
use criMathUtils
use alamYLPConstants
use dmcResultTableRecord
implicit none

    public :: ResultTable
    private

    type :: ResultTable
    
        integer,private                         :: saved_session_idx = 0
        
        type(xVector_ResultTableRecord),private :: table
        
    contains
        procedure,pass(this)    :: reserve
    
        procedure,pass(this)    :: put
       
        procedure,pass(this)    :: get
        
        procedure,pass(this)    :: store
        
        procedure,pass(this)    :: load
        
    end type


contains

    !> Reserve capacity slots in the table
    integer function reserve(this, capacity) result(info)
    class(ResultTable),intent(inout)    :: this
    integer,intent(in)                  :: capacity
    !
        call xVector_expand(this%table, capacity, info)
    !
    end function
    
    
    !> Add result to the database
    integer function put(this, A, SonA) result(info)
    class(ResultTable),intent(inout)   :: this
    double precision,dimension(alamEval_vSD_dim),intent(in) :: A
    double precision,dimension(alamEval_vSD_dim),intent(in) :: SonA
    !
        info = xVector_push(this%table, ResultTableRecord(A, SonA))
    !
    end function


    !> Find item in the database that has the smallest angle between
    !> S and item%vSonA, optionally restricting the choice to acceptable angles 
    !> smaller than max_angle.
    !> Unless criSuccess is returned, the argument A is undefined.
    !>
    !> \return criSuccess on success, criFailure if no item satisfies the 
    !> requirement 
    integer function get(this, S, A, max_angle) result(info)
    class(ResultTable),intent(inout)   :: this
    double precision,dimension(alamEval_vSD_dim),intent(in) :: S
    double precision,dimension(alamEval_vSD_dim),intent(out):: A
    double precision,intent(in),optional                    :: max_angle !< Threshold angle (in radians)
    !
    integer :: npoints, i, min_idx_a(1), min_idx
    equivalence(min_idx_a(1), min_idx)
    double precision,dimension(:),allocatable :: angles
    !
        info = criFailure
        npoints = size(this%table)
        if (npoints > 0) then
            allocate(angles(npoints))
            do concurrent (i = 1:npoints)
                angles(i) = vec_angle(S, this%table%values(i)%vSonA)
            enddo
            min_idx_a = minloc(angles)
            if (present(max_angle)) then
                CHOOSE(info, angles(min_idx) > max_angle, criFailure, criSuccess)
            else
                info = criSuccess
            endif
            if (info == criSuccess) A = this%table%values(min_idx)%vA
        endif
    !
    end function


    !> Store the values in file fpath
    integer function store(this, fpath) result(info)
    class(ResultTable),intent(inout)    :: this
    character(len=*),intent(in)         :: fpath
    !
    integer :: iounit, ierr, i
    
        if (size(this%table) > this%saved_session_idx) then
            open(newunit=iounit, file=fpath, position='APPEND', action='WRITE',&
                 status='UNKNOWN', form='UNFORMATTED', iostat=ierr)
            RETURN_IF_WITH(ierr /= 0, info=criErr_IOOpen)
            do i = this%saved_session_idx + 1, size(this%table)
                write(iounit, iostat=ierr) this%table%values(i)
                if (ierr /= 0) exit
            enddo
            if (ierr /= 0) then
                ! clear the cache
                close(iounit, status='DELETE', iostat=ierr)
                this%saved_session_idx = 0
            else
                this%saved_session_idx = this%saved_session_idx + i - 1
                close(iounit, iostat=ierr)
            endif
        endif
        info = criSuccess
    !
    end function


    !> Load the values from file fpath. The file may or may not exist.
    integer function load(this, fpath) result(info)
    class(ResultTable),intent(inout)    :: this
    character(len=*),intent(in)         :: fpath
    !
    integer :: iounit, ierr
    type(ResultTableRecord) :: tmp
    !
        info = criSuccess
        open(newunit=iounit, file=fpath, status='OLD', &
             form='UNFORMATTED', iostat=ierr)
        if (ierr == 0) then
            ! the file exists, load data from it
            do while ((ierr == 0) .or. (info /= criSuccess))
                read(iounit, iostat=ierr) tmp
                if (ierr == 0) then
                    info = xVector_push(this%table, tmp)
                else
                    exit
                endif
            enddo
            ! negative ierr on end-of-file or end-of-record; positive on error
            if (ierr > 0 .or. info /= criSuccess) then
                info = criErr_IORead
            else
                this%saved_session_idx = size(this%table)
                info = criSuccess
            endif
            close(iounit)
        endif
    end function
    
    
end module
