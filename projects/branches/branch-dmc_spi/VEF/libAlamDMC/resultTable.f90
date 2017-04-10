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

public :: ResultTable, db

private

    type :: ResultTable
    
        type(xVector_ResultTableRecord),private :: table
        
    contains
        procedure,pass(this)    :: reserve
    
        procedure,pass(this)    :: store
       
        procedure,pass(this)    :: get
        
    end type

    
    ! FIXME: static object, and at wrong place.
    type(ResultTable),target,save :: db

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
    integer function store(this, A, SonA) result(info)
    implicit none
    class(ResultTable),intent(inout)   :: this
    double precision,dimension(alamEval_vSD_dim),intent(in) :: A
    double precision,dimension(alamEval_vSD_dim),intent(in) :: SonA
    !
        info = xVector_push(this%table, ResultTableRecord(A, SonA))
    !
    end function


    !> Find item in the database that has the smallest angle betweem
    !> S and item%vSonA, optionally restricting the choice to acceptable angles 
    !> smaller than max_angle.
    !> Unless criSuccess is returned, the argument A is undefined.
    !>
    !> \return criSuccess on success, criFailure if no item satisfies the 
    !> requirement, 
    integer function get(this, S, A, max_angle) result(info)
    implicit none
    class(ResultTable),intent(inout)   :: this
    double precision,dimension(alamEval_vSD_dim),intent(in) :: S
    double precision,dimension(alamEval_vSD_dim),intent(out) :: A
    double precision,intent(in),optional                    :: max_angle
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
            ! <<-- TESTING
            write(*,'(A,1X,F7.2)') 'min angle: ', rad2deg(angles(min_idx))
            ! -->> TESTING
            if (present(max_angle)) then
                CHOOSE(info, angles(min_idx) > max_angle, criFailure, criSuccess)
            else
                info = criSuccess
            endif
            if (info == criSuccess) A = this%table%values(min_idx)%vA
        endif
    !
    end function

end module
