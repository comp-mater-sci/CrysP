!
! $Id: criTestIterUtils.f90 2479 2016-03-18 11:30:34Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of the initial release: 2014-02-16
!>    $Revision: 2479 $
!>    $Date: 2016-03-18 12:30:34 +0100 (Fri, 18 Mar 2016) $
!>
!>    History of modifications: (see svn log)
!>
#include "criStdDefs.fpp"
#include "criTest.fpp"
!
!> Test suite for criIterUtils    
module criTestIterUtils
use criIterUtils
use criTest
implicit none

    public criTestIterUtils_main

contains
      
    logical function criTestIterUtils_main() result(stat)
    implicit none
      
        stat = test_subscriptsFromMask()
      
    end function

    logical function test_subscriptsFromMask()
    implicit none
    double precision,dimension(5) :: s
    integer,dimension(5) :: sidx
    logical,dimension(5) :: smap
    integer,dimension(:),allocatable :: idx
    !
        test_subscriptsFromMask = .false.
        
        call setUp()
        call which_indices([.false.,.false.,.false.],idx) 
        _TEST('Mask with all false', (allocated(idx) .and. (size(idx) == 0)) )
        call tearDown()
        !
        call setUp()
        call which_indices([.true.,.false.,.false.],idx) 
        _TEST('First element true', (allocated(idx) .and. (size(idx) == 1) .and. (idx(1)==1)) )
        call tearDown()
        !
        call setUp()
        call which_indices([.true.,.false.,.true.],idx) 
        _TEST('First and last element true', (allocated(idx) .and. (size(idx) == 2) .and. all(idx == [1,3])) )
        call tearDown()
        !
        call setUp()
        call which_indices([.true.,.false.,.true.],idx,-1)
        _TEST('First and last element true, indexing from -1', (allocated(idx) .and. (size(idx) == 2) .and. all(idx == [-1,1])) )
        call tearDown()

        call setUp()
        call which_indices([.true.,.false.,.true.],idx,-1)
        _TEST('First and last element true, indexing from -1', (allocated(idx) .and. (size(idx) == 2) .and. all(idx == [-1,1])) )
        call tearDown()
        
        call setUp()
        smap = [.true.,.false.,.true.,.false.,.false.]
        call which_indices(smap,idx)
        _TEST('Elements: 1/5 and 3/5, subroutine', (allocated(idx) .and. (size(idx) == 2) .and. all(idx == [1,3]) .and. all(s(idx) == [5,1])) )
        call tearDown()

        call setUp()
        smap = .false.
        _TEST('Mask with all false, function', (size(s(which(smap))) == 0) )
        call tearDown()
        
        call setUp()
        smap = [.true.,.false.,.true.,.false.,.false.]
        _TEST('Elements: 1/5 and 3/5, function', all(s(which(smap)) == [5,1]) )
        call tearDown()
        
        
        test_subscriptsFromMask = .true.
        
        contains
        
            subroutine setUp()
            implicit none
                smap = .false.
                sidx = 0
                s = [5,2,1,0,4]
                if (allocated(idx)) deallocate(idx)
            end subroutine
        
            subroutine tearDown()
            implicit none
                if (allocated(idx)) deallocate(idx)
            end subroutine
            
    end function
   
end module
    