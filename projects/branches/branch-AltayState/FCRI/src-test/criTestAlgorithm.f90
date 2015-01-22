!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2012-06-11
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criTestAlgorithm.f90 
!
!
#include "criStdDefs.fpp"
#include "criTest.fpp"
!
!> Tests on the extensions to the Facet potential expression
module criTestAlgorithm
use criAlgorithm

public criTestAlgorithm_main

contains
      
      logical function criTestAlgorithm_main() result(stat)
      implicit none
      
            stat = test_replaceAll()
      
            stat = test_isPresent()
      
      end function

      subroutine testBoundsAndBinarySearch()
      implicit none
      integer,dimension(:),allocatable :: arr, tst 
      logical,dimension(:),allocatable :: tst_val
      logical,dimension(3) :: found
      integer :: i, count
      !      
            arr = [5,7,10,22,33,35,44,50,55,56,60,70,71,72,73,80,90,100]
            tst = [4,5,60,90,100,200]
            tst_val = [.false.,.true.,.true.,.true.,.true.,.false.]
            
            do i = 1,size(tst)
                  write(*,*)  (binarySearch(arr,tst(i)) == tst_val(i)), &
                              (binary_search(arr,tst(i)) == tst_val(i)),&
                              (binary_search2(arr,tst(i)) == tst_val(i))
            enddo
            
            write(*,*) '---'
            do i=1,size(arr)
                  write(*,*) binarySearch(arr,arr(i)),binary_search(arr,arr(i))
            enddo
      
            write(*,*) '---'
            
            count = 0
            do i=-10,arr(size(arr))+10
                  found = [ binarySearch(arr,i), binary_search(arr,i), binary_search2(arr,i)]
                  if (all(found)) then
                        count = count + 1
                        write(*,*) i
                  endif
            enddo
      
            if (count /= size(arr)) write(*,*) 'Error!'
            
            write(*,*) '---'
            
            write(*,*) 'lower_bound'
            do i=1,size(tst)
                  write(*,*) lower_bound(arr,tst(i))
            enddo
            write(*,*) 'upper_bound'
            do i=1,size(tst)
                  write(*,*) upper_bound(arr,tst(i))
            enddo
      !      
      end subroutine

      logical function test_replaceAll()
      implicit none
      !
            test_replaceAll = .false.
            ! Test: neutral for empty string (returns empty string)
            _TEST('empty string',(replaceAll('','a','b') == ''))
            
            ! Test: neutral for strings without "from" (returns a copy of string)
            _TEST('string with no from',(replaceAll('kbx zwd','a','b') == 'kbx zwd'))
            
            ! Test: single-element string that contains only "from"
            _TEST('single char string with one inst. of from',(replaceAll('a','a','b') == 'b'))
            
            ! Test:
            _TEST('string with two inst. of from',(replaceAll('abx awd','a','b') == 'bbx bwd'))
            
            ! Test:
            _TEST('string with three inst. of from',(replaceAll('abx awd gwda','a','b') == 'bbx bwd gwdb'))
            
            test_replaceAll = .true.
      !
      end function
      
      logical function test_isPresent()
      implicit none
      !
      character(len=0)  :: empty_string
      character(len=10),dimension(3),parameter      :: list = [character(len=10) :: 'one', 'two', 'three']
      character(len=10),dimension(-1:1),parameter   :: list_nonstandard_shape = [character(len=10) :: 'one', 'two', 'three']
      character(len=10),dimension(0) :: empty_list
      character(len=10),parameter :: val_two_10 = 'two'
      character(len=3),parameter :: val_two_3 = 'two'
      integer :: index
      logical :: is_present
            test_isPresent = .false.
            _TEST('empty string not in empty list', .not.isPresent(empty_string,empty_list))
            _TEST('empty string not in list', .not.isPresent(empty_string,list))
            _TEST('empty string not in shaped list', .not.isPresent(empty_string,list_nonstandard_shape))
            !
            _TEST('value (exact size) not in empty list', .not.isPresent(val_two_10,empty_list))
            _TEST('value (smaller size) not in empty list', .not.isPresent(val_two_3,empty_list))
            !
            _TEST('value (exact size) in list', isPresent(val_two_10,list))
            _TEST('value (smaller size) in list', isPresent(val_two_3,list))
            _TEST('value (exact size) in shaped list', isPresent(val_two_10,list_nonstandard_shape))
            _TEST('value (smaller size) in shaped list', isPresent(val_two_3,list_nonstandard_shape))
            !
            index = -100
            is_present = isPresent(val_two_10,list,index)
            _TEST('value and index (exact size) in list', (is_present .and. (index == 2)))
            index = -100
            is_present = isPresent(val_two_3,list,index)
            _TEST('value and index (smaller size) in list', (is_present .and. (index == 2)))
            ! Note: extent of the array is NOT passed to isPresent 
            index = -100
            is_present = isPresent(val_two_10,list_nonstandard_shape,index)
            _TEST('value and index (exact size) in shaped list', (is_present .and. (index == 2)))
            index = -100
            is_present = isPresent(val_two_3,list_nonstandard_shape,index)
            _TEST('value and index (smaller size) in shaped list', (is_present .and. (index == 2)))

             
      !
      end function
      
end module