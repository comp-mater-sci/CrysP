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
!>    \date Date of first release: 2012-10-31
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criTestRange.f90 
!
!
#include "criStdDefs.fpp"
#include "criTest.fpp"
!
module criTestRange
use criRange
use criTest
implicit none
private      

      character(len=8),parameter :: sizelabel = 'size of '
      character(len=5),parameter :: postlabel = ',post'

      
      ! test1_arr :  [0:10],q=2,   4 points and the endpoint
      ! test2_arr :  [0:10],q=1/2, 4 points and the endpoint
      double precision,dimension(5),parameter :: test1_arr = [ 0.D0, 3.42036912822497D0, 6.13511790435691D0, 8.28981543588752D0, 10.D0], &
                                                 test2_arr = [ 0.D0, 1.71018456411249D0, 3.86488209564309D0, 6.57963087177503D0, 10.D0 ]


public :: criTestRange_main

contains
      
      !> main driver routine of criTestPath      
      logical function criTestRange_main() result(stat)
      implicit none
      !
            stat = .true.
            stat = testRange_empty() .and.  testRange_ranges()
            
            stat = stat .and. testBiasedRange_empty() .and. testBiasedRange_ranges()
            
            stat = stat .and. testMultiBiasedRange_empty() .and. testMultiBiasedRange_ranges()
            
            stat = stat .and. testDiscreteRange()
      !
      end function
      
      
      
      logical function testRange_empty()
      implicit none
      type(uniformRange) :: rd
      double precision  :: val
      !
      character(len=128) :: testname
      !
            testRange_empty = .false.
            ! Test: empty range [2:5:-1]
            rd = uniformRange(2.D0, 5.D0,-1.D0)
            _TEST('size of empty range [2:5:-1]', (rd%size() == 0))
            _TEST('empty range [2:5:-1]', .not. rd%next(val))
            _TEST('size of empty range [2:5:-1],post', (rd%size() == 0))
      
            ! Test: empty range [-2:-5:1]
            rd = uniformRange(-2.D0, -5.D0, 1.D0)
            _TEST('size of empty range [-2:-5:1]', (rd%size() == 0))
            _TEST('empty range [-2:-5:1]', .not. rd%next(val))            

            ! Test: empty range [-2:-5:1),
            rd = uniformRange(-2.D0, -5.D0, 1.D0, endpoint=.false.)
            _TEST('empty range [-2:-5:1)', .not. rd%next(val))            
      
            ! Test: empty range (0:0:0)
            testname = 'empty uniformRange (0:0:0)'
            rd = uniformRange(0.D0, 0.D0, 0.D0, endpoint=.false.)
            _TEST(sizelabel//testname, (rd%size() == 0))
            _TEST(testname, .not. rd%next(val)) 
            
            ! Test: empty range (0:0:1)
            ! Note: the range is empty because
            ! left end == right end, and right end shall not be included.
            rd = uniformRange(0.D0, 0.D0, 1.D0, endpoint=.false.)
            _TEST('empty range (0:0:1)', .not. rd%next(val))            
      
            ! Test: empty range (0:0)
            rd = uniformRange(0.D0,0.D0, endpoint=.false.)
            _TEST('empty range (0:0), from defaults', .not. rd%next(val))            

            ! Test: empty range (1:1), 5 points inside the interval
            rd = uniformRange(1.D0,1.D0, endpoint=.false., npoints=5)
            _TEST('size of empty range (1:1), 5 points', (rd%size() == 0))
            _TEST('empty range (1:1), 5 points', .not. rd%next(val))            
            _TEST('size of empty range (1:1), 5 points, post', (rd%size() == 0))
            
            testRange_empty = .true.
      !      
      end function
      

      
      logical function testRange_ranges() result(test)
      implicit none
      type(uniformRange) :: rd
      !
      character(len=128) :: testname
      !
            test = .false.
            ! Test: range [0:0:1]
            rd = uniformRange(0.D0, 0.D0, 1.D0) 
            _TEST('size of range [0:0:1]', (rd%size() == 1))
            test = test_range('range [0:0:1]',rd,[0.D0])
            _TEST('size of range [0:0:1],post', (rd%size() == 0))

            ! Test: range [0:0:0)
            testname = 'uniformRange [0:0:0)'
            rd = uniformRange(0.D0, 0.D0, 0.D0) 
            _TEST(sizelabel//testname, (rd%size() == 1))
            test = test_range('range [0:0:1]',rd,[0.D0])
            _TEST(trim(sizelabel//testname)//postlabel, (rd%size() == 0))
            
            ! Test: range [0:0]
            rd = uniformRange(0.D0, 0.D0, npoints=2) 
            _TEST('size of range [0:0] npoints=2 (0+1)', (rd%size() == 1))
            test = test_range('range [0:0] npoints=2 (0+1)',rd,[0.D0])
            _TEST('size of range [0:0] npoints=2 (0+1),post', (rd%size() == 0))
            

            ! Test: range [0:0:1]
            rd = uniformRange(0.D0, 0.D0, -1.D0) 
            _TEST('size of range [0:0:-1]', (rd%size() == 1))
            test = test_range('range [0:0:-1]',rd,[0.D0])
            _TEST('size of range [0:0:-1],post', (rd%size() == 0))
            
            ! Test: range [0:0], from defaults
            rd = uniformRange(0.D0,0.D0) 
            test = test_range('range [0:0], from defaults',rd,[0.D0])
                  
            ! Test: positive values, endpoint included, range [2:5]
            rd = uniformRange(2.D0, 5.D0, 1.D0) 
            test = test_range('range [2:5] (3+1)',rd,[2.D0, 3.D0, 4.D0, 5.D0])
            
            ! Test: positive values, endpoint included, range [2:5)
            rd = uniformRange(2.D0, 5.D0, 1.D0,  endpoint=.false.) 
            _TEST('size of range [2:5)', (rd%size() == 3))
            test = test_range('range [2:5) (3)',rd,[2.D0, 3.D0, 4.D0])
            _TEST('size of range [2:5),post', (rd%size() == 0))
            
            ! Test: positive values, endpoint included, range [5:2:-1]
            rd = uniformRange(5.D0, 2.D0, -1.D0)
            test = test_range('range [5:2:-1] (3+1)',rd,[5.D0, 4.D0, 3.D0, 2.D0])
      
            ! Test: negative values, endpoint included, range [-2:-5]
            rd = uniformRange(-2.D0, -5.D0, -1.D0)
            _TEST('size of range [-2:-5]', (rd%size() == 4))
            test = test_range('range [-2:-5] (3+1)',rd,[-2.D0,-3.D0,-4.D0, -5.D0])
            _TEST('size of range [-2:-5],post', (rd%size() == 0))
            
            ! Test: range [0:6)
            rd = uniformRange(0.D0, 6.D0, 1.D0, endpoint=.false.)
            test = test_range('range [0:6) (6)',rd,[0.D0,1.D0,2.D0,3.D0,4.D0,5.D0])
            
            ! Test: range [-1:6)
            rd = uniformRange(-1.D0, 6.D0, 1.D0, endpoint=.false.)
            _TEST('size of range [-1:6)', (rd%size() == 7))
            test = test_range('range [-1:6) (5)',rd,[-1.D0,0.D0,1.D0,2.D0,3.D0,4.D0,5.D0])
            _TEST('size of range [-1:6),post', (rd%size() == 0))
            
            ! Test: range [1:3], 5 intervals (4 points inside the range + the endpoint)
            rd = uniformRange(1.D0, 3.D0, npoints=4, endpoint=.true.)
            test = test_range('range [1:3],npoints=(4+1)',rd,[1.D0,1.5D0,2.D0,2.5D0,3.D0])
            
            test = .true.
      !
      end function
      
      
      logical function test_range(testname,rd,arr_v) result(test)
      implicit none
      character(len=*),intent(in)  :: testname
      class(range_type),intent(inout) :: rd
      double precision,dimension(:),intent(in)  :: arr_v 
      !
      double precision,dimension(:),allocatable :: arr
      integer :: imax
      !
            test = .false.
            
            imax = size(arr_v)
            allocate(arr,mold=arr_v)
            arr = 0.D0
            !
            _TEST(testname, niters(rd,arr) == imax)
            _TEST(trim(testname)//',values', all(arr == arr_v))
            
            test = .true.
      ! 
      end function

      
      integer function niters(rd,arr)
      implicit none
      class(range_type) :: rd
      double precision  :: arr(:)
      double precision  :: val
      !
            niters = 0
            val = 0.D0
            do while (rd%next(val))
                  niters = niters + 1
                  arr(niters)= val 
                  if (niters >= size(arr)) exit
            enddo
            !
            if (rd%next(val)) niters = niters + 1 
      !
      end function

      
      logical function testBiasedRange_empty()
      implicit none
      type(biasedRange) :: rd
      double precision :: val
      character(len=128) :: testname
      
      !
            testBiasedRange_empty = .false.
            
            ! Test:
            testname = 'empty biasedRange (0:0) (0)'
            rd = biasedRange(0.D0, 0.D0, 1.D0, 0, endpoint=.false.)
            _TEST(sizelabel//testname, (rd%size() == 0))
            _TEST(testname, .not. rd%next(val))

            ! Test:
            testname = 'empty range (0:0),q=2 (0)'
            rd = biasedRange(0.D0, 0.D0, 2.D0, 0, endpoint=.false.)
            _TEST(sizelabel//testname, (rd%size() == 0))
            _TEST(testname, .not. rd%next(val))
            
            ! Test:
            testname = 'empty range (0:0),q=2,npoints=4 (0)'
            rd = biasedRange(0.D0, 0.D0, 2.D0, 4, endpoint=.false.)
            _TEST(sizelabel//testname, (rd%size() == 0))
            _TEST(testname, .not. rd%next(val))

            
            testBiasedRange_empty = .true.
      !
      end function
      
      logical function testBiasedRange_ranges() result(test)
      implicit none
      type(biasedRange) :: rd
      character(len=128) :: testname
      !
            test = .false.

            ! Test:
            testname = 'biasedRange [0:0],q=2 (0+1)'
            rd = biasedRange(0.D0, 0.D0, 2.D0, 0, endpoint=.true.)
            _TEST(sizelabel//testname, (rd%size() == 1))
            test = test_range(testname,rd, [0.D0])
            
            ! Test
            testname = 'biasedRange [1:1],q=2, (0+1)'
            rd = biasedRange(1.D0, 1.D0, 2.D0, 0)
            _TEST(sizelabel//testname, (rd%size() == 1))
            test = test_range(testname,rd, [1.D0])

            ! Test:
            testname = 'biasedRange [0:0],q=2, (0+1)'
            rd = biasedRange(0.D0, 0.D0, 2.D0, 1)
            _TEST(sizelabel//testname, (rd%size() == 1))
            test = test_range(testname,rd, [0.D0])

            
            ! Test: range [0:0]
            testname = 'biasedRange [0:0],q=1 npoints=2 (0+1)'
            rd = biasedRange(0.D0, 0.D0, 1.D0, npoints=2) 
            _TEST(sizelabel//testname, (rd%size() == 1))
            test = test_range(testname,rd,[0.D0])
            _TEST('size of range [0:0],q=1 npoints=2 (0+1),post', (rd%size() == 0))
  
            ! Test: range [0:0]
            testname = 'biasedRange [0:0],q=2 npoints=2 (0+1)'
            rd = biasedRange(0.D0, 0.D0, 2.D0, npoints=2) 
            _TEST(sizelabel//testname, (rd%size() == 1))
            test = test_range(testname,rd,[0.D0])
            _TEST(trim(sizelabel//testname)//postlabel, (rd%size() == 0))

            ! Test
            testname = 'biasedRange [0:1],q=2, (1+0)'
            rd = biasedRange(0.D0,1.D0,2.D0,1,endpoint=.false.)
            _TEST(sizelabel//testname, (rd%size() == 1))
            test = test_range(testname,rd,[0.D0])
            _TEST(trim(sizelabel//testname)//postlabel, (rd%size() == 0))
            
            ! Test
            testname = 'biasedRange [0:1],q=2, (1+1)'
            rd = biasedRange(0.D0,1.D0,2.D0,1)
            _TEST(sizelabel//testname, (rd%size() == 2))
            test = test_range(testname,rd,[0.D0, 1.D0])
            _TEST(trim(sizelabel//testname)//postlabel, (rd%size() == 0))

            
            ! Test:
            testname = 'biasedRange [0:10],q=2 npoints=4 (4)'
            rd = biasedRange(0.D0, 10.D0, 2.D0, 4, endpoint=.false.)
             _TEST(sizelabel//testname, (rd%size() == 4))
            test = test_approx(testname,rd,test1_arr(1:4), 1.D-10)
            _TEST(trim(sizelabel//testname)//postlabel, (rd%size() == 0))

            ! Test:
            testname = 'biasedRange [0:10],q=2, (4)'
            rd = biasedRange(0.D0, -10.D0, 2.D0, 4, endpoint=.false.)
            test = test_approx(testname,rd,-test1_arr(1:4), 1.D-10)

            ! Test:
            testname = 'biasedRange [0:10],q=2, (4+1)'
            rd = biasedRange(0.D0, 10.D0, 2.D0, 4)
            test = test_approx(testname,rd,test1_arr(1:5), 1.D-10, 2.D0)
            _TEST(trim(sizelabel//testname)//postlabel, (rd%size() == 0))

            ! Test:
            testname = 'biasedRange [0:10],q=0.5, (4+1)'
            rd = biasedRange(0.D0, 10.D0, 0.5D0, 4)
            test = test_approx(testname,rd,test2_arr(1:5), 1.D-10, 0.5D0)
            _TEST(trim(sizelabel//testname)//postlabel, (rd%size() == 0))

            ! Test: range [10:0], q=1/2. Should be like reversed [0:10],q=2
            testname = 'biasedRange [10:0],q=0.5, (4+1)'
            rd = biasedRange(10.D0, 0.D0, 0.5D0, 4)
            test = test_approx(testname,rd,test1_arr(5:1:-1), 1.D-10, 0.5D0)
            _TEST(trim(sizelabel//testname)//postlabel, (rd%size() == 0))


            testname = 'biasedRange [2:5],q=1,uniform (3)'
            rd = biasedRange(2.D0, 5.D0, 1.D0, 3, endpoint=.false.)
            test = test_approx(testname,rd,[2.D0, 3.D0, 4.D0], 1.D-10)
            _TEST(trim(sizelabel//testname)//postlabel, (rd%size() == 0))

            
            testname = 'biasedRange [0:1],q=4 (3+1)'
            ! rd = biasedRange(0.D0,1.D0,(1.D0/3.D0),3,endpoint=.true.) !! Weird compiler crash!
            rd = biasedRange(0.D0,1.D0,4.D0,3,endpoint=.true.) 
            test = test_approx(testname,rd,[0.D0, 4.D0/7.D0, 6.D0/7.D0, 1.D0], 1.D-10, 4.D0)
            _TEST(trim(sizelabel//testname)//postlabel, (rd%size() == 0))

            
            test = .true.
      !
      end function

      
      logical function test_approx(testname,rd,arr_v,tol,q)
      implicit none
      character(len=*),intent(in)  :: testname
      class(range_type),intent(inout) :: rd
      double precision,dimension(:),intent(in)  :: arr_v 
      double precision,intent(in)               :: tol
      double precision,intent(in),optional      :: q
      !
      double precision,dimension(:),allocatable :: arr
      integer :: imax
      double precision :: tmp
      !
            test_approx = .false.
            imax = size(arr_v)
            allocate(arr,mold=arr_v)
            arr = 0.D0
            !
            _TEST(testname, niters(rd,arr) == imax)
            tmp = norm2(arr - arr_v)
            _TEST(trim(testname)//',values', tmp < tol)
            if ((imax > 1) .and. present(q)) then
                  tmp = (arr(2)-arr(1)) / (arr(imax) - arr(imax-1))
                  _TEST(trim(testname)//',ratio', abs(tmp - q) < tol)
            endif
            test_approx = .true.
      ! 
      end function
      
      
      logical function testMultiBiasedRange_empty() result(test)
      implicit none
      type(multiBiasedRange) :: rd
      double precision :: val
      character(len=128) :: testname

      type(bias_t),dimension(3),parameter :: biases_zero =  [ bias_t(0.D0,0.D0,0), bias_t(0.D0,1.D0,0), bias_t(0.D0,1.0,1) ]
      
            test = .false.
            ! Test: multiBiased range that consists of one empty biased range
            testname = 'single-slot empty multiBiasedRange (0:0) (0)'
            rd = multiBiasedRange(0.D0, biases_zero(:1), endpoint=.false.)
            _TEST(sizelabel//testname, (rd%size() == 0))
            _TEST(testname, .not. rd%next(val))

            ! Test: multiBiased range that consists of three empty biased ranges
            testname = 'triple-slot empty multiBiasedRange (0:0) (0)'
            rd = multiBiasedRange(0.D0, biases_zero, endpoint=.false.)
            _TEST(sizelabel//testname, (rd%size() == 0))
            _TEST(testname, .not. rd%next(val))
            
            test = .true.
      end function

      
      logical function testMultiBiasedRange_ranges() result(test)
      implicit none
      type(multiBiasedRange) :: rd
      double precision :: val
      character(len=128) :: testname
      integer :: i
      ! These biases actually represent uniformly spaced range 
      type(bias_t),dimension(3),parameter :: biases_uniform =  [ bias_t(3.D0,1.D0,3), bias_t(6.D0,1.D0,3), bias_t(10.D0,1.0,4) ]
      double precision,dimension(11),parameter :: values_uniform = [(dble(i), i=0,10)]
      !
      type(bias_t),dimension(2),parameter :: biases_dual = [ bias_t(10.D0,2.D0,4), bias_t(0.D0,0.5D0,4) ]
      double precision,dimension(9),parameter :: values_dual = [ test1_arr(1:5), test1_arr(4:1:-1) ]
      !
      type(bias_t),dimension(2),parameter :: biases_doublebias = [ bias_t(10.D0,2.D0,4), bias_t(20.D0,0.5D0,4) ]
      double precision,dimension(9)      :: values_doublebias
      type(biasedRange) :: tmp ! helper for constructing values_doublebias
      !
            test = .false.
            
            ! Test:
            testname = 'sigle-slot multiBiasedRange [0:3],q=1 (3+1)'
            rd = multiBiasedRange(0.D0, biases_uniform(:1))
            _TEST(sizelabel//testname, (rd%size() == 4))
            test = test_range(testname,rd, values_uniform(:4))

            ! Test:
            testname = 'sigle-slot multiBiasedRange [0:3),q=1 (3+0)'
            rd = multiBiasedRange(0.D0, biases_uniform(:1), endpoint=.false.)
            _TEST(sizelabel//testname, (rd%size() == 3))
            test = test_range(testname,rd, values_uniform(:3))

            ! Test:
            testname = 'triple-slot multiBiasedRange [0:10],q=1 (10+1)'
            rd = multiBiasedRange(0.D0, biases_uniform(:3), endpoint=.true.)
            _TEST(sizelabel//testname, (rd%size() == 11))
            test = test_range(testname,rd, values_uniform(:11))

            ! Test:
            testname = 'triple-slot multiBiasedRange [0:10),q=1 (10+0)'
            rd = multiBiasedRange(0.D0, biases_uniform(:3), endpoint=.false.)
            _TEST(sizelabel//testname, (rd%size() == 10))
            test = test_range(testname,rd, values_uniform(:10))

            ! Test: this test actually abuses the multiBiasedRange because
            ! the actual bounds of the entire range are [0,0]. However,
            ! the multiple ranges can be glued together even if they "grow" in
            ! different directions. It is difficult to judge if this is a bug
            ! of a feature...
            testname = 'dual-slot multiBiasedRange [0:10)q=2,[10,0]q=1/2 (8+1)'
            rd = multiBiasedRange(0.D0, biases_dual, endpoint=.true.)
            _TEST(sizelabel//testname, (rd%size() == 9))
            test = test_approx(testname,rd, values_dual,1D-10)

            
            ! Little preparation is needed:
            values_doublebias(:4) = test1_arr(:4) ! For [0:10],q=2, but without the endpoint
            ! For [10:20],q=1/2, with endpoint
            tmp = biasedRange(10.D0, 20.D0, 0.5D0, 4)
            i = 4
            ! Make sure the execution is safe:
            if ((i + tmp%size()) <= size(values_doublebias)) then
                  ! construct the second part of the array
                  do while (tmp%next(val))
                        i = i + 1   
                        if (i > size(values_doublebias)) then
                              write(*,*) 'ERROR in test procedure'
                              stop
                        endif
                        values_doublebias(i) = val
                     
                  enddo
            endif
            
            ! Test:
            testname = 'dual-slot multiBiasedRange [0:10)q=2,[10,20]q=1/2 (8+1)'
            rd = multiBiasedRange(0.D0, biases_doublebias, endpoint=.true.)
            _TEST(sizelabel//testname, (rd%size() == 9))
            test = test_approx(testname,rd, values_doublebias,1D-10)

            ! Test: doubleBiasedRange should give exactly the same results.
            testname = 'doubleBiasedRange [0:20],q=2.0 (8+1)'
            rd = doubleBiasedRange(0.D0, 20.D0, 2.D0, 8, endpoint=.true.)
            _TEST(sizelabel//testname, (rd%size() == 9))
            test = test_approx(testname,rd, values_doublebias,1D-10)
            
      
            test = .true.
      end function

      logical function testDiscreteRange() result(test)
      implicit none
      
      double precision,dimension(0) :: empty_array
      double precision,dimension(5),parameter :: test_array = [1.D0,1.5D0,2.D0,2.5D0,3.D0]
      double precision :: val
      type(discreteRange) :: rd
      !
            ! Test: uninitialized range
            _TEST('size of uninitialized discrete range', (rd%size() == 0))
            _TEST('uninitialized discrete range', .not. rd%next(val))
            
            ! Test: empty initialized range
            rd = discreteRange(empty_array)
            _TEST('size of empty discrete range', (rd%size() == 0))
            _TEST('empty discrete range', .not. rd%next(val))

            ! Test: one-element discrete range
            rd = discreteRange([0.D0])
            _TEST('size of one-element discrete range', (rd%size() == 1))
            _TEST('one-element discrete range', rd%next(val))
            _TEST('size of one-element discrete range (after next)', (rd%size() == 0))
                        
            ! Test: one-element discrete range
            rd = discreteRange([0.D0])
            _TEST('one-element discrete range: one eval',  rd%next(val))
            _TEST('one-element discrete range: only eval', .not. rd%next(val))
            
            ! Test: one-element discrete range
            rd = discreteRange([0.D0])
            test = test_range('one-element discrete range',rd,[0.D0])
            _TEST('stop at the end of discrete range', .not. rd%next(val))
            
            ! Test: five-element discrete range
            rd = discreteRange(test_array)
            _TEST('size of five-element discrete range', (rd%size() == size(test_array)))
            test = test_range('five-element discrete range',rd,test_array)
            _TEST('stop at the end of discrete range', .not. rd%next(val))
            
            test = .true.
      
      end function
      
      
end module
