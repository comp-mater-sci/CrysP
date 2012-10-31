

#define _TEST(name,boolval) if (boolval) then; write(*,'(A)') 'Test "'//name//'": pass'; else; write(*,'(A,1X,I0)') 'Test"'//name//'" failed, line:',__LINE__; stop; endif;


module rangetest
use fngRange
implicit none
      
      
contains
      
      subroutine test_empty()
      implicit none
      
      type(rangeDouble) :: rd
      double precision  :: val
      integer :: cnt

      ! Test: empty range [2:5:-1]
      rd = rangeDouble(2.D0, 5.D0,-1.D0)
      _TEST('empty range 1', (rd%next(val) == .false.))
      
      ! Test: empty range [-2:-5:1]
      rd = rangeDouble(-2.D0, -5.D0, 1.D0)
      _TEST('empty range 2', (rd%next(val) == .false.))            
            
      ! Test: empty range [0:0:1)
      rd = rangeDouble(0.D0, 0.D0, 1.D0, endpoint=.false.)
      _TEST('empty range 3', (rd%next(val) == .false.))            
      
      ! Test: empty range [0:0)
      rd = rangeDouble(0.D0,0.D0, endpoint=.false.)
      _TEST('empty range, from defaults', (rd%next(val) == .false.))            

      
      end subroutine
      

      
      subroutine test_ranges()
      implicit none
      type(rangeDouble) :: rd
      !      
            ! Test: range [0:0]
            rd = rangeDouble(0.D0, 0.D0, 1.D0) 
            call test_range('range=0',rd,[0.D0])
            
            ! Test: range [0:0], from defaults
            rd = rangeDouble(0.D0,0.D0) 
            call test_range('range=0, from defaults',rd,[0.D0])
                  
            ! Test: positive values, endpoint included, range [2:5]
            rd = rangeDouble(2.D0, 5.D0, 1.D0) 
            call test_range('range=3+1',rd,[2.D0, 3.D0, 4.D0, 5.D0])
      
            ! Test: negative values, endpoint included, range [-2:-5]
            rd = rangeDouble(-2.D0, -5.D0, -1.D0)
            call test_range('range=3+1,negative',rd,[-2.D0,-3.D0,-4.D0, -5.D0])

            ! Test: range [0:6)
            rd = rangeDouble(0.D0, 6.D0, 1.D0, endpoint=.false.)
            call test_range('range=5,positive',rd,[0.D0,1.D0,2.D0,3.D0,4.D0,5.D0])
            
            ! Test: range [-1:6)
            rd = rangeDouble(-1.D0, 6.D0, 1.D0, endpoint=.false.)
            call test_range('range=5,from negative',rd,[-1.D0,0.D0,1.D0,2.D0,3.D0,4.D0,5.D0])

            
            ! Test: range [1:3], 5 intervals (4 points inside the range + the endpoint)
            rd = rangeDouble(1.D0, 3.D0, npoints=4, endpoint=.true.)
            call test_range('range=5,npoints=4+1',rd,[1.D0,1.5D0,2.D0,2.5D0,3.D0])
            
      !
      end subroutine
      
      
      subroutine test_range(testname,rd,arr_v)
      implicit none
      character(len=*),intent(in)  :: testname
      type(rangeDouble),intent(inout) :: rd
      double precision,dimension(:),intent(in)  :: arr_v 
      !
      double precision,dimension(:),allocatable :: arr
      integer :: imax
      !
            imax = size(arr_v)
            allocate(arr,mold=arr_v)
            arr = 0.D0
            !
            _TEST(testname, niters(rd,arr) == imax)
            _TEST(testname//',values', all(arr == arr_v))
      ! 
      end subroutine

      
      integer function niters(rd,arr)
      implicit none
      type(rangeDouble) :: rd
      double precision  :: arr(:)
      double precision  :: val
      !
            niters = 0
            do while (rd%next(val))
                  niters = niters + 1
                  arr(niters)= val 
                  if (niters >= size(arr)) exit
            enddo
            !
            if (next(rd,val)) niters = niters + 1 
      !
      end function

end module
      
program rangetest_main
use rangetest
implicit none
      call test_empty()
      call test_ranges()
end program