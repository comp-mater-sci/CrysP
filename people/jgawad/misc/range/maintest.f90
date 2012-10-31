
#define _TEST_STOP stop
! #define _TEST_STOP ;      
#define _TEST(name,boolval) if (boolval) then; write(*,'(A)') 'Test "'//name//'": pass'; else; write(*,'(A,1X,I0)') 'Test "'//name//'" failed, line:',__LINE__; _TEST_STOP; endif;


module fngRangeTest
use fngRange
implicit none
private      

      public test_empty, test_ranges

contains
      
      
      subroutine test_empty()
      implicit none
      type(rangeDouble) :: rd
      double precision  :: val
      !
            ! Test: empty range [2:5:-1]
            rd = rangeDouble(2.D0, 5.D0,-1.D0)
            _TEST('size of empty range [2:5:-1]', (rd%size() == 0))
            _TEST('empty range [2:5:-1]', .not. rd%next(val))
            _TEST('size of empty range [2:5:-1],post', (rd%size() == 0))
      
            ! Test: empty range [-2:-5:1]
            rd = rangeDouble(-2.D0, -5.D0, 1.D0)
            _TEST('size of empty range [-2:-5:1]', (rd%size() == 0))
            _TEST('empty range [-2:-5:1]', .not. rd%next(val))            

            ! Test: empty range [-2:-5:1),
            rd = rangeDouble(-2.D0, -5.D0, 1.D0, endpoint=.false.)
            _TEST('empty range [-2:-5:1)', .not. rd%next(val))            
      
            ! Test: empty range [0:0:1)
            rd = rangeDouble(0.D0, 0.D0, 1.D0, endpoint=.false.)
            _TEST('empty range [0:0:1)', .not. rd%next(val))            
      
            ! Test: empty range [0:0)
            rd = rangeDouble(0.D0,0.D0, endpoint=.false.)
            _TEST('empty range [0:0), from defaults', .not. rd%next(val))            

            ! Test: empty range [1:1), 5 points inside the interval
            rd = rangeDouble(1.D0,1.D0, endpoint=.false., npoints=5)
            _TEST('size of empty range [1:1), 5 points', (rd%size() == 0))
            _TEST('empty range [1:1), 5 points', .not. rd%next(val))            
            _TEST('size of empty range [1:1), 5 points, post', (rd%size() == 0))
      !      
      end subroutine
      

      
      subroutine test_ranges()
      implicit none
      type(rangeDouble) :: rd
      !      
            ! Test: range [0:0:1]
            rd = rangeDouble(0.D0, 0.D0, 1.D0) 
            _TEST('size of range [0:0:1]', (rd%size() == 1))
            call test_range('range [0:0:1]',rd,[0.D0])
            _TEST('size of range [0:0:1],post', (rd%size() == 0))

            ! Test: range [0:0:1]
            rd = rangeDouble(0.D0, 0.D0, -1.D0) 
            _TEST('size of range [0:0:-1]', (rd%size() == 1))
            call test_range('range [0:0:-1]',rd,[0.D0])
            _TEST('size of range [0:0:-1],post', (rd%size() == 0))
            
            ! Test: range [0:0], from defaults
            rd = rangeDouble(0.D0,0.D0) 
            call test_range('range [0:0], from defaults',rd,[0.D0])
                  
            ! Test: positive values, endpoint included, range [2:5]
            rd = rangeDouble(2.D0, 5.D0, 1.D0) 
            call test_range('range [2:5] (3+1)',rd,[2.D0, 3.D0, 4.D0, 5.D0])
            
            ! Test: positive values, endpoint included, range [2:5)
            rd = rangeDouble(2.D0, 5.D0, 1.D0,  endpoint=.false.) 
            _TEST('size of range [2:5)', (rd%size() == 3))
            call test_range('range [2:5) (3)',rd,[2.D0, 3.D0, 4.D0])
            _TEST('size of range [2:5),post', (rd%size() == 0))
            
            ! Test: positive values, endpoint included, range [5:2:-1]
            rd = rangeDouble(5.D0, 2.D0, -1.D0)
            call test_range('range [5:2:-1] (3+1)',rd,[5.D0, 4.D0, 3.D0, 2.D0])
      
            ! Test: negative values, endpoint included, range [-2:-5]
            rd = rangeDouble(-2.D0, -5.D0, -1.D0)
            _TEST('size of range [-2:-5]', (rd%size() == 4))
            call test_range('range [-2:-5] (3+1)',rd,[-2.D0,-3.D0,-4.D0, -5.D0])
            _TEST('size of range [-2:-5],post', (rd%size() == 0))
            
            ! Test: range [0:6)
            rd = rangeDouble(0.D0, 6.D0, 1.D0, endpoint=.false.)
            call test_range('range [0:6) (6)',rd,[0.D0,1.D0,2.D0,3.D0,4.D0,5.D0])
            
            ! Test: range [-1:6)
            rd = rangeDouble(-1.D0, 6.D0, 1.D0, endpoint=.false.)
            _TEST('size of range [-1:6)', (rd%size() == 7))
            call test_range('range [-1:6) (5)',rd,[-1.D0,0.D0,1.D0,2.D0,3.D0,4.D0,5.D0])
            _TEST('size of range [-1:6),post', (rd%size() == 0))
            
            ! Test: range [1:3], 5 intervals (4 points inside the range + the endpoint)
            rd = rangeDouble(1.D0, 3.D0, npoints=4, endpoint=.true.)
            call test_range('range [1:3],npoints=(4+1)',rd,[1.D0,1.5D0,2.D0,2.5D0,3.D0])
            
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
            val = 0.D0
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
use fngRangeTest
implicit none
      call test_empty()
      call test_ranges()
end program