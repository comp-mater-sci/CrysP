module rangetest
use range
implicit none
      
      
contains
      
      subroutine test_empty()
      implicit none
      
      type(rangeDouble) :: rd
      double precision  :: val
      integer :: cnt
      
      rd = rangeDouble(0.D0, 5.D0,-1.D0)

      call noloop()
      
      rd = rangeDouble(-1.D0, -5.D0, 1.D0)
      call noloop()
      
      contains 
            subroutine noloop()
                  cnt = 0
                  do while (next(rd,val))
                        cnt = 1
                        exit
                  enddo
                  if (cnt > 0) then
                        write(*,*) 'Test failed'
                        stop
                  endif
            end subroutine
            
      end subroutine
      
      subroutine test_forward()
      implicit none

      type(rangeDouble) :: rd
      double precision  :: val
      integer :: cnt
      
      rd = rangeDouble(0.D0, 5.D0, 1.D0)

      cnt = 0
      do while (next(rd,val))
            write(*,*) cnt, val
      enddo

      
      end subroutine
      
      
end module
      
program rangetest_main
use rangetest
implicit none
      call test_empty()
      call test_forward()
end program