module testTexAccess
      
contains 

      subroutine testCURAccess()
      use curAccess
      implicit none
      
      character(len=128)  :: title
      integer :: info
      integer,parameter :: nu = 300
      ! 
      open(unit=nu,file='example.CUR',status='old')
      call CURreadTitle(nu,title,info)
      
      call CURreadBlock(nu,0,8000,info)
      
      rewind(nu)
      call CURreadBlock(nu,1,8000,info)
      
      end subroutine
      
      
end module