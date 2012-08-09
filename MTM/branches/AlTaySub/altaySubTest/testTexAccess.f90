module testTexAccess
      
contains 

      subroutine testCURAccess()
      use curAccess
      use cubAccess
      implicit none
      
      character(len=128)  :: title
      integer :: info
      integer,parameter :: nu = 300, no = 301
      integer,parameter :: maxorient = 8000
      !
            ! Test 1: read 1st block (0 blocks to be skipped)
            open(unit=nu,file='example.CUR',status='old')
            call CURreadTitle(nu,title,info)
            call CURreadBlock(nu,0,maxorient,info)
      
            ! Test 2: write out the CUR
            open(unit=no,file='example_0.CUR',status='replace')
            call CURwriteTitle(no,title,info)
            call CURwriteBlock(no,info)
            close(no)
      
            ! Test 3: write out the CUB
            open(unit=no,file='example_0.CUB',status='replace',form='unformatted')
            ! call CUBwriteTitle(no,title,info)
            call CUBwriteBlock(no,info)
            close(no)
      
            ! Test 4: read the CUB
            open(unit=no,file='example_0.CUB',status='old',form='unformatted')
            call CUBreadTitle(no,title,info)
            call CUBreadBlock(no,maxorient,info)
            close(no)
      
            ! Test 5: write the CUR again.
            open(unit=no,file='example_01.CUR',status='replace')
            call CURwriteTitle(no,title,info)
            call CURwriteBlock(no,info)
            close(no)
      
            !! Interpretation of Test 2 and 5: example_0.CUR and example_01.CUR should be 
            !! identical except for the title line.
            
      
            ! Test 2: read 2nd block (1 blocks to be skipped)
            rewind(nu)
            call CURreadTitle(nu,title,info)
            call CURreadBlock(nu,1,8000,info)
            open(unit=no,file='example_1.CUR',status='replace')
            call CURwriteTitle(no,title,info)
            call CURwriteBlock(no,info)
            close(no)
     
      
      
            close(nu)
      
      end subroutine
      
      
end module