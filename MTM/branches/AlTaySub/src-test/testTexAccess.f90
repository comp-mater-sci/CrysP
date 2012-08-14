!
! $Id$
!

#include "assert.fpp"
      
module testTexAccess

      
contains 

      subroutine testTexAccessModules()
      use curAccess
      use cubAccess
      use smtAccess
      implicit none
      
      character(len=128)  :: title
      integer :: info
      integer,parameter :: nu = 300, no = 301
      integer,parameter :: maxorient = 8000
      !
            ! Test 1: read 1st block (0 blocks to be skipped)
            open(unit=nu,file='example.CUR',status='old',iostat=info)
            ASSERT(info == 0)
            call CURreadTitle(nu,title,info)
            ASSERT(info == 0)
            call CURreadBlock(nu,0,maxorient,info)
            ASSERT(info == 0)
      
            ! Test 2: write out the CUR
            open(unit=no,file='example_0.CUR',status='replace',iostat=info)
            ASSERT(info == 0)
            call CURwriteTitle(no,title,info)
            ASSERT(info == 0)
            call CURwriteBlock(no,info)
            ASSERT(info == 0)
            close(no)
      
            ! Test 3: write out the CUB
            open(unit=no,file='example_0.CUB',status='replace',form='unformatted',iostat=info)
            ASSERT(info == 0)
            ! call CUBwriteTitle(no,title,info)
            call CUBwriteBlock(no,info)
            ASSERT(info == 0)
            close(no)
      
            ! Test 4: read the CUB
            open(unit=no,file='example_0.CUB',status='old',form='unformatted',iostat=info)
            ASSERT(info == 0)
            call CUBreadTitle(no,title,info)
            ASSERT(info == 0)
            call CUBreadBlock(no,maxorient,info)
            ASSERT(info == 0)
            close(no)
      
            ! Test 5: write the CUR again.
            open(unit=no,file='example_01.CUR',status='replace',iostat=info)
            ASSERT(info == 0)
            call CURwriteTitle(no,title,info)
            ASSERT(info == 0)
            call CURwriteBlock(no,info)
            ASSERT(info == 0)
            close(no)
      
            ! Test 6: write the CUR again.
            open(unit=no,file='example_0.SMT',status='replace',iostat=info)
            ASSERT(info == 0)
            call SMTwriteHeader(no,title,info)
            ASSERT(info == 0)
            call SMTwriteBlock(no,info)
            ASSERT(info == 0)
            close(no)
            
            !! Interpretation of Test 2 and 5: example_0.CUR and example_01.CUR should be 
            !! identical except for the title line.
            
      
            ! Test 6: read 2nd block (1 blocks to be skipped)
            rewind(nu)
            call CURreadTitle(nu,title,info)
            ASSERT(info == 0)
            call CURreadBlock(nu,1,8000,info)
            ASSERT(info == 0)
            
            open(unit=no,file='example_1.CUR',status='replace',iostat=info)
            ASSERT(info == 0)
            call CURwriteTitle(no,title,info)
            ASSERT(info == 0)
            call CURwriteBlock(no,info)
            ASSERT(info == 0)
            close(no)
     
      
      
            close(nu)
      
      end subroutine
      
      
      subroutine testSMTAccess()
      use curAccess
      use cubAccess
      use smtAccess
      implicit none
      
      character(len=128)  :: title
      integer :: info
      integer,parameter :: nu = 300, no = 301
      integer,parameter :: maxorient = 8000
            
            ! Test 1: read the SMT file
            open(unit=nu,file='A612LM.SMT',status='old',iostat=info)
            ASSERT(info == 0)
            call SMTreadHeader(nu,maxorient,title,info)
            ASSERT(info == 0)
            call SMTreadBlock(nu,maxorient,info)
            ASSERT(info == 0)
            close(nu)
            
            ! Test 2: write out the SMT
            open(unit=no,file='example_0.SMT',status='replace',iostat=info)
            ASSERT(info == 0)
            call SMTwriteHeader(no,title,info)
            ASSERT(info == 0)
            call SMTwriteBlock(no,info)
            ASSERT(info == 0)
            close(no)

            ! Test 3: process a hand-made SMT file with NSTAP
            open(unit=nu,file='handmade.SMT',status='old',iostat=info)
            ASSERT(info == 0)
            call SMTreadHeader(nu,maxorient,title,info)
            ASSERT(info == 0)
            call SMTreadBlock(nu,maxorient,info)
            ASSERT(info == 0)
            close(nu)
            ! Test 3a: write out the SMT            
            open(unit=no,file='handmade_0.SMT',status='replace',iostat=info)
            ASSERT(info == 0)
            call SMTwriteHeader(no,title,info)
            ASSERT(info == 0)
            call SMTwriteBlock(no,info)
            ASSERT(info == 0)
            close(no)

            
            
            
      end subroutine
      
end module