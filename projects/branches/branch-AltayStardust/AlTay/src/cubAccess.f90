!
! $Id$
!
!> Support for backward-compatibile CUB format.
!>
!> \note The content of CUB file is no longer one-to-one mappable to CUR, since the CUR format has changed.

module altayCubAccess
use altayDynfil
use altayAlgorithms
implicit none

contains


      ! Write the current contents of the dynfil
      subroutine CUBwriteBlock(iounit,info)
      implicit none
      integer,intent(in)      :: iounit !< IO unit number
      integer,intent(out)     :: info !< exit code: 0 on success
      !      
      integer :: npoint, i,ii,jj
      double precision,parameter :: convf = 180.D0 / acos(-1.D0)
      
      !
            npoint = size(DFIL)
            write (iounit) NRSTEP,npoint,mf%FALG
            !
            do i=1,npoint
                  write(iounit,iostat=info) DFIL(i)%tGEW,                &
                              DFIL(i)%tfi1*convf,                       &
                              DFIL(i)%tPHI*convf,                       &
                              DFIL(i)%tfi2*convf,                       &
                              DFIL(i)%tGAM,                             &
                              ! Remaining components that are not present in CUR anymore:
                              ((DFIL(i)%tF(ii,jj),ii=1,3),jj=1,3)
                  if (info /= 0) exit
            enddo  
      !      
      end subroutine

      
      
      subroutine CUBreadTitle(iounit,title,info)
      implicit none
      integer,intent(in)      :: iounit
      character(len=*)        :: title
      integer,intent(out)     :: info
      !
      integer :: tmp
      !
            tmp = iounit
            title = ''
            filetitle = ''
            info = 0
      !
      end subroutine

      !> Read a CUB file block into DYNFIL::DFIL
      subroutine CUBreadBlock(iounit,info)
      implicit none
      integer,intent(in)      :: iounit      !< IO unit
      integer,intent(out)     :: info     !< Exit code
      !
      integer :: npoint, i, ii,jj
      double precision,parameter :: convf = acos(-1.D0) / 180.D0
      !      
            read(iounit,iostat=info) NRSTEP,npoint,mf%FALG
            if (info /= 0) return
      
            ! Request allocation of the memory
            call DYNFIL0(npoint,.false.,info)
            if (info /= 0) return
            ! Process the crystals in the block      
            do i=1,npoint
                  read(iounit,iostat=info)   DFIL(i)%tGEW,      &
                                          DFIL(i)%tfi1,      &
                                          DFIL(i)%tPHI,      &
                                          DFIL(i)%tfi2,      &
                                          DFIL(i)%tGAM,      &
                                          ! Remaining components that are not present in CUR anymore:
                                          ((DFIL(i)%tF(ii,jj),ii=1,3),jj=1,3)

                  if (info /= 0) exit
                  ! Convert the grain orientatios from degrees to radians
                  DFIL(i)%tfi1 = DFIL(i)%tfi1 * convf
                  DFIL(i)%tPHI = DFIL(i)%tPHI * convf
                  DFIL(i)%tfi2 = DFIL(i)%tfi2 * convf
                  !
                  call initFields(mf,DFIL(i))
            enddo  
      !
      end subroutine

      
end module