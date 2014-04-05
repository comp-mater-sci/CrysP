!
! $Id$
!
!> Support for backward-compatibile CUB format.
!>
!> \note The content of CUB file is no longer one-to-one mappable to CUR, since the CUR format has changed.

module cubAccess
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
      double precision,dimension(3) :: GLR
      
      !
            npoint = size(DFIL)
            GLR=mf%GEULR*convf
            write (iounit) NRSTEP,npoint,mf%FALG,mf%GAXES,GLR
            !
            do i=1,npoint
                  write(iounit,iostat=info) DFIL(i)%tGEW,                &
                              DFIL(i)%tfi1*convf,                       &
                              DFIL(i)%tPHI*convf,                       &
                              DFIL(i)%tfi2*convf,                       &
                              DFIL(i)%tGAM,                             &
                              ! Remaining components that are not present in CUR anymore:
                              ((DFIL(i)%tF(ii,jj),ii=1,3),jj=1,3),     &
                              (DFIL(i)%tAXES(jj),jj=1,3),              &
                              DFIL(i)%tEULR
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
            read(iounit,iostat=info) NRSTEP,npoint,mf%FALG,mf%GAXES,mf%GEULR
            if (info /= 0) return
            mf%GEULR = mf%GEULR * convf
            call EulRad_2_TMATRIX(mf%TAX0,mf%GEULR(1),mf%GEULR(2),mf%GEULR(3))  ! Check it!!!
            call Transf(mf%GAXES,mf%CIJ0,mf%TAX0)  ! Check it!!!
      
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
                                          ((DFIL(i)%tF(ii,jj),ii=1,3),jj=1,3),     &
                                          (DFIL(i)%tAXES(jj),jj=1,3),              &
                                          DFIL(i)%tEULR

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