!
! $Id$
!
!> Support for backward-compatibile CUB format.
!>
!> \note The content of CUB file is no longer one-to-one mappable to CUR, since the CUR format has changed.

module cubAccess
use DYNFIL
implicit none

contains


      ! Write the current contents of the dynfil
      subroutine CUBwriteBlock(nunit,info)
      implicit none
      integer,intent(in)      :: nunit !< IO unit number
      integer,intent(out)     :: info !< exit code: 0 on success
      !      
      integer :: npoint, i,ii,jj
      double precision,parameter :: convf = 180.D0 / acos(-1.D0)
      double precision,dimension(3) :: GLR
      
      !
            npoint = size(DFIL)
            GLR=mf%GEULR*convf
            write (nunit) NRSTEP,npoint,mf%FALG,mf%GAXES,GLR
            !
            do i=1,npoint
                  write(nunit,iostat=info) DFIL(i)%tGEW,                &
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

      
      
      subroutine CUBreadTitle(inp,title,info)
      implicit none
      integer,intent(in)      :: inp
      character(len=*)        :: title
      integer,intent(out)     :: info
      !
      integer :: tmp
      !
            tmp = inp
            title = ''
            filetitle = ''
            info = 0
      !
      end subroutine

      
      subroutine CUBreadBlock(inp,mpoint,info)
      use dynfil
      implicit none
      integer,intent(in)      :: inp      !< IO unit
      integer,intent(in)      :: mpoint   !< Maximal number of points in a block
      integer,intent(out)     :: info     !< Exit code
      !
      integer :: npoint, i, ii,jj
      double precision,dimension(3,3) :: TA, A
      double precision,parameter :: convf = acos(-1.D0) / 180.D0
      !      

      read(inp,iostat=info) NRSTEP,npoint,mf%FALG,mf%GAXES,mf%GEULR
      if (info /= 0) return
      mf%GEULR = mf%GEULR * convf
      call TMATRIX(mf%TAX0,mf%GEULR(1),mf%GEULR(2),mf%GEULR(3))  ! Check it!!!
      call Transf(mf%GAXES,mf%CIJ0,mf%TAX0)  ! Check it!!!
      
      ! Request allocation of the memory
      call DYNFIL0(npoint,mpoint,.false.,info)
      ! Process the crystals in the block      
      do i=1,npoint
            read(inp,iostat=info)   DFIL(i)%tGEW,      &
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
            call orientMat(DFIL(i)%tfi1,DFIL(i)%tPHI,DFIL(i)%tfi2,TA,A)
            CALL MATPROD(DFIL(i)%tT,TA,A,3,3,3)
            !
            ! Backward compatibility with type(grain):
            ! initialize the remaining components with mf data
                  
            DFIL(i)%tAXES = mf%GAXES      ! <- overwrite!
            DFIL(i)%tEULR = mf%GEULR      ! <- overwrite!
            DFIL(i)%tF   = mf%FALG        ! <- overwrite!
            DFIL(i)%tCIJ = mf%CIJ0
            DFIL(i)%tTAX = mf%TAX0
            DFIL(i)%tZERO = 0.D0
            DFIL(i)%tRHO  = 0.D0

      enddo  
      
      !
      end subroutine

      
end module