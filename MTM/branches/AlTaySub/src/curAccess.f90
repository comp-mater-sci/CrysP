!
! $Id$
!      
module curAccess
use altayDynfil
use altayAlgorithms

contains

      !> Write title line of the CUR format.
      subroutine CURwriteTitle(iounit,title,info)
      implicit none
      integer,intent(in)            :: iounit  !< IO unit number
      character(len=*),intent(in)   :: title !< Title line
      integer,intent(out)           :: info  !< exit code: 0 on success
      !
            write(iounit,fmt='(A)',iostat=info) title
      !
      end subroutine

      
      ! Write the current contents of the dynfil
      subroutine CURwriteBlock(iounit,info)
      implicit none
      integer,intent(in)      :: iounit !< IO unit number
      integer,intent(out)     :: info !< exit code: 0 on success
      integer :: npoint, i
      !
      double precision,parameter :: convf = 180.D0 / acos(-1.D0)
      double precision,dimension(3) :: GLR
      !
            npoint = size(DFIL)

            GLR=mf%GEULR*convf
            write (iounit,402)
            write (iounit,403) NRSTEP,npoint,mf%FALG,mf%GAXES,GLR
            write (iounit,401)
            !
            do i=1,npoint
                  write(iounit,400,iostat=info) i,DFIL(i)%tGEW,           &
                              DFIL(i)%tfi1*convf,                       &
                              DFIL(i)%tPHI*convf,                       &
                              DFIL(i)%tfi2*convf,                       &
                              DFIL(i)%tGAM
                  if (info /= 0) exit
            enddo  
      !      
 400  format (I6,f10.5,2X,3f10.5,2X,f10.5)                 
 401  format (' CRYSTAL WEIGHT ',5X,'phi1',6X,'PHI',7X,'phi2',6X,'  GAMMA')
 402  format (/,' Def. Step    ','Number of orientations',27X,          &
      2X,'F(1,1)',4X,'F(2,1)',4X,'F(3,1)',4X,                           &
      2X,'F(1,2)',4X,'F(2,2)',4X,'F(3,2)',4X,                           &
      2X,'F(1,3)',4X,'F(2,3)',4X,'F(3,3)',                              &
      6X,'a',9X,'b',9x,'c',9x,'G-phi1',4x,'G-PHI',4x,'G-phi2')
 403  format(I6,5X,i5,44x,3(2X,3F10.6),2(2x,3f10.5))
      !      
      end subroutine

      
      subroutine CURreadTitle(iounit,title,info)
      implicit none
      integer,intent(in)      :: iounit
      character(len=*)        :: title
      integer,intent(out)     :: info
      !
            read(iounit,'(A)',iostat=info) title
            filetitle = title 
      !
      end subroutine

      
      subroutine CURreadBlock(iounit,offset,MPOINT,info)
      implicit none
      integer,intent(in)      :: iounit      !< IO unit
      integer,intent(in)      :: offset   !< Number of blocks to be skipped
      integer,intent(in)      :: MPOINT   !< Maximal number of points in a block
      integer,intent(out)     :: info     !< Exit code
      !
      integer :: npoint, i, j, tmp
      double precision,parameter :: convf = acos(-1.D0) / 180.D0
      character(len=10) :: buf
      !   
            ! Recon first: get the number of records
            read(iounit,fmt=402,iostat=info) buf,buf
            read(iounit,fmt=403,iostat=info) NRSTEP,npoint,mf%FALG,mf%GAXES,mf%GEULR
            if (info /= 0) return
            read(iounit,fmt=401,iostat=info) buf
            if (info /= 0) return
            ! Skip N=offset blocks:
            ofs: do i = 1,offset
                  do j = 1, npoint
                        read(iounit,fmt=401,iostat=info) buf
                        if (info /= 0) exit ofs
                  enddo
                  read(iounit,fmt=402,iostat=info) buf,buf
                  read(iounit,fmt=403,iostat=info) NRSTEP,npoint,mf%FALG,mf%GAXES,mf%GEULR
                  if (info /= 0) exit
                  read(iounit,fmt=401,iostat=info) buf
            enddo ofs
            if (info /= 0) return
            !
            mf%GEULR = mf%GEULR * convf
            call EulRad_2_TMATRIX(mf%TAX0,mf%GEULR(1),mf%GEULR(2),mf%GEULR(3))  ! Check it!!!
            call Transf(mf%GAXES,mf%CIJ0,mf%TAX0)  ! Check it!!!
      
            ! Request allocation of the memory
            call DYNFIL0(npoint,MPOINT,.false.,info)
            ! Process the crystals in the block      
            do i=1,npoint
                  read(iounit,400,iostat=info) tmp,DFIL(i)%tGEW,     &
                                   DFIL(i)%tfi1,                  &
                                   DFIL(i)%tPHI,                  &
                                   DFIL(i)%tfi2,                  &
                                   DFIL(i)%tGAM
                  if (info /= 0) exit
                  ! Convert the grain orientatios from degrees to radians
                  DFIL(i)%tfi1 = DFIL(i)%tfi1 * convf
                  DFIL(i)%tPHI = DFIL(i)%tPHI * convf
                  DFIL(i)%tfi2 = DFIL(i)%tfi2 * convf
                  !
                  call initFields(mf,DFIL(i))
            enddo  
      
            !
      400  format (I6,f10.5,2X,3f10.5,2X,f10.5)
      401  format(A)     ! ignore one record
      402  format(A,/,A) ! ignore two lines
      403  format(I6,5X,i5,44x,3(2X,3F10.6),2(2x,3f10.5))      
      !
      end subroutine
      
      
end module
      
