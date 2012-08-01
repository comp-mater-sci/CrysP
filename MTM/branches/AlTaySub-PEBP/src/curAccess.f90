module curAccess
      
contains

      subroutine writeCURTitle(imp1,title,info)
      implicit none
      integer,intent(in)      :: imp1  !< IO unit number
      character(len=*)        :: title !< Title line
      integer,intent(out)     :: info  !< exit code: 0 on success
      !
            write(imp1,fmt='(A)',iostat=info) title
      !
      end subroutine

      ! Write the current contents of the dynfil
      subroutine writeCURBlock(imp1,info)
      use dynfil
      implicit none
      integer,intent(in)      :: imp1 !< IO unit number
      integer,intent(out)     :: info !< exit code: 0 on success
      integer :: npoint, i
      !
      double precision,parameter :: convf = 180.D0 / acos(-1.D0)
      double precision,dimension(3) :: GLR
      !
            npoint = size(DFIL)

            GLR=mf%GEULR*convf
            write (IMP1,402)
            write (IMP1,403) NRSTEP,npoint,mf%FALG,mf%GAXES,GLR
            write (IMP1,401)
            !
            do i=1,npoint
                  write(IMP1,400,iostat=info) i,DFIL(i)%tGEW,           &
     &                        DFIL(i)%tfi1*convf,                       &
     &                        DFIL(i)%tPHI*convf,                       &
     &                        DFIL(i)%tfi2*convf,                       &
     &                        DFIL(i)%tGAM
                  if (info /= 0) exit
            enddo  
      !      
 400  format (I6,f10.5,2X,3f10.5,2X,f10.5)                 
 401  format (' CRYSTAL WEIGHT ',5X,'phi1',6X,'PHI',7X,'phi2',6X,       &
     &'  GAMMA')
 402  format (/,' Def. Step    ','Number of orientations',27X,          &
     & 2X,'F(1,1)',4X,'F(2,1)',4X,'F(3,1)',4X,                          &
     & 2X,'F(1,2)',4X,'F(2,2)',4X,'F(3,2)',4X,                          &
     & 2X,'F(1,3)',4X,'F(2,3)',4X,'F(3,3)',                             &
     & 6X,'a',9X,'b',9x,'c',9x,'G-phi1',4x,'G-PHI',4x,'G-phi2')
 403  format(I6,5X,i5,44x,3(2X,3F10.6),2(2x,3f10.5))
      !      
      end subroutine
      
end module
      
