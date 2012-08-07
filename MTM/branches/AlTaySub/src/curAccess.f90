module curAccess
   

contains

      subroutine CURwriteTitle(imp1,title,info)
      use dynfil
      implicit none
      integer,intent(in)      :: imp1  !< IO unit number
      character(len=*)        :: title !< Title line
      integer,intent(out)     :: info  !< exit code: 0 on success
      !
            write(imp1,fmt='(A)',iostat=info) title
      !
      end subroutine

      ! Write the current contents of the dynfil
      subroutine CURwriteBlock(imp1,info)
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
      
      subroutine CURreadTitle(inp,title,info)
      implicit none
      integer,intent(in)      :: inp
      character(len=*)        :: title
      integer,intent(out)     :: info
      !
            read(inp,'(A)',iostat=info) title
      !
      end subroutine
      
      subroutine CURreadBlock(inp,offset,MPOINT,info)
      use dynfil
      implicit none
      integer,intent(in)      :: inp      !< IO unit
      integer,intent(in)      :: offset   !< Number of blocks to be skipped
      integer,intent(in)      :: MPOINT   !< Maximal number of points in a block to be skipped
      integer,intent(out)     :: info     !< Exit code
      !
      integer :: npoint, i, j, tmp
      double precision,parameter :: convf = acos(-1.D0) / 180.D0
      
      ! Recon first: get the number of records
      read(inp,fmt=401,iostat=info) 
      read(inp,fmt=403,iostat=info) NRSTEP,npoint,mf%FALG,mf%GAXES,mf%GEULR
      if (info /= 0) return
      read(inp,fmt=401,iostat=info)
      if (info /= 0) return
      
      ofs: do i = 1,offset
            read(inp,fmt=401)
            do j = 1, npoint
                  read(inp,fmt=4000,iostat=info)
                  if (info /= 0) exit ofs
            enddo
            read(inp,fmt=402,iostat=info)
            read(inp,fmt=403,iostat=info) NRSTEP,npoint,mf%FALG,mf%GAXES,mf%GEULR
            read(inp,fmt=401,iostat=info)
      enddo ofs
      if (info /= 0) return
      !
      mf%GEULR = mf%GEULR * convf
      call TMATRIX(mf%TAX0,mf%GEULR(1),mf%GEULR(2),mf%GEULR(3))  ! Check it!!!
      call Transf(mf%GAXES,mf%CIJ0,mf%TAX0)  ! Check it!!!
      
      ! Request allocation of the memory
      call DYNFIL0(npoint,MPOINT,info)
      ! Process the crystals in the block      
      do i=1,npoint
            read(inp,400,iostat=info) tmp,DFIL(i)%tGEW,     &
                             DFIL(i)%tfi1,                  &
                             DFIL(i)%tPHI,                  &
                             DFIL(i)%tfi2,                  &
                             DFIL(i)%tGAM
            if (info /= 0) exit
            ! Convert the grain orientatios from degrees to radians
            DFIL(i)%tfi1 = DFIL(i)%tfi1 * convf
            DFIL(i)%tPHI = DFIL(i)%tPHI * convf
            DFIL(i)%tPHI = DFIL(i)%tfi2 * convf
            !
            ! Backward compatibility with type(grain):
            ! initialize the remaining components with mf data
                  
            DFIL(i)%tAXES = mf%GAXES 
            DFIL(i)%tEULR = mf%GEULR
            DFIL(i)%tT   = 0.D0           !!! FIXME !!! <- this component should be initialized!
            DFIL(i)%tF   = mf%FALG
            DFIL(i)%tCIJ = mf%CIJ0
            DFIL(i)%tTAX = mf%TAX0
            DFIL(i)%tZERO = 0.D0
            DFIL(i)%tRHO  = 0.D0

      enddo  
      
      !
 400  format (I6,f10.5,2X,3f10.5,2X,f10.5)
 401  format(/)   ! ignore one record
 402  format(/,/) ! ignore two lines
 403  format(I6,5X,i5,44x,3(2X,3F10.6),2(2x,3f10.5))      
4000  format(/)   ! Fake 400  
      end subroutine
      
      
end module
      
