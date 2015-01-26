!
! $Id$
!      
module altayCurAccess
use criErrcodes
use altayTexAccess
use altayAlgorithms

    ! TODO: upgrade to OO type that extends TextureAccess

contains
    !> Read texture data in CUR format from iounit.
    subroutine CURread(this, iounit, blockIdx, info)
    implicit none
    type(TextureAssembly),intent(inout) :: this
    integer,intent(in)                  :: iounit   !< IO unit number
    integer,intent(in)                  :: blockIdx !< Index of the block to be read. The blocks are indexed from 0.
    integer,intent(out)                 :: info     !< exit code
    !
        call CURreadTitle(this, iounit, info)
        if (info == criSuccess) then
            call CURreadBlock(this, iounit, blockIdx, info)
            if (info == criSuccess) call textureData_init(this%texture, this%mf, info)
        endif
    !
    end subroutine
    
    !> Write texture data in CUR format to iounit
    subroutine CURwrite(this, iounit, full, info)
    implicit none
    type(TextureAssembly),intent(in)    :: this
    integer,intent(in)                  :: iounit   !< IO unit number
    logical,intent(in)                  :: full     !< Flag: if true, both meta-data and data will be written out. Otherwise only the data will be written out.
    integer,intent(out)                 :: info     !< exit code
    !
        if (full) call CURwriteTitle(this,iounit,info)
        if (info == 0) call CURwriteBlock(this, iounit, info)
    !
    end subroutine
    
    !> Write title line of the CUR file.
    subroutine CURwriteTitle(this,iounit,info)
    implicit none
    type(TextureAssembly),intent(in)    :: this
    integer,intent(in)            :: iounit  !< IO unit number
    integer,intent(out)           :: info       !< exit code
    !
    integer :: ioerr
    !
        write(iounit,fmt='(A)',iostat=ioerr) this%texture%title
        info = merge(criSuccess, criErr_IOWrite, (ioerr == 0))
    !
    end subroutine

      
    ! Write the current contents of the dynfil
    subroutine CURwriteBlock(this, iounit, info)
    implicit none
    type(TextureAssembly),intent(in)    :: this
    integer,intent(in)      :: iounit   !< IO unit number
    integer,intent(out)     :: info     !< exit code
    !
    integer :: npoint, i, ioerr
    !
    double precision,parameter :: convf = 180.D0 / acos(-1.D0)
    double precision,dimension(3) :: GLR
    !
        info = criErr_IOWrite
        npoint = size(this%texture%grains)
        associate(mf => this%mf, nrstep => this%texture%nrstep)
            GLR=mf%GEULR*convf
            write (iounit,402)
            write (iounit,403) nrstep,npoint,mf%FALG,mf%GAXES,GLR
            write (iounit,401, iostat=ioerr)
        end associate
        if (ioerr /= 0) return
        !
        do i=1,npoint
            associate(grain => this%texture%grains(i))
                write(iounit,400,iostat=ioerr) i,grain%tGEW,           &
                            grain%tfi1*convf,                       &
                            grain%tPHI*convf,                       &
                            grain%tfi2*convf,                       &
                            grain%tGAM
            end associate
            if (ioerr /= 0) exit
        enddo
        if (ioerr == 0) info = criSuccess
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

      
    subroutine CURreadTitle(this,iounit,info)
    implicit none
    type(TextureAssembly),intent(inout) :: this
    integer,intent(in)      :: iounit
    integer,intent(out)     :: info
    !
    integer :: ioerr
    !
        read(iounit,'(A)',iostat=ioerr) this%texture%title
        info = merge(criSuccess, criErr_IORead, (ioerr == 0))
    !
    end subroutine

          
    subroutine CURreadBlock(this,iounit,offset,info)
    implicit none
    type(TextureAssembly),intent(inout) :: this
    integer,intent(in)      :: iounit      !< IO unit
    integer,intent(in)      :: offset   !< Number of blocks to be skipped
    integer,intent(out)     :: info     !< Exit code
    !
    integer :: npoint, i, j, tmp, ioerr
    double precision,parameter :: convf = acos(-1.D0) / 180.D0
    character(len=10) :: buf
    !   
        info = criErr_IORead
        ! Recon first: get the number of records/
        read(iounit,fmt=402,iostat=ioerr) buf,buf
        if (ioerr /= 0) return
        associate(mf => this%mf, nrstep => this%texture%nrstep)
            read(iounit,fmt=403,iostat=ioerr) nrstep,npoint,mf%FALG,mf%GAXES,mf%GEULR
            if (ioerr /= 0) return
            read(iounit,fmt=401,iostat=ioerr) buf
            if (ioerr /= 0) return
            ! Skip N=offset blocks:
            ofs: do i = 1,offset
                    do j = 1, npoint
                        read(iounit,fmt=401,iostat=ioerr) buf
                        if (ioerr /= 0) exit ofs
                    enddo
                    read(iounit,fmt=402,iostat=ioerr) buf,buf
                    read(iounit,fmt=403,iostat=ioerr) nrstep,npoint,mf%FALG,mf%GAXES,mf%GEULR
                    if (ioerr /= 0) exit
                    read(iounit,fmt=401,iostat=ioerr) buf
            enddo ofs
            if (ioerr /= 0) return
            !
            mf%GEULR = mf%GEULR * convf
            mf%TAX0 = rotmat(mf%GEULR(1),mf%GEULR(2),mf%GEULR(3))  ! Check it!!!
            call Transf(mf%GAXES,mf%CIJ0,mf%TAX0)  ! Check it!!!
        end associate
        ! Request allocation of the memory
        info = textureData_resize(this%texture, npoint)
        if (info /= criSuccess) return
        ! Process the crystals in the block      
        do i=1,npoint
            associate(grain => this%texture%grains(i))
                read(iounit,400,iostat=ioerr) tmp,grain%tGEW,     &
                                grain%tfi1,                  &
                                grain%tPHI,                  &
                                grain%tfi2,                  &
                                grain%tGAM
                if (ioerr /= 0) exit
                ! Convert the grain orientatios from degrees to radians
                grain%tfi1 = grain%tfi1 * convf
                grain%tPHI = grain%tPHI * convf
                grain%tfi2 = grain%tfi2 * convf
            end associate
        enddo
        info = merge(criSuccess, criErr_IORead, (ioerr == 0))
      
        !
    400  format (I6,f10.5,2X,3f10.5,2X,f10.5)
    401  format(A)     ! ignore one record
    402  format(A,/,A) ! ignore two lines
    403  format(I6,5X,i5,44x,3(2X,3F10.6),2(2x,3f10.5))      
    !
    end subroutine
      
      
end module
      
