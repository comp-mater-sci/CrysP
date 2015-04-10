!
! $Id$
!
!> Support for backward-compatibile CUB format.
!>
!> \note The content of CUB file is no longer one-to-one mappable to CUR, since the CUR format has changed.
module altayCubAccess
use criErrcodes
use altayTexAccess
use altayAlgorithms
implicit none

    !> \todo reimplement CUBreadTitle so it reads other meta-data.

    !> \todo upgrade to OO type that extends TextureAccess

contains
    
    !> Read texture data in CUB format from `iounit`
    subroutine CUBread(this, iounit, info)
    implicit none
    type(TextureAssembly),intent(inout) :: this
    integer,intent(in)                  :: iounit   !< IO unit number
    integer,intent(out)                 :: info     !< exit code
    !
        call CUBreadTitle(this, iounit, info)
        if (info == criSuccess) then
            call CUBreadBlock(this, iounit, info)
            if (info == criSuccess) call textureData_init(this%texture, this%mf, info)
        endif
    !
    end subroutine
    
    !> Write texture data in CUB format to `iounit`
    subroutine CUBwrite(this, iounit, info)
    implicit none
    type(TextureAssembly),intent(in)    :: this
    integer,intent(in)                  :: iounit   !< IO unit number
    integer,intent(out)                 :: info     !< exit code
    !
        call CUBwriteBlock(this, iounit, info)
    !
    end subroutine
    

    ! Write the current contents of the dynfil
    subroutine CUBwriteBlock(this, iounit, info)
    implicit none
    type(TextureAssembly),intent(in) :: this
    integer,intent(in)              :: iounit   !< IO unit number
    integer,intent(out)             :: info     !< exit code
    !      
    integer :: npoint, i,ii,jj, ioerr
    double precision,parameter :: convf = 180.D0 / acos(-1.D0)
    double precision,dimension(3) :: GLR
    !
        info = criErr_IOWrite
        npoint = size(this%texture%grains)
        associate(mf => this%mf)
            GLR=mf%GEULR*convf
            write (iounit,iostat=ioerr) this%texture%nrstep,npoint,mf%FALG,mf%GAXES,GLR
        end associate
        if (ioerr /= 0) return
        !
        do i=1,npoint
            associate(grain => this%texture%grains(i))
                write(iounit,iostat=ioerr) grain%tGEW,                &
                            grain%tfi1*convf,                       &
                            grain%tPHI*convf,                       &
                            grain%tfi2*convf,                       &
                            grain%tGAM,                             &
                            ! Remaining components that are not present in CUR anymore:
                            ((grain%tF(ii,jj),ii=1,3),jj=1,3),     &
                            (grain%tAXES(jj),jj=1,3),              &
                            grain%tEULR
            end associate
            if (ioerr /= 0) exit
        enddo
        info = merge(criSuccess, criErr_IOWrite, (ioerr == 0))
    !      
    end subroutine


    !> Read title
    subroutine CUBreadTitle(this,iounit,info)
    implicit none
    type(TextureAssembly),intent(inout) :: this
    integer,intent(in)      :: iounit
    integer,intent(out)     :: info
    !
    integer :: tmp
    !
        tmp = iounit
        this%texture%title = ''
        info = criSuccess
    !
    end subroutine

    !> Read a CUB file block
    subroutine CUBreadBlock(this, iounit, info)
    implicit none
    type(TextureAssembly),intent(inout) :: this
    integer,intent(in)      :: iounit   !< IO unit
    integer,intent(out)     :: info     !< Exit code
    !
    integer :: npoint, i, ii,jj, ioerr
    double precision,parameter :: convf = acos(-1.D0) / 180.D0
    !      
        associate(mf => this%mf)
            read(iounit,iostat=info) this%texture%nrstep,npoint,mf%FALG,mf%GAXES,mf%GEULR
            if (info /= 0) return
            mf%GEULR = mf%GEULR * convf
            mf%TAX0 = rotmat(mf%GEULR(1),mf%GEULR(2),mf%GEULR(3))  ! Check it!!!
            call Transf(mf%GAXES,mf%CIJ0,mf%TAX0)  ! Check it!!!
        end associate
        !
        ! Request allocation of the memory
        info = textureData_resize(this%texture, npoint)
        if (info /= criSuccess) return
        ! Process the crystals in the block      
        do i=1,npoint
            associate(grain => this%texture%grains(i))
                read(iounit,iostat=ioerr)grain%tGEW,      &
                                        grain%tfi1,      &
                                        grain%tPHI,      &
                                        grain%tfi2,      &
                                        grain%tGAM,      &
                                        ! Remaining components that are not present in CUR anymore:
                                        ((grain%tF(ii,jj),ii=1,3),jj=1,3),     &
                                        (grain%tAXES(jj),jj=1,3),              &
                                        grain%tEULR

                if (ioerr /= 0) exit
                ! Convert the grain orientatios from degrees to radians
                grain%tfi1 = grain%tfi1 * convf
                grain%tPHI = grain%tPHI * convf
                grain%tfi2 = grain%tfi2 * convf
                !
            end associate
        enddo
        info = merge(criSuccess, criErr_IORead, (ioerr == 0))
    !
    end subroutine

      
end module