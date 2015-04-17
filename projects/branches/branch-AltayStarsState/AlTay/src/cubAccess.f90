!
! $Id$
!
!> Support for not-so-backward-compatibile CUB format.
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
    !
        info = criErr_IOWrite
        npoint = size(this%texture%grains)
        write (iounit,iostat=ioerr) this%texture%nrstep,npoint,this%mf%FALG
        if (ioerr /= 0) return
        !
        do i=1,npoint
            associate(grain => this%texture%grains(i))
                write(iounit,iostat=ioerr) grain%tGEW,                &
                            grain%tfi1*convf,                       &
                            grain%tPHI*convf,                       &
                            grain%tfi2*convf,                       &
                            grain%tGAM
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
        read(iounit,iostat=info) this%texture%nrstep,npoint,this%mf%FALG
        if (info /= 0) return
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
                                        grain%tGAM
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