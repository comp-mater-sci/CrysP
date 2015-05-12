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
    integer :: npoint, i, ioerr
    type(EulerAngles) :: euler_deg_tmp
    !
        info = criErr_IOWrite
        npoint = size(this%texture%orientations)
        write (iounit,iostat=ioerr) 0,npoint,this%mesodeformationgradient
        if (ioerr /= 0) return
        !
        do i=1,npoint
            associate(orientation => this%texture%orientations(i))
                euler_deg_tmp = rad2deg(orientation%euler)
                write(iounit,iostat=ioerr) orientation%weight, euler_deg_tmp, &
                                           orientation%tGAM
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
    integer :: npoint, i, ioerr, dummy
    type(EulerAngles) :: euler_deg_tmp
    !      
        read(iounit,iostat=info) dummy,npoint,this%mesodeformationgradient
        if (info /= 0) return
        !
        ! Request allocation of the memory
        info = DiscreteODF_resize(this%texture, npoint)
        if (info /= criSuccess) return
        ! Process the crystals in the block      
        do i=1,npoint
            associate(orientation => this%texture%orientations(i))
                read(iounit,iostat=ioerr)orientation%weight,euler_deg_tmp, &
                                         orientation%tGAM
                if (ioerr /= 0) exit
                ! Convert the euler angles of texture constituent from degrees to radians
                orientation%euler = deg2rad(euler_deg_tmp)
                !
            end associate
        enddo
        info = merge(criSuccess, criErr_IORead, (ioerr == 0))
    !
    end subroutine

      
end module