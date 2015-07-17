!
! $Id$
!
!> Support for not-so-backward-compatibile (NSBC)-CUB format.
!>
!> \note The content of CUB file is no longer one-to-one mappable to CUR, since the CUR format has changed.
module altayCubAccess
use criErrcodes
use criMathUtils
use altayTexAccess
use altayAlgorithms
implicit none
#include "criMacros.fpp"


    !> Native Fortran unformatted storage in (NSBC)-CUB format.
    type,extends(TextureMetaRawFileAccess) :: CUBFileAccess
        
    contains
        procedure,pass(this)    :: initialize => CUBFileAccess_initialize
        ! Implementation procedures
        procedure,pass(this)    :: readMetaData => CUBFileAccess_readMeta
        
        procedure,pass(this)    :: writeMetaData => CUBFileAccess_writeMeta
        
        procedure,pass(this)    :: readBlock => CUBFileAccess_readBlock
        
        procedure,pass(this)    :: writeBlock => CUBFileAccess_writeBlock
    end type
    
contains

    subroutine CUBFileAccess_initialize(this, context, readonly, info)
    class(CUBFileAccess),intent(inout)      :: this
    type(RawFileContext),intent(inout)      :: context
    logical,intent(in)                      :: readonly
    integer,intent(out)                     :: info
    !
        info = context%open(mode=merge('rb','wb',readonly))
        if (info == criSuccess) this%iounit = context%iounit
        !
    end subroutine

    ! Write the current contents of the dynfil
    subroutine CUBFileAccess_writeBlock(this, blockid, odf, info)
    implicit none
    class(CUBFileAccess),intent(inout)      :: this
    integer,intent(in)                      :: blockid
    type(DiscreteODF),intent(in)            :: odf
    integer,intent(out)                     :: info     !< exit code
    !      
    integer :: i, ioerr, npoints
    type(EulerAngles) :: euler_deg_tmp
    !
        info = criErr_BadDims
        npoints = size(odf)
        ! Check whether extended attributes are set.
        if (.not. allocated(this%accumulatedshear)) then
            allocate(this%accumulatedshear(npoints))
            this%accumulatedshear = 0.D0
        else
            if (size(this%accumulatedshear) /= npoints) return
        endif
        !
        do i = 1,npoints
            associate(orientation => odf%orientations(i), &
                      accumulatedshear => this%accumulatedshear(i))
                euler_deg_tmp = rad2deg(orientation%euler)
                write(this%iounit,iostat=ioerr) orientation%weight, euler_deg_tmp, &
                                                accumulatedshear
            end associate
            if (ioerr /= 0) exit
        enddo
        CHOOSE(info, ioerr == 0, criSuccess, criErr_IOWrite)
    !      
    end subroutine


    !> Read texture meta-data in CUB format
    subroutine CUBFileAccess_readMeta(this,odf, info)
    implicit none
    class(CUBFileAccess),intent(inout)      :: this
    type(DiscreteODF),intent(inout)         :: odf
    integer,intent(out)                     :: info
    !
    integer :: ioerr
    !
        odf%title = ''
        read(this%iounit,iostat=ioerr)  this%step_number, &
                                        this%ngrains, &
                                        this%mesodeformationgradient
        CHOOSE(info, ioerr == 0, criSuccess, criErr_IORead)
    !
    end subroutine


    !> Write texture meta-data in CUB format
    subroutine CUBFileAccess_writeMeta(this, odf, info)
    implicit none
    class(CUBFileAccess),intent(inout)      :: this
    type(DiscreteODF),intent(in)            :: odf
    integer,intent(out)                     :: info     !< exit code
    !
    integer :: ioerr
    !
        write (this%iounit,iostat=ioerr) this%step_number, &
                                         size(odf), &
                                         this%mesodeformationgradient
        CHOOSE(info, ioerr == 0, criSuccess, criErr_IOWrite)
    !
    end subroutine
    
    
    !> Read a CUB file block
    subroutine CUBFileAccess_readBlock(this, blockid, odf, info)
    implicit none
    class(CUBFileAccess),intent(inout)      :: this
    integer,intent(in)                      :: blockid
    type(DiscreteODF),intent(inout)         :: odf
    integer,intent(out)                     :: info     !< Exit code
    !
    integer :: i, ioerr
    type(EulerAngles) :: euler_deg_tmp
    !   
        info = criErr_BadDims
        !
        ! Request allocation of the memory
        info = DiscreteODF_resize(odf, this%ngrains)
        if (info /= criSuccess) return
        ! Allocate space for extended attributes
        if (allocated(this%accumulatedshear)) deallocate(this%accumulatedshear)
        allocate(this%accumulatedshear(this%ngrains))
        this%accumulatedshear = 0.D0
        ! Process the crystals in the block      
        do i= 1, this%ngrains
            associate(orientation => odf%orientations(i), &
                      accumulatedshear => this%accumulatedshear(i))
                read(this%iounit,iostat=ioerr) orientation%weight,euler_deg_tmp, &
                                               accumulatedshear
                if (ioerr /= 0) exit
                ! Convert the euler angles of texture constituent from degrees to radians
                orientation%euler = deg2rad(euler_deg_tmp)
                !
            end associate
        enddo
        CHOOSE(info, ioerr == 0, criSuccess, criErr_IORead)
    !
    end subroutine

      
end module