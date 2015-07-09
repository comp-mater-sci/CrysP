!
! $Id$
!      
module altayCurAccess
use criErrcodes
use altayTexAccess
use altayAlgorithms
#include "criMacros.fpp"
    
    !> CUR format- native Fortran formatted, multiblock.
    type,extends(TextureMetaRawFileAccess) :: CURFileAccess
        
        logical                 :: is_header_processed = .false.
        
    contains

        ! Implementation procedures
        procedure,pass(this)    :: readMetaData => CURFileAccess_readMeta
        
        procedure,pass(this)    :: writeMetaData => CURFileAccess_writeMeta
        
        procedure,pass(this)    :: readBlock => CURFileAccess_readBlock
        
        procedure,pass(this)    :: writeBlock => CURFileAccess_writeBlock
    end type
contains
    
    
    !> Write meta-datea of the CUR file.
    subroutine CURFileAccess_writeMeta(this, odf, info)
    implicit none
    class(CURFileAccess),intent(inout)      :: this
    type(DiscreteODF),intent(in)            :: odf
    integer,intent(out)                     :: info !< exit code
    !
    integer :: ioerr
    !
        info = criSuccess
        if (.not. this%is_header_processed) then
            this%is_header_processed = .true.
            write(this%iounit,fmt='(A)',iostat=ioerr) odf%title
            CHOOSE(info, (ioerr == 0), criSuccess, criErr_IOWrite)
        endif
    !
    end subroutine

      
    ! Write the current contents of the dynfil
    subroutine CURFileAccess_writeBlock(this, blockid, odf, info)
    implicit none
    class(CURFileAccess),intent(inout)      :: this
    integer,intent(in)                      :: blockid
    type(DiscreteODF),intent(in)            :: odf
    integer,intent(out)                     :: info
    !
    integer :: npoint, i, ioerr
    !
    type(EulerAngles) :: euler_deg_tmp
    !
        info = criErr_IOWrite
        npoint = size(odf)
        associate(mesodeformationgradient => this%mesodeformationgradient)
            write (this%iounit,402)
            write (this%iounit,403) npoint,mesodeformationgradient
            write (this%iounit,401, iostat=ioerr)
        end associate
        if (ioerr /= 0) return
        !
        do i=1, npoint
            associate(orientation => odf%orientations(i))
                euler_deg_tmp = rad2deg(orientation%euler)
                write(this%iounit,400,iostat=ioerr) i,orientation%weight, &
                            euler_deg_tmp%fi1,               &
                            euler_deg_tmp%PHI,               &
                            euler_deg_tmp%fi2,               &
                            0.0D0 !> \todo FIX to limitations of Rev. 2221
            end associate
            if (ioerr /= 0) exit
        enddo
        if (ioerr == 0) info = criSuccess
    !      
    400  format (I6,f10.5,2X,3f10.5,2X,f10.5)                 
    401  format (' CRYSTAL WEIGHT ',5X,'phi1',6X,'PHI',7X,'phi2',6X,'  GAMMA')
    402  format (/,14X,'Number of orientations',27X,          &
        2X,'F(1,1)',4X,'F(2,1)',4X,'F(3,1)',4X,                           &
        2X,'F(1,2)',4X,'F(2,2)',4X,'F(3,2)',4X,                           &
        2X,'F(1,3)',4X,'F(2,3)',4X,'F(3,3)')
    403  format(11X,i5,44x,3(2X,3F10.6))
    !      
    end subroutine

      
    subroutine CURFileAccess_readMeta(this, odf,info)
    implicit none
    class(CURFileAccess),intent(inout)      :: this
    type(DiscreteODF),intent(inout)         :: odf
    integer,intent(out)                     :: info
    !
    integer :: ioerr
    !
        info = criSuccess
        if (.not. this%is_header_processed) then
            this%is_header_processed = .true.
            read(this%iounit,'(A)',iostat=ioerr) odf%title
            CHOOSE(info, (ioerr == 0), criSuccess, criErr_IOWrite)
        endif
    !
    end subroutine


    subroutine CURFileAccess_readBlock(this, blockid, odf, info)
    implicit none
    class(CURFileAccess),intent(inout)      :: this
    integer,intent(in)                      :: blockid
    type(DiscreteODF),intent(inout)         :: odf
    integer,intent(out)                     :: info     !< Exit code
    !
    integer :: npoint, i, j, tmp, ioerr, dummy
    double precision :: dummy_dp ! <-- FIXME
    type(EulerAngles) :: euler_deg_tmp
    character(len=10) :: buf
    !   
        info = criErr_IORead
        ! Recon first: get the number of records/
        read(this%iounit,fmt=402,iostat=ioerr) buf,buf
        if (ioerr /= 0) return
        associate(mesodeformationgradient => this%mesodeformationgradient)
            read(this%iounit,fmt=403,iostat=ioerr) dummy,npoint,mesodeformationgradient
            if (ioerr /= 0) return
            read(this%iounit,fmt=401,iostat=ioerr) buf
            if (ioerr /= 0) return
            ! Skip N=offset blocks:
            ofs: do i = 1,blockid
                    do j = 1, npoint
                        read(this%iounit,fmt=401,iostat=ioerr) buf
                        if (ioerr /= 0) exit ofs
                    enddo
                    read(this%iounit,fmt=402,iostat=ioerr) buf,buf
                    read(this%iounit,fmt=403,iostat=ioerr) dummy,npoint,mesodeformationgradient !> \todo FIXME: dummy contains actual information
                    if (ioerr /= 0) exit
                    read(this%iounit,fmt=401,iostat=ioerr) buf
            enddo ofs
            if (ioerr /= 0) return
            !
        end associate
        ! Request allocation of the memory
        info = DiscreteODF_resize(odf, npoint)
        if (info /= criSuccess) return
        ! Process the crystals in the block      
        do i=1,npoint
            associate(orientation => odf%orientations(i))
                read(this%iounit,400,iostat=ioerr) tmp,orientation%weight,     & 
                                euler_deg_tmp%fi1,                & 
                                euler_deg_tmp%PHI,                & 
                                euler_deg_tmp%fi2,                & 
                                dummy_dp !> \todo FIX to limitations of Rev. 2221
                if (ioerr /= 0) exit
                ! Convert the euler angles of texture constituent from degrees to radians
                orientation%euler = deg2rad(euler_deg_tmp)
            end associate
        enddo
        CHOOSE(info, (ioerr == 0), criSuccess, criErr_IORead)
        !
    400  format (I6,f10.5,2X,3f10.5,2X,f10.5)
    401  format(A)     ! ignore one record
    402  format(A,/,A) ! ignore two lines
    403  format(I6,5X,i5,44x,3(2X,3F10.6))
    !
    end subroutine
      
      
end module
      
