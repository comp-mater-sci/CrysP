module altaySmtAccess
use criErrcodes
use altayTexAccess
use altayIOContext
implicit none

    !> Native Fortran formatted storage in SMT format.
    type,extends(TextureRawFileAccess) :: SMTFileAccess
        integer                 :: ngrains = 0
    contains

        ! Implementation procedures
        procedure,pass(this)    :: readMetaData => SMTFileAccess_readMeta
        
        procedure,pass(this)    :: writeMetaData => SMTFileAccess_writeMeta
        
        procedure,pass(this)    :: readBlock => SMTFileAccess_readBlock
        
        procedure,pass(this)    :: writeBlock => SMTFileAccess_writeBlock
    end type

    contains
    
    
    !> Read meta-data from the SMT header
    subroutine SMTFileAccess_readMeta(this, odf, info)
    implicit none
    class(SMTFileAccess),intent(inout)      :: this
    type(DiscreteODF),intent(inout)         :: odf
    integer,intent(out)                     :: info
    !
    integer :: ioerr
    !
        info = criErr_IORead
        odf%title = ''
        read (this%iounit,94,iostat=ioerr) this%ngrains, odf%title
        if ((ioerr == 0) .and. (this%ngrains > 0)) info = criSuccess
    !
    94  format(I5,5x,A)
    !
    end subroutine
    
    
    subroutine SMTFileAccess_writeMeta(this, odf, info)
    implicit none
    class(SMTFileAccess),intent(inout)      :: this
    type(DiscreteODF),intent(in)            :: odf
    integer,intent(out)                     :: info
    !
        !
        info = criErr_IOWrite
        write(this%iounit,94,iostat=info) size(odf), odf%title
        if (info == 0) info = criSuccess
        94  format(I5,5x,A)
    !
    end subroutine
    
    
    subroutine SMTFileAccess_writeBlock(this, blockid, odf, info)
    implicit none
    class(SMTFileAccess),intent(inout)      :: this
    integer,intent(in)                      :: blockid
    type(DiscreteODF),intent(in)            :: odf
    integer,intent(out)                     :: info
    !
    integer :: i
    integer,parameter :: NSTAP = 1
    type(EulerAngles) :: euler_deg_tmp
    !
        info = criErr_IOWrite
        do i = 1, size(odf)
            associate(orientation => odf%orientations(i))
            euler_deg_tmp = rad2deg(orientation%euler)    
            write(this%iounit,97,iostat=info) euler_deg_tmp%fi2, &
                                         euler_deg_tmp%PHI, &
                                         euler_deg_tmp%fi1, &
                                         NSTAP,        &
                                         orientation%weight
            end associate
            if (info /= 0) exit
        enddo
        if (info == 0) info = criSuccess
        !
    97 format (3F10.3,10X,I5,5X,F10.5) 
    end subroutine      


    
    subroutine SMTFileAccess_readBlock(this, blockid, odf, info)
    implicit none
    class(SMTFileAccess),intent(inout)      :: this
    integer,intent(in)                      :: blockid
    type(DiscreteODF),intent(inout)         :: odf
    integer,intent(out)                     :: info
    !
    double precision,parameter :: convf =  acos(-1.D0) / 180.D0
    integer :: i,j,i0,k,NSTAP,nrec,ngrains
    double precision :: STAP = 0.D0, dummy_dp
    !
        ! Number of records in the SMT file
        info = criErr_BadArgs
        if (this%ngrains > 0) info = DiscreteODF_resize(odf, this%ngrains)
        if (info /= criSuccess) return
        nrec = size(odf)
        ! Number of grains (these are different things: one record
        ! in the SMT file may in principle provide multiple grains.
        ngrains = nrec
        info = criErr_IOWrite
        i = 1
        do j = 1, nrec
            NSTAP=1
            STAP=0.0D0
            associate(orientation => odf%orientations(i))
                ! order: PHI2,PHI,PHI1,STAP,NSTAP,GEW,"a dummy"
                read(this%iounit,96,iostat=info) orientation%euler%fi2,         &
                                            orientation%euler%PHI,         &
                                            orientation%euler%fi1,         &
                                            STAP,NSTAP,         &
                                            orientation%weight,         &
                                            dummy_dp
                if (info /= 0) exit
                info = criSuccess
                ! Convert the euler angles of texture constituent from degrees to radians
                orientation%euler = deg2rad(orientation%euler)
            end associate
            i = i + 1
            if (NSTAP > 1) then
                ! More than one grain per record. This path is more complex,
                ! but is very infrequently followed.
                ngrains = ngrains + NSTAP - 1
                if (DiscreteODF_resize(odf, ngrains, keep_state=.true.) /= criSuccess) exit
                i0 = i - 1 ! Store the index of the "parent" grain
                do k=1,NSTAP-1
                        odf%orientations(i) = odf%orientations(i0)
                        odf%orientations(i)%euler%fi1 = odf%orientations(i0)%euler%fi1 + dble(k)*STAP*convf
                        i = i + 1
                enddo
            endif
        enddo
    96  FORMAT (4F10.0,I5,5X,2F10.0)
    !
    end subroutine
      
end module
    