module altaySmtAccess
use criErrcodes
use altayTexAccess
implicit none

!> \todo upgrade to OO type that extends TextureAccess

contains
    
    !> Read texture data from SMT file and place the result in  DiscreteODF object.
    !>
    subroutine SMTread(this, iounit,  info)
    implicit none
    type(TextureAssembly),intent(inout) :: this
    integer,intent(in)              :: iounit
    integer,intent(out)             :: info
    !
    integer :: ngrains
    !
        call SMTreadHeader(iounit, this%texture%title, ngrains, info)
        if (info /= criSuccess .or. (ngrains <= 0)) return
        !
        info = DiscreteODF_resize(this%texture, ngrains)
        if (info == criSuccess) then
            call SMTreadBlock(iounit, this%texture, info)
        endif
    !
    end subroutine
    
    !> Write out texture data in the SMT format.
    subroutine SMTwrite(this,iounit, info)
    implicit none
    type(TextureAssembly),intent(in):: this
    integer,intent(in)              :: iounit
    integer,intent(out)             :: info
    !
        call SMTwriteHeader(iounit, size(this%texture%orientations), this%texture%title, info)
        if (info == criSuccess) call SMTwriteBlock(iounit, this%texture, info)
    !
    end subroutine
    
    !> Read meta-data from the SMT header
    subroutine SMTreadHeader(iounit, title, ngrains, info)
    implicit none
    integer,intent(in)              :: iounit
    character(len=*),intent(out)    :: title
    integer, intent(out)            :: ngrains
    integer,intent(out)             :: info
    !
    integer :: ioerr
    !
        info = criErr_IORead
        ngrains = 0
        title = ''
        read (iounit,94,iostat=ioerr) ngrains, title
        if ((ioerr == 0) .and. (ngrains > 0)) info = criSuccess
    !
    94  format(I5,5x,A)
    !
    end subroutine
      
      
      subroutine SMTwriteHeader(iounit,ngrains,title,info)
      implicit none
      integer,intent(in)            :: iounit
      integer, intent(in)           :: ngrains
      character(len=*),intent(in)   :: title !< Title line
      integer,intent(out)           :: info
      !
            info = criErr_IOWrite
            write(iounit,94,iostat=info) ngrains, title
            if (info == 0) info = criSuccess
      94  format(I5,5x,A)
      !
      end subroutine
      
    subroutine SMTwriteBlock(iounit,texture,info)
    implicit none
    integer,intent(in)            :: iounit   !< IO unit
    type(DiscreteODF),intent(in)  :: texture
    integer,intent(out)           :: info     !< Exit code
    !
    integer :: i,ngrains
    integer,parameter :: NSTAP = 1
    type(EulerAngles) :: euler_deg_tmp
    !
        info = criErr_IOWrite
        ngrains = size(texture%orientations)
        do i = 1, ngrains
            associate(orientation => texture%orientations(i))
            euler_deg_tmp = rad2deg(orientation%euler)    
            write(iounit,97,iostat=info) euler_deg_tmp%fi2, &
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

    subroutine SMTreadBlock(iounit,texture,info)
    implicit none
    integer,intent(in)              :: iounit   !< IO unit
    type(DiscreteODF),intent(inout) :: texture  !< Texture data
    integer,intent(out)             :: info     !< Exit code
    !
    double precision,parameter :: convf =  acos(-1.D0) / 180.D0
    integer :: i,j,i0,k,NSTAP,nrec,ngrains
    double precision :: STAP = 0.D0, dummy_dp
    !
        ! Number of records in the SMT file
        nrec = size(texture%orientations)
        ! Number of grains (these are different things: one record
        ! in the SMT file may in principle provide multiple grains.
        ngrains = nrec
        info = criErr_IOWrite
        i = 1
        do j = 1, nrec
            NSTAP=1
            STAP=0.0D0
            associate(orientation => texture%orientations(i))
                ! order: PHI2,PHI,PHI1,STAP,NSTAP,GEW,"a dummy"
                read(iounit,96,iostat=info) orientation%euler%fi2,         &
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
                if (DiscreteODF_resize(texture, ngrains, keep_state=.true.) /= criSuccess) exit
                i0 = i - 1 ! Store the index of the "parent" grain
                do k=1,NSTAP-1
                        texture%orientations(i) = texture%orientations(i0)
                        texture%orientations(i)%euler%fi1 = texture%orientations(i0)%euler%fi1 + dble(k)*STAP*convf
                        i = i + 1
                enddo
            endif
        enddo
    96  FORMAT (4F10.0,I5,5X,2F10.0)                                      
    !
    end subroutine
      
end module
    