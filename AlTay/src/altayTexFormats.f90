!> Dispatcher subroutines for IO operation on texture data files.
module altayTexFormats
    use altayDynfil
    use definitions
    use criMathUtils

    implicit none
    private
    integer,parameter,public :: TF_SMT = 1

    public :: loadTexture, &
              openTextureFile
contains

    subroutine loadTexture(fname,info)
        character(len=*),intent(in)   :: fname
        integer,intent(out)           :: info

        integer  :: nunit

        info = -1
        if (openTextureFile(nunit,fname,'r') /= 0) return

        call SMTreadHeader(nunit,filetitle,info)
        if (info /= 0) return
        call SMTreadBlock(nunit,info)
        close(nunit)

    end subroutine


    !> Open a file for texture output
    integer function openTextureFile(iounit,fname,mode) result(info)
        integer,intent(out)            :: iounit   !< I/O unit
        character(len=*),intent(in)   :: fname    !< File name to be opened
        character(len=1),intent(in)   :: mode     !< Mode: ['r'|'w']

        character(len=10) :: stat


        select case(mode)
        case('r')
              stat = 'old'
        case('w')
              stat = 'replace'
        case default
             info = -1
              return
        end select

        open(newunit=iounit,file=trim(fname),status=trim(stat),form='formatted',iostat=info)

    end function

    subroutine SMTreadHeader(iounit,title,info)
        integer,intent(in)      :: iounit
        character(len=*)        :: title     !< Title for texture
        integer,intent(out)     :: info

        integer :: nrec, &     !< number of grains
                   ioerr

        info = -1
        nrec = 0
        read (iounit,94,iostat=ioerr) nrec,title ! Read number of grains and title
        if ((ioerr == 0) .and. (nrec > 0)) then
              filetitle = title
              ! Pre-allocate the storage. Chances are that there will be no need to reallocate it.
              call DYNFIL_init(nrec,.false.,info)
        endif
    94  format(I5,5x,A)

    end subroutine

    subroutine SMTreadBlock(iounit,info)
        integer,intent(in)      :: iounit      !< IO unit
        integer,intent(out)     :: info        !< Exit code

        integer :: i,j,NSTAP,nrec,ngrains
        real(DP) :: STAP = 0.D0, eu(3)

        ! Number of records (orientations) in the SMT file
        nrec = size(DFIL)
        ngrains = nrec
        i = 1
        do j = 1, nrec
            STAP=0.0D0
            read(iounit,96,iostat=info) eu(3),eu(2),eu(1),STAP,NSTAP,DFIL(i)%tGEW, DFIL(i)%tGAM
            if (info /= 0) exit
            if (NSTAP > 1) error stop !MD was an unused option
            DFIL(i)%tT   = rotmat(eu(1)*pi_deg,eu(2)*pi_deg,eu(3)*pi_deg)
            DFIL(i)%tTAX = mf%TAX0
            DFIL(i)%tZERO = 0.0_DP
            i = i + 1
        enddo
  96    FORMAT (4F10.0,I5,5X,2F10.0)
    end subroutine

end module
