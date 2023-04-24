module altaySmtAccess
    use definitions
    use altayDynfil

    implicit none
    private

    public :: &
        SMTreadHeader, &
        SMTwriteHeader, &
        SMTwriteBlock, &
        SMTreadBlock

    contains

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
              call DYNFIL0(nrec,.false.,info)
        endif
    94  format(I5,5x,A)

    end subroutine

    subroutine SMTwriteHeader(iounit,title,info)
        integer,intent(in)            :: iounit
        character(len=*),intent(in)   :: title !< Title line
        integer,intent(out)           :: info

        info = -1
        write(iounit,94,iostat=info) size(DFIL),title
    94  format(I5,5x,A)

    end subroutine

    subroutine SMTwriteBlock(iounit,info)
        integer,intent(in)      :: iounit   !< IO unit
        integer,intent(out)     :: info     !< Exit code

        integer :: i,ngrains
        integer,parameter :: NSTAP = 1
        real(dp),parameter :: rad2deg = 180.D0 / acos(-1.D0)

        ngrains = size(DFIL)
        do i = 1, ngrains
            ! order: PHI2,PHI,PHI1,blank,NSTAP,GEW,blank
            write(iounit,97,iostat=info) DFIL(i)%tfi2*rad2deg,DFIL(i)%tPHI*rad2deg,DFIL(i)%tfi1*rad2deg,NSTAP,DFIL(i)%tGEW
            if (info /= 0) exit
        enddo

      97 format (3F10.3,10X,I5,5X,F10.5)
    end subroutine

    subroutine SMTreadBlock(iounit,info)
        integer,intent(in)      :: iounit      !< IO unit
        integer,intent(out)     :: info        !< Exit code

        real(dp),parameter :: deg2rad =  acos(-1.D0) / 180.D0
        integer :: i,j,i0,k,NSTAP,nrec,ngrains
        real(dp) :: STAP = 0.D0

        ! Number of records (orientations) in the SMT file
        nrec = size(DFIL)
        ngrains = nrec
        i = 1
        do j = 1, nrec
            NSTAP=1
            STAP=0.0D0
            read(iounit,96,iostat=info) DFIL(i)%tfi2,DFIL(i)%tPHI,DFIL(i)%tfi1,STAP,NSTAP,DFIL(i)%tGEW, DFIL(i)%tGAM
            if (info /= 0) exit
            DFIL(i)%tfi1 = DFIL(i)%tfi1 * deg2rad
            DFIL(i)%tPHI = DFIL(i)%tPHI * deg2rad
            DFIL(i)%tfi2 = DFIL(i)%tfi2 * deg2rad
            call initFields(mf,DFIL(i))
            i = i + 1
            if (NSTAP > 1) then
                ! More than one grain per record. This path is more complex,
                ! but is very infrequently followed.
                ngrains = ngrains + NSTAP - 1
                call DYNFIL0(ngrains,.true.,info)
                if (info /= 0) exit
                i0 = i - 1 ! Store the index of the "parent" grain
                do k=1,NSTAP-1
                    DFIL(i) = DFIL(i0)
                    DFIL(i)%tfi1 = DFIL(i0)%tfi1 + dble(k)*STAP*deg2rad
                    call initFields(mf,DFIL(i))
                    i = i + 1
                enddo
            endif
        enddo
  96    FORMAT (4F10.0,I5,5X,2F10.0)
    end subroutine

end module
