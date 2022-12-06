module altay_io
    use altayHardTypes
    use altay_definitions, only: dp
    use altay_log


    implicit none
    
    interface KS_readState
        module procedure KS_readState_unit, KS_readState_file
    end interface

contains


    !> Output state-derived variables (SDV) or/and a header line.
    integer function writeSDV(unit,SDV,header) result(info)
    integer,intent(in)                :: unit
    logical,intent(in),optional       :: header
    type(StateDerivedVars),intent(in),optional :: SDV
    !
    integer :: ierr
    !
    info = VEF_IO
    if (present(header)) then
        if (header) write(unit,fmt=100,iostat=ierr)
        if (ierr /= 0) return
    endif
    if (present(SDV)) then
        write(unit,fmt=101,iostat=ierr) SDV
        if (ierr /= 0) return
    endif
    info = VEF_OK
    !
    100 format(T4,'rho_CBs',T20,'rho_CBBs',T36,'rho_polCBBs',T52,'rho_avg')
    101 format(4(E15.7,1X))
    !
    end function



      !> Perform an IO formatted read operation on StatVar
      !>
      !> \param dummy if true, the function performs a fake read operation of by simply skipping the same number of lines as the ReadSVfile would normally read. The resulting SV becomes initialized to default values.
      integer function ReadSVfile(unit,SV,dummy) result(iError)
      integer,      intent(in)  :: unit
      type(StatVar),intent(out) :: SV
      logical,optional,intent(in)   :: dummy

      !local variables declarations
      integer :: i,j
      logical :: is_dummy
      character(len=5)             :: tmpstr
      !
      is_dummy = .false.
      if (present(dummy)) is_dummy = dummy
      if (is_dummy) then !< when dummy = .true. perform fake read
            do i=1,10
                  read(unit,fmt=100,err=666,end=666) tmpstr
            enddo
      else               !< when dummy = .false. or not present (default option) read SV from file
            read(unit,fmt=101,err=666,end=666) SV%RHOcb
            do i=1,6 !one line per WALL
              read(unit,fmt=102,err=666,end=666)SV%CBB(i)%RHOwd,        &
                                                SV%CBB(i)%RHOwp,        &
                                                SV%CBB(i)%RHOwdHOM,     &
                                                SV%CBB(i)%accGAMMA_new, &
                                                SV%CBB(i)%RHOwd_ini
            end do
            read(unit,fmt=103,err=666,end=666) SV%ActiveCBB(1),SV%ActiveCBB(2)
            do i=1,2 !first line for positive sense, 2nd line for negative sense
              read(unit,fmt=104,err=666,end=666)(SV%CRSS(i,j),j=1,24)
            end do
      endif
      iError = VEF_OK
      return
100   format(A5)             ! 5 characters
101   format(   E15.8 )      ! real number in scientific notation, 15 digits total (including 1
                             ! digit for sign and 4 for exponent, 8 digits after decimal point)
102   format( 5(E15.8,1X))   ! 5 times E15.8 with 1 blank spacing in between
103   format( 2(I5,1X   ))   ! 2 5-digit integers with 1 blank spacing
104   format(24(E15.8,1X))
      !
666   iError = VEF_IO !Error in reading from file
      !
      end function ReadSVfile



      integer function WriteSVfile(unit,SV) result(iError)
      integer,      intent(in)  :: unit
      type(StatVar),intent(in)  :: SV

      !local variables declarations
      integer :: i,j

      write(unit,fmt=101,err=666) SV%RHOcb
      do i=1,6 !one line per WALL
            write(unit,fmt=102,err=666)SV%CBB(i)%RHOwd,        &
                                    SV%CBB(i)%RHOwp,        &
                                    SV%CBB(i)%RHOwdHOM,     &
                                    SV%CBB(i)%accGAMMA_new, &
                                    SV%CBB(i)%RHOwd_ini
      end do
      write(unit,fmt=103,err=666) SV%ActiveCBB(1),SV%ActiveCBB(2)
      do i=1,2 !first line for positive sense, 2nd line for negative sense
            write(unit,fmt=104,err=666)(SV%CRSS(i,j),j=1,24)
      end do
      iError = VEF_OK
      return
      !
101   format(   E15.8 )
102   format( 5(E15.8,1X))
103   format( 2(I5,1X   ))
104   format(24(E15.8,1X))
      !
666   iError = VEF_IO !Error in reading from file
      !
      end function WriteSVfile




      integer function WriteHeadSVfile(unit) result(iError)
      integer,intent(in)  :: unit
      !
      write(unit,fmt=100,err=666)"# CB         : [1]RHOcb                                                    "
      write(unit,fmt=100,err=666)"# CBB1(01-1) : [1]RHOwd [2]RHOwp [3]RHOwdHOM [4]accGAMMA_new [5]RHOwd_ini  "
      write(unit,fmt=100,err=666)"# CBB2(-101) : [1]RHOwd [2]RHOwp [3]RHOwdHOM [4]accGAMMA_new [5]RHOwd_ini  "
      write(unit,fmt=100,err=666)"# CBB3(1-10) : [1]RHOwd [2]RHOwp [3]RHOwdHOM [4]accGAMMA_new [5]RHOwd_ini  "
      write(unit,fmt=100,err=666)"# CBB4(0-1-1): [1]RHOwd [2]RHOwp [3]RHOwdHOM [4]accGAMMA_new [5]RHOwd_ini  "
      write(unit,fmt=100,err=666)"# CBB5(101)  : [1]RHOwd [2]RHOwp [3]RHOwdHOM [4]accGAMMA_new [5]RHOwd_ini  "
      write(unit,fmt=100,err=666)"# CBB6(-1-10): [1]RHOwd [2]RHOwp [3]RHOwdHOM [4]accGAMMA_new [5]RHOwd_ini  "
      write(unit,fmt=100,err=666)"# ActiveCBBs : [1]ID_ActiveCBB_highest_slip [2]ID_ActiveCBB_2ndhighest_slip"
      write(unit,fmt=100,err=666)"# CRSS+sense : [1]CRSS(1+) [2]CRSS(2+) ...  [23]CRSS(23+) [24]CRSS(24+)    "
      write(unit,fmt=100,err=666)"# CRSS-sense : [1]CRSS(1-) [2]CRSS(2-) ...  [23]CRSS(23-) [24]CRSS(24-)    "
      write(unit,fmt=100,err=666)"#--------------------------------------------------------------------------"
      write(unit,fmt=100,err=666)"# units:  RHOx:          micrometer^(-2)                                   "
      write(unit,fmt=100,err=666)"#         accGAMMA_new:  /                                                 "
      write(unit,fmt=100,err=666)"#         CRSS:          MPa                                               "
      write(unit,fmt=100,err=666)"#--------------------------------------------------------------------------"
      iError = VEF_OK
      return
      !
100   format(A76)
101   format(A26,L1)
666   iError = VEF_IO !Error in writing to file
      !
      end function WriteHeadSVfile



      integer function ReadHeadSVfile(unit) result(iError)
      integer,intent(in)  :: unit

      !local variables declarations
      integer ::  i
      character :: tmp

      do i=1,15
        read(unit,fmt=100,err=666) tmp !read 15 lines
      end do

      iError = VEF_OK
      return
      !
100   format(A76)
666   iError = VEF_IO !Error in reading from file
      !
      end function ReadHeadSVfile




    !> Open state file either for reading or writing.
    !> If requested, performs some initialization, such as processing or writing file header.
    !> @param mode: File opening mode: 'r' for read access or 'w' for write access
    !> @param use_header: Request for processing  the file header. Default is: .true.
    !> @param iounit: IO unit to be used
    !> @param fname: Name of the file
    integer function KS_openStateFile(iounit, fname, mode, use_header) result(info)
        integer,intent(in)              :: iounit
        character(*), intent(in)        :: fname
        character, intent(in)           :: mode
        logical, optional, intent(in)   :: use_header
        logical                         :: is_header
        integer                         :: ierr
          
        info = -1
        is_header = .true.
        if (present(use_header)) is_header = use_header
        select case(mode)
        case('r')
            !open existing file (status='old')
            open(unit=iounit, file=fname, status='old', iostat=ierr) 
            if (ierr /= 0) return
            if (is_header) info = ReadHeadSVfile(iounit)
        case('w')
            !replace if already existing
            open(unit=iounit, file=fname, status='replace', iostat=ierr) 
            if (ierr /= 0) return
            if (is_header) info = WriteHeadSVfile(iounit)
        end select
    end function

    !> Write block (=snapshot of KS_state) into file.
    !> @param iounit: I/O unit number
    integer function KS_writeState(iounit, KS_state, len) result(info)
        integer, intent(in) :: iounit, len   
        integer             :: i, n
        type(StatVar), dimension(len), intent(in) :: KS_state 
    
        info = VEF_IO
        n = size(KS_state)
        write(iounit,fmt=100) n
        write(iounit,fmt=110)
        do i = 1, n
              write(iounit,fmt=200) i
              if (WriteSVfile(iounit,KS_state(i)) /= VEF_OK) exit
        enddo
        write(iounit,fmt=111)
        if (i > n) info = VEF_OK

        !Tolerate line numbers here since file I/O will be removed anyway
100     format(I5,1X,' # of points in KOST11 block')
110     format('-->')
111     format('<--')
200     format(I5)
    end function


    !>Call ReadSVfile and store state variables in ks_state.
    !>Perform fake reads on the first nblock blocks, where each block corresponds to one snapshot of ks_state.
    !>@param iounit: I/O unit number
    !>@param nblock: Number of blocks to be skipped
    integer function KS_readState_unit(iounit, nblock, KS_state, len) result(info)
        integer,intent(in)              :: iounit, len
        integer, optional, intent(in)   :: nblock
        integer                         :: i, n, nf, tmp, ioerr, iblock 
        character(5)                    :: tmp_str
        logical                         :: is_dummy
        type(StatVar), dimension(len), intent(inout) :: KS_state 
        
        info = VEF_Uninitialized
        if (len == 0) return
        n = size(KS_state) 
        if (n < 1) return
        info = VEF_IO
        nf = 0
        is_dummy = .true.
        
        do iblock = 0, nblock 
            if (iblock == nblock) is_dummy = .false. 
            read(iounit,fmt=100,iostat=ioerr) nf
            if ((nf /= n) .or. (ioerr /= 0)) return
            read(iounit,fmt=110) tmp_str
            do i = 1, n 
                read(iounit,fmt=100,iostat=ioerr) tmp
                if (ioerr /= 0) return
                if (ReadSVfile(iounit,KS_state(i),is_dummy) /= VEF_OK) return 
            enddo
            read(iounit,fmt=110,iostat=ioerr) tmp_str
            if (.not.is_dummy) exit 
        enddo

        if ((i > n) .and. (ioerr == 0)) info = VEF_OK 

        !Tolerate line numbers here since file I/O will be removed anyway
100     format(I5)
110     format(A)
    end function


    !> Open state file for reading and read state variables of snapshot.
    !> The snapshot (block) to be read is specified by nblock.
    !>@param nblock:  Number of blocks to be skipped
    integer function KS_readState_file(fname,iounit,nblock,use_header, KS_state, len) result(info)
        character(len=*), intent(in)    :: fname    
        integer, intent(in)             :: iounit, len
        integer, optional, intent(in)   :: nblock 
        logical, optional, intent(in)   :: use_header
        type(StatVar), dimension(len), intent(inout) :: KS_state 

        ! open file for read access ('r')
        info = KS_openStateFile(iounit, fname, 'r', use_header) 
        if (info == 0) then
            info = KS_readState_unit(iounit, nblock, KS_state, len)
        else
            info = VEF_IO
        endif
        close(iounit)
    end function
end module
