module altayDSHState
    use altayHardLaw_DSH
    use altay_definitions
    use altayHardTypes
    use altay_log

    implicit none
    
    !>array of state variables
    type(StatVar), dimension(:), allocatable, save :: KS_state 

    interface KS_readState
        module procedure KS_readState_unit, KS_readState_file
    end interface

    private
    public  ::  iKOST,              &
                KS_initState,       &
                KS_openStateFile,   &    
                KS_readState,       &
                KS_getSDV,          &
                KS_getCRSS,         &
                KS_updateState,     &
                KS_writeState,      &
                KS_finalize

contains

    !> Query the number of elements in the state array.
    integer function KS_getStateSize() result(state_size)
        if (allocated(KS_state)) then 
            state_size = size(KS_state)
        else
            state_size = 0
        end if
    end function

    !> Allocate memory to the KS_state array.
    !> @param norient: Number of orientations in the material
    integer function KS_initState(norient) result(info)
        integer, intent(in) :: norient 
        integer             :: i
    
        info = VEF_BADDIMS
        if (norient > 0) allocate(KS_state(norient), stat=info)
        if (info /= 0) return
        ! All elements (orientations) of the KS_state array must have the same initial state.
        call GetInitStatVar(KS_state(1),info)
        if (info /= VEF_OK) return
        do i = 2, norient
              KS_state(i) = KS_state(1)
        enddo
    end function

    !> Deallocate the KS_state array.
    integer function KS_finalize() result(info)
        integer :: memstat
    
        info = VEF_OK
        if (allocated(KS_state)) then
            deallocate(KS_state,stat=memstat)
            if (memstat /= 0) info = VEF_ERROR
        endif
    end function

    !> Update the state variables of the PEBP model for i-th grain.
    subroutine KS_updateState(i,sliprate,deltaT,info)
        integer,intent(in)                          :: i        
        real(dp),intent(in), dimension(24)  :: sliprate 
        real(dp),intent(in)                 :: deltaT   
        integer,intent(out)                         :: info
        type(StatVar)                               :: SV_tmp
        
        info = VEF_BADDIMS
        if (size(KS_state) < i) return
        
        call MakeInc(KS_state(i), sliprate, deltaT, SV_tmp, info)

        if (info /= VEF_OK) return
        KS_state(i) = SV_tmp
    end subroutine

    !> Get CRSS for i-th grain.
    !> maximum number of slip systems restricted to 24
    subroutine KS_getCRSS(i, Mcrss, info)
        integer,intent(in)      :: i        
        integer,intent(out)     :: info
        type(CRSS),intent(out)  :: Mcrss    
        integer                 :: l   
        
        info = VEF_BADDIMS
        if (size(KS_state) < i) return
        !Corresponds to the number of slip systems
        l = min(24, ubound(Mcrss%crss,2)) 
        !Extract the CRSSes
        Mcrss%crss(:,1:l) = KS_state(i)%CRSS(:,1:l)
        info = VEF_OK
    end subroutine

    !> Retrieve state-derived variables for the i-th grain.
    subroutine KS_getSDV(i,SDV,info)
        integer,intent(in)                  :: i    
        type(StateDerivedVars), intent(out) :: SDV
        integer,intent(out)                 :: info 
        
        info = VEF_BADDIMS
        if (size(KS_state) < i) return

        call GetStateDerivedVar(KS_state(i),SDV,info)
    end subroutine


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
    integer function KS_writeState(iounit) result(info)
        integer, intent(in) :: iounit   
        integer             :: i, n
    
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
    integer function KS_readState_unit(iounit, nblock) result(info)
        integer,intent(in)              :: iounit
        integer, optional, intent(in)   :: nblock
        integer                         :: i, n, nf, tmp, ioerr, iblock 
        character(len=5)                :: tmp_str
        logical                         :: is_dummy
        
        info = VEF_Uninitialized
        if (.not. allocated(KS_state)) return
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
    integer function KS_readState_file(fname,iounit,nblock,use_header) result(info)
        character(len=*), intent(in)    :: fname    
        integer, intent(in)             :: iounit  
        integer, optional, intent(in)   :: nblock 
        logical, optional, intent(in)   :: use_header

        ! open file for read access ('r')
        info = KS_openStateFile(iounit, fname, 'r', use_header) 
        if (info == 0) then
            info = KS_readState_unit(iounit, nblock)
        else
            info = VEF_IO
        endif
        close(iounit)
    end function
end module
