module altayDSHState
use altayHardLaw_DSH
use altayHardTypes
implicit none

      type(StatVar),allocatable,dimension(:),private    :: KS_state ! array of state variables

      interface KS_readState
            module procedure KS_readState_unit, KS_readState_file
      end interface

contains


      !> Query the number of elements in the state array.
      integer function KS_getStateSize()
      !
            KS_getStateSize = 0
            if (allocated(KS_state)) KS_getStateSize = size(KS_state)
      !
      end function


      !> Allocate memory to the KS_state array.
      !>
      !> The function simply makes allocation. It relies on a default initializer
      !> of StatVar type.
      integer function KS_initState(norient) result(info)
      integer,intent(in)      :: norient !< Number of orientations in the material
      !
      integer :: i
      !
            info = KS_ErrBadDims
            if (norient > 0) allocate(KS_state(norient),stat=info)
            if (info /= 0) return
            ! All elements (orientations) of the KS_state array must have the same initial state.
            call GetInitStatVar(KS_state(1),info)
            if (info /= KS_OK) return
            do i = 2, norient
                  KS_state(i) = KS_state(1)
            enddo
      !
      end function


      !> Deallocate the KS_state array.
      integer function KS_finalize() result(info)
      integer :: memstat
      !
            info = KS_OK
            if (allocated(KS_state)) then
                  deallocate(KS_state,stat=memstat)
                  if (memstat /= 0) info = KS_Error
            endif
      !
      end function


      !> Update the state variables of the PEBP model for i-th grain.
      subroutine KS_updateState(i,sliprate,deltaT,info)
      integer,intent(in)                              :: i        !< Grain identifier
      double precision,intent(in), dimension(24)      :: sliprate !< slip rates on 2*12 slip systems
      double precision,intent(in)                     :: deltaT   !< Time increment
      integer,intent(out)                             :: info
      !
      type(StatVar) :: SV_tmp
      !
            info = KS_ErrBadDims
            if (size(KS_state) < i) return
            !
            call MakeInc(KS_state(i),sliprate,deltaT,SV_tmp,info)
            if (info /= 0) return
            ! Update the state of the i-th grain
            KS_state(i) = SV_tmp
      !
      end subroutine


      !> Get CRSS for i-th grain.
      subroutine KS_getCRSS(i,Mcrss,info)
      integer,intent(in)                              :: i        !< Grain identifier
      integer,intent(out)                             :: info
      type(CRSS),intent(out)                          :: Mcrss    !< CRSS output
      integer l   !< maximum number of slip systems restricted to 24
      !
            info = KS_ErrBadDims
            if (size(KS_state) < i) return
            !if ( any(shape(Mcrss) /= shape(KS_state(i)%CRSS)) ) return
            l = min(24, ubound(Mcrss%crss,2)) ! corresponds to the number of slip systems
            ! Extract the CRSSes
            Mcrss%crss(:,1:l) = KS_state(i)%CRSS(:,1:l)
            info = KS_OK
      !
      end subroutine


      !> Retrieve state-derived variables for the i-th grain.
      subroutine KS_getSDV(i,SDV,info)
      integer,intent(in)                              :: i    !< Grain identifier
      type(StateDerivedVars), intent(out)             :: SDV
      integer,intent(out)                             :: info !< exit code
      !
            info = KS_ErrBadDims
            if (size(KS_state) < i) return
            call GetStateDerivedVar(KS_state(i),SDV,info)
      !
      end subroutine


      !> Open state file either for reading or writing.
      !>
      !> The function opens the file and, if requested, performs some initialization
      !> actions, such as processing or writing file header.
      integer function KS_openStateFile(iounit,fname,mode,use_header) result(info)
      integer,intent(in)                              :: iounit   !< IO unit to be used
      character(len=*),intent(in)                     :: fname    !< Name of the file
      !> File opening mode: 'r' for read access or 'w' for write access
      character,intent(in)                            :: mode
      !> Request for processing  the file header. Default is: .true.
      logical,optional,intent(in)                     :: use_header
      !
      logical :: is_header
      integer :: ierr
      !
            info = -1
            is_header = .true.
            if (present(use_header))  is_header = use_header
            select case(mode)
            case('r')
                  open(unit=iounit,file=fname,status='old',iostat=ierr) ! open existing file (status='old')
                  if (ierr /= 0) return
                  if (is_header) info = ReadHeadSVfile(iounit)
            case('w')
                  open(unit=iounit,file=fname,status='replace',iostat=ierr) ! replace if already existing
                  if (ierr /= 0) return
                  if (is_header) info = WriteHeadSVfile(iounit)
            end select
      !
      end function


      !> Write block (=snapshot of KS_state) into file.
      integer function KS_writeState(iounit) result(info)
      integer,intent(in)                              :: iounit   !< I/O unit number
      !
      integer :: i, &    !< loop counter
                  n      !< size of KS_state
      !
            info = KS_ErrIO
            n = size(KS_state)
            write(iounit,fmt=100) n
            write(iounit,fmt=110)
            do i = 1, n
                  write(iounit,fmt=200) i
                  if (WriteSVfile(iounit,KS_state(i)) /= KS_OK) exit
            enddo
            write(iounit,fmt=111)
            if (i > n) info = KS_OK

100         format(I5,1X,' # of points in KOST11 block')
110         format('-->')
111         format('<--')
200         format(I5)
      !
      end function


      !> Call ReadSVfile and store state variables in ks_state.
      !> Perform fake reads on the first nblock blocks, where each block corresponds to one snapshot of ks_state.
      integer function KS_readState_unit(iounit,nblock) result(info)
      integer,intent(in)                              :: iounit   !< I/O unit number
      integer,optional,intent(in)                     :: nblock   !< Number of blocks to be skipped
      !
      integer :: i, & !< index for loop over ks_state elements
                 n, & !< number of elements (=grains) in ks_state
                 nf, & !< number of grains per block in file
                 tmp, ioerr, &
                 iblock !< index for loop over blocks (=snapshots)
      character(len=5) :: tmp_str
      logical :: is_dummy
      !
            info = KS_ErrUninitialized
            if (.not. allocated(KS_state)) return
            n = size(KS_state) ! number of grains
            if (n < 1) return ! stop if ks_state is empty
            info = KS_ErrIO
            nf = 0
            is_dummy = .true. ! set fake read flag to true
            !
            do iblock = 0, nblock ! loop over blocks
                  if (iblock == nblock) is_dummy = .false. ! read number of grains from file and exit if not agree with ks_state
                  read(iounit,fmt=100,iostat=ioerr) nf
                  if ((nf /= n) .or. (ioerr /= 0)) return
                  read(iounit,fmt=110) tmp_str
                  do i = 1, n ! loop till i=size(ks_state)
                        read(iounit,fmt=200,iostat=ioerr) tmp
                        if (ioerr /= 0) return
                        if (ReadSVfile(iounit,KS_state(i),is_dummy) /= KS_OK) return ! read from iounit into KS_state(i)
                  enddo
                  read(iounit,fmt=111,iostat=ioerr) tmp_str
                  if (.not.is_dummy) exit ! exit do loop if none-fake read took place
            enddo
            if ((i > n) .and. (ioerr == 0)) info = KS_OK ! if all went well return success code (ks_ok)
100         format(I5)
110         format(A)
111         format(A)
200         format(I5)      !
      end function


      !> Open state file for reading and read state variables of snapshot.
      !> The snapshot (block) to be read is specified by nblock.
      integer function KS_readState_file(fname,iounit,nblock,use_header) result(info)
      character(len=*),intent(in)                     :: fname    !< Filename
      integer,intent(in)                              :: iounit   !< I/O unit number to be used by the function
      integer,optional,intent(in)                     :: nblock   !< Number of blocks to be skipped
      logical,optional,intent(in)                     :: use_header
      !
            info = KS_openStateFile(iounit,fname,'r',use_header) ! open file for read access ('r')
            if (info == 0) then
                  info = KS_readState_unit(iounit,nblock)
            else
                  info = KS_ErrIO
            endif
            close(iounit)
      !
      end function

end module
