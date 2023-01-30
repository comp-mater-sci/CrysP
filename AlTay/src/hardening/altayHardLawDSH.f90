module altayHardLaw_DSH
    use altay_definitions
    use altayHardTypes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     KOST=11 & PRE-file contains 24 (110)+(112)[111] slip systems;
!     -----------------------------------------------------
!     The original 'Bart Peeters hardening model', as described in:
!     PhD B. Peeters, MTM, 2002, paragraph 3.2.2: 'Mesoscopic model'
!     This implementation differs only in a few details:

!     (1) The sqrt() [square root] in Eq. (3.15), is replaced in this implementation
!      with tanh() [tangent hyperbolic]. This replacement was also found
!      in the original source code by B. Peeters.

!     (2) Eq. (3.17) (evolution equation of RHO) is integrated here analyticaly,
!      while in the PhD, it is mentioned that a Runge-Kutta method is used.
!      Differences in results (in LST-, CUR-, RES-files) between both methods
!      are only marginal. Explicit integration requires less operations and is
!      more accurate.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IMPLICIT NONE

      !> BP model parameters including saturation and minimum values for state dependent dislocation densities
      TYPE :: PAR
            !PUBLIC components
            double precision :: b,G,alfa,f,tau0
            double precision :: I,R,Iwd,Rwd,Rncg,beta1,beta2
            double precision :: Iwp,Rwp,Rrev,R2
            double precision :: RHOcbSAT,RHOwdSAT,RHOwpSAT
            double precision :: RHOcbMIN,RHOwdMIN,RHOwpMIN
            double precision :: RHOwpLOW
      END TYPE PAR

      TYPE :: CBBtype
            !PUBLIC components
            double precision :: RHOwd = 0.D0
            double precision :: RHOwp = 0.D0
            double precision :: RHOwdHOM = 0.D0
            double precision :: accGAMMA_new = 0.D0
            double precision :: RHOwd_ini = 0.D0
      END TYPE CBBtype

      !> State variables for single grain
      TYPE :: StatVar
      !PUBLIC components
            double precision                    :: RHOcb = 0.D0
            TYPE(CBBtype), DIMENSION(6)         :: CBB
            integer, DIMENSION(2)               :: ActiveCBB = 0
            double precision, DIMENSION(2,24)   :: CRSS = 0.D0 !Up to 24 slip systems supported
      END TYPE StatVar

      TYPE :: StateDerivedVars
            !> Dislocation density of cell boundaries; unit: m^(-2)
            double precision :: rho_CBs = 0.D0
            !> Dislocation density of cell block boundaries; unit: m^(-2)
            double precision :: rho_CBBs = 0.D0
            !> Dislocation density of polarized dislocations at cell block boundaries; unit: m^(-2)
            double precision :: rho_polCBBs = 0.D0
            !> Average dislocation density; unit: m^(-2)
            double precision :: rho_avg = 0.D0
      END TYPE

      INTERFACE OPERATOR(+)
            MODULE PROCEDURE  StateDerivedVar_plus
      END INTERFACE

      INTERFACE OPERATOR(*)
            MODULE PROCEDURE  StateDerivedVar_times
      END INTERFACE

      INTERFACE InitModuleAltayHardLaw_DSH !Generic Interface
        MODULE PROCEDURE Init_file,Init_PAR
      END INTERFACE

      PUBLIC                    &
      !procedures:
            InitModuleAltayHardLaw_DSH,   &
            ReadPar,            &
            GetInitStatVar,     &
            MakeInc,            &
            WriteHeadSVfile,    &
            ReadHeadSVfile,     &
            WriteSVfile,        &
            ReadSVfile,         &
            GetStateDerivedVar, &
            WriteSDV,     &
      !operators
            operator(+),        &
            operator(*),        &
      !derived types:
            PAR,                &
            StatVar,            &
            CBBtype,            &
            StateDerivedVars

      !> \name Exit codes from altayHardLaw_DSH subroutines and functions:
      !>@{
      integer,PARAMETER,PUBLIC :: KS_OK = 0           !< OK
      integer,PARAMETER,PUBLIC :: KS_Error = -1       !< General error (not covered by any specific error code).
      integer,PARAMETER,PUBLIC :: KS_ErrBadDims = -2  !< At least one parameter out of boundaries
      integer,PARAMETER,PUBLIC :: KS_ErrBadValue = -5 !< At least one input parameter has unacceptable value
      integer,PARAMETER,PUBLIC :: KS_ErrOutOfRange = -6 !< At least one input parameter has a value outside acceptable range
      integer,PARAMETER,PUBLIC :: KS_ErrIO = -15      !< Error during an IO operation
      integer,PARAMETER,PUBLIC :: KS_ErrNss = -16     !< Unsupported number of slip systems proposed. Supported values are: 12, 24
      integer,PARAMETER,PUBLIC :: KS_ErrUninitialized = -50 !< Call to module procedures without proper initialization of the module
      !>@}

      integer, SAVE, PUBLIC :: iKOST=0
      !Remaining declarations all PRIVATE:
      TYPE(PAR), SAVE :: P !unit system: MPa; micrometer
      logical, SAVE :: InitOK=.FALSE.
      integer, SAVE :: Nss !Number of slip systems. Supported values:
                           !     Nss=12: (110)[111] - 1 family
                           !     Nss=24: (110)+(112)[111] - 2 families
      integer, PRIVATE :: i !running index
      double precision, SAVE :: alfa_G_b
      double precision, SAVE, DIMENSION(24,6):: eff             = 0.D0 ,&
                                                effslashb       = 0.D0 ,&
                                                alfa_G_b_eff    = 0.D0 ,&
                                                alfa_G_b_ABSeff = 0.D0

      double precision, PARAMETER :: MINfrac= 2.0D-3
      double precision, PARAMETER :: LOWfrac=10.0D-3

      double precision, PARAMETER :: TENpow6 = 1.D6

      double precision, PARAMETER :: p2= 1.D0/sqrt(2.D0)
      double precision, PARAMETER :: n2= -p2
      double precision, PARAMETER :: p3= 1.D0/sqrt(3.D0)
      double precision, PARAMETER :: n3= -p3
      double precision, PARAMETER :: p6= 1.D0/sqrt(6.D0)
      double precision, PARAMETER :: n6= -p6
      double precision, PARAMETER :: pd6= 2.D0/sqrt(6.D0)
      double precision, PARAMETER :: nd6= -pd6
      !double precision, PARAMETER :: p1_42= 0.154303349962 !1.0/sqrt(42.0)
      !double precision, PARAMETER :: n1_42=-0.154303349962
      !double precision, PARAMETER :: p4_42= 0.617213399848 !4.0/sqrt(42.0)
      !double precision, PARAMETER :: n4_42=-0.617213399848
      !double precision, PARAMETER :: p5_42= 0.771516749810 !5.0/sqrt(42.0)
      !double precision, PARAMETER :: n5_42=-0.771516749810

      !EdgeDir(s,1:3): normalized movement vector of EDGE disl. on slip system s
      !               (it equals the normalized burgers vector of slip system s)
      double precision, SAVE, DIMENSION(24,3)::EdgeDir
      DATA (EdgeDir( 1: 3,i),i=1,3) /3*p3,3*p3,3*p3/ !s.s. 1 to 3
      DATA (EdgeDir( 4: 6,i),i=1,3) /3*n3,3*n3,3*p3/ !s.s. 4 to 6
      DATA (EdgeDir( 7: 9,i),i=1,3) /3*n3,3*p3,3*p3/ !..
      DATA (EdgeDir(10:12,i),i=1,3) /3*p3,3*n3,3*p3/ !..
      DATA (EdgeDir(13:15,i),i=1,3) /3*p3,3*p3,3*p3/ !..
      DATA (EdgeDir(16:18,i),i=1,3) /3*n3,3*n3,3*p3/ !..
      DATA (EdgeDir(19:21,i),i=1,3) /3*n3,3*p3,3*p3/ !..
      DATA (EdgeDir(22:24,i),i=1,3) /3*p3,3*n3,3*p3/ !s.s. 21 to 24

      !ScrewDir(s,1:3): normalized movement vector of SCREW disl. on slip system s
      !  If NormSS(s,:) denotes slip plane normal vector and x the cross product, then:
      !      ScrewDir(s,:) = EdgeDir(s,:) x NormSS(s,:)
      double precision, SAVE, DIMENSION(24,3)::ScrewDir
      DATA (ScrewDir(01,i),i=1,3) /nd6,p6,p6/ !s.s. 01
      DATA (ScrewDir(02,i),i=1,3) /p6,nd6,p6/ !s.s. 02
      DATA (ScrewDir(03,i),i=1,3) /p6,p6,nd6/ !s.s. 03
      DATA (ScrewDir(04,i),i=1,3) /pd6,n6,p6/ !s.s. 04
      DATA (ScrewDir(05,i),i=1,3) /n6,pd6,p6/ !s.s. 05
      DATA (ScrewDir(06,i),i=1,3) /n6,n6,nd6/ !s.s. 06
      DATA (ScrewDir(07,i),i=1,3) /nd6,n6,n6/ !s.s. 07
      DATA (ScrewDir(08,i),i=1,3) /p6,pd6,n6/ !s.s. 08
      DATA (ScrewDir(09,i),i=1,3) /p6,n6,pd6/ !s.s. 09
      DATA (ScrewDir(10,i),i=1,3) /pd6,p6,n6/ !s.s. 10
      DATA (ScrewDir(11,i),i=1,3) /n6,nd6,n6/ !s.s. 11
      DATA (ScrewDir(12,i),i=1,3) /n6,p6,pd6/ !s.s. 12
      DATA (ScrewDir(13,i),i=1,3) /0.,p2,n2/ !s.s. 13
      DATA (ScrewDir(14,i),i=1,3) /n2,0.,p2/ !s.s. 14
      DATA (ScrewDir(15,i),i=1,3) /p2,n2,0./ !s.s. 15
      DATA (ScrewDir(16,i),i=1,3) /0.,n2,n2/ !s.s. 16
      DATA (ScrewDir(17,i),i=1,3) /p2,0.,p2/ !s.s. 17
      DATA (ScrewDir(18,i),i=1,3) /n2,p2,0./ !s.s. 18
      DATA (ScrewDir(19,i),i=1,3) /0.,n2,p2/ !s.s. 19
      DATA (ScrewDir(20,i),i=1,3) /n2,0.,n2/ !s.s. 20
      DATA (ScrewDir(21,i),i=1,3) /p2,p2,0./ !s.s. 21
      DATA (ScrewDir(22,i),i=1,3) /0.,p2,p2/ !s.s. 22
      DATA (ScrewDir(23,i),i=1,3) /p2,0.,n2/ !s.s. 23
      DATA (ScrewDir(24,i),i=1,3) /n2,n2,0./ !s.s. 24
      !DATA (ScrewDir(25,i),i=1,3) /n5_42,p4_42,p1_42/ !s.s. 25
      !DATA (ScrewDir(26,i),i=1,3) /n4_42,p5_42,n1_42/ !s.s. 26
      !DATA (ScrewDir(27,i),i=1,3) /p5_42,n1_42,n4_42/ !s.s. 27
      !DATA (ScrewDir(28,i),i=1,3) /p4_42,p1_42,n5_42/ !s.s. 28
      !DATA (ScrewDir(29,i),i=1,3) /p1_42,n5_42,p4_42/ !s.s. 29
      !DATA (ScrewDir(30,i),i=1,3) /n1_42,n4_42,p5_42/ !s.s. 30
      !DATA (ScrewDir(31,i),i=1,3) /p5_42,n4_42,p1_42/ !s.s. 31
      !DATA (ScrewDir(32,i),i=1,3) /p4_42,n5_42,n1_42/ !s.s. 32
      !DATA (ScrewDir(33,i),i=1,3) /p5_42,n1_42,p4_42/ !s.s. 33
      !DATA (ScrewDir(34,i),i=1,3) /p4_42,p1_42,p5_42/ !s.s. 34
      !DATA (ScrewDir(35,i),i=1,3) /p1_42,n5_42,n4_42/ !s.s. 35
      !DATA (ScrewDir(36,i),i=1,3) /n1_42,n4_42,n5_42/ !s.s. 36
      !DATA (ScrewDir(37,i),i=1,3) /n5_42,n4_42,p1_42/ !s.s. 37
      !DATA (ScrewDir(38,i),i=1,3) /n4_42,n5_42,n1_42/ !s.s. 38
      !DATA (ScrewDir(39,i),i=1,3) /n5_42,n1_42,p4_42/ !s.s. 39
      !DATA (ScrewDir(40,i),i=1,3) /n4_42,p1_42,p5_42/ !s.s. 40
      !DATA (ScrewDir(41,i),i=1,3) /p1_42,p5_42,p4_42/ !s.s. 41
      !DATA (ScrewDir(42,i),i=1,3) /n1_42,p4_42,p5_42/ !s.s. 42
      !DATA (ScrewDir(43,i),i=1,3) /p5_42,p4_42,p1_42/ !s.s. 43
      !DATA (ScrewDir(44,i),i=1,3) /p4_42,p5_42,n1_42/ !s.s. 44
      !DATA (ScrewDir(45,i),i=1,3) /n5_42,n1_42,n4_42/ !s.s. 45
      !DATA (ScrewDir(46,i),i=1,3) /n4_42,p1_42,n5_42/ !s.s. 46
      !DATA (ScrewDir(47,i),i=1,3) /p1_42,p5_42,n4_42/ !s.s. 47
      !DATA (ScrewDir(48,i),i=1,3) /n1_42,p4_42,n5_42/ !s.s. 48

      !NormDir(s,1:3): normalized slip plane normal vector of slip system s
      double precision, SAVE, DIMENSION(24,3)::NormDir
      DATA (NormDir(01,i),i=1,3) /0.,p2,n2/ !s.s. 01
      DATA (NormDir(02,i),i=1,3) /n2,0.,p2/ !s.s. 02
      DATA (NormDir(03,i),i=1,3) /p2,n2,0./ !s.s. 03
      DATA (NormDir(04,i),i=1,3) /0.,n2,n2/ !s.s. 04
      DATA (NormDir(05,i),i=1,3) /p2,0.,p2/ !s.s. 05
      DATA (NormDir(06,i),i=1,3) /n2,p2,0./ !s.s. 06
      DATA (NormDir(07,i),i=1,3) /0.,p2,n2/ !s.s. 07
      DATA (NormDir(08,i),i=1,3) /p2,0.,p2/ !s.s. 08
      DATA (NormDir(09,i),i=1,3) /n2,n2,0./ !s.s. 09
      DATA (NormDir(10,i),i=1,3) /0.,n2,n2/ !s.s. 10
      DATA (NormDir(11,i),i=1,3) /n2,0.,p2/ !s.s. 11
      DATA (NormDir(12,i),i=1,3) /p2,p2,0./ !s.s. 12
      DATA (NormDir(13,i),i=1,3) /pd6,n6,n6/ !s.s. 13
      DATA (NormDir(14,i),i=1,3) /n6,pd6,n6/ !s.s. 14
      DATA (NormDir(15,i),i=1,3) /n6,n6,pd6/ !s.s. 15
      DATA (NormDir(16,i),i=1,3) /nd6,p6,n6/ !s.s. 16
      DATA (NormDir(17,i),i=1,3) /p6,nd6,n6/ !s.s. 17
      DATA (NormDir(18,i),i=1,3) /p6,p6,pd6/ !s.s. 18
      DATA (NormDir(19,i),i=1,3) /nd6,n6,n6/ !s.s. 19
      DATA (NormDir(20,i),i=1,3) /p6,pd6,n6/ !s.s. 20
      DATA (NormDir(21,i),i=1,3) /p6,n6,pd6/ !s.s. 21
      DATA (NormDir(22,i),i=1,3) /pd6,p6,n6/ !s.s. 22
      DATA (NormDir(23,i),i=1,3) /n6,nd6,n6/ !s.s. 23
      DATA (NormDir(24,i),i=1,3) /n6,p6,pd6/ !s.s. 24

      !CBBnormal(i,1:3): normalized vector normal to CBB i
      double precision, SAVE, DIMENSION(6,3)::CBBnormal
      DATA (CBBnormal(1,i),i=1,3) /0.,p2,n2/ !CBBs on (01-1)-plane
      DATA (CBBnormal(2,i),i=1,3) /n2,0.,p2/ !CBBs on (-101)-plane
      DATA (CBBnormal(3,i),i=1,3) /p2,n2,0./ !CBBs on (1-10)-plane
      DATA (CBBnormal(4,i),i=1,3) /0.,n2,n2/ !CBBs on (0-1-1)-plane
      DATA (CBBnormal(5,i),i=1,3) /p2,0.,p2/ !CBBs on (101)-plane
      DATA (CBBnormal(6,i),i=1,3) /n2,n2,0./ !CBBs on (-1-10)-plane



      type(StatVar),allocatable,dimension(:),private    :: KS_state ! array of state variables

      interface KS_readState
            module procedure KS_readState_unit, KS_readState_file
      end interface



      CONTAINS


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



      !> Initialization of altayHardLaw_DSH.
      !>
      !> \return This procedure returns an error code (iError):
      !>    * KS_OK : no error
      !>    * KS_ErrBadValue : incorrect value of KOSTtry for the inputted parameter-type
      !>    * KS_ErrOutOfRange : (at least one) parameter out of boundaries
      !>    * KS_ErrIO : slipsystem file (read from LEC) does not meet requirements about its format
      integer FUNCTION Init_PAR(Ptry,KOSTtry,LEC) result(iError)
      TYPE(PAR),INTENT(IN)   :: Ptry    !proposed parameter set
      integer    ,INTENT(IN) :: KOSTtry !proposed value of KOST
      integer    ,INTENT(IN) :: LEC !unit number of PRE-file

      !local variables declarations:
      character(LEN=128) :: line1
      integer           :: s,i,Idum=0,Nsstry=0

      InitOK=.FALSE.

      !Check KOSTtry
      select case (KOSTtry)
      case (11,12,13) !supported
          iKOST=KOSTtry !iKOST: PRIVATE to this module.
      case default !unsupported
          iError = KS_ErrBadValue
          return
      end select

      !Check PRE-file #1: Does 1st comment line contain strings 'BCC' and '{BP}'?
      rewind (unit=LEC)
      read (LEC,FMT='(A)') line1 !line1
      if ( (index(line1,'BCC') == 0) .or. (index(line1,'{BP}') == 0)) then
        iError = KS_ErrIO
        return
      end if !File is OK.

      !Check PRE-file #2: Is number of slip systems (Nss) supported?
      read (LEC,FMT='(8I4)') Idum, Nsstry, Idum, Idum, Idum, Idum, Idum, Idum
      rewind (unit=LEC)
      select case (Nsstry)
      case (12, 24) !supported number of slip systems
          Nss=Nsstry
      case default !unsupported number of slip systems specified  in LEC
          iError = KS_ErrNss
          return
      end select

      !Check the input parameters                              ! Units of input parameters:
      if(Ptry%b    >  0.    .AND. Ptry%b    <= 1.e-8    .AND.& ! [m]
         Ptry%G    >= 10.e3 .AND. Ptry%G    <= 500.e3   .AND.& ! [MPa]
         Ptry%alfa >  0.    .AND. Ptry%alfa <= 5.       .AND.& ! [/]
         Ptry%f    >= 0.    .AND. Ptry%f    <= 1.       .AND.& ! [/]
         Ptry%tau0 >= 0.    .AND. Ptry%tau0 <= 1.e4     .AND.& ! [MPa]
         Ptry%I    >= 0.    .AND. Ptry%I    <= 10.      .AND.& ! [/]
         Ptry%Iwd  >= 0.    .AND. Ptry%Iwd  <= 10.      .AND.& ! [/]
         Ptry%Iwp  >= 0.    .AND. Ptry%Iwp  <= 10.      .AND.& ! [/]
         Ptry%R    >  0.    .AND. Ptry%R    <= 1.e-6    .AND.& ! [m]
         Ptry%Rwd  >  0.    .AND. Ptry%Rwd  <= 1.e-6    .AND.& ! [m]
         Ptry%Rncg >  0.    .AND. Ptry%Rncg <= 1.e-6    .AND.& ! [m]
         Ptry%Rwp  >  0.    .AND. Ptry%Rwp  <= 1.e-6    .AND.& ! [m]
         Ptry%Rrev >  0.    .AND. Ptry%Rrev <= 1.e-6    .AND.& ! [m]
         Ptry%R2   >  0.    .AND. Ptry%R2   <= 1.e-6    .AND.& ! [m]
         Ptry%beta1>= 0.    .AND. Ptry%beta1<= 100.     .AND.& ! [/]
         Ptry%beta2>= 0.    .AND. Ptry%beta2<= 100.          & ! [/]
          )then
            !Save the parameters in P (private to this module)
            P=Ptry
            !change of units if different in Ptry from P (units of P are: MPa; micrometer)
            P%b    = P%b    * TENpow6 ![m] -> [micrometer]
            P%R    = P%R    * TENpow6 ![m] -> [micrometer]
            P%Rwd  = P%Rwd  * TENpow6 ![m] -> [micrometer]
            P%Rncg = P%Rncg * TENpow6 ![m] -> [micrometer]
            P%Rwp  = P%Rwp  * TENpow6 ![m] -> [micrometer]
            P%Rrev = P%Rrev * TENpow6 ![m] -> [micrometer]
            P%R2   = P%R2   * TENpow6 ![m] -> [micrometer]
          else
            iError = KS_ErrOutOfRange
            return
      end if

      !Calculate dependent hardening parameters
      P%RHOcbSAT=P%I  * P%I  /( P%R  * P%R  )
      P%RHOwdSAT=P%Iwd* P%Iwd/( P%Rwd* P%Rwd)
      P%RHOwpSAT=(sqrt((P%Iwp/P%Rwp)**4 +               &
                 4.D0*(P%Iwp*P%Iwd/(P%Rwp*P%Rwd))**2) +   &
                 (P%Iwp/P%Rwp)**2)/2.D0

      P%RHOcbMIN=  MINfrac * P%RHOcbSAT
      P%RHOwdMIN=  MINfrac * P%RHOwdSAT
      P%RHOwpMIN=  MINfrac * P%RHOwpSAT

      P%RHOwpLOW=  LOWfrac * P%RHOwpSAT

      !Calculate "Wall-effectivity"-matrices
      select case(iKOST)
      case(13) ! "LoopSlip": introduced in v.1.11; invokable through KOST=13
          do s=1,24
            do i=1,6
              eff(s,i)=DOT_PRODUCT( NormDir(s,:) , CBBnormal(i,:) )
              if (abs(eff(s,i)) >= 0.99999D0) then !treat as "1" or "-1"
                  eff(s,i)=0.0D0
              else
                  eff(s,i)=sqrt(1.0D0-(eff(s,i))**2)
              endif
            end do
          end do
      case(12) ! "ScrewSlip": introduced in v.1.9; invokable through KOST=12 in v.1.10
          do s=1,24
            do i=1,6
              eff(s,i)=DOT_PRODUCT( ScrewDir(s,:) , CBBnormal(i,:) )
            end do
          end do
      case(11) ! according to PhD Peeters
          do s=1,24
            do i=1,6
              eff(s,i)=DOT_PRODUCT( EdgeDir(s,:) , CBBnormal(i,:) )
            end do
          end do
      end select
      effslashb       = eff / P%b
      alfa_G_b= P%alfa* P%G * P%b
      alfa_G_b_eff    = alfa_G_b * eff
      alfa_G_b_ABSeff = ABS(alfa_G_b_eff)

      !If control passes here, initialization is done without errors
      InitOK=.TRUE. !PRIVATE to this module
      iError=KS_OK      !OUT

      END FUNCTION Init_PAR



      integer FUNCTION Init_file(inunit,KOST,LEC) result(info)
      integer,intent(in)      :: inunit   !< number of
      integer,intent(in)      :: KOST     !< Id of the model version.
      integer,intent(in)      :: LEC
      !
      TYPE(PAR) :: PARtry
      !
      info = KS_Error
      select case(KOST)
      case(11,12,13)
            ! Supported value of KOST
            ! Read parameters of PE-BP hardening model
            if (ReadPar(inunit,KOST,PARtry) == 0) then
                  info = Init_PAR(PARtry,KOST,LEC)
            endif
      case default
            info = KS_ErrBadValue !Unsupported value of KOST
      end select
      !
      end FUNCTION Init_file



      integer FUNCTION ReadPar(inunit,KOST,Pf)
      integer,intent(in)      :: inunit   !< IO unit number
      integer,intent(in)      :: KOST     !< model identifier
      TYPE(PAR),INTENT(OUT) :: Pf       !< Parameters to be read from a formatted file.
      !
      read(inunit,fmt=100,err=666,end=666) Pf%b
      read(inunit,fmt=100,err=666,end=666) Pf%G
      read(inunit,fmt=100,err=666,end=666) Pf%alfa
      read(inunit,fmt=100,err=666,end=666) Pf%f
      read(inunit,fmt=100,err=666,end=666) Pf%tau0
      read(inunit,fmt=100,err=666,end=666) Pf%I
      read(inunit,fmt=100,err=666,end=666) Pf%R
      read(inunit,fmt=100,err=666,end=666) Pf%Iwd
      read(inunit,fmt=100,err=666,end=666) Pf%Rwd
      read(inunit,fmt=100,err=666,end=666) Pf%Rncg
      read(inunit,fmt=100,err=666,end=666) Pf%beta1
      read(inunit,fmt=100,err=666,end=666) Pf%beta2
      read(inunit,fmt=100,err=666,end=666) Pf%Iwp
      read(inunit,fmt=100,err=666,end=666) Pf%Rwp
      read(inunit,fmt=100,err=666,end=666) Pf%Rrev
      read(inunit,fmt=100,err=666,end=666) Pf%R2
100   format(F12.5)
      !
      ReadPar = KS_OK
      return
      !
666   ReadPar = KS_ErrIO !Error in reading from file
      !
      end FUNCTION ReadPar



      SUBROUTINE GetInitStatVar(SV0,iError)
      !This procedure returns:
      ! state variables for an annealed & undeformed substructure for single grain (SV0)
      ! an error code (iError):
      !      KS_OK , no error
      !      KS_ErrUninitialized, in case this module is not correctly initialized

      TYPE(StatVar),INTENT(OUT) :: SV0
      integer,      INTENT(OUT) :: iError

      if(.NOT.InitOK) then
            iError = KS_ErrUninitialized
            return
      end if
      iError=KS_OK

      SV0%RHOcb               = P%RHOcbMIN
      SV0%CBB(:)%RHOwd        = P%RHOwdMIN
      SV0%CBB(:)%RHOwp        = 0.
      SV0%CBB(:)%RHOwdHOM     = P%RHOwdMIN
      SV0%CBB(:)%accGAMMA_new = 0.
      SV0%CBB(:)%RHOwd_ini    = P%RHOwdMIN
      SV0%ActiveCBB(:)        = 0
      SV0%CRSS                = F_CRSS(SV0)

      END SUBROUTINE GetInitStatVar



      SUBROUTINE MakeInc(SVa,sliprate,deltaT,SVb,iError)
      !This procedure requires as input:
      ! state variable at beginning of increment (SVa)
      ! the slip rates, assumed constant throughout the increment (sliprate)
      ! the time increment (deltaT)
      !This procedure returns:
      ! state variables at end of the increment (SVb)
      ! an error code (iError):
      !   *  KS_OK , no error
      !   *  KS_ErrBadValue, if negative deltaT is provided
      !   *  KS_ErrUninitialized, in case this module is not correctly initialized
      TYPE(StatVar),INTENT(IN)       :: SVa
      double precision,INTENT(IN), DIMENSION(24) :: sliprate
      double precision,INTENT(IN)                      :: deltaT
      TYPE(StatVar),INTENT(OUT)      :: SVb !OUT
      integer,INTENT(OUT)            :: iError

      !local variable declarations
      double precision :: SUMabsGamDot=0.,GAMMAdot_new=0.,RHObausch=0.
      double precision :: SUMabsGam   =0.,GAMMA_new   =0.
      double precision,    DIMENSION(6) :: GAMMAdot=0.,GAMMA=0.
      integer, DIMENSION(6) :: r
      integer :: j
      double precision :: fl,wd

      if(.NOT.InitOK) then
            SVb=SVa
            iError = KS_ErrUninitialized
            return
      end if
      iError = KS_OK

      !! Calc. quantities of slip rates and slips
      !! Identify currently generated and non-currently generated walls
      !!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ! 'Gam'   ~ small-caps gamma: for a slip system
      ! 'GAMMA' ~ large-caps GAMMA: for a wall


      SUMabsGamDot=sum(abs(sliprate(1:Nss)))
      SUMabsGam=SUMabsGamDot*deltaT
      !
      if (SUMabsGam < epsilon(0.D0)) then
            ! No slip rate in the current grain => no deformation, no update of the state
            SVb=SVa
            ! Issue error only on negative time increment.
            if (deltaT < 0.D0) iError = KS_ErrBadValue
            return
      endif

      GAMMAdot=F_GAMMAdot(sliprate)
      GAMMA=GAMMAdot*deltaT

      r= sort110planes(GAMMAdot) !sort the walls in r
      SVb%ActiveCBB(1)=r(1)
      SVb%ActiveCBB(2)=r(2)

      GAMMAdot_new=GAMMAdot(r(1))+GAMMAdot(r(2))
      GAMMA_new=GAMMAdot_new*deltaT

      !! Update dislocation densities
      !!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      RHObausch=0. !init.
      do j=1,2 !Loop over 2 currently generated walls
        !RHOwd
        SVb%CBB(r(j))%RHOwd= F_KocksMeck(SVa%CBB(r(j))%RHOwd             &
                                         ,GAMMA(r(j)),P%Iwd,P%Rwd)
        SVb%CBB(r(j))%RHOwdHOM= SVb%CBB(r(j))%RHOwd
        !RHOwp
        call UPD_cur_wp(r(j),SVa%CBB(r(j))%RHOwp,                        & !in
                             SVb%CBB(r(j))%RHOwp,RHObausch) !out
      end do

      do j=3,6 !Loop over 4 non-currently generated walls
        !RHOwd
        call UPD_ncg_wd(r(j),SVa,                                        & !in
                             SVb ) !out
        !RHOwp
        call UPD_ncg_wp(SVa%CBB(r(j))%RHOwp,                             & !in
                        SVb%CBB(r(j))%RHOwp ) !out
      end do

      !RHOcb
      call UPD_cb(RHObausch,SUMabsGam,SVa%RHOcb,                         & !in
                                      SVb%RHOcb ) !out

      !! Calculate Critical Resolved Shear Stresses
      !!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SVb%CRSS= F_CRSS(SVb)



      CONTAINS

      FUNCTION F_GAMMAdot(sr)
      ! Calculate the total slip rates on each of the six (110)-planes
      double precision, DIMENSION(24), INTENT(IN)  :: sr !Slip Rate
      double precision, DIMENSION( 6)              :: F_GAMMAdot !OUT

      F_GAMMAdot(1)= abs(sr( 1))+abs(sr( 7))!(01-1)-plane
      F_GAMMAdot(2)= abs(sr( 2))+abs(sr(11))!(-101)-plane
      F_GAMMAdot(3)= abs(sr( 3))+abs(sr( 6))!(1-10)-plane
      F_GAMMAdot(4)= abs(sr( 4))+abs(sr(10))!(0-1-1)-plane
      F_GAMMAdot(5)= abs(sr( 5))+abs(sr( 8))!(101)-plane
      F_GAMMAdot(6)= abs(sr( 9))+abs(sr(12))!(-1-10)-plane

      END FUNCTION F_GAMMAdot



      FUNCTION sort110planes(PlaneSlip)
      !For the six total PlaneSlips on the (110)-planes:
      !The plane of the largest PlaneSlip is identified by sort110planes(1).
      !The plane of 2nd-largest PlaneSlip is identified by sort110planes(2).
      !The remaining 4 planes are identified by            sort110planes(3:6).
      ! (note: the 4 remaining planes are not ordered from higher to lower total PlaneSlip!)
      double precision,    DIMENSION(6), INTENT(IN)  :: PlaneSlip
      integer, DIMENSION(6)              :: sort110planes !OUT

      !declaration of local variables
      integer r(6), i

      if (PlaneSlip(1) >= PlaneSlip(2)) then
        r(1)=1
        r(2)=2
      else
        r(1)=2
        r(2)=1
      end if

      do i=3,6
        if      (PlaneSlip(i) > PlaneSlip(r(1))) then
          r(i)=r(2)
          r(2)=r(1)
          r(1)=i
        else if (PlaneSlip(i) > PlaneSlip(r(2))) then
          r(i)=r(2)
          r(2)=i
        else
          r(i)=i
        end if
      end do

      sort110planes = r

      END FUNCTION sort110planes



      double precision FUNCTION F_KocksMeck(RHO_a,delta_g,II,RR) !PE27062012-2
      !Returns RHO_b, the value of RHO at the end of an interval (a,b)
      ! for the following differential equation:
      !
      ! d(RHO)    1
      ! ------ = --- * ( II*sqrt(RHO) - RR*RHO )
      !  d(g)    P%b
      !
      ! The value of 'P%b', the size of burgers vector, is inherited.
      !
      ! To calc. RHO_b, following inputs are required:
      !   -> RHO_a, the value of RHO at the start of the interval (a,b)
      !   -> delta_g = g_b - g_a, the increment in g during the interval (a,b)
      double precision ,INTENT(IN):: RHO_a,delta_g,II,RR

!     local variable declarations
      double precision x

      x=exp(-0.5D0*RR*delta_g/P%b)
      x=II/RR*(1.D0-x)+sqrt(RHO_a)*x
      F_KocksMeck=x*x

      END FUNCTION F_KocksMeck



      SUBROUTINE UPD_cur_wp(rdr,RHOwp_a,RHOwp_b,RHObausch)
      integer, INTENT(IN):: rdr
      double precision ,INTENT(IN)   :: RHOwp_a
      double precision ,INTENT(OUT)  :: RHOwp_b
      double precision, INTENT(INOUT):: RHObausch

      !inherited variables:
      !P%Iwd, P%Rwd, P%b
      !glidedir, wall
      !effslashb, sliprate
      !P%RHOwpSAT, P%RHOwpLOW
      !fl, wd

      !local variable declarations:
      double precision :: wpFLUX !wp-flux on the wall 'rdr'

      logical :: FLUXreversal,wpLOW

      wpFLUX=DOT_PRODUCT( effslashb(:,rdr) , sliprate(:) )

      FLUXreversal= wpFLUX*RHOwp_a  <  0.0
      wpLOW= abs(RHOwp_a)  <=  P%RHOwpLOW

      if ( FLUXreversal .and. .NOT.(wpLOW) ) then
        ! |RHOwp| gets smaller, following analytic time integration
        RHOwp_b=RHOwp_a*exp(-P%Rrev*abs(wpFLUX)*deltaT) !wpFLUX is a rate!
        RHObausch=RHObausch+abs(RHOwp_a)
      else
        ! |RHOwp| gets larger, following numeric time integration (4th order Runge-Kutta)
        fl=wpFLUX                       !to be used by RungeKutta->dwp_dt
        wd=SVb%CBB(rdr)%RHOwdHOM  !to be used by RungeKutta->dwp_dt
        if ( FLUXreversal ) then
          !In this case, it must also be that: wpLOW=.TRUE.
          !AFTER change of its sign, RHOwp will build up again.
          RHOwp_b = RungeKutta(-RHOwp_a)
        else
          RHOwp_b = RungeKutta( RHOwp_a)
        end if
        !RHObausch=RHObausch : No contribution to RHObausch
      end if

      END SUBROUTINE UPD_cur_wp



      FUNCTION RungeKutta(wpini)
      !This function returns the 4th order Runge-Kutta approximation
      !of the differential equation given by
      !
      !                             d(wp)/dt = F(wp)
      !
      ! The dif. eq. is implemented as another function: function dwp_dt(wp).
      !
      ! This function returns:
      ! RungeKutta = wpini + (K1+2*K2+2*K3+K4)/6
      !    in which     K1= deltaT * F(wpini     )
      !                       K2= deltaT * F(wpini+K1/2)
      !                       K3= deltaT * F(wpini+K2/2)
      !                       K4= deltaT * F(wpini+K3  )
      !    with wpini : the value of wp at the start of the increment
      !         deltaT: the time of the increment
      double precision, INTENT(IN) :: wpini
      double precision                RungeKutta   !OUT

      !local variable declarations:
      double precision, DIMENSION(4) :: K

      K(1)=deltaT*dwp_dt(wpini        )
      K(2)=deltaT*dwp_dt(wpini+K(1)/2.D0)
      K(3)=deltaT*dwp_dt(wpini+K(2)/2.D0)
      K(4)=deltaT*dwp_dt(wpini+K(3)   )
      RungeKutta=wpini+(K(1)+2.D0*K(2)+2.D0*K(3)+K(4))/6.D0

      END FUNCTION RungeKutta



      FUNCTION dwp_dt(wp)
      double precision, INTENT(IN) :: wp
      double precision             :: dwp_dt !OUT

      !inherited variables:
      !fl, wd
      !P%Iwp, P%Rwp

      dwp_dt=(sign(1.D0,fl)*P%Iwp*sqrt(wd+abs(wp)) - P%Rwp*wp) * abs(fl)

      END FUNCTION dwp_dt



      SUBROUTINE UPD_ncg_wp(RHOwp_a,RHOwp_b)
      double precision, INTENT(IN)  :: RHOwp_a
      double precision, INTENT(OUT) :: RHOwp_b

      !inherited variables:
      !P%Rncg, GAMMAdot_new, P%b, P%RHOwpMIN

      if (abs(RHOwp_a)  >  P%RHOwpMIN) then
        RHOwp_b= RHOwp_a*exp(-P%Rncg*GAMMA_new/P%b)
      else
        if (RHOwp_a  >=  0.0) then
          RHOwp_b=  P%RHOwpMIN
        else
          RHOwp_b= -P%RHOwpMIN
        end if
      end if

      END SUBROUTINE UPD_ncg_wp



      SUBROUTINE UPD_ncg_wd(rdr,SV_a,SV_b)
      integer,INTENT(IN )       :: rdr
      TYPE(StatVar), INTENT(IN) :: SV_a
      TYPE(StatVar), INTENT(INOUT):: SV_b

      !inherited variables:
      !P%b, P%Rncg, P%beta1, P%beta2, P%RHOwdMIN
      !SVa%ActiveCBB
      !GAMMAdot_new

!     local variable declarations
      double precision RHOwdLOC
      double precision RHOwdHOM,accGAMMA_new,RHOwd_ini
      double precision RHOwd

      RHOwdHOM     = SV_a%CBB(rdr)%RHOwdHOM
      accGAMMA_new = SV_a%CBB(rdr)%accGAMMA_new
      RHOwd_ini    = SV_a%CBB(rdr)%RHOwd_ini

      if (RHOwdHOM > P%RHOwdMIN) then
       !if the wall was NOT active in prev. inc.
        if (rdr  /=  SVa%ActiveCBB(1) .AND.                              &
            rdr  /=  SVa%ActiveCBB(2)      ) then
         !accGAMMA_new=[accGAMMA_new]_inc(i-1) + [GAMMA_new]_inc(i)
            accGAMMA_new=accGAMMA_new+GAMMA_new
        else !the wall was active in prev. inc.
          accGAMMA_new=GAMMA_new
          RHOwd_ini=RHOwdHOM
        end if

        RHOwdLOC=-tanh( P%beta1*accGAMMA_new)*                           &
                   exp(-P%beta1*accGAMMA_new)*RHOwd_ini*P%beta2
        RHOwdHOM=RHOwdHOM*exp(-P%Rncg*GAMMA_new/P%b)
        RHOwd=RHOwdHOM+RHOwdLOC
        if (RHOwd  <  P%RHOwdMIN)  RHOwd=P%RHOwdMIN
      else
        RHOwdHOM=P%RHOwdMIN
        RHOwd   =P%RHOwdMIN
      end if

      SV_b%CBB(rdr)%RHOwd           = RHOwd
      SV_b%CBB(rdr)%RHOwdHOM     = RHOwdHOM
      SV_b%CBB(rdr)%accGAMMA_new = accGAMMA_new
      SV_b%CBB(rdr)%RHOwd_ini    = RHOwd_ini

      END SUBROUTINE UPD_ncg_wd



      SUBROUTINE UPD_cb(RHObausch,SUMabsGam,RHO_a,RHO_b)
      double precision, INTENT(IN)  :: RHObausch,SUMabsGam
      double precision, INTENT(IN)  :: RHO_a
      double precision, INTENT(OUT) :: RHO_b

      !inherited variables:
      !P%I, P%R, P%R2, P%b, P%RHOwpSAT

!     local variable declarations
      double precision Reffective

      if(RHObausch  >  0.0) then
        Reffective=P%R + P%R2*RHObausch/(2.D0*P%RHOwpSAT)
        if (P%I*sqrt(RHO_a) - Reffective*RHO_a  <=  0.0) then ! Heaviside bracket
          RHO_b=RHO_a !Keep as is.
        else
            RHO_b= F_KocksMeck(RHO_a,SUMabsGam,P%I,Reffective)
        end if
      else !RHObausch  ==  0.0
        RHO_b= F_KocksMeck(  RHO_a,SUMabsGam,P%I,P%R       )
      end if

      END SUBROUTINE UPD_cb

      END SUBROUTINE MakeInc



      FUNCTION F_CRSS(SV)
      TYPE(StatVar), INTENT(IN) :: SV
      double precision, DIMENSION(2,24):: F_CRSS !OUT

!     P%tau0,P%f  ->inherited
!     alfa_G_b ->inherited
!     alfa_G_b_eff,alfa_G_b_ABSeff ->inherited

      !local variables declarations
      double precision :: tau_CB,CRSS_0_CB
      double precision,DIMENSION(2,24)::tau_CBB
      integer :: j,s,i
      double precision :: signfac
      double precision,DIMENSION(6)::wpcontr,wdcontr

      !Slip systems not allowed to become active retain initialization value of -1.0
      F_CRSS=-1.D0

      !CRSS within cells & CBs
      tau_CB=alfa_G_b*sqrt(SV%RHOcb)

      !contributions from tau_0 and CBs to CRSS
      CRSS_0_CB=P%tau0 +  (1.D0-P%f)*tau_CB

      !Calc. CRSS for each slip system s, for the sense of slip j
      do j=1,2
      signfac=3.D0-2.D0*dble(j) ! 1 for j=1 ; -1 for j=2
        do s=1,Nss
          !wp- and wd-contributions from all CBBs i
          do i=1,6
                  wpcontr(i)=sqrt(abs(SV%CBB(i)%RHOwp)) *             &
                       signfac * alfa_G_b_eff(s,i) *                    &
                       sign(1.D0,SV%CBB(i)%RHOwp) ! sign returns +/-1 depending on the sign of the second argument
            if (wpcontr(i)  <  0.0) wpcontr(i)=0.0 ! Heaviside bracket
            wdcontr(i)=sqrt(SV%CBB(i)%RHOwd)*alfa_G_b_ABSeff(s,i)
          end do
          !CRSS within CBB = wp- and wd-contributions for all 6 walls
          tau_CBB(j,s)=sum(wpcontr)+sum(wdcontr)
          !C.R.S.S. for the "two-phase composite"
          F_CRSS(j,s)= CRSS_0_CB + P%f*tau_CBB(j,s)
        end do
      end do

      END FUNCTION F_CRSS



      !> Perform an IO formatted read operation on StatVar
      !>
      !> \param dummy if true, the function performs a fake read operation of by simply skipping the same number of lines as the ReadSVfile would normally read. The resulting SV becomes initialized to default values.
      integer FUNCTION ReadSVfile(unit,SV,dummy) result(iError)
      integer,      INTENT(IN)  :: unit
      TYPE(StatVar),INTENT(OUT) :: SV
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
      iError = KS_OK
      return
100   format(A5)             ! 5 characters
101   format(   E15.8 )      ! real number in scientific notation, 15 digits total (including 1
                             ! digit for sign and 4 for exponent, 8 digits after decimal point)
102   format( 5(E15.8,1X))   ! 5 times E15.8 with 1 blank spacing in between
103   format( 2(I5,1X   ))   ! 2 5-digit integers with 1 blank spacing
104   format(24(E15.8,1X))
      !
666   iError = KS_ErrIO !Error in reading from file
      !
      END FUNCTION ReadSVfile



      integer FUNCTION WriteSVfile(unit,SV) result(iError)
      integer,      INTENT(IN)  :: unit
      TYPE(StatVar),INTENT(IN)  :: SV

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
      iError = KS_OK
      return
      !
101   format(   E15.8 )
102   format( 5(E15.8,1X))
103   format( 2(I5,1X   ))
104   format(24(E15.8,1X))
      !
666   iError = KS_ErrIO !Error in reading from file
      !
      END FUNCTION WriteSVfile




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
      iError = KS_OK
      return
      !
100   format(A76)
101   format(A26,L1)
666   iError = KS_ErrIO !Error in writing to file
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

      iError = KS_OK
      return
      !
100   format(A76)
666   iError = KS_ErrIO !Error in reading from file
      !
      end function ReadHeadSVfile



      SUBROUTINE GetStateDerivedVar(SV,SDV,iError)
      TYPE(StatVar),   INTENT(IN)  :: SV
      !> An object of type StateDerivedVars, which contains state-derived variables calculated from SV
      TYPE(StateDerivedVars), INTENT(OUT) :: SDV
      !> Exit code:
      !> - KS_OK , no error
      !> - KS_ErrUninitialized, in case this module is not correctly initialized
      integer,         INTENT(OUT) :: iError

      iError= KS_Error !init
      if(.NOT.InitOK) then
            iError = KS_ErrUninitialized
            return
      end if

      SDV%rho_CBs     = SV%RHOcb                        * TENpow6**2 !unit conversion micrometer^(-2) -> m^(-2)
      SDV%rho_CBBs    = sum(    SV%CBB(:)%RHOwd ) /6.D0 * TENpow6**2 !unit conversion micrometer^(-2) -> m^(-2)
      SDV%rho_polCBBs = sum(abs(SV%CBB(:)%RHOwp)) /6.D0 * TENpow6**2 !unit conversion micrometer^(-2) -> m^(-2)
      SDV%rho_avg     = (1.D0-P%f)*SDV%rho_CBs + P%f*(SDV%rho_CBBs+SDV%rho_PolCBBs)
      !Note: Number of CBBs is 6 (currently hard-coded)

      iError=KS_OK

      END SUBROUTINE GetStateDerivedVar

      !> Calculate component-wise sum of two StateDerivedVars objects
      elemental function StateDerivedVar_plus(first,second) result(res)
      type(StateDerivedVars),intent(in) :: first,second
      type(StateDerivedVars) :: res
      !
      res%rho_CBs = first%rho_CBs + second%rho_CBs
      res%rho_CBBs = first%rho_CBBs + second%rho_CBBs
      res%rho_polCBBs = first%rho_polCBBs + second%rho_polCBBs
      res%rho_avg = first%rho_avg + second%rho_avg
      !
      end function StateDerivedVar_plus

      !> Multiply all components of SDV by the scalar
      elemental function StateDerivedVar_times(SDV,scalar) result(res)
      type(StateDerivedVars),intent(in) :: SDV
      double precision,intent(in)       :: scalar
      type(StateDerivedVars) :: res
      !
      res%rho_CBs = scalar * SDV%rho_CBs
      res%rho_CBBs = scalar * SDV%rho_CBBs
      res%rho_polCBBs = scalar * SDV%rho_polCBBs
      res%rho_avg = scalar * SDV%rho_avg
      !
      end function StateDerivedVar_times

      !> Output state-derived variables (SDV) or/and a header line.
      integer function writeSDV(unit,SDV,header) result(info)
      integer,intent(in)                :: unit
      logical,intent(in),optional       :: header
      type(StateDerivedVars),intent(in),optional :: SDV
      !
      integer :: ierr
      !
      info = KS_ErrIO
      if (present(header)) then
          if (header) write(unit,fmt=100,iostat=ierr)
          if (ierr /= 0) return
      endif
      if (present(SDV)) then
          write(unit,fmt=101,iostat=ierr) SDV
          if (ierr /= 0) return
      endif
      info = KS_OK
      !
      100 format(T4,'rho_CBs',T20,'rho_CBBs',T36,'rho_polCBBs',T52,'rho_avg')
      101 format(4(E15.7,1X))
      !
      end function

      END MODULE altayHardLaw_DSH
