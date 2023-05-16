module hardening_model_dsh
    use altayIOConfig, only: LEC
    use definitions
    use hardening_model
    use altayConfig
    use parameters
    use logging

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

    implicit none
    private

    character(*), parameter, private :: MOD_NAME = 'hardening_model_dsh'

      TYPE :: CBBtype
            !PUBLIC components
            real(dp) :: RHOwd = 0.D0
            real(dp) :: RHOwp = 0.D0
            real(dp) :: RHOwdHOM = 0.D0
            real(dp) :: accGAMMA_new = 0.D0
            real(dp) :: RHOwd_ini = 0.D0
      END TYPE CBBtype


      !> State variables for single grain
      TYPE :: StatVar
      !PUBLIC components
            real(dp)                    :: RHOcb = 0.D0
            TYPE(CBBtype), DIMENSION(6)         :: CBB
            integer, DIMENSION(2)               :: ActiveCBB = 0
            real(dp), DIMENSION(2,24)   :: CRSS = 0.D0 !Up to 24 slip systems supported
      END TYPE StatVar

      TYPE, public :: StateDerivedVars
            !> Dislocation density of cell boundaries; unit: m^(-2)
            real(dp) :: rho_CBs = 0.D0
            !> Dislocation density of cell block boundaries; unit: m^(-2)
            real(dp) :: rho_CBBs = 0.D0
            !> Dislocation density of polarized dislocations at cell block boundaries; unit: m^(-2)
            real(dp) :: rho_polCBBs = 0.D0
            !> Average dislocation density; unit: m^(-2)
            real(dp) :: rho_avg = 0.D0
      END TYPE

      INTERFACE OPERATOR(+)
            MODULE PROCEDURE  StateDerivedVar_plus
      END INTERFACE

      INTERFACE OPERATOR(*)
            MODULE PROCEDURE  StateDerivedVar_times
      END INTERFACE

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

      !Remaining declarations all PRIVATE:
      logical, SAVE :: InitOK=.FALSE.
      integer, SAVE :: Nss !Number of slip systems. Supported values:
                           !     Nss=12: (110)[111] - 1 family
                           !     Nss=24: (110)+(112)[111] - 2 families
      integer, PRIVATE :: i !running index
      real(dp), PARAMETER :: MINfrac= 2.0D-3
      real(dp), PARAMETER :: LOWfrac=10.0D-3

      real(dp), PARAMETER, public :: p2= 1.D0/sqrt(2.D0)
      real(dp), PARAMETER, public :: n2= -p2
      real(dp), PARAMETER, public :: p3= 1.D0/sqrt(3.D0)
      real(dp), PARAMETER, public :: n3= -p3
      real(dp), PARAMETER, public :: p6= 1.D0/sqrt(6.D0)
      real(dp), PARAMETER, public :: n6= -p6
      real(dp), PARAMETER, public :: pd6= 2.D0/sqrt(6.D0)
      real(dp), PARAMETER, public :: nd6= -pd6
      !real(dp), PARAMETER :: p1_42= 0.154303349962 !1.0/sqrt(42.0)
      !real(dp), PARAMETER :: n1_42=-0.154303349962
      !real(dp), PARAMETER :: p4_42= 0.617213399848 !4.0/sqrt(42.0)
      !real(dp), PARAMETER :: n4_42=-0.617213399848
      !real(dp), PARAMETER :: p5_42= 0.771516749810 !5.0/sqrt(42.0)
      !real(dp), PARAMETER :: n5_42=-0.771516749810

      !CBBnormal(i,1:3): normalized vector normal to CBB i
      real(dp), public, DIMENSION(6,3) ::CBBnormal
      DATA (CBBnormal(1,i),i=1,3) /0.,p2,n2/ !CBBs on (01-1)-plane
      DATA (CBBnormal(2,i),i=1,3) /n2,0.,p2/ !CBBs on (-101)-plane
      DATA (CBBnormal(3,i),i=1,3) /p2,n2,0./ !CBBs on (1-10)-plane
      DATA (CBBnormal(4,i),i=1,3) /0.,n2,n2/ !CBBs on (0-1-1)-plane
      DATA (CBBnormal(5,i),i=1,3) /p2,0.,p2/ !CBBs on (101)-plane
      DATA (CBBnormal(6,i),i=1,3) /n2,n2,0./ !CBBs on (-1-10)-plane



      type(StatVar),allocatable,dimension(:)    :: KS_state ! array of state variables


    type, public, extends(HardeningModel) :: HardeningModelDSH
        type(StatVar), dimension(:), allocatable    ::  state
        real(dp), dimension(:,:,:), allocatable     ::  crss
        real(dp)                                    ::  b,                      &
                                                        G,                      &
                                                        alfa,                   &
                                                        f,                      &
                                                        tau0,                   &
                                                        I,                      &
                                                        R,                      &
                                                        Iwd,                    &
                                                        Rwd,                    &
                                                        Rncg,                   &
                                                        beta1,                  &
                                                        beta2,                  &
                                                        Iwp,                    &
                                                        Rwp,                    &
                                                        Rrev,                   &
                                                        R2,                     &
                                                        RHOcbSAT,               &
                                                        RHOwdSAT,               &
                                                        RHOwpSAT,               &
                                                        RHOcbMIN,               &
                                                        RHOwdMIN,               &
                                                        RHOwpMIN,               &
                                                        RHOwpLOW,               &
                                                        alfa_G_b
        real(dp), dimension(24,6)                   ::  eff             = 0.D0, &
                                                        effslashb       = 0.D0, &
                                                        alfa_G_b_eff    = 0.D0, &
                                                        alfa_G_b_ABSeff = 0.D0
    contains
        procedure :: get_parameters => dsh_get_parameters
        procedure :: validate_parameters => dsh_validate_parameters
        procedure :: init           => dsh_init
        procedure :: update         => dsh_update
        procedure :: get_crss       => dsh_get_crss
        procedure :: finalize       => dsh_finalize
    end type

    public ::   dsh_init, &
                dsh_initstate, &
                KS_finalize, &
                KS_openstatefile

CONTAINS
    function dsh_get_parameters(this) result(params)
        class(HardeningModelDSH), intent(in)    :: this
        type(Parameter), allocatable    :: params(:)

        params = [  parameter_init('n_slip_systems', TYPE_STRING), &
                    parameter_init('b', TYPE_REAL),                 &
                    parameter_init('G', TYPE_REAL),                 &
                    parameter_init('alfa', TYPE_REAL),              &
                    parameter_init('f', TYPE_REAL),                 &
                    parameter_init('tau0', TYPE_REAL),              &
                    parameter_init('I', TYPE_REAL),                 &
                    parameter_init('R', TYPE_REAL),                 &
                    parameter_init('Iwd', TYPE_REAL),               &
                    parameter_init('Rwd', TYPE_REAL),               &
                    parameter_init('Rncg', TYPE_REAL),              &
                    parameter_init('beta1', TYPE_REAL),             &
                    parameter_init('beta2', TYPE_REAL),             &
                    parameter_init('Iwp', TYPE_REAL),               &
                    parameter_init('Rwp', TYPE_REAL),               &
                    parameter_init('Rrev', TYPE_REAL),              &
                    parameter_init('R2', TYPE_REAL)]

    end function dsh_get_parameters

    subroutine dsh_validate_parameters(this, params)
        class(HardeningModelDSH), intent(in)    :: this
        type(Parameter), allocatable, intent(in) :: params(:)
        character(:), allocatable :: nss

        nss = params .find. 'n_slip_systems'

        if (nss /= 'fcc12' .and. nss /= 'bcc24')  &
            call log_error(MOD_NAME, 'validate_parameters', ERR_VAL, 'DSH only supports FCC12 and BCC24 slip systems.')

        call check_param('b',       0._dp,      1.e-8_dp)   ! [m]
        call check_param('G',       1.e4_dp,    5.e5_dp)    ! [MPa]
        call check_param('alfa',    0._dp,      5._dp)      ! [/]
        call check_param('f',       0._dp,      1.0_dp)     ! [/]
        call check_param('tau0',    0._dp,      1.e4_dp)    ! [MPa]
        call check_param('I',       0._dp,      10._dp)     ! [/]
        call check_param('Iwd',     0._dp,      10._dp)     ! [/]
        call check_param('Iwp',     0._dp,      10._dp)     ! [/]
        call check_param('R',       0._dp,      1.e-6_dp)   ! [m]
        call check_param('Rwd',     0._dp,      1.e-6_dp)   ! [m]
        call check_param('Rncg',    0._dp,      1.e-6_dp)   ! [m]
        call check_param('Rwp',     0._dp,      1.e-6_dp)   ! [m]
        call check_param('Rrev',    0._dp,      1.e-6_dp)   ! [m]
        call check_param('R2',      0._dp,      1.e-6_dp)   ! [m]
        call check_param('beta1',   0._dp,      100._dp)    ! [/]
        call check_param('beta2',   0._dp,      100._dp)    ! [/]
    contains
        subroutine check_param(name, min, max)
            character(*), intent(in)    ::  name
            real(dp), intent(in)        ::  min,    &
                                            max
            character(32)               ::  min_str, &
                                            max_str
            real(dp)                    ::  param_val
            character(*), parameter     ::  PROC_NAME = 'dsh_check_param'

            param_val = params .find. name

            if (param_val < min .or. param_val > max) then
                write (min_str, *), min
                write (max_str, *), max
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Parameter ' // name // ' must lie between ' // min_str // ' and ' // max_str)
            end if
        end subroutine check_param
    end subroutine dsh_validate_parameters

    subroutine dsh_init(this, params)
        class(HardeningModelDSH), intent(inout) :: this
        type(Parameter), allocatable, intent(in)             :: params(:)
        integer :: dummy

        call hardening_model_init(this, params)
        nss = this%nss

        InitOK=.TRUE.

        this%b          = params .find. 'b'
        this%G          = params .find. 'G'
        this%alfa       = params .find. 'alfa'
        this%f          = params .find. 'f'
        this%tau0       = params .find. 'tau0'
        this%I          = params .find. 'I'
        this%R          = params .find. 'R'
        this%Iwd        = params .find. 'Iwd'
        this%Rwd        = params .find. 'Rwd'
        this%Rncg       = params .find. 'Rncg'
        this%beta1      = params .find. 'beta1'
        this%beta2      = params .find. 'beta2'
        this%Iwp        = params .find. 'Iwp'
        this%Rwp        = params .find. 'Rwp'
        this%Rrev       = params .find. 'Rrev'
        this%R2         = params .find. 'R2'

        !change of units if different in params from this (units of this are: MPa; micrometer)
        this%b          = this%b    * 1.e6_dp       ![m] -> [um]
        this%R          = this%R    * 1.e6_dp       ![m] -> [um]
        this%Rwd        = this%Rwd  * 1.e6_dp       ![m] -> [um]
        this%Rncg       = this%Rncg * 1.e6_dp       ![m] -> [um]
        this%Rwp        = this%Rwp  * 1.e6_dp       ![m] -> [um]
        this%Rrev       = this%Rrev * 1.e6_dp       ![m] -> [um]
        this%R2         = this%R2   * 1.e6_dp       ![m] -> [um]

        !Calculate dependent hardening parameters
        this%RHOcbSAT = (this%I)**2 / (this%R)**2
        this%RHOwdSAT = (this%Iwd)**2 / (this%Rwd)**2
        this%RHOwpSAT = (sqrt((this%Iwp / this%Rwp)**4 + 4._dp * (this%Iwp * this%Iwd / (this%Rwp * this%Rwd))**2) + (this%Iwp / this%Rwp)**2) / 2._dp
        this%RHOcbMIN = MINfrac * this%RHOcbSAT
        this%RHOwpLOW = LOWfrac * this%RHOwpSAT
        this%RHOwdMIN   = MINfrac   * this%RHOwdSAT
        this%RHOwpMIN   = MINfrac   * this%RHOwpSAT
        this%RHOwpLOW   = LOWfrac   * this%RHOwpSAT
    end subroutine dsh_init

    subroutine dsh_update(this, grain, time, strain, slip_rates)
        class(HardeningModelDSH), intent(inout)     ::  this
        integer, intent(in)                         ::  grain
        real(dp), intent(in)                        ::  time,                 &
                                                    strain
        real(dp), dimension(this%nss), intent(in)   ::  slip_rates
        type(StatVar)                               ::  SVa,                    &
                                                        SVb
        integer :: dummy

        SVa = this%state(grain)
        call makeinc(this, sva, slip_rates, time, svb, dummy)
        this%state(grain) = svb
    end subroutine dsh_update

    function dsh_get_crss(this, grain) result(crss)
        class(HardeningModelDSH), intent(in)    :: this
        integer, intent(in) :: grain
        real(dp)            :: crss(2,this%nss)

        crss = this%state(grain)%crss
    end function dsh_get_crss

    subroutine dsh_finalize(this)
        class(HardeningModelDSH), intent(inout) :: this
        integer :: dummy

        dummy = KS_finalize()
    end subroutine dsh_finalize

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
    integer function dsh_initState(this, norient) result(info)
        type(HardeningModelDSH) :: this
        integer,intent(in)      :: norient !< Number of orientations in the material
        integer :: i

        if (.not. allocated(this%state)) allocate(this%state(norient),stat=info)
        ! All elements (orientations) of the KS_state array must have the same initial state.
        do i=1,norient
            this%state(i)%RHOcb               = this%RHOcbMIN
            this%state(i)%CBB%RHOwd        = this%RHOwdMIN
            this%state(i)%CBB%RHOwp        = 0._dp
            this%state(i)%CBB%RHOwdHOM     = this%RHOwdMIN
            this%state(i)%CBB%accGAMMA_new = 0._dp
            this%state(i)%CBB%RHOwd_ini    = this%RHOwdMIN
            this%state(i)%ActiveCBB        = 0
            this%state(i)%CRSS                = F_CRSS(this, this%state(1))
        end do

        info = 0
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

      !> Get CRSS for i-th grain.
      subroutine KS_getCRSS(i,Mcrss,info)
      integer,intent(in)                              :: i        !< Grain identifier
      integer,intent(out)                             :: info
      real(dp), dimension(2,96),intent(out)                          :: Mcrss    !< CRSS output
      integer l   !< maximum number of slip systems restricted to 24
      !
            info = KS_ErrBadDims
            if (size(KS_state) < i) return
            !if ( any(shape(Mcrss) /= shape(KS_state(i)%CRSS)) ) return
            l = min(24, ubound(Mcrss,2)) ! corresponds to the number of slip systems
            ! Extract the CRSSes
            Mcrss(:,1:l) = KS_state(i)%CRSS(:,1:l)
            info = KS_OK
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

    SUBROUTINE MakeInc(this, SVa,sliprate,deltaT,SVb,iError)
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
        class(HardeningModelDSH), intent(in) :: this
      TYPE(StatVar),INTENT(IN)       :: SVa
      real(dp),INTENT(IN), DIMENSION(24) :: sliprate
      real(dp),INTENT(IN)                      :: deltaT
      TYPE(StatVar),INTENT(OUT)      :: SVb !OUT
      integer,INTENT(OUT)            :: iError

      !local variable declarations
      real(dp) :: SUMabsGamDot=0.,GAMMAdot_new=0.,RHObausch=0.
      real(dp) :: SUMabsGam   =0.,GAMMA_new   =0.
      real(dp),    DIMENSION(6) :: GAMMAdot=0.,GAMMA=0.
      integer, DIMENSION(6) :: r
      integer :: j
      real(dp) :: fl,wd

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
                                         ,GAMMA(r(j)),this%Iwd,this%Rwd)
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
      SVb%CRSS= F_CRSS(this, SVb)



      CONTAINS

      FUNCTION F_GAMMAdot(sr)
      ! Calculate the total slip rates on each of the six (110)-planes
      real(dp), DIMENSION(24), INTENT(IN)  :: sr !Slip Rate
      real(dp), DIMENSION( 6)              :: F_GAMMAdot !OUT

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
      real(dp),    DIMENSION(6), INTENT(IN)  :: PlaneSlip
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



      real(dp) FUNCTION F_KocksMeck(RHO_a,delta_g,II,RR) !PE27062012-2
      !Returns RHO_b, the value of RHO at the end of an interval (a,b)
      ! for the following differential equation:
      !
      ! d(RHO)    1
      ! ------ = --- * ( II*sqrt(RHO) - RR*RHO )
      !  d(g)    this%b
      !
      ! The value of 'this%b', the size of burgers vector, is inherited.
      !
      ! To calc. RHO_b, following inputs are required:
      !   -> RHO_a, the value of RHO at the start of the interval (a,b)
      !   -> delta_g = g_b - g_a, the increment in g during the interval (a,b)
      real(dp) ,INTENT(IN):: RHO_a,delta_g,II,RR

!     local variable declarations
      real(dp) x

      x=exp(-0.5D0*RR*delta_g/this%b)
      x=II/RR*(1.D0-x)+sqrt(RHO_a)*x
      F_KocksMeck=x*x

      END FUNCTION F_KocksMeck



      SUBROUTINE UPD_cur_wp(rdr,RHOwp_a,RHOwp_b,RHObausch)
      integer, INTENT(IN):: rdr
      real(dp) ,INTENT(IN)   :: RHOwp_a
      real(dp) ,INTENT(OUT)  :: RHOwp_b
      real(dp), INTENT(INOUT):: RHObausch

      !inherited variables:
      !this%Iwd, this%Rwd, this%b
      !glidedir, wall
      !effslashb, sliprate
      !this%RHOwpSAT, this%RHOwpLOW
      !fl, wd

      !local variable declarations:
      real(dp) :: wpFLUX !wp-flux on the wall 'rdr'

      logical :: FLUXreversal,wpLOW

      wpFLUX=DOT_PRODUCT(this%effslashb(:,rdr) , sliprate(:) )

      FLUXreversal= wpFLUX*RHOwp_a  <  0.0
      wpLOW= abs(RHOwp_a)  <=  this%RHOwpLOW

      if ( FLUXreversal .and. .NOT.(wpLOW) ) then
        ! |RHOwp| gets smaller, following analytic time integration
        RHOwp_b=RHOwp_a*exp(-this%Rrev*abs(wpFLUX)*deltaT) !wpFLUX is a rate!
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
      real(dp), INTENT(IN) :: wpini
      real(dp)                RungeKutta   !OUT

      !local variable declarations:
      real(dp), DIMENSION(4) :: K

      K(1)=deltaT*dwp_dt(wpini        )
      K(2)=deltaT*dwp_dt(wpini+K(1)/2.D0)
      K(3)=deltaT*dwp_dt(wpini+K(2)/2.D0)
      K(4)=deltaT*dwp_dt(wpini+K(3)   )
      RungeKutta=wpini+(K(1)+2.D0*K(2)+2.D0*K(3)+K(4))/6.D0

      END FUNCTION RungeKutta



      FUNCTION dwp_dt(wp)
      real(dp), INTENT(IN) :: wp
      real(dp)             :: dwp_dt !OUT

      !inherited variables:
      !fl, wd
      !this%Iwp, this%Rwp

      dwp_dt=(sign(1.D0,fl)*this%Iwp*sqrt(wd+abs(wp)) - this%Rwp*wp) * abs(fl)

      END FUNCTION dwp_dt



      SUBROUTINE UPD_ncg_wp(RHOwp_a,RHOwp_b)
      real(dp), INTENT(IN)  :: RHOwp_a
      real(dp), INTENT(OUT) :: RHOwp_b

      !inherited variables:
      !this%Rncg, GAMMAdot_new, this%b, this%RHOwpMIN

      if (abs(RHOwp_a)  >  this%RHOwpMIN) then
        RHOwp_b= RHOwp_a*exp(-this%Rncg*GAMMA_new/this%b)
      else
        if (RHOwp_a  >=  0.0) then
          RHOwp_b=  this%RHOwpMIN
        else
          RHOwp_b= -this%RHOwpMIN
        end if
      end if

      END SUBROUTINE UPD_ncg_wp



      SUBROUTINE UPD_ncg_wd(rdr,SV_a,SV_b)
      integer,INTENT(IN )       :: rdr
      TYPE(StatVar), INTENT(IN) :: SV_a
      TYPE(StatVar), INTENT(INOUT):: SV_b

      !inherited variables:
      !this%b, this%Rncg, this%beta1, this%beta2, this%RHOwdMIN
      !SVa%ActiveCBB
      !GAMMAdot_new

!     local variable declarations
      real(dp) RHOwdLOC
      real(dp) RHOwdHOM,accGAMMA_new,RHOwd_ini
      real(dp) RHOwd

      RHOwdHOM     = SV_a%CBB(rdr)%RHOwdHOM
      accGAMMA_new = SV_a%CBB(rdr)%accGAMMA_new
      RHOwd_ini    = SV_a%CBB(rdr)%RHOwd_ini

      if (RHOwdHOM > this%RHOwdMIN) then
       !if the wall was NOT active in prev. inc.
        if (rdr  /=  SVa%ActiveCBB(1) .AND.                              &
            rdr  /=  SVa%ActiveCBB(2)      ) then
         !accGAMMA_new=[accGAMMA_new]_inc(i-1) + [GAMMA_new]_inc(i)
            accGAMMA_new=accGAMMA_new+GAMMA_new
        else !the wall was active in prev. inc.
          accGAMMA_new=GAMMA_new
          RHOwd_ini=RHOwdHOM
        end if

        RHOwdLOC=-tanh( this%beta1*accGAMMA_new)*                           &
                   exp(-this%beta1*accGAMMA_new)*RHOwd_ini*this%beta2
        RHOwdHOM=RHOwdHOM*exp(-this%Rncg*GAMMA_new/this%b)
        RHOwd=RHOwdHOM+RHOwdLOC
        if (RHOwd  <  this%RHOwdMIN)  RHOwd=this%RHOwdMIN
      else
        RHOwdHOM=this%RHOwdMIN
        RHOwd   =this%RHOwdMIN
      end if

      SV_b%CBB(rdr)%RHOwd           = RHOwd
      SV_b%CBB(rdr)%RHOwdHOM     = RHOwdHOM
      SV_b%CBB(rdr)%accGAMMA_new = accGAMMA_new
      SV_b%CBB(rdr)%RHOwd_ini    = RHOwd_ini

      END SUBROUTINE UPD_ncg_wd



      SUBROUTINE UPD_cb(RHObausch,SUMabsGam,RHO_a,RHO_b)
      real(dp), INTENT(IN)  :: RHObausch,SUMabsGam
      real(dp), INTENT(IN)  :: RHO_a
      real(dp), INTENT(OUT) :: RHO_b

      !inherited variables:
      !this%I, this%R, this%R2, this%b, this%RHOwpSAT

!     local variable declarations
      real(dp) Reffective

      if(RHObausch  >  0.0) then
        Reffective=this%R + this%R2*RHObausch/(2.D0*this%RHOwpSAT)
        if (this%I*sqrt(RHO_a) - Reffective*RHO_a  <=  0.0) then ! Heaviside bracket
          RHO_b=RHO_a !Keep as is.
        else
            RHO_b= F_KocksMeck(RHO_a,SUMabsGam,this%I,Reffective)
        end if
      else !RHObausch  ==  0.0
        RHO_b= F_KocksMeck(  RHO_a,SUMabsGam,this%I,this%R       )
      end if

      END SUBROUTINE UPD_cb

      END SUBROUTINE MakeInc



      FUNCTION F_CRSS(this, SV)
        class(HardeningModelDSH), intent(in) :: this
      TYPE(StatVar), INTENT(IN) :: SV
      real(dp), DIMENSION(2,24):: F_CRSS !OUT

!     this%tau0,this%f  ->inherited
!     alfa_G_b ->inherited
!     alfa_G_b_eff,alfa_G_b_ABSeff ->inherited

      !local variables declarations
      real(dp) :: tau_CB,CRSS_0_CB
      real(dp),DIMENSION(2,24)::tau_CBB
      integer :: j,s,i
      real(dp) :: signfac
      real(dp),DIMENSION(6)::wpcontr,wdcontr

      !Slip systems not allowed to become active retain initialization value of -1.0
      F_CRSS=-1.D0

      !CRSS within cells & CBs
      tau_CB=this%alfa_G_b*sqrt(SV%RHOcb)

      !contributions from tau_0 and CBs to CRSS
      CRSS_0_CB=this%tau0 +  (1.D0-this%f)*tau_CB

      !Calc. CRSS for each slip system s, for the sense of slip j
      do j=1,2
      signfac=3.D0-2.D0*dble(j) ! 1 for j=1 ; -1 for j=2
        do s=1,Nss
          !wp- and wd-contributions from all CBBs i
          do i=1,6
                  wpcontr(i)=sqrt(abs(SV%CBB(i)%RHOwp)) *             &
                       signfac * this%alfa_G_b_eff(s,i) *                    &
                       sign(1.D0,SV%CBB(i)%RHOwp) ! sign returns +/-1 depending on the sign of the second argument
            if (wpcontr(i)  <  0.0) wpcontr(i)=0.0 ! Heaviside bracket
            wdcontr(i)=sqrt(SV%CBB(i)%RHOwd)*this%alfa_G_b_ABSeff(s,i)
          end do
          !CRSS within CBB = wp- and wd-contributions for all 6 walls
          tau_CBB(j,s)=sum(wpcontr)+sum(wdcontr)
          !C.R.S.S. for the "two-phase composite"
          F_CRSS(j,s)= CRSS_0_CB + this%f*tau_CBB(j,s)
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
      real(dp),intent(in)       :: scalar
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

      END MODULE hardening_model_dsh
