!> nllsTR  -- wrapper module for MKL Non-Linear Least Squares Trust Region algorithm
! Provide modules: MKL_RCI_TYPE and MKL_RCI
include 'mkl_rci.f90'
module nllsTR
    use utils
    use mkl_rci
    use altay
    use altayconfig
    use logging

    implicit none

    character(*), parameter:: MOD_NAME = 'nllstr'

      !> Solution at given point. It consists of: 1) the point, 2) function value, and 3) Jacobi matrix.
      type:: SolutionPoint
            !> Dimensionality of vector X (argument)
            integer                                         :: n_X_dim = 0

            !> Dimensionality of objective function
            integer                                         :: m_F_dim = 0

            real(DP), dimension(:), allocatable       :: vX       !< The point
            real(DP), dimension(:), allocatable       :: vF       !< Function at vX
            real(DP), dimension(:,:), allocatable     :: mJ       !< Jacobi matrix at vX, [m_F_dim x n_X_dim]

      contains
            !> Constructor: initialization guided by the dimensions n_X_dim and m_F_dim
            procedure, pass(this)       :: init => SolutionPoint_Init

            !> Constructor: copy from other SolutionPoint object
            procedure, pass(this)       :: copy => SolutionPoint_Copy

            !> Destructor: state of the object is changed to uninitialized.
            procedure, pass(this)       :: finalize => SolutionPoint_Finalize

            !> Generic name for constructors
            generic, public:: construct => init, copy

      end type


      !> Abstract data type for objective functions.
      !>
      !> It is assumed that every objective function contains a state variable that
      !> represents the value of the function and its Jacobian.
      type, abstract:: objectiveFunction

            !> Normalized stress vector
            real(DP), dimension(5)        :: vSn = 0.D0
            !> State variable
            type(SolutionPoint)                               :: state


      contains
            !> Initialization function
            procedure, pass(this)                 :: initFx => objectiveFunction_initFx

            !>@{ \name Stateful interface

            !> Evaluation of objective function vector.
            !>
            !> See remarks in IF_objectiveFx_stateful for a guidance how to implement it.
            procedure(IF_objectiveFx_stateful), deferred, pass(this)     :: objectiveEval

            procedure, pass(this)                            :: getProblemSize
            procedure, pass(this)                            :: getXSize
            procedure, pass(this)                            :: getFSize


      end type

      !> Named constants for trackableObjFunc
      integer, parameter       :: TOF_None = 0, TOF_Function = 1, TOF_Jacobian = 2, TOF_All = 4

      !> Objective function with tacking of the last evaluations.
      !>
      !> vX corresponding to the last evaluation of the function is stored in state%vX
      !> (member of the parent class)
      !> vX that corresponds to the last evaluation of the Jacobian is stored in vXofJacobi
      type, abstract, extends(objectiveFunction):: trackableObjFunc

            integer                                         :: tracking_level = TOF_None

            real(DP), dimension(:), allocatable       :: vXofJacobi

      contains

            procedure:: objectiveEval => trackableObjFunc_objectiveEval
            procedure, pass(this):: track => trackableObjFunc_track
      end type


      abstract interface
            !> Abstract interface for initalization of an instance of objectiveFunction object.
            subroutine IF_initFx(this, n_X_dim, m_F_dim, info)
                  import  ::  objectiveFunction
                  class(objectiveFunction), intent(inout)      :: this         !< Instance of the object.
                  integer, intent(in)                          :: n_X_dim      !< Requested dimensionality of the function argument.
                  integer, intent(in)                          :: m_F_dim      !< Requested dimensionality of the function value.
                  integer, intent(out)                         :: info   !< Set to 0 on success
            end subroutine

            !> Abstract interface for stateful-style objective function calculations.
            !>
            !> The function should update the vF component of state member (\sa SolutionPoint)
            !> \note It is user's responsibility to provide a function that complies with this interface.
            !> \note This function must not modify any component of SolutionPoint except for vX and vF.
            subroutine IF_objectiveFx_stateful(this, vX, info)
                  import  ::  objectiveFunction, DP
                  class(objectiveFunction), intent(inout)      :: this   !< Instance of the object.
                  real(DP), dimension(:), intent(in)    :: vX     !< Dimension must be: [n_X_dim]
                  integer, intent(out)                         :: info   !< Set to 0 on success
            end subroutine

            !> Abstract interface for a function that calculates Jacobi matrix in a stateful-style.
            !>
            !> The function should update the mF component of state member (\sa SolutionPoint).
            !> \note It is user's responsibility to provide a function that complies with this interface.
            !> \note This function must not modify any component of SolutionPoint except for vX and mJ.
            subroutine IF_JacobiObjFx_stateful(this, vX, info)
                  import:: objectiveFunction, DP
                  class(objectiveFunction), target, intent(inout)          :: this     !< Instance of the object.
                  real(DP), dimension(5), intent(in)        :: vX       !< Dimension must be: [n_X_dim]
                  integer, intent(out)                             :: info     !< Set to 0 on success
            end subroutine

      end interface


      !> Control over Trust Region algorithm
      type nllsTRConf
            !> Array of parameters controling stop criteria
            !>
            !> Various convergence criteria are evaluated, see MKL documentation for details
            real(DP), dimension(6)                :: eps = 1.e-10_DP
            integer                                      :: iter1 = 300 !< Maximum number of iterations
            integer                                      :: iter2 = 50  !< Maximum number of trial steps
            real(DP)                             :: init_step = 100.0_DP  !< Initial step bound factor
            !> Lower constraints for design vector
            real(DP)                             :: lo_limit = 0._DP
            !> Upper constraints for design vector
            real(DP)                             :: up_limit = 1.e2_DP

            !> Helper for problems with invariant Jacobi matrix
            !>
            !> If this variable is set to .true., the library will assume that Jacobi
            !> matrix is constant over subsequent steps. As effect, the matrix will be
            !> evaluated only once, before the first step of optimization procedure.
            !> This parameter is useful if the optimization problem is linear or can be
            !> linearized.
            logical                                      :: constJacobi = .false.

            !> If set true, the algorithm will use the contents of "state" field in the objective
            !> function object for the very first iteration. This implies an assumption that
            !> the state field contains a consistent starting point.
            logical                                      :: use_init_state = .false.

            !> If set true, every evaluation of either the objective function or Jacobian
            !> will be tested against NaN or Inf.
            logical                                      :: use_input_checks = .true.

      end type

      !>@{ \name Other parameters
      !>  These parameters are not directly accessible. Use \ref nlls_TR_init to control them. \sa nlls_TR_init

      !> Output unit
      integer, private                            :: nllsTR_ounit = 6

      !> Verbosity level.
      !>
      !> The following values of verbosity are allowed:
      integer, private                            :: nllsTR_iw = 0

     !>@}

      type nllsTRRes
            integer                             :: iteration = 0        !< Interation number
            integer                             :: stop_criterion = 0   !< Identifier of stop criterion, see MKL documentation
            real(DP)                    :: r1 = 0.D0            !< Initial norm of residual
            real(DP)                    :: r2 = 0.D0            !< Final norm of residual
      end type


      ! Internal components of the module.
      ! private writeMatrix
      private checkMKLRescode

contains



      !> Initialization of nlls_TR module.
      subroutine nlls_TR_init(ounit, verbose)
      implicit none
      integer, optional, intent(in)         ::  ounit  !< IO unit number for outputs
      integer, optional, intent(in)         ::  verbose !< Verbosity level, \sa nllsTR_iw
      !!
      if (present(ounit)) nllsTR_ounit = ounit
      if (present(verbose)) nllsTR_iw = verbose
      end subroutine


      !> This subroutine solves the mnimization problem. Trust region algorithm from MKL library is used.
      !> \param objFx Objective function to minimize
      !> \param jacobiFx Function that calculates Jacobi matrix of objective function
      subroutine nlls_TR_solve(objFx, vX, config, r1, r2, info, resInfo, SolutionInitOut)
      use, intrinsic:: IEEE_EXCEPTIONS
      use, intrinsic:: IEEE_ARITHMETIC
      implicit none
      ! Formal parameters
      class(objectiveFunction), target, intent(inout)          :: objFx    !< objective function
      !> Design vector, dimension of vX must correspond to those in objFX
      real(DP), dimension(:), target, intent(inout)     :: vX
      type(nllsTRConf), intent(in)                     :: config   !< Configuration of nllsTR
      real(DP), intent(out)                    :: r1       !< Initial residual of the solution
      real(DP), intent(out)                    :: r2       !< Final residual of the solution
      !> Exit code: 0 on success, < 0 on error, > 0 on failure/warning
      integer, intent(out)                             :: info
      !> Full termination status of the TR solver
      type(nllsTRRes), intent(inout), optional          :: resInfo
      !> Initial solution to be stored after initial evaluation
      type(SolutionPoint), intent(out), optional        :: SolutionInitOut
      !!!! Local variables
      integer                              :: n        !< Dimension of design vector
      integer                              :: m        !< Dimension of objective function vector
      type(HANDLE_TR)   :: handle
      integer           :: res, linfo
      !
      ! Variables for TR query
      type(nllsTRRes)                :: resultInfo
      ! RCI loop control
      logical                        :: next_solve
      integer                        :: RCI_Req, RCI_Count
      ! Initialization of vFval and mJacobi
      logical                        :: use_init_mJacobi, use_init_vFval
      logical                        :: is_firstFval, is_firstJacobi
      !
      ! Other variables
      integer                        :: ierr, i
      character(len = 512)             :: message
      real(DP):: jacobi_interval, &
                 eps(6), &
                 vlw(5), &
                 vup(5)

      !All tolerances can be set to TOLERANCE except for the tolerance on the residual (i.e. the 'success threshold'). This is set
      !to 0.01 because we are using normalized stresses and strains and 1% is about as accurate as you can hope the underlying model
      !to be. See dtrnlspbc_init documentation for more details.
      eps = TOLERANCE
      eps(2) = 1.E-2

      !jacobi_interval = 1.E-8_DP
      vlw = -10._DP
      vup = 10._DP
      !---------------------------------------------------
            RCI_Req = 0; next_solve = .true.
            info = -1
            !! Check preconditions
            ! TODO
            n = objFx%state%n_X_dim        ! Dimensionality of vector X (argument)
            m = objFx%state%m_F_dim        ! Dimensionality of objective function
            !
            ! Set square box constraints
            vLW =  config%lo_limit
            vUP =  config%up_limit
            ! Make sure that the initial guess is inside the constraints
            where (vX < vLW) vX = vLW
            where (vX > vUP) vX = vUP
            !
            ! Prepare variables for reusing the inital state (if requested)
            if (config%use_init_state) then
                  use_init_mJacobi = .true.
                  use_init_vFval = .true.
            else
                  use_init_mJacobi = .false.
                  use_init_vFval = .false.
            endif
            !
            if ((config%constJacobi) .and. (.not. config%use_init_state)) then
                  ! Calculate Jacobi matrix
                  objfx%state%mj = calc_jacobi(vx)
                  if (nllsTR_iw > 3) then
                        write( nllsTR_ounit, fmt = 100)  'Jacobi matrix  -->'
                        call writeMatrix(objFx%state%mJ, nllsTR_ounit)
                        write( nllsTR_ounit, fmt = 100)  'Jacobi matrix  <--'
                        flush(nllsTR_ounit)
                  endif
            endif
            !
            if ( (config%use_input_checks) .and. (config%use_init_state) ) then
                  call checkSolverInput(objFx%state%vF, objFx%state%mJ, linfo)
                  if (linfo /= 0) then
                        write(nllsTR_ounit, fmt = 100) 'Initial state contains invalid values.'
                        info = -1
                        return
                  endif
            endif
            !
            !! Prepare variables to handle the request for output of the initial state
            is_firstFval = .true.
            is_firstJacobi = .true.
            !! Check if it is requested to preserve the initial solution, allocate storage if so.
            if (present(SolutionInitOut)) then
                  linfo = SolutionInitOut%init(n, m)
                  SolutionInitOut%vX = vX
            endif
            !
            !! Initialize MKL solver
            res = dtrnlspbc_init(handle, 5, 5, vX, vLW, vUP, eps, 350, 50, 0.1_DP)
            ! Check result
            if (checkMKLRescode(res, 'initialization of TR nlls solver', nllsTR_ounit) /= 0) return
            if (nllsTR_iw > 2) write( nllsTR_ounit, fmt = 200) 'TR initialized, handle: ', handle
            !
            call IEEE_SET_FLAG (IEEE_ALL, .FALSE.)  ! Hush up all the FP exceptions.
            !
            bindState: associate (vFval => objFx%state%vF, mJacobi => objFx%state%mJ)
                  !! RCI loop for 'solve'
                  next_solve = .true.
                  RCI_Req = 0
                  RCI_Count = 0
                  linfo = 0
                  !
                  do while (next_solve)
                        RCI_Count = RCI_Count+1
                        !
                        if (trapFPErrors()) then
                              write(nllsTR_ounit, fmt = 200) 'Warning: floating point problem before the solver, RCI_Count',RCI_Count
                        endif
                        !
                        res = dtrnlspbc_solve(handle, vFval, mJacobi, RCI_Req)
                        !
                        if (trapFPErrors()) then
                              write(nllsTR_ounit, fmt = 200) 'Warning: floating point problem after the solver, RCI_Count',RCI_Count
                        endif
                        call IEEE_SET_FLAG (IEEE_ALL, .FALSE.)  ! Hush up all the FP exceptions.
                        !
                        if (res /= TR_SUCCESS) exit
                        ! Make sure that the vX is still inside the constraints
                        where (vX < vLW) vX = vLW
                        where (vX > vUP) vX = vUP
                        ! RCI status
                        select case (RCI_Req)
                            case (-6:-2)
                                    next_solve = .false.
                              !!-----------------------------------------------------------------------
                              case(-1)          ! Iteration count has been exceeded
                                    next_solve = .false.
                              !!-----------------------------------------------------------------------
                              case(0)           ! Successful
                                    next_solve = .true.
                              !!-----------------------------------------------------------------------
                              case(1)           ! Recalculate function at vector x
                                    if (nllsTR_iw > 2) write( nllsTR_ounit, fmt = 100) 'Recalculation of the vF'
                                    ! Use initial guess specified by the user, just once.
                                    linfo = 0
                                    if (.not. use_init_vFval) then
                                          call objFx%objectiveEval(vX, linfo)
                                          if ((linfo == 0) .and. (config%use_input_checks)) &
                                                call checkSolverInput(vF = vFval, info = linfo)
                                    endif
                                    ! Terminate the RCI loop on error in vF
                                    if (linfo /= 0) then
                                          write(nllsTR_ounit, fmt = 100) 'Cannot recalculate the objective function.'
                                          exit
                                    endif
                                    use_init_vFval = .false.
                                    if (nllsTR_iw >= 1) write( nllsTR_ounit, '(A, F15.10, 1X, A, F15.10)')   &
                                                       '||X|| = ', norm2(vX),             &
                                                       '||vF|| = ', norm2(vFval)
                                    if (nllsTR_iw > 2) then
                                          write(nllsTR_ounit, fmt = 100) 'X'
                                          write(nllsTR_ounit, fmt = 500) (vX(i), i = 1, n)
                                          write(nllsTR_ounit, fmt = 100) 'vF'
                                          write(nllsTR_ounit, fmt = 500) (vFval(i), i = 1, m)
                                    endif
                                    ! Store the initial guess if requested to do so
                                    if (is_firstFval .and. present(SolutionInitOut)) then
                                          SolutionInitOut%vF = vFval
                                    endif
                                    is_firstFval = .false.
                              !!-----------------------------------------------------------------------
                              case(2)           ! Recalculate Jacobian
                                    if (.not.(config%constJacobi)) then
                                          if (nllsTR_iw > 2) write( nllsTR_ounit, fmt = 100) 'Recalculation of the Jacobi matrix'
                                          linfo = 0
                                          if (.not. use_init_mJacobi) then
                                               objfx%state%mj = calc_jacobi(vx)
                                          endif
                                          use_init_mJacobi = .false.
                                          !
                                          if (nllsTR_iw > 3) then
                                               write( nllsTR_ounit, fmt = 100)  'Jacobi matrix  -->'
                                                call writeMatrix(mJacobi,  nllsTR_ounit)
                                                write( nllsTR_ounit, fmt = 100)  'Jacobi matrix  <--'
                                                flush( nllsTR_ounit)
                                          endif
                                    endif
                                    ! Store the initial guess if requested to do so
                                    if (is_firstJacobi .and. present(SolutionInitOut)) then
                                          SolutionInitOut%mJ =  mJacobi
                                    endif
                                    is_firstJacobi = .false.
                              !!-----------------------------------------------------------------------
                              case default      ! Unknown RCI, it should never happen!!
                                    write( nllsTR_ounit, fmt = 100) 'Error: unknown RCI control code!!!'
                                    exit
                        end select
                  end do
            end associate bindState
            !
            if (RCI_Req == -1) then
                  ! RCI loop finished due to iteration count, issue a warning.
                  info = 1
            else
                  info = 0
            endif
            !
            ! Query solution info
            res = dtrnlspbc_get(handle, resultInfo%iteration, resultInfo%stop_criterion, resultInfo%r1, resultInfo%r2)
            r1 = resultInfo%r1
            r2 = resultInfo%r2
            !
            if (present(resInfo)) resInfo = resultInfo
            !
            if (nllsTR_iw > 0) then
                  call nlls_TR_exit_message(message, resultInfo, config, linfo)
                  write( nllsTR_ounit, fmt = 200) 'Stop criterion code: ', resultInfo%stop_criterion
                  write( nllsTR_ounit, fmt = 100) trim(message)
                  write( nllsTR_ounit, '(A, 1X, I0, 2(1X, A, 1X, E16.8))') 'Step ',resultInfo%iteration, 'R0=', r1, 'R1=',r2
            endif
            if (nllsTR_iw > 2) then
                  write( nllsTR_ounit, fmt = 100) 'X = '
                  write( nllsTR_ounit, fmt = 500) (vX(i), i = 1, n)
            endif

            ! Release MKL resources
            res = dtrnlspbc_delete(handle)
            if (res /= TR_SUCCESS) then
                  write( nllsTR_ounit, fmt = 200) 'dtrnlspbc_delete failed, exit code:',res
            endif
            call mkl_free_buffers()
            !
            ! Formats
            100 format(A)           ! fmt = 100  ! just a string
            200 format(A, 1X, I0)     ! fmt = 400  ! a string followed by an integer
            500 format(F15.8, 1X)    ! fmt = 500  ! long float, followed by one space
            !
      end subroutine

      subroutine nlls_TR_exit_message(str, resInfo, config, info)
      implicit none
      character(len=*), intent(out)                    :: str
      type(nllsTRRes), intent(in)                      :: resInfo
      type(nllsTRConf), intent(in)                     :: config
      integer, intent(out)                             :: info
      !
            ! See documentation of ?trnlspbc_get in MKL manual for
            ! meaning of the stop criterion codes.
            info = 0
            select case (resInfo%stop_criterion)
            case(1)
                  write(str, fmt = 200) 'The TR solver exceeded the maximal number of iterations:',resInfo%iteration
            case(2)
                  write(str, fmt = 201) 'Area of the trust region is smaller than',config%eps(1)
            case(3)
                  write(str, fmt = 201) 'Requested quality of the solution is reached. ||F(x)|| is smaller than',config%eps(2)
            case(4)
                  write(str, fmt = 201) 'The Jacobian matrix is singular. ||J(x)[:,i]|| is smaller than',config%eps(3)
            case(5)
                  write(str, fmt = 201) 'Size of the trial step is smaller than',config%eps(4)
            case(6)
                  write(str, fmt = 201) 'Achievable improvement to the solution is smaller than',config%eps(5)
            case default
                  str = 'TR solver has prematurely stopped for unknown reason.'
                  info = -1
            end select
            200   format(A, 1X, I0)
            201   format(A, 1X, E15.7)
      !
      end subroutine


      subroutine checkSolverInput(vF, mJ, info)
      use, intrinsic:: IEEE_EXCEPTIONS
      use, intrinsic:: IEEE_ARITHMETIC
      implicit none
      real(DP), dimension(:), intent(in), optional     :: vF
      real(DP), dimension(:,:), intent(in), optional   :: mJ
      integer, intent(out)                                   :: info
      !
            info = 0
            if (present(vF)) then
                  ! Test for NaN and Infty
                  if ( any(IEEE_IS_NAN(vF)) ) then
                        write(nllsTR_ounit, fmt = 101) 'the objective function'
                        info = -1
                  endif
                  if (.not. all(IEEE_IS_FINITE(vF)) ) then
                        write(nllsTR_ounit, fmt = 102) 'the objective function'
                        info = -1
                  endif
            endif
            if (present(mJ)) then
                  ! Test for NaN and Infty
                  if ( any(IEEE_IS_NAN(mJ)) ) then
                        write(nllsTR_ounit, fmt = 101) 'the Jacobian'
                        info = -1
                  endif
                  if (.not. all(IEEE_IS_FINITE(mJ)) ) then
                        write(nllsTR_ounit, fmt = 102) 'the Jacobian'
                        info = -1
                  endif
            endif
            !
            101 format('Error: NaN is detected in ',A)  ! For NaN messages
            102 format('Error: Inf is detected in ',A)  ! For Inf messages
      !
      end subroutine

      !> Check floating point exceptions
      logical function trapFPErrors()
      use, intrinsic:: IEEE_EXCEPTIONS
      use, intrinsic:: IEEE_ARITHMETIC
      implicit none
      !
      integer:: i
      ! We do not investigate:
      !  - the first component of IEEE_ALL, namely: IEEE_OVERFLOW
      !  - the last component of IEEE_ALL, namely IEEE_INEXACT
      integer, parameter:: firstflag = 2
      logical:: fp_errflags(firstflag:size(IEEE_ALL)-1)
      !
            do i = firstflag, ubound(fp_errflags, 1)
                  call IEEE_GET_FLAG(IEEE_ALL(i), fp_errflags(i))
            enddo
            trapFPErrors = any(fp_errflags)
      !
      end function

    !>@Brief Calculate the local change in the stress response at a given strain mode (== Jacobi matrix of AlTay)
    !>@Details Internally calls MKL, which uses a finite differences method.
    recursive function calc_jacobi(strain_mode, interval) result(jacobi)
        real(DP), dimension(5), intent(in):: strain_mode    !> Strain mode at which to calculate the Jacobi
        real(DP), intent(in), optional:: interval           !> Optional initial interval for the finite difference algorithm.
        real(DP), dimension(5, 5):: jacobi                  !> The Jacobi

        character(*), parameter:: PROC_NAME = 'calc_jacobi'

        integer:: i
        real(DP):: eps

        !Initial value experimentally determined to be optimal
        eps = merge(interval, 2.E-2_DP, present(interval))

        if (djacobi(altay_wrapper, 5, 5, jacobi, strain_mode, eps) /= TR_SUCCESS) &
            call log_error(MOD_NAME, PROC_NAME, ERR, 'Internal MKL error')

        do i = 1, 5
            if (norm2(jacobi(:,i)) < TOLERANCE) then
                if (eps < 1._DP) then
                    !Increase in eps experimentally determined to be optimal
                    jacobi = calc_jacobi(strain_mode, 2*eps)
                    return
                else
                    call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Interval too large')
                end if
            end if
        end do
    contains
        !>@Brief Wrapper for calling AlTay from within MKL. See djacobi documentation.
        subroutine altay_wrapper(m, n, strain_mode, stress_mode)
            integer, intent(in):: m !> Needed by MKL
            integer, intent(in):: n !> Needed by MKL
            real(DP), dimension(n), intent(in):: strain_mode    !> Strain mode to calculate the stress response for
            real(DP), dimension(m), intent(out):: stress_mode   !> Stress response of the material

            integer:: info
            real(DP):: v_grad(3, 3)

            v_grad = convert_stress_strain_space(strain_mode/norm2(strain_mode))

            call initstepdata(1, astate, info)

            associate (input => astate%simulCalls(1)%input)
                input%dgf = v_grad
                input%keep_texture = .true.
                input%keep_state = .true.
                input%full_model = .false.
                input%do_output_init = .false.
                input%do_output_final = .false.
            end associate

            stress_mode = convert_stress_strain_space(altay_get_stress_state(v_grad))

            !Normalize for good measure and multiply by-1 because the least squares problem for which we are calculating the jacobi is
            !(target_stress_mode-stress_mode(strain_mode)) and thus its jacobi is (0-(jacobi(stress_mode(strain_mode))))
            !Round to TOLERANCE to compensate for variations in the results due to scheduling. The underlying model can never nearly as accurate anyway.
            stress_mode = anint(-stress_mode/norm2(stress_mode)/TOLERANCE) * TOLERANCE
        end subroutine
    end function

!----------------------------------------------------------------------------------------------------------------------------------
! Private components
!----------------------------------------------------------------------------------------------------------------------------------

      !> \internal
      !> Performs a check of TR MKL specific return values. If the value
      !> indicates an error conditions, the function will write an error message
      !> containing 'decrypted' description of error.
      integer function checkMKLRescode(res, leadmsg,  ounit)
      implicit none
      integer, intent(in)            :: res
      character(len=*), intent(in)   :: leadmsg
      integer, intent(in)            :: ounit
      !!
      character(len = 20)             :: errname
      !!
            checkMKLRescode = 0
            if (res /= TR_SUCCESS) then
                  checkMKLRescode = -1
                  select case (res)
                        case(TR_INVALID_OPTION)
                                    errname = 'TR_INVALID_OPTION'
                        case(TR_OUT_OF_MEMORY)
                                    errname = 'TR_OUT_OF_MEMORY'
                        case default
                                    errname = 'Unknown'
                  end select
                  write( ounit, 9900) leadmsg, errname
            endif
      9900 format(A, 1X, 'failed, reason:',1X, A)
      end function

      subroutine writeMatrix(A, ounit)
      implicit none
      real(DP), dimension(:,:), intent(in):: A
      integer, intent(in)                         :: ounit
      !
      integer:: l, u, i, j
      l = lbound(A, dim = 2)
      u = ubound(A, dim = 2)
      do i = lbound(A, dim = 1), ubound(A, dim = 1)
            write(ounit, '(ES18.9E3, 1X, $)') (A(i, j), j = l, u)  ! does not conform f2003, but no temporary needed
            write(ounit, *)
      end do
      end subroutine

            !
            ! Methods of SolutionPoint
            !

            integer function SolutionPoint_Init(this, n_X_dim, m_F_dim) result(info)
            class(SolutionPoint), intent(out)    :: this
            integer, intent(in)                  :: n_X_dim
            integer, intent(in)                  :: m_F_dim
            integer:: memstat
            !
                  info = -1
                  if ((n_X_dim <= 0) .or. (m_F_dim <= 0)) return
                  this%n_X_dim = n_X_dim
                  this%m_F_dim = m_F_dim
                  allocate(this%vX(n_X_dim), this%vF(m_F_dim), this%mJ(m_F_dim, n_X_dim), stat = memstat)
                  if (memstat == 0) info = 0
            end function


            integer function SolutionPoint_Copy(this, other) result(info)
            class(SolutionPoint), intent(out)    :: this
            class(SolutionPoint), intent(in)     :: other
            !
                  info = SolutionPoint_Init(this, other%n_X_dim, other%m_F_dim)
                  if (info == 0) then
                        this%vX = other%vX
                        this%vF = other%vF
                        this%mJ = other%mJ
                  endif
            end function

            subroutine SolutionPoint_Finalize(this)
            class(SolutionPoint), intent(inout)    :: this
            !
                  if (allocated(this%vX)) deallocate(this%vX)
                  if (allocated(this%vF)) deallocate(this%vF)
                  if (allocated(this%mJ)) deallocate(this%mJ)
                  this%n_X_dim = 0
                  this%m_F_dim = 0
            end subroutine


            !
            ! Methods of objectiveFunction
            !

            !> Initialization. It must be called by all derived classes.
            subroutine objectiveFunction_initFx(this, n_X_dim, m_F_dim, info)
            class(objectiveFunction), intent(inout)      :: this
            integer, intent(in)                          :: n_X_dim
            integer, intent(in)                          :: m_F_dim
            integer, intent(out)                         :: info
            !
                  info = -1
                  if (this%state%init(n_X_dim, m_F_dim) == 0) info = 0
                  this%state%vX = 0.D0
                  this%state%vF = 0.D0
                  this%state%mJ = 0.D0
            !
            end subroutine

            !> Returns rank-one two-elemental array containing:
            !> (1) dimension of variables X and (2) number of components in the function F.
            pure function getProblemSize(this) result(outval)
            class(objectiveFunction), intent(in)       :: this
            integer, dimension(2)                      :: outval
            !
                  outval = [ this%state%n_X_dim, this%state%m_F_dim ]
            !
            end function

            !> Returns dimension of variables X.
            pure function getXSize(this)
            class(objectiveFunction), intent(in)         :: this
            integer                                   :: getXSize
            !
                  getXSize = this%state%n_X_dim
            !
            end function

            !> Returns number of components in the objective function F.
            pure function getFSize(this)
            class(objectiveFunction), intent(in)         :: this
            integer                                   :: getFSize
            !
                  getFSize = this%state%m_F_dim
            !
            end function




            !
            ! Methods of trackableObjFunc
            !


            !> Basic implementation of trackableObjInterface. The method defers actual
            !> calculations of the Jacobi matrix to the derived class. The implementation
            !> of objectiveEval should call this method at the end of its execution.
            subroutine trackableObjFunc_objectiveEval(this, vX, info)
            class(trackableObjFunc), intent(inout)     :: this
            real(DP), dimension(:), intent(in)  :: vX       !< Dimension must be: [n_X_dim]
            integer, intent(out)                       :: info
            !
                  call this%track(vX, TOF_Function, info)
            !
            end subroutine

            !> Basic implementation of trackableObjInterface. The method defers actual
            !> calculations of the Jacobi matrix to the derived class. The implementation
            !> of jacobiMatrixEval should call this method at the end of its execution.
            subroutine trackableObjFunc_jacobiMatrixEval(this, vX, info)
            class(trackableObjFunc), target, intent(inout)     :: this
            real(DP), dimension(5), intent(in)  :: vX       !< Dimension must be: [n_X_dim]
            integer, intent(out)                       :: info
            !
                  call this%track(vX, TOF_Jacobian, info)
            !
            end subroutine

            !> Tracking of the evaluations
            subroutine trackableObjFunc_track(this, vX, request, info)
            class(trackableObjFunc), target, intent(inout)     :: this
            real(DP), dimension(5), intent(in)  :: vX       !< Dimension must be: [n_X_dim]
            integer, intent(in)                        :: request
            integer, intent(out)                       :: info
            !
                  info = 0
                  select case(this%tracking_level)
                  !
                  case(TOF_None)
                        continue
                  !
                  case(TOF_Function)
                        if (request == TOF_Function) this%state%vX = vX
                  !
                  case(TOF_Jacobian)
                        ! lhs-reallocation if needed
                        if (request == TOF_Jacobian) this%vXofJacobi = vX
                  !
                  case(TOF_All)
                        ! Handle action for two requests:
                        if (request == TOF_Function) this%state%vX = vX
                        ! lhs-reallocation if needed
                        if (request == TOF_Jacobian) this%vXofJacobi = vX
                  case default
                        info = -1
                  end select
            !
            end subroutine


end module

