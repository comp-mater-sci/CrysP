!
! $Id$
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>    \author     Jerzy Gawad 
!>    Email:      Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    
!>    \date Date of initial release: 2010-06-25
!>    $Revision$
!>    $Date$
!>    History of modifications: (see SVN log).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!

!> nllsTR  -- wrapper module for MKL Non-Linear Least Squares Trust Region algorithm
!> \remark The module can be compiled with: ifort >= 12.1, gfortran >= 4.6. Earlier versions
!>         are unable to handle OO technique in a proper way or simply fail at compilation time. 
module nllsTR
use objectiveFx


      !> Control over Trust Region algorithm
      type nllsTRConf
            !> Array of parameters controling stop criteria
            !>
            !> Various convergence criteria are evaluated, see MKL documentation for details
            double precision,dimension(6)                :: eps = 1.e-10
            integer                                      :: iter1 = 300 !< Maximum number of iterations
            integer                                      :: iter2 = 50  !< Maximum number of trial steps
            double precision                             :: init_step = 100.0  !< Initial step bound factor
            !> Lower constraints for design vector
            double precision                             :: lo_limit = 0.D0
            !> Upper constraints for design vector
            double precision                             :: up_limit = 1.D2

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
      

      !> Default step for finite difference evaluation of Jacobi matrix
      double precision,parameter                        :: nllsTR_jacobi_eps = 1.D-7


      !>@{ \name Other parameters 
      !>  These parameters are not directly accessible. Use \ref nlls_TR_init to control them. \sa nlls_TR_init

      !> Output unit
      integer,private                            :: nllsTR_ounit = 6

      !> Verbosity level. 
      !>
      !> The following values of verbosity are allowed:
      !>   -  0 - only error messages, 
      !>   -  1 - some diagnostic informations, 
      !>   -  2 and higher - detailed informations (huge amount of output is expected!)
      integer,private                            :: nllsTR_iw = 0

     !>@} 

      type nllsTRRes
            integer                             :: iteration = 0        !< Interation number
            integer                             :: stop_criterion = 0   !< Identifier of stop criterion, see MKL documentation
            double precision                    :: r1 = 0.D0            !< Initial norm of residual
            double precision                    :: r2 = 0.D0            !< Final norm of residual
      end type 


      type,abstract,extends(objectiveFunction) :: MKLFDJacobiObjFunction
            double precision                          ::  jacobi_eps = nllsTR_jacobi_eps 
      contains
            procedure :: jacobiMatrixEval => JacobiObjEval_djacobi
      end type


      ! Internal components of the module.
      ! private writeMatrix
      private checkMKLRescode 
      
contains 
 


      !> Initialization of nlls_TR module. 
      subroutine nlls_TR_init(ounit,verbose)
      implicit none
      integer,optional,intent(in)         ::  ounit  !< IO unit number for outputs
      integer,optional,intent(in)         ::  verbose !< Verbosity level, \sa nllsTR_iw
      !!
      if (present(ounit)) nllsTR_ounit = ounit
      if (present(verbose)) nllsTR_iw = verbose
      end subroutine
      

      !> This subroutine solves the mnimization problem. Trust region algorithm from MKL library is used.
      !> \param objFx Objective function to minimize
      !> \param jacobiFx Function that calculates Jacobi matrix of objective function
      subroutine nlls_TR_solve(objFx,vX,config,r1,r2,info,resInfo,SolutionInitOut)
      use, intrinsic :: IEEE_EXCEPTIONS
      use, intrinsic :: IEEE_ARITHMETIC
      implicit none
      include  "mkl_rci.fi"
      ! Formal parameters
      class(objectiveFunction),intent(inout)          :: objFx    !< objective function
      !> Design vector, dimension of vX must correspond to those in objFX
      double precision,dimension(:),intent(inout)     :: vX       
      type(nllsTRConf),intent(in)                     :: config   !< Configuration of nllsTR 
      double precision,intent(out)                    :: r1       !< Initial residual of the solution
      double precision,intent(out)                    :: r2       !< Final residual of the solution
      !> Exit code: 0 on success, < 0 on error, > 0 on failure/warning
      integer,intent(out)                             :: info
      !> Full termination status of the TR solver
      type(nllsTRRes),intent(inout),optional          :: resInfo  
      !> Initial solution to be stored after initial evaluation
      type(SolutionPoint),intent(out),optional        :: SolutionInitOut 
      !!!! Local variables
      integer                              :: n        !< Dimension of design vector
      integer                              :: m        !< Dimension of objective function vector
      integer(kind=8)   :: handle
      integer           :: res, linfo 
      ! 
      double precision,allocatable,dimension(:)    :: vLW, vUP   ! would be of size    
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
      character(len=512)             :: message
      !---------------------------------------------------
            handle = 0; RCI_Req = 0; next_solve = .true.
            info = -1
            !! Check preconditions
            ! TODO 
            n = objFx%state%n_X_dim        ! Dimensionality of vector X (argument)
            m = objFx%state%m_F_dim        ! Dimensionality of objective function
            !
            !! Allocate memory
            allocate(vLW(n),vUP(n),stat=ierr)
            if (ierr /= 0) return
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
                  call objFx%jacobiMatrixEval(vX, linfo)
                  if ( (linfo == 0) .and. (config%use_input_checks) ) &
                        call checkSolverInput(mJ=objFx%state%mJ,info=linfo)
                  if (linfo /= 0) then
                        write(nllsTR_ounit,fmt=100) 'Cannot calculate initial Jacobi matrix.'
                        info = -1
                        return
                  endif
                  if (nllsTR_iw > 3) then
                        write( nllsTR_ounit,fmt=100)  'Jacobi matrix  -->'
                        call writeMatrix(objFx%state%mJ, nllsTR_ounit)
                        write( nllsTR_ounit,fmt=100)  'Jacobi matrix  <--'
                        flush(nllsTR_ounit)
                  endif
            endif
            !
            if ( (config%use_input_checks) .and. (config%use_init_state) ) then
                  call checkSolverInput(objFx%state%vF, objFx%state%mJ,linfo)
                  if (linfo /= 0) then
                        write(nllsTR_ounit,fmt=100) 'Initial state contains invalid values.'
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
                  linfo = SolutionInitOut%init(n,m)
                  SolutionInitOut%vX = vX
            endif
            !
            !! Initialize MKL solver
            handle = 0
            res = dtrnlspbc_init(handle, n, m, vX, vLW, vUP, config%eps,  config%iter1,  config%iter2,  config%init_step)
            ! Check result      
            if (checkMKLRescode(res,'initialization of TR nlls solver', nllsTR_ounit) /= 0) return
            if (nllsTR_iw > 2) write( nllsTR_ounit,fmt=200) 'TR initialized, handle: ', handle
            !
            call IEEE_SET_FLAG (IEEE_ALL,.FALSE.) ! Hush up all the FP exceptions.
            !
            bindState: associate (vFval => objFx%state%vF, mJacobi => objFx%state%mJ)
                  !! RCI loop for 'solve'
                  next_solve = .true.
                  RCI_Req = 0
                  RCI_Count = 0
                  linfo = 0
                  !
                  do while (next_solve)
                        RCI_Count = RCI_Count + 1
                        !
                        if (trapFPErrors()) then
                              write(nllsTR_ounit,fmt=200) 'Warning: floating point problem before the solver, RCI_Count',RCI_Count
                        endif
                        !                        
                        res = dtrnlspbc_solve(handle, vFval, mJacobi, RCI_Req)
                        !
                        if (trapFPErrors()) then
                              write(nllsTR_ounit,fmt=200) 'Warning: floating point problem after the solver, RCI_Count',RCI_Count
                        endif
                        call IEEE_SET_FLAG (IEEE_ALL,.FALSE.) ! Hush up all the FP exceptions.
                        !
                        if (res /= TR_SUCCESS) exit
                        ! Make sure that the vX is still inside the constraints
                        where (vX < vLW) vX = vLW
                        where (vX > vUP) vX = vUP
                        ! RCI status
                        select case (RCI_Req)
                              !!-----------------------------------------------------------------------
                              case(-1)          ! Iteration count has been exceeded
                                    next_solve = .false.      
                              !!-----------------------------------------------------------------------
                              case(-6:-2)       ! Epsilon has been reached
                                    next_solve = .false.      
                        
                              !!-----------------------------------------------------------------------
                              case(0)           ! Successful
                                    next_solve = .true.
                              !!-----------------------------------------------------------------------
                              case(1)           ! Recalculate function at vector x
                                    if (nllsTR_iw > 2) write( nllsTR_ounit,fmt=100) 'Recalculation of the vF'
                                    ! Use initial guess specified by the user, just once.
                                    linfo = 0
                                    if (.not. use_init_vFval) then
                                          call objFx%objectiveEval(vX,linfo)
                                          if ((linfo == 0) .and. (config%use_input_checks)) &
                                                call checkSolverInput(vF=vFval,info=linfo)
                                    endif
                                    ! Terminate the RCI loop on error in vF
                                    if (linfo /= 0) then
                                          write(nllsTR_ounit,fmt=100) 'Cannot recalculate the objective function.'
                                          exit
                                    endif
                                    use_init_vFval = .false.
                                    if (nllsTR_iw >= 1) write( nllsTR_ounit,'(A,F15.10,1X,A,F15.10)')   &
                                                       '||X|| = ', norm2(vX),             &
                                                       '||vF|| = ', norm2(vFval)
                                    if (nllsTR_iw > 2) then
                                          write(nllsTR_ounit,fmt=100) 'X' 
                                          write(nllsTR_ounit,fmt=500) (vX(i), i=1,n) 
                                          write(nllsTR_ounit,fmt=100) 'vF' 
                                          write(nllsTR_ounit,fmt=500) (vFval(i), i=1,m)
                                    endif
                                    ! Store the initial guess if requested to do so
                                    if (is_firstFval .and. present(SolutionInitOut)) then
                                          SolutionInitOut%vF = vFval
                                    endif
                                    is_firstFval = .false.
                              !!-----------------------------------------------------------------------
                              case(2)           ! Recalculate Jacobian 
                                    if (.not.(config%constJacobi)) then
                                          if (nllsTR_iw > 2) write( nllsTR_ounit,fmt=100) 'Recalculation of the Jacobi matrix'
                                          linfo = 0 
                                          if (.not. use_init_mJacobi) then
                                                call objFx%jacobiMatrixEval(vX,linfo)
                                                if ((linfo == 0) .and. (config%use_input_checks)) &
                                                      call checkSolverInput(mJ=mJacobi,info=linfo)
                                          endif
                                          ! Terminate the RCI loop on error in Jacobi
                                          if (linfo /= 0) then
                                                write(nllsTR_ounit,fmt=100) 'Cannot recalculate the Jacobi matrix'
                                                exit
                                          endif
                                          use_init_mJacobi = .false.
                                          !
                                          if (nllsTR_iw > 3) then
                                               write( nllsTR_ounit,fmt=100)  'Jacobi matrix  -->'
                                                call writeMatrix(mJacobi,  nllsTR_ounit)
                                                write( nllsTR_ounit,fmt=100)  'Jacobi matrix  <--'
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
                                    write( nllsTR_ounit,fmt=100) 'Error: unknown RCI control code!!!'
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
            ! Check errors in evaluation of the objective function and the Jacobian
            if (linfo < 0) info = -1
            !
            ! Query solution info
            res = dtrnlspbc_get(handle, resultInfo%iteration, resultInfo%stop_criterion, resultInfo%r1, resultInfo%r2)
            r1 = resultInfo%r1
            r2 = resultInfo%r2
            !            
            if (present(resInfo)) resInfo = resultInfo
            !            
            if (nllsTR_iw > 0) then
                  call nlls_TR_exit_message(message,resultInfo,config,linfo)
                  write( nllsTR_ounit,fmt=200) 'Stop criterion code: ', resultInfo%stop_criterion
                  write( nllsTR_ounit,fmt=100) trim(message)
                  write( nllsTR_ounit,'(A,1X,I0,2(1X,A,1X,E16.8))') 'Step ',resultInfo%iteration, 'R0=', r1, 'R1=',r2 
            endif
            if (nllsTR_iw > 2) then
                  write( nllsTR_ounit,fmt=100) 'X = '
                  write( nllsTR_ounit,fmt=500) (vX(i), i=1,n)
            endif

            ! Release MKL resources
            res = dtrnlspbc_delete(handle)
            if (res /= TR_SUCCESS) then
                  write( nllsTR_ounit,fmt=200) 'dtrnlspbc_delete failed, exit code:',res 
            endif
            call mkl_free_buffers()
            ! Deallocate temporary arrays
            deallocate(vLW,vUP,stat=ierr)
            !
            ! Formats
            100 format(A)           ! fmt=100  ! just a string
            200 format(A,1X,I0)     ! fmt=400  ! a string followed by an integer
            500 format(F15.8,1X)    ! fmt=500  ! long float, followed by one space
            !
      end subroutine            

      subroutine nlls_TR_exit_message(str,resInfo,config,info)
      implicit none
      character(len=*),intent(out)                    :: str
      type(nllsTRRes),intent(in)                      :: resInfo
      type(nllsTRConf),intent(in)                     :: config
      integer,intent(out)                             :: info
      !
            ! See documentation of ?trnlspbc_get in MKL manual for 
            ! meaning of the stop criterion codes.
            info = 0
            select case (resInfo%stop_criterion)
            case(1)
                  write(str,fmt=200) 'The TR solver exceeded the maximal number of iterations:',resInfo%iteration
            case(2)
                  write(str,fmt=201) 'Area of the trust region is smaller than',config%eps(1)
            case(3)
                  write(str,fmt=201) 'Requested quality of the solution is reached. ||F(x)|| is smaller than',config%eps(2)
            case(4)
                  write(str,fmt=201) 'The Jacobian matrix is singular. ||J(x)[:,i]|| is smaller than',config%eps(3)
            case(5)
                  write(str,fmt=201) 'Size of the trial step is smaller than',config%eps(4)
            case(6)
                  write(str,fmt=201) 'Achievable improvement to the solution is smaller than',config%eps(5)
            case default
                  str = 'TR solver has prematurely stopped for unknown reason.'
                  info = -1
            end select
            200   format(A,1X,I0)            
            201   format(A,1X,E15.7)
      !
      end subroutine

      
      subroutine checkSolverInput(vF,mJ,info)
      use, intrinsic :: IEEE_EXCEPTIONS
      use, intrinsic :: IEEE_ARITHMETIC
      implicit none
      double precision,dimension(:),intent(in),optional     :: vF
      double precision,dimension(:,:),intent(in),optional   :: mJ
      integer,intent(out)                                   :: info
      !
            info = 0
            if (present(vF)) then
                  ! Test for NaN and Infty
                  if ( any(IEEE_IS_NAN(vF)) ) then 
                        write(nllsTR_ounit,fmt=101) 'the objective function'
                        info = -1
                  endif
                  if (.not. all(IEEE_IS_FINITE(vF)) ) then
                        write(nllsTR_ounit,fmt=102) 'the objective function'
                        info = -1
                  endif
            endif
            if (present(mJ)) then
                  ! Test for NaN and Infty
                  if ( any(IEEE_IS_NAN(mJ)) ) then
                        write(nllsTR_ounit,fmt=101) 'the Jacobian'
                        info = -1
                  endif
                  if (.not. all(IEEE_IS_FINITE(mJ)) ) then
                        write(nllsTR_ounit,fmt=102) 'the Jacobian'
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
      use, intrinsic :: IEEE_EXCEPTIONS
      use, intrinsic :: IEEE_ARITHMETIC
      implicit none
      !
      integer :: i
      ! We do not investigate:
      !  - the first component of IEEE_ALL, namely: IEEE_OVERFLOW
      !  - the last component of IEEE_ALL, namely IEEE_INEXACT
      integer,parameter :: firstflag = 2
      logical :: fp_errflags(firstflag:size(IEEE_ALL)-1)
      !
            do i=firstflag, size(fp_errflags)
                  call IEEE_GET_FLAG(IEEE_ALL(i),fp_errflags(i))
            enddo
            trapFPErrors = any(fp_errflags)
      !
      end function
      
      
      !> Calculation of Jacobi matrix by means of central difference method.
      !>
      !> This subroutine uses djacobi_solve RCI subroutine from MKL.
      subroutine JacobiObjEval_djacobi(this,vX, info)
      implicit none
      include  "mkl_rci.fi"
      class(MKLFDJacobiObjFunction),intent(inout)     :: this
      double precision,dimension(:),intent(in)        :: vX       !< Dimension must be: [n_X_dim]
      integer,intent(out)                             :: info

      integer     :: res, memstat
      integer(kind=8)   :: handle
      integer     :: RCI_Req
      logical     :: next_solve
      ! Temporary arrays f1 & f2 which contain: f1 = f(x+eps) | f2 = f(x-eps)
      double precision,allocatable,dimension(:)       :: f0, f1, f2, tmp_vX
      !
      handle = 0
      info = 1
      ! Check dimensions, return error if mismatch is detected.
      if (  (size(this%state%mJ,dim=1) /= this%state%m_F_dim) .or.      &
            (size(this%state%mJ,dim=2) /= this%state%n_X_dim) .or.      &
            (size(vX) /= this%state%n_X_dim)                      &
         ) return
      ! Allocate temporary arrays
      allocate(f0(this%state%m_F_dim), f1(this%state%m_F_dim), f2(this%state%m_F_dim), tmp_vX(this%state%n_X_dim), stat=memstat)
      if (memstat /= 0) return
      ! Protect the initial value of state%vF
      f0 = this%state%vF
      !
      tmp_vX = vX
      handle = 0
      res =  djacobi_init(handle, this%state%n_X_dim, this%state%m_F_dim, tmp_vX, this%state%mJ, this%jacobi_eps)
      ! detect error conditions
      if (checkMKLRescode(res,'recalculation of Jacobi matrix', nllsTR_ounit) /= 0) then
            info = 1
            return
      endif
      !
      ! Enter RCI loop
      info = 2
      next_solve = .true.
      RCI_Req = 0
      do while (next_solve)
            res = djacobi_solve(handle, f1, f2, RCI_Req)      
            !! TESTING -->>
            ! write(*,*) 'RCI_Req = ',RCI_Req, ' res = ', res, ' success = ', res == TR_SUCCESS
            !! TESTING <<--
            if (res /= TR_SUCCESS) exit
            ! RCI status
            select case (RCI_Req)
                  !!-----------------------------------------------------------------------
                  case(1)       
                        call this%objectiveEval(tmp_vX,info)
                        ! Grab the state
                        f1 = this%state%vF
                  !!-----------------------------------------------------------------------
                  case(2)       
                        call this%objectiveEval(tmp_vX,info)
                        ! Grab the state
                        f2 = this%state%vF
                  !!-----------------------------------------------------------------------
                  case(0)           ! Successful
                        info = 0
                        next_solve = .false.
                  case default           ! Unknown error conditionn
                        info = 3
                        exit
            end select
      enddo
      ! Restore the initial value of state%vF
      this%state%vF = f0
      ! Free temporary arrays
      deallocate(f0,f1,f2,tmp_vX)
      ! Finalize Jacobi solver, release resources
      res = djacobi_delete(handle)
      if (checkMKLRescode(res,'recalculation of Jacobi matrix', nllsTR_ounit) /= 0) then
            info = 1
      else
            info = 0
      endif
      end subroutine


!----------------------------------------------------------------------------------------------------------------------------------
! Private components
!----------------------------------------------------------------------------------------------------------------------------------

      !> \internal
      !> Performs a check of TR MKL specific return values. If the value
      !> indicates an error conditions, the function will write an error message
      !> containing 'decrypted' description of error.
      integer function checkMKLRescode(res, leadmsg,  ounit)
      implicit none
      include  "mkl_rci.fi"
      integer,intent(in)            :: res
      character(len=*),intent(in)   :: leadmsg
      integer,intent(in)            :: ounit
      !!
      character(len=20)             :: errname
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
                  write( ounit,9900) leadmsg, errname 
            endif
      9900 format(A,1X,'failed, reason:',1X,A)
      end function

      subroutine writeMatrix(A, ounit)
      implicit none
      double precision,dimension(:,:),intent(in) :: A
      integer,intent(in)                         :: ounit
      !
      integer :: l,u,i,j
      l = lbound(A,dim=2)
      u = ubound(A,dim=2)
      do i=lbound(A,dim=1),ubound(A,dim=1)
            write(ounit,'(E18.9,1X,$)') (A(i,j), j=l,u) ! does not conform f2003, but no temporary needed
            write(ounit,*)
      end do
      end subroutine

end module

