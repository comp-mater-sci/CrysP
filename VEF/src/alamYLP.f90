!>    \file alamYLP.f90 The file contains modules that calculate
!>          yield locus position directly from the ALAMEL model.
!>


!> Implementation of YLP function that can directly use the ALAMEL multilevel model instead of a plastic potential function.
module alamYLP
use definitions
implicit none

      type multilevelYLPConfig
            !> Epsilon used for numerical estimation of Jacobi matrix.
            !>
            !> Note: this is a reasonable value. Lowering it can lead to poor convergence or lack of convergence.
            real(DP)        :: jacobi_eps = 5.e-1_DP
            !> Request for preliminary solution of linearized problem
            logical                 :: linearize = .true.
            !> Request for solving the non-linear problem
            logical                 :: nonlinear = .true.
            !> Default epsilon to be set for all TR-solver convergence criteria, except ||F||_2
            real(DP)        :: default_eps = 1.e-5_DP
            !> Epsilon to be set on norm of objective function ||F||_2
            real(DP)        :: obj_func_eps = 1.e-3_DP

            !> Request for usage of full multilevel model in the search phase
            logical                 :: search_full_model = .false.

            !> Request for a full multilevel call in the very last evaluation
            !> of the objective function.
            logical                 :: evaluate_full_model = .true.
      end type

    contains


      !> Calculates plastic strain rate corresponding to given deviatoric stress
      !>
      !> The subroutine assumes that multilevel model is already configured and initialized.
      !> Exit code is retured in info: VEF_OK on success; VEF_ERROR if no converged solution can
      !> be found; VEF_ERROR or VEF_ERROR if error conditions have been detected.
      subroutine multilevelYLP(vS,vA,vSonA,R,info,useVMGuess,YLPconfig,outunit,verbose,objective_function)
      use nllsTR
      use alamEval
      implicit none
      real(DP),intent(in)   :: vS(alamEval_vSD_dim)      !< Imposed stress vector
      real(DP),intent(inout):: vA(alamEval_vSD_dim)      !< Strain rate mode on yield locus
      real(DP),intent(out)  :: vSonA(alamEval_vSD_dim)   !< Stress vector corresponding to A
      real(DP),intent(out)  :: R          !< Square norm of residual error
      integer                       :: info       !< Exit code
      !> Flag: use von Mises initial guess, otherwise assume vA as an initial strain rate (default: .true.)
      logical,optional,intent(in)   :: useVMGuess
      type(multilevelYLPConfig),optional,intent(in) :: YLPconfig !< Configuration parameters to be imposed to the search method
      integer,intent(in),optional   :: outunit    !< Unit number for messages
      integer,intent(in),optional   :: verbose
      class(NormalizedV5DComp),target,optional,intent(inout) :: objective_function
      !

      real(DP), dimension(alamEval_vSD_dim) :: vX, vX_lin
      type(multilevelYLPConfig) :: config !< Effective configuration parameters (defaults on entry)
      !
      !
      class(NormalizedV5DComp),pointer :: objFunc
      ! Default objective function declared as local variable: it will get
      ! deallocated on return.
      type(NormalizedV5DComp),allocatable,target :: objective_function_local
      type(nllsTRConf)        :: tr_config
      real(DP)        :: r1,r2
      logical                 :: use_vmGuess
      logical                 :: attempt_linearized,linearized_successful
      real(DP)        :: r1_lin,r2_lin
      type(nllsTRRes)         :: TR_res
      type(SolutionPoint)     :: initState
      integer                 :: ounit, tr_verbose, ierr
      integer,parameter       :: stdout = 6
      logical                 :: log_info,log_debug
      real(DP)        :: norm
      !
      if (present(useVMGuess)) then
            use_vmGuess = useVMGuess
      else
            use_vmGuess = .true.
      endif
      tr_verbose = 0
      log_info = .false.
      log_debug = .false.
      if (present(verbose)) then
            if (verbose > 2) then
                  log_info = .true.
                  tr_verbose = 1
            endif
            if (verbose > 3) then
                  log_debug = .true.
                  tr_verbose = 3
            endif
      endif
      ! Override the defaults by the user's settings:
      if (present(YLPconfig)) config = YLPconfig
      ! Check if the configuration is consistent and allows at least one
      ! search procedure to be started.
      info  = VEF_ERROR
      if (.not. (config%linearize .or. config%nonlinear)) return
      !
      ! Set the objective function
      if (present(objective_function)) then
          objFunc => objective_function
      else
          allocate(objective_function_local)
          objFunc => objective_function_local
      endif
      ! Configure objective function
      call objFunc%initFx(alamEval_vSD_dim,alamEval_vSD_dim,ierr)
      if (ierr /= 0) then
          info = VEF_ERROR
          return
      endif
      info  = VEF_ERROR
      !
      !Get normalized stress vector
      norm = norm2(vS)
      if (norm < epsilon(0.D0)) return
      objFunc%vSn = vS / norm
      objFunc%full_model = config%search_full_model
      !
      ounit = stdout
      if (present(outunit))  ounit = outunit
      ! Initialize TR solver
      ! (note: outunit argument has "optional" modifier in both the caller and callee)
      call nlls_TR_init(outunit,tr_verbose)
      ! Use von Mises guess
      vX = merge(vS,vA,use_vmGuess)
      !
      r1 = 0.0_DP; r2 = 0.0_DP
      !
      !!! call objFunc%jacobiMatrixFx(vX, mJ,info)
      !
      ! TODO: more reliable lower limit, it should lead to tr(d) > 1.e-7
      !
      tr_config%lo_limit = -10.0_DP
      tr_config%up_limit = 10.0_DP
      tr_config%init_step = 100.0_DP
      ! Impose configuration settings
      ! Tr:
      tr_config%eps = config%default_eps   !<< beware!
      tr_config%eps(2) = config%obj_func_eps  ! Norm of F: ||F||_2
      ! Obj func:
      objFunc%jacobi_eps = config%jacobi_eps
      attempt_linearized = config%linearize
      !
      linearized_successful = .false.
      !
      ! Run linearized problem if requested
      if (attempt_linearized) then
            tr_config%constJacobi = .true.
            vX_lin = vX
            ! Start the TR solver for linearized problem
            ! More thorough exit status is necessary: TR_res
            call nlls_TR_solve(objFunc,vX_lin,tr_config,r1_lin,r2_lin,ierr,TR_res,SolutionInitOut=initState)
            R = r2_lin
            ! do checks if the solution is OK:
            ! Stop criterion: magic number "3" means: ||F(x)||_2 < eps(2)
            if ( (r2_lin <= r1_lin) .and. (TR_res%stop_criterion == 3) .and. (r2_lin <= tr_config%eps(2)) ) then
                  vX = vX_lin
                  linearized_successful = .true.
            endif
      endif
      ! The linearized analysis is either not done or failed.
      if (.not. linearized_successful .and. config%nonlinear) then
            ! Set non-linear analysis
            tr_config%constJacobi = .false.
            !
            ! initState is invalid if nlls_TR_solve in the "if (attempt_linearized)"
            ! branch above returns ierr /= 0
            if (attempt_linearized .and. (ierr == 0)) then
                  ! Profit from the initial point stored by the solver for the linearized problem
                  ierr = objFunc%state%copy(initState)
                  tr_config%use_init_state = (ierr == 0)
            endif
            ! start TR solver
            call nlls_TR_solve(objFunc,vX,tr_config,r1,r2,ierr)
            R = r2
            !TODO: check exit status of the solver
            if (attempt_linearized) then
                  ! choose better of non-linear and linearized solution
                  if (r2 > r2_lin) then
                        vX = vX_lin
                        R = r2_lin
                  endif
            endif
      endif
      call initState%finalize()
      !
      ! Set output strain rate
      info  = VEF_ERROR
      norm = norm2(vX)
      if (norm < epsilon(0.D0)) return
      vA = vX / norm
      ! Call objective function again to get corresponding yield stress and other quantities.
      objFunc%full_model = config%evaluate_full_model
      call objFunc%objectiveEval(vA,ierr)

      if (log_info) write(ounit,'(A,1X,5(E15.8,1X))') 'Final residual vector: ',objFunc%state%vF

      vSonA = objFunc%vSml
      info = merge(VEF_FAIL,VEF_OK,R > config%obj_func_eps)

      end subroutine

end module
