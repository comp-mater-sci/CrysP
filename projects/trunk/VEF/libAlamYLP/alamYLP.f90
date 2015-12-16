!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2010-11-03
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!>    \file alamYLP.f90 The file contains modules that calculate
!>          yield locus position directly from the ALAMEL model.      
!>                                  

    
!> Implementation of YLP function that can directly use the ALAMEL multilevel model instead of a plastic potential function.
module alamYLP
implicit none

      type multilevelYLPConfig
            !> Epsilon used for numerical estimation of Jacobi matrix.
            !>
            !> Note: this is a reasonable value. Lowering it can lead to poor convergence or lack of convergence.
            double precision        :: jacobi_eps = 5.E-2 
            !> Request for preliminary solution of linearized problem 
            logical                 :: linearize = .false.
            !> Default epsilon to be set for all TR-solver convergence criteria, except ||F||_2
            double precision        :: default_eps = 1.E-5
            !> Epsilon to be set on norm of objective function ||F||_2
            double precision        :: obj_func_eps = 1.E-3
            
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
      subroutine multilevelYLP(vS,vA,vSonA,R,info,useVMGuess,YLPconfig,outunit,verbose)
      use nllsTR
      use alamEval
      implicit none
      double precision,intent(in)   :: vS(alamEval_vSD_dim)      !< Stress vector
      double precision,intent(inout):: vA(alamEval_vSD_dim)      !< Strain rate mode on yield locus
      double precision,intent(out)  :: vSonA(alamEval_vSD_dim)   !< Stress vector corresponding to A
      double precision,intent(out)  :: R          !< Square norm of residual error
      integer                       :: info       !< Exit code
      !> Flag: use von Mises initial guess, otherwise assume vA as an initial strain rate (default: .true.)
      logical,optional,intent(in)   :: useVMGuess
      type(multilevelYLPConfig),optional,intent(in) :: YLPconfig !< Configuration parameters to be imposed to the search method
      integer,intent(in),optional   :: outunit    !< Unit number for messages
      integer,intent(in),optional   :: verbose
      !

      double precision, dimension(alamEval_vSD_dim) :: vX, vX_lin
      type(multilevelYLPConfig) :: config !< Effective configuration parameters (defaults on entry)
      ! 
      !
      type(NormalizedV5DComp) :: objFunc
      type(nllsTRConf)        :: tr_config
      double precision        :: r1,r2
      logical                 :: use_vmGuess
      logical                 :: attempt_linearized,linearized_successful
      double precision        :: r1_lin,r2_lin
      type(nllsTRRes)         :: TR_res
      type(SolutionPoint)     :: initState
      integer                 :: ounit
      integer,parameter       :: stdout = 6
      logical                 :: log_info,log_debug
      integer                 :: tr_verbose
      double precision        :: norm
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
      !
      ! Configure objective function      
      call objFunc%initFx(alamEval_vSD_dim,alamEval_vSD_dim,info)
      if (info /= 0) return 
      info  = -1
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
      if (use_vmGuess) then
            vX = vS
      else
            vX = vA
      endif
      !      
      r1 = 0.D0; r2 = 0.D0
      !
      !!! call objFunc%jacobiMatrixFx(vX, mJ,info)
      !
      ! TODO: more reliable lower limit, it should lead to tr(d) > 1.e-7
      !
      tr_config%lo_limit = -10.0
      tr_config%up_limit = 10.0
      tr_config%init_step = 100.0
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
            call nlls_TR_solve(objFunc,vX_lin,tr_config,r1_lin,r2_lin,info,TR_res,SolutionInitOut=initState)
            R = r2_lin
            ! do checks if the solution is OK:
            ! Stop criterion: magic number "3" means: ||F(x)||_2 < eps(2)
            if ( (r2_lin < r1_lin) .and. (TR_res%stop_criterion == 3) .and. (r2_lin <= tr_config%eps(2)) ) then
                  vX = vX_lin
                  linearized_successful = .true.
            endif
      endif
      ! The linearized analysis is either not done or failed.
      if (.not. linearized_successful) then
            ! Set non-linear analysis
            tr_config%constJacobi = .false.
            !
            ! initState is invalid if nlls_TR_solve in the "if (attempt_linearized)" 
            ! branch above returns info /= 0
            if (attempt_linearized .and. (info == 0)) then
                  ! Profit from the initial point stored by the solver for the linearized problem
                  info = objFunc%state%copy(initState)
                  tr_config%use_init_state = (info == 0)
            endif
            ! start TR solver
            call nlls_TR_solve(objFunc,vX,tr_config,r1,r2,info)
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
      info  = -1
      norm = norm2(vX)
      if (norm < epsilon(0.D0)) return
      vA = vX / norm
      ! Call objective function again to get corresponding yield stress and other quantities.
      objFunc%full_model = config%evaluate_full_model
      call objFunc%objectiveEval(vA,info)
      
      if (log_info) write(ounit,'(A,1X,5(E15.8,1X))') 'Final residual vector: ',objFunc%state%vF
      
      vSonA = objFunc%vSml  
      !info = 0
      
      end subroutine

end module
