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
!  
!
!> Objective function for minimization of difference between requested stress tensor 
!> and stresses obtained from the ALAMEL
module alamEval
use nllsTR
use AlamelSub
use alamelConfig

      !> Dimensionality of th search space for vector representation (Stress/Strain rate)
      integer,parameter :: alamEval_vSD_dim = 5
      !> Dimensions of second-rank tensor representation of Stress and Strain rate
      integer,parameter :: alamEval_tSD_dim = 3 

      type,extends(MKLFDJacobiObjFunction) :: NormalizedV5DComp
      
            !NOTE: [n_X_dim] must be 5
            !      [m_F_dim] must be 5 
      
            !> Normalized stress vector
            double precision,dimension(alamEval_vSD_dim)        :: vSn = 0.D0 
            
            !> Multilevel prediction of stress from the previous call
            double precision,dimension(alamEval_vSD_dim)        :: vSml = 0.D0 
      contains
            !> Implementation of virtual method defined in ObjectiveFunction
            procedure,pass(this)           :: objectiveEval => objectiveEval_NV5DComp
      end type

      !> Performance counter: number of evaluations of the objective function
      integer                              :: alamEval_objFx_call_count = 0

contains

      subroutine objectiveEval_NV5DComp(this,vX,info)
      use Kutils
      implicit none
            class(NormalizedV5DComp),intent(inout)      :: this
            double precision,dimension(:),intent(in)    :: vX       !< Dimension must be: 5
            integer,intent(out)                         :: info
            !
            double precision,dimension(alamEval_tSD_dim,alamEval_tSD_dim)     :: Atens
            double precision,dimension(alamEval_vSD_dim)       :: vS, vXn
            double precision                    :: norm
            integer                             :: i
            !
            i = 0
            alamEval_objFx_call_count = alamEval_objFx_call_count + 1
            !
            ! Transfer normalized vX into second rank tensor.
            vXn = vX/sqrt(dot_product(vX,vX))  ! Avoid creation of on-call temporary
            call KVEC5D2MAT(vXn,Atens) 
            ! Set Atens as current value for processing 
#ifdef DIAGNOSTIC_OUTPUT                
            write(*,'(A,1X,5(F12.8))') 'eval for ', vXn
#endif        
            !!! TESTING !!!
            ! WARNING!! Taylor is requested below !!!
            !acnf%simulCalls(1)%rlx1 = 0
            !acnf%simulCalls(1)%rlx2 = 0
            !!! TESTING !!!
          
            acnf%simulCalls(1)%dgf = Atens
            acnf%simulCalls(1)%keep_texture = .true.
            acnf%simulCalls(1)%do_output = .false.
            acnf%nSimulCalls = 1
            ! Fill output data
            ares%stress_tensors(:,:,1) = 0.D0
            ! Call alamel
            call ALAMEL(3)
            ! Retrieve output stress into 5D vector
            call KMAT2VEC5D(ares%stress_tensors(:,:,1),vS)
            ! Transfer vS to vSml
            this%vSml = vS  
#ifdef DIAGNOSTIC_OUTPUT            
            write(*,'(A,1X,5(F12.8))') 'stress is ', vS
#endif            
            ! Normalize vS
            norm = sqrt(dot_product(vS,vS))
            if (norm > 0.D0) then
                  vS = vS / norm
                  this%state%vF = this%vSn - vS
#ifdef DIAGNOSTIC_OUTPUT            
                  write(*,'(2(F12.8,1X))') (vSn(i), vS(i),i=1,alamEval_vSD_dim)
#endif            
            else
                 ! norm is zero, so vS=0
                 this%state%vF = this%vSn
            endif
            info = 0
      end subroutine



end module


!> Implementation of YLP function that can directly use the ALAMEL multilevel model instead of a plastic potential function.
module alamYLP
use nllsTR
use AlamelSub
use alamelConfig
      
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
      end type
      
contains

      subroutine InitializeAlamel(alamelcnf)
      implicit none
      type(alamelConfigData),intent(in)	:: alamelcnf
      
      
      
      end subroutine

      !> Calculates plastic strain rate corresponding to given deviatoric stress
      !>
      !> The subroutine assumes that multilevel model is already configured and initialized.
      subroutine multilevelYLP(vS,vA,vSonA,R,info,useVMGuess,YLPconfig,outunit)
      use alamEval
      implicit none
      double precision,intent(in)   :: vS(alamEval_vSD_dim)      !< Stress vector
      double precision,intent(inout):: vA(alamEval_vSD_dim)      !< Strain rate mode on yield locus
      double precision,intent(out)  :: vSonA(alamEval_vSD_dim)   !< Stress vector corresponding to A
      double precision,intent(out)  :: R          !< Square norm of residual error
      integer                       :: info       !< Exit code
      logical,optional,intent(in)   :: useVMGuess !< use von Mises initial guess, otherwise assume vA as an initial strain rate
      type(multilevelYLPConfig),optional,intent(in) :: YLPconfig !< Configuration parameters to be imposed to the search method
      integer,intent(in),optional   :: outunit    !< Unit number for messages
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
      !
      if (present(useVMGuess)) then
            use_vmGuess = useVMGuess
      else
            use_vmGuess = .true.
      endif
      ! Override the defaults by the user's settings:
      if (present(YLPconfig)) config = YLPconfig
      !
      ! Configure objective function      
      call objFunc%initFx(alamEval_vSD_dim,alamEval_vSD_dim,info)
      if (info /= 0) return 
      !
      ! Normalized stress vector 
      objFunc%vSn = vS / sqrt(dot_product(vS,vS)) 
      !
      ounit = stdout
      if (present(outunit))  ounit = outunit
      ! Initialize TR solver
      call nlls_TR_init(verbose=1,ounit=6)
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
            ! start TR solver
            if (attempt_linearized) then
                  ! Profit from the initial point stored by the solver for the linearized problem
                  info = objFunc%state%copy(initState)
                  tr_config%use_init_state = .true.
            endif
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
      vA = vX/sqrt(dot_product(vX,vX))
      ! Call objective function again to get corresponding yield stress
      call objFunc%objectiveEval(vA,info)
      
      write(*,'(A,1X,5(E15.8,1X))') 'Final residual vector: ',objFunc%state%vF
      
      vSonA = objFunc%vSml  
      !info = 0
      
      end subroutine

end module
