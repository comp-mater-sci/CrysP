module alamEval
use nllsTR
use AlamelSub
use alamelConfig

      type,extends(FDJacobiObjFunction) :: NormalizedV5DComp
      
            !NOTE: [n_X_dim] must be 5
            !      [m_F_dim] must be 5 
      

            !double precision,dimension(5)        :: vSn = 0.D0 !< Normalized stress vector
            !
            !double precision,dimension(5)        :: vSml = 0.D0 !< Multilevel prediction of stress from previous call
      contains
            procedure :: objectiveFx => objectiveFx_NV5DComp
      end type

      !NASTY TRICK
      double precision,dimension(5)        :: vSn = 0.D0 !< Normalized stress vector
            !
      double precision,dimension(5)        :: vSml = 0.D0 !< Multilevel prediction of stress from previous call

      integer                              :: alamEval_objFx_call_count = 0

contains

      subroutine objectiveFx_NV5DComp(this,vX,vFval,info)
      use Kutils
      implicit none
            class(NormalizedV5DComp)                    :: this
            double precision,dimension(:),intent(inout) :: vX       !< Dimension must be: 5
            double precision,dimension(:),intent(inout) :: vFval    !< Dimension must be: [m_F_dim]
            integer,intent(out)                         :: info
            !
            double precision,dimension(3,3)     :: Atens
            double precision,dimension(5)       :: vS
            double precision                    :: norm
            integer                             :: i
            !
            alamEval_objFx_call_count = alamEval_objFx_call_count + 1
            !
            ! Transfer normalized vX into second rank tensor.
            
            call KVEC5D2MAT(vX/sqrt(dot_product(vX,vX)),Atens) 
            ! Set Atens as current value for processing 
#ifdef DIAGNOSTIC_OUTPUT                
            write(*,'(A,1X,5(F12.8))') 'eval for ', vX
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
            !this%vSml = vS  
            vSml = vS  !<--- FIXME !!!
#ifdef DIAGNOSTIC_OUTPUT            
            write(*,'(A,1X,5(F12.8))') 'stress is ', vS
#endif            
            ! Normalize vS
            norm = sqrt(dot_product(vS,vS))
            if (norm > 0.D0) then
                  vS = vS / norm
                  ! vFval = this%vSn - (vS/norm)
                  vFval = vSn - vS   !<--- FIXME !!!
#ifdef DIAGNOSTIC_OUTPUT            
                  write(*,'(2(F12.8,1X))') (vSn(i), vS(i),i=1,5)
#endif            
            else
                 ! norm is zero, so vS=0
                 ! vFval = this%vSn
                 vFval = vSn   !<--- FIXME !!!
            endif
            info = 0
      end subroutine



end module

module alamYLP
use nllsTR
use AlamelSub
use alamelConfig
      
      type multilevelYLPConfig
            double precision        :: jacobi_eps = 5.E-2
            logical                 :: linearize = .false.
      end type
      
contains

      subroutine InitializeAlamel(alamelcnf)
      implicit none
      type(alamelConfigData),intent(in)	:: alamelcnf
      
      
      
      end subroutine

      !> Calculates plastic strain rate corresponding to given deviatoric stress
      !>
      !> The subroutine assumes that multilevel model is already configured and initialized.
      subroutine multilevelYLP(vS,vA,vSonA,R,info,useVMGuess,config)
      use alamEval
      implicit none
      double precision,intent(in)   :: vS(5)      !< Stress vector
      double precision,intent(inout):: vA(5)      !< Strain rate mode on yield locus
      double precision,intent(out)  :: vSonA(5)   !< Stress vector corresponding to A
      double precision,intent(out)  :: R          !< Square norm of residual error
      integer                       :: info       !< Exit code
      logical,optional,intent(in)   :: useVMGuess !< use von Mises initial guess, otherwise assume vA as an initial strain rate
      type(multilevelYLPConfig),optional,intent(in) :: config !< Configuration parameters to be imposed to the search method
      !
      double precision, dimension(5) :: Snormal, Anormal, vX, vF, vX_lin
      ! 
      double precision, dimension(5,5) :: mJ
      !
      type(NormalizedV5DComp) :: objFunc
      type(nllsTRConf)        :: tr_config
      double precision        :: r1,r2
      logical                 :: use_vmGuess
      logical                 :: attempt_linearized,linearized_successful
      double precision        :: r1_lin,r2_lin
      type(nllsTRRes)         :: TR_res
      !
      if (present(useVMGuess)) then
            use_vmGuess = useVMGuess
      else
            use_vmGuess = .true.
      endif
      !
      ! Configure objective function      
      objFunc%n_X_dim = 5
      objFunc%m_F_dim = 5
      objFunc%jacobi_eps=5.e-2      !! Quite good value!! Lowering it leads to lack of convergence!
      !
      ! Normalized stress vector 
      !objFunc%vSn = vS / sqrt(dot_product(vS,vS)) 
      vSn = vS / sqrt(dot_product(vS,vS)) !<--- FIXME !!!
      !
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
      tr_config%eps = 1e-5    !<< beware!
      tr_config%eps(2) = 1e-3  ! Norm of F: ||F||_2
      !
      linearized_successful = .false.
      attempt_linearized = .false.
      ! Override the defaults by the user's settings:
      if (present(config)) then
            objFunc%jacobi_eps = config%jacobi_eps
            attempt_linearized = config%linearize
      endif
      !
      ! Run linearized problem if requested
      if (attempt_linearized) then
            tr_config%constJacobi = .true.
            vX_lin = vX    
            ! Start the TR solver for linearized problem
            ! More thorough exit status is necessary: TR_res
            call nlls_TR_solve(objFunc,vX_lin,tr_config,r1_lin,r2_lin,info,TR_res)
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
      !
      ! Set output strain rate
      vA = vX
      ! Call objective function again to get corresponding yield stress
      call objFunc%objectiveFx(vX,vF,info)
      
      write(*,'(A,1X,5(E15.8,1X))') 'Final residual vector: ',vF
      
      !vSonA = objFunc%vSml  
      vSonA = vSml  !<--- FIXME !!!
      !info = 0
      
      end subroutine

end module
