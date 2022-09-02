!> Objective function for minimization of difference between requested stress tensor
!> and stresses obtained from the ALAMEL
module alamEval
use alamYLPConstants
use nllsTR
implicit none



      !> Objective function: difference between the searched-for normalized stress and the normalized
      !> stress given by the multilevel model.
      type,extends(MKLFDJacobiObjFunction) :: NormalizedV5DComp

            !NOTE: [n_X_dim] must be 5
            !      [m_F_dim] must be 5

            !> Normalized stress vector
            double precision,dimension(alamEval_vSD_dim)        :: vSn = 0.D0

            !> Multilevel prediction of stress from the previous call
            double precision,dimension(alamEval_vSD_dim)        :: vSml = 0.D0

            !> Flag: request for simulation outputs other than just deviatoric stress.
            !>
            !> The full model is not needed for calculation of stresses.
            logical                                             :: full_model = .false.

      contains
            !> Implementation of virtual method defined in ObjectiveFunction
            procedure,pass(this)           :: objectiveEval => objectiveEval_NV5DComp

      end type

      !> Performance counter: number of evaluations of the objective function
      integer                              :: alamEval_objFx_call_count = 0

contains

      subroutine objectiveEval_NV5DComp(this,vX,info)
      use altaySub
      use altayConfig
	  use criMathUtils, only: vec5D2tens,tens2vec5D
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
            integer,parameter :: istp = 1
            !
            info = -1
            i = 0
            alamEval_objFx_call_count = alamEval_objFx_call_count + 1
            !
            ! Transfer normalized vX into second rank tensor.
            norm = norm2(vX)
            if (norm < epsilon(0.D0)) return
            vXn = vX/norm
            Atens = vec5D2tens(vXn)
            ! Set Atens as current value for processing
#ifdef DIAGNOSTIC_OUTPUT
            write(*,'(A,1X,5(F12.8))') 'eval for ', vXn
#endif
            ! Re-initialize with a request for just one single step
            call initStepData(istp,astate,info)
            if (info /= 0) return
            !
            associate (input => astate%simulCalls(istp)%input)
                  input%dgf = Atens
                  input%keep_texture = .true.
                  input%keep_state = .true.
                  input%full_model = this%full_model
                  input%do_output_init = .false.
                  input%do_output_final = .false.
                  call setStepType(input,acnf%model_id,info)
            end associate
            ! Call the simulation
            call runSteps(astate,info)
            if (info /= 0) return
            !
            ! Retrieve output stress into 5D vector
            vS = tens2vec5D(astate%simulCalls(istp)%output%stress_tensor)
            ! Transfer vS to vSml
            this%vSml = vS
#ifdef DIAGNOSTIC_OUTPUT
            write(*,'(A,1X,5(F12.8))') 'stress is ', vS
#endif
            ! Normalize vS
            norm = norm2(vS)
            if (norm > 0.D0) then
                  vS = vS / norm
                  this%state%vF = this%vSn - vS
#ifdef DIAGNOSTIC_OUTPUT
                  write(*,'(F12.8,1X)') (vS(i),i=1,alamEval_vSD_dim)
#endif
            else
                 ! norm is zero, so vS=0
                 this%state%vF = this%vSn
            endif
            info = 0
      end subroutine

end module
