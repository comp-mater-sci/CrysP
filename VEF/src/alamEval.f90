!> Objective function for minimization of difference between requested stress tensor
!> and stresses obtained from the ALAMEL
module alamEval
use nllsTR

implicit none



      !> Objective function: difference between the searched-for normalized stress and the normalized
      !> stress given by the multilevel model.
      type, extends(ObjectiveFunction):: NormalizedV5DComp

            !> Multilevel prediction of stress from the previous call
            real(DP), dimension(5)        :: vSml = 0.D0
      contains
            !> Implementation of virtual method defined in ObjectiveFunction
            procedure, pass(this)           :: objectiveEval => objectiveEval_NV5DComp

      end type

      !> Performance counter: number of evaluations of the objective function
      integer                              :: alamEval_objFx_call_count = 0

contains

      subroutine objectiveEval_NV5DComp(this, vX, info)
      use altay
      use altayConfig
      implicit none
            class(NormalizedV5DComp), intent(inout)      :: this
            real(DP), dimension(:), intent(in)    :: vX       !< Dimension must be: 5
            integer, intent(out)                         :: info
            !
            real(DP), dimension(3, 3)     :: Atens
            real(DP), dimension(5)       :: vS, vXn
            real(DP)                    :: norm
            integer                             :: i
            !
            integer, parameter:: istp = 1
            !
            info = -1
            i = 0
            alamEval_objFx_call_count = alamEval_objFx_call_count+1
            !
            ! Transfer normalized vX into second rank tensor.
            norm = norm2(vX)
            if (norm < epsilon(0.D0)) return
            vXn = vX/norm
            Atens = convert_stress_strain_space(vXn)
            ! Set Atens as current value for processing
#ifdef DIAGNOSTIC_OUTPUT
            write(*,'(A, 1X, 5(F12.8))') 'eval for ', vXn
#endif
            ! Re-initialize with a request for just one single step
            call initStepData(istp, astate, info)
            if (info /= 0) return
            !
            associate (input => astate%simulCalls(istp)%input)
                  input%dgf = Atens
                  input%keep_texture = .true.
                  input%keep_state = .true.
                  input%full_model = .false.
                  input%do_output_init = .false.
                  input%do_output_final = .false.
            end associate
            ! Call the simulation
            !Round to TOLERANCE to get rid of numerical instability due to scheduling. The underlying model is much less accurate
            !anyway.
            vs = anint(convert_stress_strain_space(altay_get_stress_state(atens))/TOLERANCE) * TOLERANCE


            if (info /= 0) return
            !
            ! Retrieve output stress into 5D vector
            !vS = convert_stress_strain_space(astate%simulCalls(istp)%output%stress_tensor)
            ! Transfer vS to vSml
            this%vSml = vS
#ifdef DIAGNOSTIC_OUTPUT
            write(*,'(A, 1X, 5(F12.8))') 'stress is ', vS
#endif
            ! Normalize vS
            norm = norm2(vS)
            if (norm > 0.D0) then
                  vS = vS/norm
                  this%state%vF = this%vSn-vS
#ifdef DIAGNOSTIC_OUTPUT
                  write(*,'(F12.8, 1X)') (vS(i), i = 1, 5)
#endif
            else
                 ! norm is zero, so vS = 0
                 this%state%vF = this%vSn
            endif
            info = 0
      end subroutine

end module
