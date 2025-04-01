!> nllsTR  -- wrapper module for MKL Non-Linear Least Squares Trust Region algorithm
! Provide modules: MKL_RCI_TYPE and MKL_RCI
include 'mkl_rci.f90'

!>@Brief Wrapper for calling AlTay from within MKL. See djacobi documentation.
!>@Details Has to be declared external for reasons because MKL documentation says so and gfortran will not compile otherwise.
subroutine altay_wrapper(m, n, strain_mode, stress_mode)
    use utils
    use altay

    integer, intent(in):: m !> Needed by MKL
    integer, intent(in):: n !> Needed by MKL
    real(DP), dimension(n), intent(in):: strain_mode    !> Strain mode to calculate the stress response for
    real(DP), dimension(m), intent(out):: stress_mode   !> Stress response of the material

    integer:: info

    stress_mode = convert_stress_strain_space(altay_get_stress_state(convert_stress_strain_space(strain_mode)))

    !Normalize for good measure and multiply by-1 because the least squares problem for which we are calculating the jacobi is
    !(target_stress_mode-stress_mode(strain_mode)) and thus its jacobi is (0-(jacobi(stress_mode(strain_mode))))
    !Round to TOLERANCE to compensate for variations in the results due to scheduling. The underlying model can never nearly as accurate anyway.
    stress_mode = anint(-stress_mode/norm2(stress_mode)/TOLERANCE) * TOLERANCE
end subroutine

module nllsTR
    use utils
    use mkl_rci
    use altay
    use logging
    use dmcResultTable

    implicit none

    character(*), parameter:: MOD_NAME = 'nllstr'
    real(DP), parameter:: OBJECTIVE_THRESHOLD = 1.E-2_DP

      !> Solution at given point. It consists of: 1) the point, 2) function value, and 3) Jacobi matrix.
      type:: SolutionPoint
          real(DP), dimension(5):: vX       !< The point
          real(DP), dimension(5):: vF       !< Function at vX
          real(DP), dimension(5, 5):: mJ       !< Jacobi matrix at vX, [m_F_dim x n_X_dim]
      end type

      !> Abstract data type for objective functions.
      !>
      !> It is assumed that every objective function contains a state variable that
      !> represents the value of the function and its Jacobian.
      type:: objectiveFunction
            !> Normalized stress vector
            real(DP), dimension(5)        :: vSn = 0.D0
            !> State variable
            type(SolutionPoint)           :: state

            real(DP), dimension(5)        :: vSml = 0.D0
            type(ResultTable), pointer:: ptr_db => null()
      contains
            procedure, pass(this):: objectiveEval => objectiveEval_NV5DComp
      end type

      type nllsTRRes
            integer                             :: iteration = 0        !< Interation number
            integer                             :: stop_criterion = 0   !< Identifier of stop criterion, see MKL documentation
            real(DP)                    :: r1 = 0.D0            !< Initial norm of residual
            real(DP)                    :: r2 = 0.D0            !< Final norm of residual
      end type

contains

    subroutine objectiveEval_NV5DComp(this, vX, info)
        class(ObjectiveFunction), intent(inout):: this
        real(DP), dimension(5), intent(in):: vX
        integer, intent(out):: info

        real(DP):: vS(5)

        !Round to TOLERANCE to get rid of numerical instability due to scheduling. The underlying model is much less accurate
        !anyway.
        vs = anint(convert_stress_strain_space(altay_get_stress_state(convert_stress_strain_space(vX)))/TOLERANCE) * TOLERANCE

        ! Retrieve output stress into 5D vector
        !vS = convert_stress_strain_space(astate%simulCalls(istp)%output%stress_tensor)
        ! Transfer vS to vSml
        this%vSml = vS
        vS = vS/norm2(vs)
        this%state%vF = this%vSn-vS

        if (info == 0 .and. associated(this%ptr_db)) &
            call this%ptr_db%put(vX/norm2(vx), this%vSml)  ! Normalize because the magnitude has no impact on the response.

        info = VEF_OK
    end subroutine

      !> This subroutine solves the mnimization problem. Trust region algorithm from MKL library is used.
      !> \param objFx Objective function to minimize
      !> \param jacobiFx Function that calculates Jacobi matrix of objective function
      subroutine nlls_TR_solve(objFx, vX, r1, r2, info, resInfo)
      use, intrinsic:: IEEE_EXCEPTIONS
      use, intrinsic:: IEEE_ARITHMETIC

      ! Formal parameters
      class(objectiveFunction), target, intent(inout)          :: objFx    !< objective function
      !> Design vector, dimension of vX must correspond to those in objFX
      real(DP), dimension(:), target, intent(inout)     :: vX
      real(DP), intent(out)                    :: r1       !< Initial residual of the solution
      real(DP), intent(out)                    :: r2       !< Final residual of the solution
      !> Exit code: 0 on success, < 0 on error, > 0 on failure/warning
      integer, intent(out)                             :: info
      !> Full termination status of the TR solver
      type(nllsTRRes), intent(inout), optional          :: resInfo

      character(*), parameter:: PROC_NAME = 'nlls_tr_solve'
      real(DP), dimension(5), parameter:: LOWER_BOUND = -1._DP
      real(DP), dimension(5), parameter:: UPPER_BOUND = 1._DP

      type(HANDLE_TR)   :: handle
      integer           :: res, &
                           linfo, &
                           rci_req, &
                           rci_count, &
                           ierr, &
                           i
      type(nllsTRRes)                :: resultInfo
      logical                        :: next_solve
      character(len = 512)             :: message
      real(DP):: jacobi_interval, &
                 eps(6)

      !All tolerances can be set to TOLERANCE except for the tolerance on the residual (i.e. the 'success threshold'). This is set
      !to 0.01 because we are using normalized stresses and strains and 1% is about as accurate as you can hope the underlying model
      !to be. See dtrnlspbc_init documentation for more details.
      eps = TOLERANCE
      eps(2) = OBJECTIVE_THRESHOLD

            RCI_Req = 0; next_solve = .true.
            info = -1

            !! Initialize MKL solver
            if (dtrnlspbc_init(handle, 5, 5, vX, LOWER_BOUND, UPPER_BOUND, eps, 350, 50, 0.1_DP) /= TR_SUCCESS) &
                call log_error(MOD_NAME, PROC_NAME, ERR, 'Could not initialize TR solver.')

            call IEEE_SET_FLAG (IEEE_ALL, .FALSE.)  ! Hush up all the FP exceptions.

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
                        if (dtrnlspbc_solve(handle, vFval, mJacobi, RCI_Req) /= TR_SUCCESS) &
                            call log_error(MOD_NAME, PROC_NAME, ERR, 'Error in trust region solver.')
                        if (trapFPErrors()) &
                            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Floating point problem in solver.')
                        call IEEE_SET_FLAG (IEEE_ALL, .FALSE.)  ! Hush up all the FP exceptions.

                        ! RCI status
                        select case (RCI_Req)
                            case (-6:-1)  ! One of the stop criteria has been reached
                                print *, rci_req
                                  next_solve = .false.
                            case(0)  ! Need another iteration.
                                  next_solve = .true.
                            case(1)  !Evaluate objective function at current strain mode
                                call objFx%objectiveEval(vX, linfo)
                            case(2)           ! Recalculate Jacobian
                                objfx%state%mj = calc_jacobi(vx)
                            case default      ! Unknown RCI, it should never happen!!
                                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Error: unknown RCI control code!!!')
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

            ! Release MKL resources
            res = dtrnlspbc_delete(handle)
            if (res /= TR_SUCCESS) &
                call log_error(MOD_NAME, PROC_NAME, ERR, 'dtrnlspbc_delete failed')

            call mkl_free_buffers()
            !
            ! Formats
            100 format(A)           ! fmt = 100  ! just a string
            200 format(A, 1X, I0)     ! fmt = 400  ! a string followed by an integer
            500 format(F15.8, 1X)    ! fmt = 500  ! long float, followed by one space
            !
      end subroutine

      subroutine nlls_TR_exit_message(str, resInfo, info)
      implicit none
      character(len=*), intent(out)                    :: str
      type(nllsTRRes), intent(in)                      :: resInfo
      integer, intent(out)                             :: info
      !
            ! See documentation of ?trnlspbc_get in MKL manual for
            ! meaning of the stop criterion codes.
            info = 0
            select case (resInfo%stop_criterion)
            case(1)
                  write(str, fmt = 200) 'The TR solver exceeded the maximal number of iterations:',resInfo%iteration
            case(2)
                  write(str, fmt = 201) 'Area of the trust region is smaller than',TOLERANCE
            case(3)
                  write(str, fmt = 201) 'Requested quality of the solution is reached. ||F(x)|| is smaller than',OBJECTIVE_THRESHOLD
            case(4)
                  write(str, fmt = 201) 'The Jacobian matrix is singular. ||J(x)[:,i]|| is smaller than',TOLERANCE
            case(5)
                  write(str, fmt = 201) 'Size of the trial step is smaller than',TOLERANCE
            case(6)
                  write(str, fmt = 201) 'Achievable improvement to the solution is smaller than',TOLERANCE
            case default
                  str = 'TR solver has prematurely stopped for unknown reason.'
                  info = -1
            end select
            200   format(A, 1X, I0)
            201   format(A, 1X, E15.7)
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
        external altay_wrapper

        real(DP), dimension(5), intent(in):: strain_mode    !> Strain mode at which to calculate the Jacobi
        real(DP), intent(in), optional:: interval           !> Optional initial interval for the finite difference algorithm.
        real(DP), dimension(5, 5):: jacobi                  !> The Jacobi

        character(*), parameter:: PROC_NAME = 'calc_jacobi'

        integer:: i
        real(DP):: eps

        !Gfortran can not handle shorter notation with merge()
        if (present(interval)) then
            eps = interval
        else
            !Experimentally determined to be optimal
            eps = 2.E-2_DP
        end if

        if (djacobi(altay_wrapper, 5, 5, jacobi, strain_mode, eps) /= TR_SUCCESS) &
            call log_error(MOD_NAME, PROC_NAME, ERR, 'Internal MKL error')

        !The MKL trust region algorithm requires the Jacobi to not have 0 columns. Check for this and if it occurs, increase the
        !finite differences interval and recalculate the Jacobi. This works due to the step-wise nature of the stress response,
        !which is in turn caused by the finite number of slip systems determining it.
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
    end function
end module
