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
    use iso_c_binding

    implicit none

    character(*), parameter:: MOD_NAME = 'nllstr'


    interface
        integer(C_INT) function trust_region_solve(stress_target, stress_mode, strain_mode, jacobi, residual) bind(C) result(mkl_result_code)
            import C_INT, &
                   C_DOUBLE

            real(C_DOUBLE), dimension(5), intent(in):: stress_target
            real(C_DOUBLE), dimension(5), intent(out):: stress_mode
            real(C_DOUBLE), dimension(5), intent(out):: strain_mode
            real(C_DOUBLE), dimension(5, 5), intent(out):: jacobi
            real(C_DOUBLE), dimension(5), intent(out):: residual
        end function
        integer(C_INT) function trust_region_solve_from_guess(stress_target, stress_mode, strain_mode, jacobi, residual) bind(C) result(mkl_result_code)
            import C_INT, &
                   C_DOUBLE

            real(C_DOUBLE), dimension(5), intent(in):: stress_target
            real(C_DOUBLE), dimension(5), intent(out):: stress_mode
            real(C_DOUBLE), dimension(5), intent(inout):: strain_mode
            real(C_DOUBLE), dimension(5, 5), intent(inout):: jacobi
            real(C_DOUBLE), dimension(5), intent(out):: residual
        end function
    end interface





    !Tolerance on the residual (i.e. the 'success threshold'). This is set to 0.01 because we are using normalized stresses and
    !strains and 1% is about as accurate as you can hope the underlying model to be.
    real(DP), parameter:: OBJECTIVE_THRESHOLD = 1.E-2_DP

    !>Data type for objective functions.
    type:: objectiveFunction
        real(DP), dimension(5):: strain_mode
        real(DP), dimension(5):: residual
        real(DP), dimension(5, 5):: jacobi
        real(DP), dimension(5):: vSn = 0.D0
        real(DP), dimension(5):: vSml = 0.D0
        type(ResultTable), pointer:: ptr_db => null()
    contains
          procedure, pass(this):: objectiveEval => objectiveEval_NV5DComp
    end type

contains

    subroutine objectiveEval_NV5DComp(this, vX)
        class(ObjectiveFunction), intent(inout):: this
        real(DP), dimension(5), intent(in):: vX

        real(DP):: vS(5)

        !Round to TOLERANCE to get rid of numerical instability due to scheduling. The underlying model is much less accurate
        !anyway.
        vs = anint(convert_stress_strain_space(altay_get_stress_state(convert_stress_strain_space(vX)))/TOLERANCE) * TOLERANCE

        ! Retrieve output stress into 5D vector
        !vS = convert_stress_strain_space(astate%simulCalls(istp)%output%stress_tensor)
        ! Transfer vS to vSml
        this%vSml = vS
        vS = vS/norm2(vs)
        this%residual = this%vSn-vS

    end subroutine

    !> This subroutine solves the mnimization problem. Trust region algorithm from MKL library is used.
    !> \param objFx Objective function to minimize
    !> \param jacobiFx Function that calculates Jacobi matrix of objective function
    subroutine nlls_TR_solve(objFx, vX)
        type(objectiveFunction), target, intent(inout):: objFx    !< objective function
        real(DP), dimension(5), target, intent(inout)::   vX

        character(*), parameter::           PROC_NAME   = 'nlls_tr_solve'
        real(DP), dimension(5), parameter:: LOWER_BOUND = -1._DP
        real(DP), dimension(5), parameter:: UPPER_BOUND = 1._DP

        type(HANDLE_TR):: handle
        integer::         rci_req
        real(DP), target:: eps(6)  ! Target because handle points tot his
        integer:: err

        err = trust_region_solve_from_guess(objfx%vsn, objfx%vsml, vx, objfx%jacobi, objfx%residual)

        if (associated(objfx%ptr_db)) &
            call objfx%ptr_db%put(vX/norm2(vx), objfx%vSml)  ! Normalize because the magnitude has no impact on the response.

        !Tolerance on all stop criteria except for the norm of the residual is set to 1% of the objective threshold. This means we
        !quit if the progress we are making is much smaller than the accuracy we are looking for and therefore negligible.
        !Experimentally determined to be optimal. See dtrnlspbc_init documentation for more details.
        !eps = OBJECTIVE_THRESHOLD*1.E-2_DP
        !eps(2) = OBJECTIVE_THRESHOLD
        !if (dtrnlspbc_init(handle, 5, 5, vX, LOWER_BOUND, UPPER_BOUND, eps, 350, 50, 0.1_DP) /= TR_SUCCESS) &
        !    call log_error(MOD_NAME, PROC_NAME, ERR, 'Could not initialize TR solver.')

        !RCI_Req = 0
        !do while (rci_req >= 0)
        !    if (dtrnlspbc_solve(handle, objfx%residual, objfx%jacobi, RCI_Req) /= TR_SUCCESS) &
        !        call log_error(MOD_NAME, PROC_NAME, ERR, 'Error in trust region solver.')

        !    !RCI status. See dtrnlspbc_solve documentation for details.
        !    select case (RCI_Req)
        !        case(1)  !Evaluate objective function at current strain mode
        !            call objFx%objectiveEval(vX)
        !        case(2)  !Recalculate Jacobian
        !            call calc_jacobi(vx, objfx%jacobi)
        !            !objfx%jacobi = calc_jacobi(vx)
        !    end select
        !end do

        !! Release MKL resources
        !if (dtrnlspbc_delete(handle) /= TR_SUCCESS) &
        !    call log_error(MOD_NAME, PROC_NAME, ERR, 'dtrnlspbc_delete failed')

        !call mkl_free_buffers()
    end subroutine

!    !>@Brief Calculate the local change in the stress response at a given strain mode (== Jacobi matrix of AlTay)
!    !>@Details Internally calls MKL, which uses a finite differences method.
!    recursive function calc_jacobi(strain_mode, interval) result(jacobi)
!        external altay_wrapper
!
!        real(DP), dimension(5), intent(in):: strain_mode    !> Strain mode at which to calculate the Jacobi
!        real(DP), intent(in), optional:: interval           !> Optional initial interval for the finite difference algorithm.
!        real(DP), dimension(5, 5):: jacobi                  !> The Jacobi
!
!        character(*), parameter:: PROC_NAME = 'calc_jacobi'
!
!        integer:: i
!        real(DP):: eps
!
!        !Gfortran can not handle shorter notation with merge()
!        if (present(interval)) then
!            eps = interval
!        else
!            !Experimentally determined to be optimal
!            eps = 2.E-2_DP
!        end if
!
!        if (djacobi(altay_wrapper, 5, 5, jacobi, strain_mode, eps) /= TR_SUCCESS) &
!            call log_error(MOD_NAME, PROC_NAME, ERR, 'Internal MKL error')
!
!        !The MKL trust region algorithm requires the Jacobi to not have 0 columns. Check for this and if it occurs, increase the
!        !finite differences interval and recalculate the Jacobi. This works due to the step-wise nature of the stress response,
!        !which is in turn caused by the finite number of slip systems determining it.
!        do i = 1, 5
!            if (norm2(jacobi(:,i)) < TOLERANCE) then
!                if (eps < 1._DP) then
!                    !Increase in eps experimentally determined to be optimal
!                    jacobi = calc_jacobi(strain_mode, 2*eps)
!                    return
!                else
!                    call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Interval too large')
!                end if
!            end if
!        end do
!    end function
end module
