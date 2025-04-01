!>    \file alamYLP.f90 The file contains modules that calculate
!>          yield locus position directly from the ALAMEL model.
!>


!> Implementation of YLP function that can directly use the ALAMEL multilevel model instead of a plastic potential function.
module alamYLP
    use utils
    use nllsTR

    implicit none

contains

    !> Calculates plastic strain rate corresponding to given deviatoric stress
    !>
    !> The subroutine assumes that multilevel model is already configured and initialized.
    !> Exit code is retured in info: VEF_OK on success; VEF_FAIL if no converged solution can
    !> be found; VEF_ERROR if error conditions have been detected.
    subroutine multilevelYLP(vS, vA, vSonA, R, info, useVMGuess, outunit, verbose, objective_function)
        real(DP), intent(in)   :: vS(5)      !< Imposed stress vector
        real(DP), intent(inout):: vA(5)      !< Strain rate mode on yield locus
        real(DP), intent(out)  :: vSonA(5)   !< Stress vector corresponding to A
        real(DP), intent(out)  :: R          !< Square norm of residual error
        integer                       :: info       !< Exit code
        !> Flag: use von Mises initial guess, otherwise assume vA as an initial strain rate (default: .true.)
        logical, optional, intent(in)   :: useVMGuess
        integer, intent(in), optional   :: outunit    !< Unit number for messages
        integer, intent(in), optional   :: verbose
        class(ObjectiveFunction), target, optional, intent(inout):: objective_function

        real(DP), dimension(5):: vX, vX_lin
        class(ObjectiveFunction), pointer:: objFunc
        type(ObjectiveFunction), allocatable, target:: objective_function_local
        real(DP)        :: r1, r2
        logical                 :: use_vmGuess
        real(DP)        :: r1_lin, r2_lin
        type(nllsTRRes)         :: TR_res
        integer                 :: ounit, tr_verbose
        integer, parameter       :: stdout = 6
        logical                 :: log_info, log_debug
        real(DP)        :: norm

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
        ! Set the objective function
        if (present(objective_function)) then
            objFunc => objective_function
        else
            allocate(objective_function_local)
            objFunc => objective_function_local
        endif
        info  = VEF_ERROR
        !
        !Get normalized stress vector
        norm = norm2(vS)
        if (norm < epsilon(0.D0)) return
        objFunc%vSn = vS/norm
        !
        ounit = stdout
        if (present(outunit))  ounit = outunit
        ! Use von Mises guess
        vX = merge(vS, vA, use_vmGuess)
        !
        r1 = 0.0_DP; r2 = 0.0_DP
        ! Run linearized problem if requested
        ! The linearized analysis is either not done or failed.
        ! Set non-linear analysis
        call nlls_TR_solve(objFunc, vX, r1, r2)
        R = r2

        ! Set output strain rate
        info  = VEF_ERROR
        norm = norm2(vX)
        if (norm < epsilon(0.D0)) return
        vA = vX/norm

        if (log_info) write(ounit, '(A, 1X, 5(E15.8, 1X))') 'Final residual vector: ',objFunc%stress_mode

        vSonA = objFunc%vSml
        info = merge(VEF_FAIL, VEF_OK, R > OBJECTIVE_THRESHOLD)
    end subroutine
end module
