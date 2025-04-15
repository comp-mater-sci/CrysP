include 'mkl_rci.f90'

!>    \file alamYLP.f90 The file contains modules that calculate
!>          yield locus position directly from the ALAMEL model.
!>


!> Implementation of YLP function that can directly use the ALAMEL multilevel model instead of a plastic potential function.
module alamYLP
    use iso_c_binding
    use mkl_rci
    use utils
    use dmcresulttable
    use logging

    implicit none

    private

    public:: OBJECTIVE_THRESHOLD, &
             ObjectiveFunction, &
             multilevelylp

    character(*), parameter:: MOD_NAME = 'alamYLP'
    real(DP), parameter:: OBJECTIVE_THRESHOLD = 1.E-2_DP

    !>Data type for objective functions.
    type:: objectiveFunction
        real(DP), dimension(5):: strain_mode
        real(DP), dimension(5):: residual
        real(DP), dimension(5, 5):: jacobi = 0._DP
        real(DP), dimension(5):: vSn = 0._DP
        real(DP), dimension(5):: vSml = 0._DP
        type(ResultTable), pointer:: ptr_db => null()
    end type

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
    end interface

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
        integer:: ounit
        class(ObjectiveFunction), pointer:: objFunc
        type(ObjectiveFunction), allocatable, target:: objective_function_local
        logical                 :: use_vmGuess
        integer, parameter       :: stdout = 6
        logical                 :: log_info, log_debug
        real(DP)        :: norm


        if (present(useVMGuess)) then
            use_vmGuess = useVMGuess
        else
            use_vmGuess = .true.
        endif

        log_info = .false.
        log_debug = .false.
        if (present(verbose)) then
            if (verbose > 2) then
                log_info = .true.
            endif
            if (verbose > 3) then
                log_debug = .true.
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

        if(trust_region_solve(objfunc%vsn, objfunc%vsml, vx, objfunc%jacobi, objfunc%residual) /= TR_SUCCESS) &
            call log_error(MOD_NAME, 'multilevelYlp', ERR, 'Error in MKL')

        if (associated(objfunc%ptr_db)) &
            call objfunc%ptr_db%put(vX/norm2(vx), objfunc%vSml)  ! Normalize because the magnitude has no impact on the response.

        R = norm2(objfunc%residual)

        ! Set output strain rate
        info  = VEF_ERROR
        norm = norm2(vX)
        if (norm < epsilon(0.D0)) return
        vA = vX/norm

        if (log_info) write(ounit, '(A, 1X, 5(E15.8, 1X))') 'Final residual vector: ',objFunc%residual

        vSonA = objFunc%vSml
        info = merge(VEF_FAIL, VEF_OK, R > OBJECTIVE_THRESHOLD)
    end subroutine

end module
