#include "criMacros.fpp"

!> Implementation of a DMC computiational module that allows stress-driven evolution of
!> material state.
module dmcStressDrivenEvolutionModule
use utils
use dmcStressDrivenModule
use dmcIncrementationControl
use dmcEvolutionOutputRecord
use commonUtils

implicit none


    public:: StressDrivenEvolutionModule
    private


    type, extends(StressDrivenModule):: StressDrivenEvolutionModule

        type(IncrementationControlSettings):: control

    contains

        !> Main loop of incremental stress driven state evolution
        !>
        !> Under normal circumstances the subclasses do not need to override this method.
        !> Event handlers should be used to get info and/or control how the main loop
        !> advances.
        procedure, pass(this)    :: calculateStressPath => StressDrivenEvolutionModule_calculateStressPath

        !> Event handler invoked on increment start.
        !>
        !> A subclass can overload this to get informed about the incrementation and
        !> influence the incrementation.
        procedure, pass(this)    :: onIncrementStart => StressDrivenEvolutionModule_onIncrementStart

        !> Event handler invoked on increment end
        !>
        !> A subclass can overload this to get informed about the incrementation and
        !> influence the incrementation.
        procedure, pass(this)    :: onIncrementEnd => StressDrivenEvolutionModule_onIncrementEnd

    end type

contains


    !> Main loop of incremental stress driven state evolution
    integer function StressDrivenEvolutionModule_calculateStressPath(this, sigma, control, output, rotmat, &
                                                                     incrementation_control, use_icv_as_is) result(info)
        class(StressDrivenEvolutionModule), intent(inout):: this
        real(DP), dimension(3, 3), intent(in)              :: sigma !< Imposed stress tensor
        !> Settings that control the incrementation process
        class(IncrementationControlSettings), intent(inout):: control
                !> Rotation matrix. Relevant only if scalingStrainTensorComponent is used
        real(DP), dimension(3, 3), intent(in), optional  :: rotmat
        !> Incrementation control variables to override the defaults.
        !>
        !> Typical use is to inherit some control variables (the totals) from a previous
        !> call to this function.
        !> On exit, the parameter will contain updated control variables.
        type(IncrementationControl), intent(inout), optional  :: incrementation_control
        !> Suppress re-initialization of step-wide and increment-wide incrementation control variables
        !> (default: false)
        logical, intent(in), optional                         :: use_icv_as_is
        type(IncrementOutputRecord), dimension(:), allocatable, intent(out):: output
        type(IncrementOutputRecord), dimension(:), allocatable  :: buffer
        !
        real(DP):: scaling_factor, control_variable, stop_control_variable, taylor_factor, stretch, &
                D(3, 3), X_tmp(3, 3), D_retry(3, 3)
        type(YLPResult):: ylp, ylp_retry
        real(DP), dimension(5):: vDe, vSe
        type(IncrementationControl):: icv
        real(DP), dimension(6):: X_tmp_voigt

        !
        type(IncrementOutputRecord)         :: tmp_record
        !> Results if the incrementation procedure.
        !>
        !> On successful exit it will include  n+1 entries, where n is the number of
        !> increments needed to reach the end of the step.
        !> The leading n contain complete results (search for strain rate
        !> AND strain incrementation), while the last one just the result of the search for
        !> the strain rate. Therefore, the last entry corresponds to the state of
        !> the material at the end of the step.
        integer:: i, n_roots, n_records
        real(DP), dimension(2):: xi
        logical:: stop_flag
        real(DP), parameter:: stretch_ratio = 1e-3_DP
        real(DP), dimension(3, 3):: zero = 0._DP

        n_records = 0

        if (present(incrementation_control)) then
            icv = incrementation_control
            if (.not. optionalDefault(use_icv_as_is, .false.)) call icv%initStep(info)
        endif

        !Trick: allow the increment to "stretch" a bit.
        !The trick is used in the stop condition of the loop to prevent starting
        !a new increment because stop_control_variable-control%step_size gives some
        !small positive value. The trick does not eliminate the main cause of that
        !drift, which is the accumulation of increment tensor components of different sign.
        stretch = stretch_ratio*control%increment_size
        !
        ! Follow the evolution line along S
        ! in the main loop over deformation increments
        do
            call this%onIncrementStart(control, icv, info)
            if (info /= VEF_OK) exit
            !
            ! Calculate the strain rate mode
            info = this%findSolution(sigma, D, ylp)
            if (info /= VEF_OK) then
                ! Re-attempt, try A from the previous increment as the starting point
                !
                ! Pick the most recent converged solution
                do i = icv%increment, 1, -1
                    if (output(i)%R < OBJECTIVE_THRESHOLD) then
                        D_retry = output(i)%A
                        exit
                    endif
                enddo
                ! Check post-condition of the loop: i > 0 means
                ! we have such a solution:
                if (i > 0) then
                    info = this%findSolution(sigma, D_retry, ylp_retry, vM_guess=.false.)
                    ! Accept the solution only if it is better than the original one
                    if (ylp_retry%R < ylp%R) then
                        D = D_retry
                        ylp = ylp_retry
                    endif
                endif
                info = VEF_OK
            endif
            if (info == VEF_ERROR) exit
            !
            ! Nasty hack: drilling a hole to libaltay to get the Taylor factor
            call getTaylorFactor(1, taylor_factor, info)
            !
            ! Make output record and prepare variables for updating icv
            tmp_record = IncrementOutputRecord(icv%IncrementationControlVariables, &
                                               ylp, &
                                               zero, zero, &
                                               taylor_factor)
            !
            ! Check if we start a/another increment
            stop_flag = .false.
            select case(control%scaling_type)
            case(scalingStrainTensor, scalingStrainTensorIncrement)
                stop_control_variable = norm2(icv%vP_step)
            !
            case(scalingPlasticWork)
                stop_control_variable = icv%plastic_work_total
            !
            case(scalingStrainTensorComponent)
                ! Get total plastic strain in appropriate reference frame
                ! and check the tensor component of interest.
                X_tmp = convert_stress_strain_space(icv%vP_step)
                if (present(rotmat)) X_tmp = rotate_to(X_tmp, rotmat)
                X_tmp_voigt = to_voigt(X_tmp, 6)
                stop_control_variable = abs(X_tmp_voigt(control%selected_tensor_component))
            case default
                ! Make sure it stops immediately
                stop_flag = .true.
                stop_control_variable = control%step_size+control%increment_size
            !
            end select
            !
            stop_flag = stop_flag &
                        .or.(stop_control_variable+stretch > control%step_size)
            !
            if (.not. stop_flag) then
                !
                ! Calculate incrementation control variables
                !
                ! Calculate increment of plastic strain to be imposed for texture evolution:
                select case(control%scaling_type)
                case(scalingStrainTensorIncrement)
                    control_variable = norm2(ylp%vA)
                !
                case(scalingStrainTensor)
                    ! Find scaling factor x such as
                    ! ||vP_step-x vA|| - ||vP_step|| = increment_size   (*)
                    n_roots = solveQuadraticPolynomial(a = dot_product(ylp%vA, ylp%vA), &
                                                       b = 2*dot_product(ylp%vA, icv%vP_step), &
                                                       c = dot_product(icv%vP_step, icv%vP_step) - &
                                                         (control%increment_size+norm2(icv%vP_step))**2, &
                                                       x = xi)
                    ! Up to two roots; we pick the largest one;
                    control_variable = -1.0_DP
                    if (n_roots > 0) control_variable = control%increment_size/maxval(xi(1:n_roots))
                    ! If control variable is negative (the only way to satisfy (*) is
                    ! to decrease the strain), fall back to a less accurate scheme.
                    if (control_variable < 0.D0) control_variable = norm2(ylp%vA)
                    !
                case(scalingPlasticWork)
                    control_variable = ylp%dotWonA
                !
                case(scalingStrainTensorComponent)
                    if (present(rotmat)) then
                        X_tmp = rotate_to(D, rotmat)
                    else
                        X_tmp = D
                    endif
                    X_tmp_voigt = to_voigt(X_tmp, 6)
                    control_variable = abs(X_tmp_voigt(control%selected_tensor_component))
                !
                case default
                    info = VEF_ERROR
                    exit
                end select
                !
                if (control_variable < epsilon(0.D0)) then
                      info = VEF_ERROR
                      exit
                endif
                scaling_factor = (control%increment_size/control_variable)
                !
                ! Calculate strain increment for material state evolution
                vDe = ylp%vA*scaling_factor
                tmp_record%P_inc_evol = convert_stress_strain_space(vDe)
                ! Update material state
                call makeTextureUpdateStep(tmp_record%P_inc_evol, &
                                           tmp_record%S_evol, &
                                           taylor_factor, &
                                           this%output%outputRequest, info)
                if (info /= 0) exit !< \fixme Literal constant in makeTextureUpdateStep
                vSe = convert_stress_strain_space(tmp_record%S_evol)
            else
                vDe = 0.D0
                vSe = 0.D0
            endif

            ! Append the output record
            if (.not. allocated(output)) then
                allocate(output(8))
            else if (size(output) == n_records) then
                allocate(buffer(2*size(output)))
                buffer(1:n_records) = output
                call move_alloc(buffer, output)
            end if
            n_records = n_records+1
            output(n_records) = tmp_record

            ! Update icv
            call icv%update(vDe, vSe, info)
            if (info /= VEF_OK) exit
            call this%onIncrementEnd(control, icv, tmp_record, info)
            if (stop_flag .or. (info /= VEF_OK)) exit
        enddo
        if (info /= VEF_OK) return

        ! Report back the incrementation control variables if requested
        if (present(incrementation_control)) incrementation_control = icv
    !
        output = output(1:n_records)
    end function



    !> Event handler in calculateStressPath: invoked at the begining of each
    !> increment
    subroutine StressDrivenEvolutionModule_onIncrementStart(this, control, icv, info)
    class(StressDrivenEvolutionModule), intent(inout)    :: this
    class(IncrementationControlSettings), intent(inout)  :: control
    class(IncrementationControl), intent(inout)          :: icv
    integer, intent(out)                                 :: info
    !
        info = VEF_OK
    !
    end subroutine


    !> Event handler in calculateStressPath: invoked at the end of each
    !> increment
    subroutine StressDrivenEvolutionModule_onIncrementEnd(this, control, icv, output_record, info)
    class(StressDrivenEvolutionModule), intent(inout)    :: this
    class(IncrementationControlSettings), intent(inout)  :: control
    class(IncrementationControl), intent(inout)          :: icv
    type(IncrementOutputRecord), intent(in)              :: output_record
    integer, intent(out)                                 :: info
    !
                info = VEF_OK
    !
    end subroutine

    !> Calculate the real roots of quadratic polynomial given in form
    !> a^2 x+b x+c = 0
    !> Provides x1 and x2. Both x1 and x2 are guaranteed to be set to a defined value,
    !> even if no real roots exist.
    integer function solveQuadraticPolynomial(a, b, c, x) result(n_roots)
        real(DP), intent(in)   :: a, b, c
        real(DP), dimension(2), intent(out)  :: x
        real(DP):: delta

        ! Satisfy intent(out)
        x = 0.D0
        n_roots = 0
        if (abs(a) > tiny(0.D0)) then
              delta = b**2 - 4.D0*a * c
              if (delta >= 0) then
                    x(1) = 0.5D0 * (-b-sqrt(delta)) / a
                    x(2) = 0.5D0 * (-b+sqrt(delta)) / a
                    n_roots = 2
              endif
        else
              ! Solve linear equation b x = -c
              if (abs(a) > epsilon(0.D0)) then
                    x(1) = -c/b
                    n_roots = 1
              endif
        endif
    end function


end module
