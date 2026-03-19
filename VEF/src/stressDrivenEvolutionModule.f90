!> Implementation of a DMC computiational module that allows stress-driven evolution of
!> material state.
module dmcStressDrivenEvolutionModule
use base_defs
use conversions
use dmcStressDrivenModule
use dmcIncrementationControl
use dmcEvolutionOutputRecord
use commonUtils
use altay
use altayconfig

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
        real(DP):: scaling_factor, control_variable, stop_control_variable, stretch, X_tmp(3, 3)
        real(DP), dimension(5):: vDe, vSe
        type(IncrementationControl):: icv
        real(DP), dimension(6):: X_tmp_voigt

        real(DP):: target_stress_mode(5), &
                   target_stress_norm, &
                   strain_mode(5), &
                   stress(5), &
                   residual(5)

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
            ! Calculate the strain rate mode
            target_stress_mode = tensor_to_deviatoric(sigma)
            target_stress_norm = norm2(target_stress_mode)
            target_stress_mode = target_stress_mode / target_stress_norm
            call this%findsolution(target_stress_mode, strain_mode, stress, residual)

            ! Make output record and prepare variables for updating icv
            tmp_record = IncrementOutputRecord(icv%IncrementationControlVariables, &
                                               zero, zero, &
                                               target_stress_mode, &
                                               strain_mode, &
                                               stress/norm2(stress), &
                                               norm2(stress)/target_stress_norm, &
                                               norm2(deviatoric_to_unscaled_voigt(residual)))

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
                X_tmp = deviatoric_to_tensor(icv%vP_step)
                if (present(rotmat)) X_tmp = rotate_to(X_tmp, rotmat)
                X_tmp_voigt = tensor_to_unscaled_voigt(X_tmp)
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
                    control_variable = 1._DP
                !
                case(scalingStrainTensor)
                    ! Find scaling factor x such as
                    ! ||vP_step-x vA|| - ||vP_step|| = increment_size   (*)
                    n_roots = solveQuadraticPolynomial(a = strain_mode .dot. strain_mode, &
                                                       b = 2 * strain_mode .dot. icv%vp_step, &
                                                       c = (icv%vp_step .dot. icv%vp_step) - &
                                                           (control%increment_size+norm2(icv%vp_step))**2, &
                                                       x=xi)
                    ! Up to two roots; we pick the largest one;
                    control_variable = -1.0_DP
                    if (n_roots > 0) control_variable = control%increment_size/maxval(xi(1:n_roots))
                    ! If control variable is negative (the only way to satisfy (*) is
                    ! to decrease the strain), fall back to a less accurate scheme.
                    if (control_variable < 0.D0) control_variable = 1._DP
                    !
                case(scalingPlasticWork)
                    control_variable = strain_mode .dot. stress
                !
                case(scalingStrainTensorComponent)
                    if (present(rotmat)) then
                        X_tmp = rotate_to(deviatoric_to_tensor(strain_mode), rotmat)
                    else
                        X_tmp = deviatoric_to_tensor(strain_mode)
                    endif
                    X_tmp_voigt = tensor_to_unscaled_voigt(X_tmp)
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
                vDe = strain_mode * scaling_factor
                tmp_record%P_inc_evol = deviatoric_to_tensor(vDe)
                ! Update material state
                call makeTextureUpdateStep(tmp_record%P_inc_evol, &
                                           tmp_record%S_evol, &
                                           this%output%outputRequest, info)
                if (info /= 0) exit !< \fixme Literal constant in makeTextureUpdateStep

                vSe = tensor_to_deviatoric(tmp_record%S_evol)
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
            if (stop_flag .or. (info /= VEF_OK)) exit
        enddo
        if (info /= VEF_OK) return

        ! Report back the incrementation control variables if requested
        if (present(incrementation_control)) incrementation_control = icv
    !
        output = output(1:n_records)
    end function

      subroutine makeTextureUpdateStep(D, S, output_flag, info)
          real(DP), dimension(3, 3), intent(in)      :: D
          real(DP), dimension(3, 3), intent(out)     :: S
          logical, intent(in)                              :: output_flag
          integer, intent(out)                             :: info

          real(DP):: taylor_factor

          call deformation_step(D, S, taylor_factor)
          info = VEF_OK
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
