!> DMC Arbitrary Stress Response
module dmcASR
    use conversions
    use file_io
    use dmcIncrementationControl
    use dmcStressDrivenModule
    use dmcEvolutionOutputRecord
    use commonUtils
    use logging
    use file_io
    use deformation

    implicit none

    private
    public:: ASRModule

    character(*), parameter:: MOD_NAME = 'ASRModule'

    type:: StressDrivenStep
        real(DP), dimension(6)   :: stress_mode = 0.D0
        type(IncrementationControlSettings)             :: icv
        logical                                         :: update_state = .false.
    end type


    type, extends(StressDrivenModule):: ASRModule
        real(DP)::                              rotframe(3)
        type(StressDrivenStep), allocatable::   steps(:)
        type(IncrementationControlSettings):: control
    contains
        procedure:: readConfig => ASRModule_readConfig
        procedure:: run =>        ASRModule_run
        procedure:: outputFile => ASRModule_outputFile
    end type

    type:: ASROutput
        integer                 :: step = 0
        type(IncrementOutputRecord), dimension(:), allocatable   :: evolution_output
        real(DP), dimension(3, 3)   :: rotation_matrix = UNIT_MATRIX_3X3
    end type

contains

    integer function ASRModule_readConfig(this, cnfunit) result(info)
        class(ASRModule), intent(inout)            :: this
        integer, intent(in)                        :: cnfunit

        integer:: i, n_steps

        info = this%StressDrivenModule%readConfig(cnfunit)
        if (info /= VEF_OK) return

        info = VEF_ERROR
        ! Read parameters specific for the ASRModule
        if (.not. readValue(cnfunit, this%rotframe)) return
        if (.not. readValue(cnfunit, n_steps)) return
        if (n_steps <= 0) &
            call log_error(MOD_NAME, 'readconfig', ERR_VAL, 'Number of steps must at least be 1.')
        allocate(this%steps(n_steps))
        do i = 1, n_steps
            associate (step => this%steps(i))
                if (.not. readValue(cnfunit, step%stress_mode)) return
                if (.not. readValue(cnfunit, step%update_state)) return
                if (step%update_state) then
                    call IncrementationControlSettings_read(step%icv, cnfunit, info, &
                                                            allowed=[scalingStrainTensor, &
                                                                     scalingStrainTensorIncrement, &
                                                                     scalingPlasticWork])
                    if (info /= VEF_OK) return
                endif
                end associate
        enddo
        info = VEF_OK
    end function


    subroutine ASRModule_run(this, info)
        class(ASRModule), intent(inout)          :: this
        integer, intent(out)                     :: info

        real(DP), parameter:: STRETCH_RATIO = 1e-3_DP
        real(DP), dimension(3, 3), parameter:: ZERO = 0._DP
        character(*), parameter:: PROC_NAME =  "ASRModule_run"

        logical:: stop_flag
        integer:: istep, &
                  nsteps, &
                  ofunit, &
                  i, &
                  n_roots, &
                  n_records
        ! Quantities in the global (aka. material = texture) reference frame
        real(DP):: sigma(3,3), &
                   S(3,3), &
                   Pressure(3,3), &  !< total stress, deviatoric stress, hydrostatic stress
                   Mrot(3,3), &
                   scaling_factor, &
                   control_variable, &
                   stop_control_variable, &
                   stretch, &
                   X_tmp(3, 3), &
                   vDe(5), &
                   vSe(5), &
                   X_tmp_voigt(6), &
                   target_stress_mode(5), &
                   target_stress_norm, &
                   strain_mode(5), &
                   stress(5), &
                   residual(5), &
                   taylor_factor, &
                   xi(2), &
                   target_strain

        ! Quantities in rotated (aka. sample) reference frame
        type(ASROutput):: output
        type(IncrementationControl):: icv
        type(IncrementOutputRecord), allocatable:: buffer(:)
        type(IncrementOutputRecord):: tmp_record
        type(Increment), allocatable:: incs(:)

        !
        RETURN_IF(info /= VEF_OK, call this%StressDrivenEvolutionModule%run(info))

        ! Open and initialize result files
        RETURN_IF(info /= VEF_OK, info = this%openOutputFile('.asr',ofunit))
        RETURN_IF(info /= VEF_OK, info = this%outputFile(ofunit, header=.true.))
        !
        nsteps = size(this%steps)

        ! Calculate rotation matrix (active rotation from material (=texture) to sample frame)
        Mrot = euler_to_tensor(deg_to_rad(this%rotframe))

        do  istep = 1, nsteps
            associate(step => this%steps(istep), &
                      control => this%steps(istep)%icv)
                ! Acquire full stress tensor sigma
                sigma = unscaled_voigt_to_tensor(step%stress_mode)
                n_records = 0

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
                                                       norm2(stress), &
                                                       target_stress_norm, &
                                                       norm2(deviatoric_to_unscaled_voigt(residual)))

                    ! Check if we start a/another increment
                    stop_flag = .false.
                    select case(control%scaling_type)
                    case(scalingStrainTensor, scalingStrainTensorIncrement)
                        stop_control_variable = norm2(icv%vP_step)
                    case(scalingPlasticWork)
                        stop_control_variable = icv%plastic_work_total
                    case(scalingStrainTensorComponent)
                        ! Get total plastic strain in appropriate reference frame
                        ! and check the tensor component of interest.
                        X_tmp = deviatoric_to_tensor(icv%vP_step)
                        X_tmp = rotate_to(X_tmp, Mrot)
                        X_tmp_voigt = tensor_to_unscaled_voigt(X_tmp)
                        stop_control_variable = abs(X_tmp_voigt(control%selected_tensor_component))
                    case default
                        ! Make sure it stops immediately
                        stop_flag = .true.
                        stop_control_variable = control%step_size+control%increment_size
                    end select

                    stop_flag = stop_flag &
                                .or.(stop_control_variable+stretch > control%step_size)
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
                            X_tmp = rotate_to(deviatoric_to_tensor(strain_mode), Mrot)
                            X_tmp_voigt = tensor_to_unscaled_voigt(X_tmp)
                            control_variable = abs(X_tmp_voigt(control%selected_tensor_component))
                        !
                        case default
                            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, "Invalid incrementation type.")
                        end select
                        scaling_factor = (control%increment_size/control_variable)
                        !
                        ! Calculate strain increment for material state evolution
                        vDe = strain_mode * scaling_factor
                        target_strain = deviatoric_strain_to_von_mises(vde)
                        tmp_record%P_inc_evol = deviatoric_to_tensor(vDe)

                        ! Update material state
                        call altay_deform(tmp_record%P_inc_evol, target_strain, incs)
                        tmp_record%S_evol = incs(size(incs))%stress

                        vSe = tensor_to_deviatoric(tmp_record%S_evol)
                    else
                        vDe = 0.D0
                        vSe = 0.D0
                    endif

                    ! Append the output record
                    if (.not. allocated(output%evolution_output)) then
                        allocate(output%evolution_output(8))
                    else if (size(output%evolution_output) == n_records) then
                        allocate(buffer(2*size(output%evolution_output)))
                        buffer(1:n_records) = output%evolution_output
                        call move_alloc(buffer, output%evolution_output)
                    end if
                    n_records = n_records+1
                    output%evolution_output(n_records) = tmp_record

                    ! Update icv
                    call icv%update(vDe, incs, info)
                    if (info /= VEF_OK) &
                        call log_error(MOD_NAME, PROC_NAME, ERR, "Unable to update incrementation control variables.")
                    if (stop_flag) &
                        exit
                enddo
                output%evolution_output = output%evolution_output(1:n_records)
            end associate

            if (info /= VEF_OK) &
                call log_error(MOD_NAME, 'run', ERR, 'Unable to calculate stress path.')
            !
            ! Collect the outputs
            !
            output%step = istep
            output%rotation_matrix = Mrot
            !
            ! Post-process & report
            !
            info = this%outputFile(ofunit, output)
            if (info /= VEF_OK) &
                call log_error(MOD_NAME, 'run', ERR_IO, 'Can not write output')
        enddo
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


    !> Output the results
    !>
    !> The procedure writes either header, data or both.
    integer function ASRModule_outputFile(this, iounit, output, header) result(info)
        class(ASRModule), intent(in)         :: this
        integer, intent(in)                  :: iounit   !< I/O output unit
        type(ASROutput), intent(in), optional:: output   !< Data to be written out
        logical, intent(in), optional         :: header   !< Request for header to be written out
        !
        integer:: i, ierr, increment
        integer, parameter:: ncolumn_labels = 19, column_width = 12
        character(len = column_width), dimension(ncolumn_labels):: column_labels = &
            [ character(len = column_width) ::  &
                'step','increment', & ! 2 fields
                'epsilon_vM', 'sigma_vM', 'dotW','M', 'residual', & ! 5 fields
                'sigma_xx','sigma_yy','sigma_zz','sigma_yz','sigma_xz','sigma_xy', & ! 6 fields
                'A_xx','A_yy','A_zz','A_yz','A_xz','A_xy' & ! 6 fields
            ]
        !
        info = VEF_OK

        column_labels = adjustr(column_labels)

        !
        ! Write out header lines
        !
        if (optionalDefault(header, .false.)) then
            info = VEF_ERROR
            ! Column numbers
            write(iounit, '(18(I12))', iostat = ierr) (i, i = 1, ncolumn_labels)
            write(iounit, '(18(A12))', iostat = ierr) (column_labels(i), i = 1, ncolumn_labels)
            if (ierr /= 0) return
            info = VEF_OK
        endif
        !
        ! Write out data output
        if (present(output)) then
            ierr = 0
            info = VEF_ERROR
            !
            do increment = 1, size(output%evolution_output)


                associate(v => output%evolution_output(increment))
                    write(iounit, fmt = 710, iostat = ierr) &
                                output%step, &
                                v%icv%increment, & ! 2 fields
                                v%vm_strain_total, &
                                v%scal_s, &
                                v%dotWonA, &
                                v%taylor_factor, &
                                v%R, &
                                tensor_to_unscaled_voigt(v%SonA), &
                                tensor_to_unscaled_voigt(v%A)
                end associate
            enddo
            if (ierr == 0) info = VEF_OK
        endif
        ! Formats for output file
        710 format(2(I12), 17(ES12.3E2))
    end function
end module
