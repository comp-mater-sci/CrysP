!> DMC Arbitrary Stress Response
module dmcASR
    use conversions
    use criUncomment, only: readValue
    use dmcIncrementationControl
    use dmcStressDrivenEvolutionModule
    use dmcEvolutionOutputRecord
    use commonUtils
    use logging

    implicit none

    private
    public:: ASRModule

    character(*), parameter:: MOD_NAME = 'ASRModule'

    type:: StressDrivenStep
        real(DP), dimension(6)   :: stress_mode = 0.D0
        type(IncrementationControlSettings)             :: incrementation_control
        logical                                         :: update_state = .false.
    end type


    type, extends(StressDrivenEvolutionModule):: ASRModule
        real(DP)::                              rotframe(3)
        type(StressDrivenStep), allocatable::   steps(:)
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
        !
        integer:: i, n_steps
        !
        info = this%StressDrivenEvolutionModule%readConfig(cnfunit)
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
                    call IncrementationControlSettings_read(step%incrementation_control, cnfunit, info, &
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
        !
        ! Quantities in the global (aka. material = texture) reference frame
        real(DP), dimension(3, 3)    :: sigma, S,  Pressure  !< total stress, deviatoric stress, hydrostatic stress
        ! Quantities in rotated (aka. sample) reference frame
        type(ASROutput)                 :: output
        type(IncrementationControl)     :: icv
        real(DP), dimension(3, 3)   :: Mrot
        integer     :: istep, nsteps, ofunit

        !
        RETURN_IF(info /= VEF_OK, call this%StressDrivenEvolutionModule%run(info))

        ! Open and initialize result files
        RETURN_IF(info /= VEF_OK, info = this%openOutputFile('.asr',ofunit))
        RETURN_IF(info /= VEF_OK, info = this%outputFile(ofunit, header=.true.))
        !
        nsteps = size(this%steps)
        !
        ! Calculate rotation matrix (active rotation from material (=texture) to sample frame)
        Mrot = euler_to_tensor(deg_to_rad(this%rotframe))
        !
        do  istep = 1, nsteps
            associate(step => this%steps(istep), control => this%steps(istep)%incrementation_control)
                ! Acquire full stress tensor sigma
                sigma = unscaled_voigt_to_tensor(step%stress_mode)
                ! Follow the stress path
                info = this%calculateStressPath(sigma, control, output%evolution_output, Mrot, &
                                                incrementation_control = icv)
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
            write(iounit, '(19(I12))', iostat = ierr) (i, i = 1, ncolumn_labels)
            write(iounit, '(19(A12))', iostat = ierr) (column_labels(i), i = 1, ncolumn_labels)
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
                                sqrt(1.5_DP) * norm2(v%SonA), &
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
