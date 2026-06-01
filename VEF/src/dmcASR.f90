!> DMC Arbitrary Stress Response
module dmcASR
    use conversions
    use file_io
    use commonUtils
    use logging
    use file_io
    use deformation
    use macro
    use incrementation
    use altay
    use dmcBasicModule

    implicit none

    private
    public:: ASRModule

    character(*), parameter:: MOD_NAME = 'ASRModule'

    type:: StressDrivenStep
        real(DP), dimension(6):: target_stress_mode
        real(DP):: target_vm_strain
        type(StressIncrement), dimension(:), allocatable:: increments
    end type

    type, extends(BasicModule):: ASRModule
        type(StressDrivenStep), dimension(:), allocatable:: steps
    contains
        procedure:: readConfig => ASRModule_readConfig
        procedure:: run =>        ASRModule_run
        procedure:: outputFile => ASRModule_outputFile
    end type

contains

    integer function ASRModule_readConfig(this, cnfunit) result(info)
        class(ASRModule), intent(inout)            :: this
        integer, intent(in)                        :: cnfunit

        integer:: i, n_steps

        info = this%BasicModule%readConfig(cnfunit)
        if (info /= VEF_OK) return

        info = VEF_ERROR
        ! Read parameters specific for the ASRModule
        if (.not. readValue(cnfunit, n_steps)) return
        if (n_steps <= 0) &
            call log_error(MOD_NAME, 'readconfig', ERR_VAL, 'Number of steps must at least be 1.')
        allocate(this%steps(n_steps))
        do i = 1, n_steps
            associate (step => this%steps(i))
                if (.not. readValue(cnfunit, step%target_stress_mode)) return
                !There is no real reason to normalize but we do anyway for concistency
                step%target_stress_mode = step%target_stress_mode / norm2(step%target_stress_mode)
                if (.not. readValue(cnfunit, step%target_vm_strain)) return
            end associate
        enddo
        info = VEF_OK
    end function

    subroutine asrmodule_run(this,info)
        class(ASRModule), intent(inout):: this
        integer, intent(out):: info

        integer:: i
        real(DP):: dev_stress(5), &
                   target_dev_stress(5)

        do i=1, size(this%steps)
            associate (step => this%steps(i))
                target_dev_stress = unscaled_voigt_to_deviatoric(step%target_stress_mode)
                target_dev_stress = target_dev_stress / norm2(target_dev_stress)

                if (step%target_vm_strain < TOLERANCE) then
                    allocate(step%increments(1))
                    associate (inc => step%increments(1))
                        allocate(inc%strain_increments(1))
                        inc%strain_rate = target_dev_stress
                        call altay_simulate_stress_mode(this%material, target_dev_stress, inc%strain_rate, dev_stress, inc%residual)
                        inc%strain_increments(1)%stress = deviatoric_to_tensor(dev_stress)
                    end associate
                else
                    call altay_stress_driven_deformation(this%material, target_dev_stress, step%target_vm_strain, step%increments)
                end if
            end associate
        end do

        call this%outputfile()
    end subroutine

    !> Output the results
    !>
    !> The procedure writes either header, data or both.
    subroutine ASRModule_outputFile(this)
        class(ASRModule), intent(in):: this

        character(*), parameter:: PROC_NAME = 'asermodule_openoutputfile'
        integer, parameter:: ncolumn_labels = 18, column_width = 12
        character(len = column_width), dimension(ncolumn_labels):: column_labels = &
                [ character(len = column_width) ::  &
                    'step','increment', & ! 2 fields
                    'epsilon_vM', 'sigma_vM', 'dotW', 'residual', & ! 4 fields
                    'sigma_xx','sigma_yy','sigma_zz','sigma_yz','sigma_xz','sigma_xy', & ! 6 fields
                    'A_xx','A_yy','A_zz','A_yz','A_xz','A_xy' & ! 6 fields
                ]

        integer:: i, j, k, &
                  tot_incs, &
                  ierr, &
                  iounit
        real(DP):: stress_scaling_factor, &
                   def_grad(3,3), &
                   hydro, &
                   dev, &
                   stress(6)

        call write_texture(this%material%clusters)

        if (this%openOutputFile('.asr',iounit) /= VEF_OK) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Unable to upen output file')

        ! Write out header lines
        column_labels = adjustr(column_labels)
        write(iounit, '(18(A12))', iostat = ierr) (column_labels(i), i = 1, ncolumn_labels)
        if (ierr /= 0) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Unable to write ouptut file header.')

        ! Write out data output
        def_grad = UNIT_MATRIX_3X3
        do i=1, size(this%steps)
            tot_incs = 0
            !Determine by how much to scale the deviatoric stress to obtain the total stress
            !Because the actual stress tensor is just the stress mode scaled by the stress norm,
            !The hydrostatic component is also the hydrostatic component of the stress mode scaled by the stress norm.
            !The sress norm is in turn the norm of the deviatoric component over the norm of the deviatoric component of the stress mode

            dev = norm2(unscaled_voigt_to_deviatoric(this%steps(i)%target_stress_mode))
            hydro = sum(this%steps(i)%target_stress_mode(1:3)) / 3._DP
            stress_scaling_factor = hydro / dev

            do j = 1, size(this%steps(i)%increments)
                associate (stress_inc => this%steps(i)%increments(j))
                    do k=1,size(stress_inc%strain_increments)
                        associate (strain_inc => stress_inc%strain_increments(k))
                            stress = tensor_to_unscaled_voigt(strain_inc%stress)
                            stress(1:3) = stress(1:3) + stress_scaling_factor * norm2(strain_inc%stress)
                            tot_incs = tot_incs + 1
                            write(iounit, fmt = 710, iostat = ierr) &
                                i, &
                                tot_incs, & ! 2 fields
                                stretch_to_von_mises_true_strain(matmul(strain_inc%deformation_gradient, def_grad)), &
                                sqrt(3._DP/2._DP) * norm2(strain_inc%stress), &
                                deviatoric_to_tensor(stress_inc%strain_rate) .dot. strain_inc%stress, &
                                norm2(stress_inc%residual), &
                                stress, &
                                deviatoric_to_unscaled_voigt(stress_inc%strain_rate)
                        end associate
                    end do
                    !Value of k is guaranteed by the standard
                    def_grad = matmul(stress_inc%strain_increments(k-1)%deformation_gradient, def_grad)
                end associate
            end do
        end do
        ! Formats for output file
        710 format(2(I12), 16(ES12.3E2))
    end subroutine
end module
