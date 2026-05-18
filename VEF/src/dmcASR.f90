!> DMC Arbitrary Stress Response
module dmcASR
    use conversions
    use file_io
    use dmcStressDrivenModule
    use commonUtils
    use logging
    use file_io
    use deformation
    use incrementation

    implicit none

    private
    public:: ASRModule

    character(*), parameter:: MOD_NAME = 'ASRModule'

    type:: StressDrivenStep
        real(DP), dimension(6):: target_stress_mode
        real(DP):: target_vm_strain
        type(StressIncrement), dimension(:), allocatable:: increments
    end type

    type, extends(StressDrivenModule):: ASRModule
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

        info = this%StressDrivenModule%readConfig(cnfunit)
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
                        call altay_simulate_stress_mode(target_dev_stress, inc%strain_rate, dev_stress, inc%residual)
                        inc%strain_increments(1)%stress = deviatoric_to_tensor(dev_stress)
                    end associate
                else
                    call altay_stress_driven_deformation(target_dev_stress, step%target_vm_strain, step%increments)
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
                   true_strain(3,3)

        if (this%openOutputFile('.asr',iounit) /= VEF_OK) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Unable to upen output file')

        ! Write out header lines
        column_labels = adjustr(column_labels)
        write(iounit, '(18(A12))', iostat = ierr) (column_labels(i), i = 1, ncolumn_labels)
        if (ierr /= 0) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Unable to write ouptut file header.')

        ! Write out data output
        true_strain = 0._DP
        do i=1, size(this%steps)
            tot_incs = 0
            !! Determine by how much to scale the deviatoric stress to obtain the total stress
            stress_scaling_factor = norm2(unscaled_voigt_to_tensor(this%steps(i)%target_stress_mode)) / norm2(unscaled_voigt_to_deviatoric(this%steps(i)%target_stress_mode))
            do j = 1, size(this%steps(i)%increments)
                associate (stress_inc => this%steps(i)%increments(j))
                    do k=1,size(stress_inc%strain_increments)
                        associate (strain_inc => stress_inc%strain_increments(k))
                            tot_incs = tot_incs + 1
                            write(iounit, fmt = 710, iostat = ierr) &
                                i, &
                                tot_incs, & ! 2 fields
                                strain_tensor_to_von_mises(true_strain + stretch_to_true_strain(strain_inc%deformation_gradient)), &
                                sqrt(3._DP/2._DP) * norm2(strain_inc%stress), &
                                deviatoric_to_tensor(stress_inc%strain_rate) .dot. strain_inc%stress, &
                                norm2(stress_inc%residual), &
                                tensor_to_unscaled_voigt(strain_inc%stress) * stress_scaling_factor, &
                                deviatoric_to_unscaled_voigt(stress_inc%strain_rate)
                        end associate
                    end do
                    !Value of k is guaranteed by the standard
                    true_strain = true_strain + stretch_to_true_strain(stress_inc%strain_increments(k-1)%deformation_gradient)
                end associate
            end do
        end do
        ! Formats for output file
        710 format(2(I12), 16(ES12.3E2))
    end subroutine
end module
