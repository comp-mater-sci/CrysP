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
    use libcrysp
    use dmcBasicModule

    implicit none

    private
    public:: ASRModule

    character(*), parameter:: MOD_NAME = 'ASRModule'
    !> Reported output quantities for each increment
    character(10), dimension(16), parameter:: OUTPUT_HEADER = [character(10):: 'epsilon_vM', &
                                                               'sigma_vM', &
                                                               'dotW', &
                                                               'residual', &
                                                               'sigma_11','sigma_22','sigma_33','sigma_23','sigma_13','sigma_12', &
                                                               'A_11','A_22','A_33','A_23','A_13','A_12']

    type:: StressDrivenStep
        real(DP), dimension(6):: target_stress_mode
        real(DP):: target_vm_strain
        type(StressIncrement), dimension(:), allocatable:: stress_increments
        type(StrainIncrement), dimension(:), allocatable:: strain_increments
    end type

    type, extends(BasicModule):: ASRModule
        type(StressDrivenStep), dimension(:), allocatable:: steps
    contains
        procedure:: initialize => asr_initialize
        procedure:: run        => asr_run
    end type

contains

    subroutine asr_initialize(this, cnfunit)
        class(ASRModule), intent(inout)            :: this
        integer, intent(in)                        :: cnfunit

        character(*), parameter:: PROC_NAME = 'asrmodule_readconfig'

        integer:: i, n_steps

        call this%BasicModule%initialize(cnfunit)

        ! Read parameters specific for the ASRModule
        call read_value(cnfunit, n_steps)
        if (n_steps <= 0) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Number of steps must at least be 1.')
        allocate(this%steps(n_steps))
        do i = 1, n_steps
            associate (step => this%steps(i))
                call read_value(cnfunit, step%target_stress_mode)
                !There is no real reason to normalize but we do anyway for concistency
                step%target_stress_mode = step%target_stress_mode / norm2(step%target_stress_mode)
                call read_value(cnfunit, step%target_vm_strain)
            end associate
        enddo
    end subroutine

    subroutine asr_run(this)
        class(ASRModule), intent(inout):: this

        integer:: i_step, i_stress, i_strain, &
                  out_unit, texture_unit, &
                  n_incs, &
                  offset
        real(DP):: dev_stress(5), &
                   stress(6), &
                   target_dev_stress(5), &
                   def_grad(3,3), &
                   hydro, &
                   dev, &
                   stress_scaling_factor
        type(Material), target:: mat_

        out_unit = open_output_file(this%output_prefix, OUTPUT_HEADER)
        texture_unit = open_texture_evolution_file(this%output_prefix)

        n_incs = 0
        def_grad = UNIT_MATRIX_3X3
        do i_step=1, size(this%steps)
            associate (step => this%steps(i_step))
                !Determine by how much to scale the deviatoric stress to obtain the total stress
                !Because the actual stress tensor is just the stress mode scaled by the stress norm,
                !The hydrostatic component is also the hydrostatic component of the stress mode scaled by the stress norm.
                !The sress norm is in turn the norm of the deviatoric component over the norm of the deviatoric component of the stress mode
                target_dev_stress = unscaled_voigt_to_deviatoric(step%target_stress_mode)
                dev = norm2(target_dev_stress)
                hydro = sum(step%target_stress_mode(1:3)) / 3._DP
                stress_scaling_factor = hydro / dev

                !Run deformation
                target_dev_stress = target_dev_stress / norm2(target_dev_stress)
                if (step%target_vm_strain < TOLERANCE) then
                    allocate(step%stress_increments(1))
                    allocate(step%strain_increments(1))
                    associate (stress_inc => step%stress_increments(1), &
                               strain_inc => step%strain_increments(1))
                        stress_inc%strain_rate = target_dev_stress
                        stress_inc%n_strain_increments = 1
                        call crysp_simulate_stress_mode(this%material, target_dev_stress, stress_inc%strain_rate, dev_stress, stress_inc%residual)
                        strain_inc%stress = deviatoric_to_tensor(dev_stress)
                        strain_inc%deformation_gradient = UNIT_MATRIX_3X3
                        n_incs = n_incs + 1
                    end associate
                else
                    call crysp_stress_driven_deformation(this%material, target_dev_stress, step%target_vm_strain, step%stress_increments, step%strain_increments)
                    !Write new texture to file
                    do i_stress = 1, size(step%stress_increments)
                        n_incs = n_incs + step%stress_increments(i_stress)%n_strain_increments
                    end do
                    call mat_%deserialize(this%material)
                    call write_texture_increment(texture_unit, n_incs, mat_%clusters)
                    this%material = mat_%serialize()
                end if

                offset = 0
                !Write increments to file
                do i_stress = 1, size(step%stress_increments)
                    associate (stress_inc => step%stress_increments(i_stress))
                        do i_strain=offset+1, offset+stress_inc%n_strain_increments
                            associate (strain_inc => step%strain_increments(i_strain))
                                stress = tensor_to_unscaled_voigt(strain_inc%stress)
                                stress(1:3) = stress(1:3) + stress_scaling_factor * norm2(strain_inc%stress)

                                call write_output_increment(out_unit, [stretch_to_von_mises_true_strain(matmul(strain_inc%deformation_gradient, def_grad)), &
                                                                       sqrt(3._DP/2._DP) * norm2(strain_inc%stress), &
                                                                       deviatoric_to_tensor(stress_inc%strain_rate) .dot. strain_inc%stress, &
                                                                       norm2(stress_inc%residual), &
                                                                       stress, &
                                                                       deviatoric_to_unscaled_voigt(stress_inc%strain_rate)])
                            end associate
                        end do
                        offset = offset + stress_inc%n_strain_increments
                        def_grad = matmul(step%strain_increments(offset)%deformation_gradient, def_grad)
                    end associate
                end do
            end associate
        end do

        close(out_unit)
        close(texture_unit)
    end subroutine
end module
