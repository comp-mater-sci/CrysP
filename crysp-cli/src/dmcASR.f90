!> DMC Arbitrary Stress Response
module dmcASR
    use conversions
    use file_io
    use commonUtils
    use logging
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
                                                               'stress_mode_deviation', &
                                                               'sigma_11','sigma_22','sigma_33','sigma_23','sigma_13','sigma_12', &
                                                               'A_11','A_22','A_33','A_23','A_13','A_12']

    type:: StressDrivenStep
        real(DP), dimension(6):: target_stress_mode
        real(DP):: target_vm_strain
        type(Increment), dimension(:), allocatable:: increments
    end type

    type, extends(BasicModule):: ASRModule
        type(StressDrivenStep), dimension(:), allocatable:: steps
    contains
        procedure:: initialize => asr_initialize
        procedure:: run        => asr_run
    end type

contains

    subroutine asr_initialize(this, cnfunit)
        class(ASRModule), target, intent(inout)    :: this
        integer, intent(in)                        :: cnfunit

        character(*), parameter:: PROC_NAME = 'asrmodule_readconfig'

        integer:: i, n_steps

        call this%BasicModule%initialize(cnfunit)

        ! Read parameters specific for the ASRModule
        call read_value(cnfunit, n_steps)
        if (n_steps <= 0) &
            call log_error(MOD_NAME, PROC_NAME, CRYSP_ERR_VAL, 'Number of steps must at least be 1.')
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
        class(ASRModule), target, intent(inout):: this

        integer:: i_step, i_inc, &
                  out_unit, texture_unit, &
                  n_incs
        real(DP):: strain_mode(6,1), &
                   stress_mode_deviation(1), &
                   stress(6,1), &
                   def_grad(3,3)

        out_unit = open_output_file(this%output_prefix, OUTPUT_HEADER)
        texture_unit = open_texture_evolution_file(this%output_prefix)

        n_incs = 0
        def_grad = UNIT_MATRIX_3X3
        do i_step=1, size(this%steps)
            associate (step => this%steps(i_step))
                !Run deformation
                if (step%target_vm_strain < TOLERANCE) then
                    allocate(step%increments(1))
                    associate (inc => step%increments(1))
                        !The increment keeps its default initialization apart from the strain rate,
                        !residual and stress: no duration, no deformation.
                        call handle_crysp_status(crysp_simulate_stress_modes(this%material, &
                                                                             reshape(step%target_stress_mode,[6,1]), &
                                                                             strain_mode, &
                                                                             stress, &
                                                                             stress_mode_deviation))
                        inc%velocity_gradient = unscaled_voigt_to_tensor(strain_mode(:,1))
                        inc%stress = unscaled_voigt_to_tensor(stress(:,1))
                        inc%residual = residual(1)
                        n_incs = n_incs + 1
                    end associate
                else
                    call handle_crysp_status(crysp_stress_driven_deformation(this%material, [step%target_stress_mode], &
                                                                             step%target_vm_strain, step%increments))
                    !Write new texture to file
                    n_incs = n_incs + size(step%increments)
                    call write_texture_increment(texture_unit, n_incs, step%increments(size(step%increments))%texture)
                end if

                !Write increments to file
                do i_inc = 1, size(step%increments)
                    associate (inc => step%increments(i_inc))
                        call write_output_increment(out_unit, [stretch_to_von_mises_true_strain(matmul(inc%deformation_gradient, def_grad)), &
                                                               sqrt(3._DP/2._DP) * norm2(tensor_to_deviatoric(inc%stress)), &
                                                               inc%velocity_gradient .dot. inc%stress, &
                                                               norm2(inc%stress_mode_deviation), &
                                                               tensor_to_unscaled_voigt(inc%stress), &
                                                               tensor_to_unscaled_voigt(inc%velocity_gradient)])
                    end associate
                end do
                def_grad = matmul(step%increments(size(step%increments))%deformation_gradient, def_grad)
            end associate
        end do

        close(out_unit)
        close(texture_unit)
    end subroutine
end module
