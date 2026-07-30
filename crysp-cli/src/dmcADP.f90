!> Arbitrary Deformation Path strain-(rate) driven simulations
module dmcADP
    use base_defs
    use conversions
    use dmcBasicModule
    use file_io
    use libcrysp
    use incrementation
    use logging
    use libcrysp

    implicit none

    private
    public:: ADPModule

    character(*), parameter:: MOD_NAME = 'dmcADP'
    !> Reported output quantities for each increment
    character(8), dimension(18), parameter:: OUTPUT_HEADER = [character(8):: 'duration', &
                                                              'eps_vm', &
                                                              'L_11','L_21','L_31','L_12','L_22','L_32','L_13','L_23','L_33',  &
                                                              'S_11','S_22','S_33','S_23','S_13','S_12', &
                                                              'M']


    type, extends(BasicModule):: ADPModule
        type(StrainDrivenStep),dimension(:),allocatable :: steps
    contains
        procedure, pass(this):: initialize => adp_initialize
        procedure, pass(this):: run        => adp_run
    end type

    !> A strain-(rate) driven step
    type:: StrainDrivenStep
        real(DP), dimension(3, 3):: velocity_gradient
        real(DP):: target_strain
        type(StrainIncrement), dimension(:), allocatable:: increments
    end type

contains

    !> Read configuration from IO unit (type-bound function)
    subroutine adp_initialize(this, cnfunit)
        class(ADPModule), intent(inout)   :: this   !! passed implicitly
        integer, intent(in)              :: cnfunit !! IO input unit; pass explicitly

        character(*), parameter:: PROC_NAME = 'adp_initialize'

        integer:: n_steps, i

        call this%BasicModule%initialize(cnfunit)

        !Read the module-specific config
        call read_value(cnfunit, n_steps)

        if (n_steps < 1) then
            return
        end if

        allocate(this%steps(n_steps))

        do i = 1, n_steps
            associate (step => this%steps(i))
                call read_value(cnfunit, step%velocity_gradient)
                if (math_trace33(step%velocity_gradient) > TOLERANCE) &
                    call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Volumetric deformation is not alowed.')

                call read_value(cnfunit, step%target_strain)
                if (step%target_strain < 0._DP) &
                    call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Target strain must not be negative.')
            end associate
        end do
    end subroutine

    !> Run the simulation
    subroutine adp_run(this)
        class(ADPModule), intent(inout)  :: this

        integer:: i_step, i_inc, &
                  out_unit, texture_unit, &
                  n_incs
        real(DP):: v_grad_inc(3,3), &
                   taylor_factor, &
                   dev_stress(5), &
                   def_grad(3,3)

        !Open output files
        out_unit = open_output_file(this%output_prefix, OUTPUT_HEADER)
        texture_unit = open_texture_evolution_file(this%output_prefix)

        ! Main loop over the steps
        n_incs = 0
        def_grad = UNIT_MATRIX_3X3
        do i_step = 1, size(this%steps)
            associate(step => this%steps(i_step))
                if (step%target_strain < TOLERANCE) then
                    allocate(step%increments(1))
                    call altay_simulate_strain_mode(this%material, tensor_to_deviatoric(step%velocity_gradient), dev_stress)
                    step%increments(1)%stress = deviatoric_to_tensor(dev_stress)
                    n_incs = n_incs + 1
                else
                    call altay_strain_driven_deformation(this%material, step%velocity_gradient, step%target_strain, step%increments)
                    n_incs = n_incs + size(step%increments)
                    call write_texture_increment(texture_unit, n_incs, this%material%clusters)
                end if

                do i_inc = 1, size(step%increments)
                    associate(inc => step%increments(i_inc))
                        call write_output_increment(out_unit, [inc%duration, &
                                                               deformation_gradient_to_von_mises_true_strain(matmul(inc%deformation_gradient, def_grad)), &
                                                               step%velocity_gradient, &
                                                               tensor_to_unscaled_voigt(inc%stress), &
                                                               inc%taylor_factor])
                    end associate
                end do
                !Value of i_inc is guaranteed by the standard.
                def_grad = matmul(step%increments(i_inc-1)%deformation_gradient, def_grad)
            end associate
        enddo

        close(out_unit)
        close(texture_unit)
    end subroutine
end module
