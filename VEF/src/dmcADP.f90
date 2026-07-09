!> Arbitrary Deformation Path strain-(rate) driven simulations
module dmcADP
    use base_defs
    use conversions
    use dmcBasicModule
    use file_io
    use altay
    use incrementation
    use logging
    use altay

    implicit none

    private
    public:: ADPModule

    character(*), parameter:: MOD_NAME = 'dmcADP'

    !> Arbitrary Strain Mode (extends DeformationDrivenModule by 4 procedures)
    type, extends(BasicModule):: ADPModule
        type(StrainDrivenStep),dimension(:),allocatable :: steps
    contains  ! type-bound procedures; pass(this) passes object itself, through which procedure referenced, as first argument to procedure
        procedure, pass(this):: readConfig => ADPModule_readConfig
        procedure, pass(this):: run => ADPModule_run
        procedure, pass(this):: fileOutput => ADPModule_fileOutput
    end type

    !> A strain-(rate) driven step
    type:: StrainDrivenStep
        real(DP), dimension(3, 3):: velocity_gradient
        real(DP):: target_strain
        type(StrainIncrement), dimension(:), allocatable:: increments
    end type

contains

    !> Read configuration from IO unit (type-bound function)
    subroutine ADPModule_readConfig(this, cnfunit) ! call with 1 argument (cnfunit) when referenced through object
        class(ADPModule), intent(inout)   :: this !< passed implicitly
        integer, intent(in)              :: cnfunit !< IO input unit; pass explicitly

        integer:: n_steps, i

        ! Read generic configuration section
        call this%BasicModule%readConfig(cnfunit)

        !Read the module-specific config
        if (.not. readValue(cnfunit, n_steps)) return

        if (n_steps < 1) then
            return
        end if

        allocate(this%steps(n_steps))

        do i = 1, n_steps
            associate (step => this%steps(i))
                if (.not. readValue(cnfunit, step%velocity_gradient)) return
                if (math_trace33(step%velocity_gradient) > TOLERANCE) &
                    call log_error(MOD_NAME, 'adpmodule_readconfig', ERR_VAL, 'Volumetric deformation is not alowed.')
                if (.not. readValue(cnfunit, step%target_strain)) return
            end associate
        end do
    end subroutine

    !> Run the simulation
    subroutine ADPModule_run(this, info)
        class(ADPModule), intent(inout)  :: this
        integer, intent(out)             :: info

        integer:: iounit, &
                  i_step, i_inc, &
                  n_incs
        real(DP):: v_grad_inc(3,3), &      !! Velocity gradient for an increment. Assumed time step of 1s
                   taylor_factor, &
                   dev_stress(5)

        !Run the simulation
        info = VEF_ERROR

        if (.not. allocated(this%steps)) &
            call log_error(MOD_NAME, 'run', ERR_VAL, 'Steps array not initialized')

        ! Main loop over the steps
        do i_step = 1, size(this%steps)
            associate(step => this%steps(i_step))
                if (step%target_strain == 0._DP) then
                    allocate(step%increments(1))
                    call altay_simulate_strain_mode(this%material, tensor_to_deviatoric(step%velocity_gradient), dev_stress)
                    step%increments(1)%stress = deviatoric_to_tensor(dev_stress)
                else
                    call altay_strain_driven_deformation(this%material, step%velocity_gradient, step%target_strain, step%increments)
                end if
            end associate
        enddo
        info = this%fileOutput()
    end subroutine

    !> Write out results to the output file
    integer function ADPModule_fileOutput(this) result(info)
        class(ADPModule), intent(in):: this

        integer:: step, &
                  increment, &
                  ierr, &
                  iounit
        integer, parameter:: ncolumn_labels = 20, column_width = 18
        character(len = column_width), dimension(ncolumn_labels):: column_names = [character(len = column_width) :: &
        'step', 'increment', 'duration', 'eps_vm', &
        'L_11','L_21','L_31','L_12','L_22','L_32','L_13','L_23','L_33',  &
        'S_11','S_22','S_33','S_23','S_13','S_12', &
        'M']

        call write_texture(this%material%clusters)

        ! Open output file
        info = this%openOutputFile('.adp',iounit)
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, 'fileoutput', ERR_IO, 'Could not open output file.')

        ! Write column labels
        call write_standard_header(iounit, column_names)

        ! Write the data
        do step = 1, size(this%steps)
            associate (stp => this%steps(step))
                do increment = 1, size(stp%increments)
                    associate (inc => stp%increments(increment))
                        write(iounit, fmt = 710, iostat = ierr) &
                            step, increment, &            ! 2 fields
                            inc%duration, &
                            inc%vm_strain, &
                            stp%velocity_gradient, &
                            tensor_to_unscaled_voigt(inc%stress), &         ! 6 fields: deviatoric stress
                            inc%taylor_factor
                        if (ierr /= 0) return
                    end associate
                enddo
            end associate
        enddo
        info = VEF_OK

        ! Formats for the output file
        710 format(1X, 2(I18, 1X), 18(ES18.9E3, 1X))
    end function
end module
