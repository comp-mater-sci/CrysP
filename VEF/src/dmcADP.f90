!> Arbitrary Deformation Path strain-(rate) driven simulations
module dmcADP
    use base_defs
    use conversions
    use dmcResultFileOutput
    use dmcBasicModule
    use file_io

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
        type(Increment), dimension(:), allocatable:: increments
    end type

    real(DP):: acc_von_mises_strain

contains

    !> Read configuration from IO unit (type-bound function)
    integer function ADPModule_readConfig(this, cnfunit) result(info)  ! call with 1 argument (cnfunit) when referenced through object
        class(ADPModule), intent(inout)   :: this !< passed implicitly
        integer, intent(in)              :: cnfunit !< IO input unit; pass explicitly

        integer, parameter:: n_deformation_types = 3
        integer, parameter:: deformation_id = 1, strainmode_id = 2, strain_id = 3
        integer:: n_steps, ierr, i
        real(DP):: tmp_strain(6)
        logical :: default_solver_config

        ! Read generic configuration section (output settings, AlTay (texture, microstructure, hardening), solver settings
        ! read output and AlTay configuration sections
        info = this%BasicModule%readConfig(cnfunit)
        ! Read "solver config flag" that belongs to the global section
        ! as it is done in the stressDrivenModule.
        info = VEF_ERROR

        !Read the module-specific config
        if (.not. readValue(cnfunit, n_steps)) return

        if (n_steps < 1) then
            info = VEF_ERROR
            return
        end if

        allocate(this%steps(n_steps))

        do i = 1, n_steps
            ! Read the step definition and convert it into
            ! StrainDrivenStep object step
            if (.not. readValue(cnfunit, this%steps(i)%velocity_gradient)) return
            if (.not. readValue(cnfunit, this%steps(i)%target_strain)) return
        end do
        info = VEF_OK
    end function

    !> Run the simulation
    subroutine ADPModule_run(this, info)
        class(ADPModule), intent(inout)  :: this
        integer, intent(out)             :: info
        !
        integer:: iounit, &
                  i_step, i_inc, &
                  n_incs
        real(DP):: v_grad_inc(3,3), &      !! Velocity gradient for an increment. Assumed time step of 1s
                   taylor_factor

        !Run the simulation
        info = VEF_ERROR

        if (.not. allocated(this%steps)) &
            call log_error(MOD_NAME, 'run', ERR_VAL, 'Steps array not initialized')

        ! Main loop over the steps
        acc_von_mises_strain = 0._DP
        do i_step = 1, size(this%steps)
            associate(step => this%steps(i_step))
                call altay_deform(step%velocity_gradient, step%target_strain, step%increments)
                if (this%altay%nfile /=0) &
                    call cur_write_block()
            end associate
        enddo
        info = this%fileOutput()
    end subroutine

    !> Write out results to the output file
    integer function ADPModule_fileOutput(this) result(info)
        class(ADPModule), intent(in)                 :: this

        integer:: step, &
                  increment, &
                  ierr, &
                  iounit
        integer, parameter:: ncolumn_labels = 21, column_width = 18
        character(len = column_width), dimension(ncolumn_labels):: column_names = [character(len = column_width) :: &
        'step', 'increment', 'duration', 'vm_strain', &
        'L_11','L_21','L_31','L_12','L_22','L_32','L_13','L_23','L_33',  &
        'S_11','S_22','S_33','S_23','S_13','S_12', &
        'M']

        info = VEF_ERROR

        ! Open output file
        info = this%openOutputFile('.adp',iounit)
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, 'fileoutput', ERR_IO, 'Could not open output file.')

        ! Write column numbers
        info = writeColumnNumbers(iounit, size(column_names), [column_width] )
        if (info /= VEF_OK) return
        ! Write column labels
        info = writeColumnNames(iounit, column_names, [column_width] )
        if (info /= VEF_OK) return

        ! Write the data
        do step = 1, size(this%steps)
            associate (stp => this%steps(step))
                do increment = 1, size(stp%increments)
                    associate (inc => stp%increments(increment))
                        write(iounit, fmt = 710, iostat = ierr) &
                            step, increment, &            ! 2 fields
                            inc%duration, &
                            inc%vm_strain, &
                            step%velocity_gradient
                            tensor_to_unscaled_voigt(inc%stress), &         ! 6 fields: deviatoric stress
                            inc%taylor_factor
                        if (ierr /= 0) return
                    end associate
                enddo
            end associate
        enddo
        info = VEF_OK

        ! Formats for the output file
        710 format(1X, 2(I18, 1X), 19(ES18.9E3, 1X))
    end function
end module
