!> Arbitrary Deformation Path strain-(rate) driven simulations
module dmcADP
    use base_defs
    use conversions
    use criConfigReader
    use dmcResultFileOutput
    use dmcBasicModule

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


    type:: Increment
        real(DP), dimension(3,3):: velocity_gradient
        real(DP), dimension(3,3):: stress
        real(DP)::                 taylor_factor
    end type

    !> A strain-(rate) driven step
    type:: StrainDrivenStep
        real(DP), dimension(3, 3):: velocity_gradient
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
        real(DP):: tmp_strain(6), &
                   step_size, &
                   tmp_deformation_rate(3, 3)
        logical :: default_solver_config

        ! Read generic configuration section (output settings, AlTay (texture, microstructure, hardening), solver settings
        ! read output and AlTay configuration sections
        info = this%BasicModule%readConfig(cnfunit)
        ! Read "solver config flag" that belongs to the global section
        ! as it is done in the stressDrivenModule.
        info = VEF_ERROR
        if (.not. readValue(cnfunit, default_solver_config)) return
        !For the time being, only default solver configuration is accepted for this module.
        if (.not. default_solver_config) return

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
            if (.not. readValue(cnfunit, tmp_deformation_rate)) return
            if (.not. readValue(cnfunit, step_size)) return

            if (norm2(tmp_deformation_rate) < TOLERANCE) &
                call log_error(MOD_NAME, 'readconfig', ERR_IO, 'Strain mode must not be 0')
            tmp_deformation_rate = tmp_deformation_rate/norm2(tmp_deformation_rate)*step_size

            this%steps(i)%velocity_gradient = tmp_deformation_rate
        enddo
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
                ! Execute the step
                n_incs = ceiling(norm2(step%velocity_gradient)/ ACCURACY)
                v_grad_inc = step%velocity_gradient / n_incs

                allocate(step%increments(n_incs))
                do i_inc=1,n_incs
                    step%increments(i_inc)%velocity_gradient = v_grad_inc
                    call deformation_step(v_grad_inc, step%increments(i_inc)%stress, step%increments(i_inc)%taylor_factor)
                end do
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
        real(DP):: l_voigt(6), &
                   von_mises_strain_rate

        integer, parameter:: ncolumn_labels = 2+9+3*6+3+6, column_width = 18
        character(len = column_width), dimension(ncolumn_labels):: column_names = [character(len = column_width) :: &
        'step', 'increment', & ! 2 fields
        'L_11','L_21','L_31','L_12','L_22','L_32','L_13','L_23','L_33',  & ! 9 fields  (I)
        'D_11','D_22','D_33','D_23','D_13','D_12', & ! 6 fields  (I)
        'O_12','O_23','O_13', & ! 3 fields  (I)
        'A_11','A_22','A_33','A_23','A_13','A_12', & ! 6 fields  (I)
        'S_11','S_22','S_33','S_23','S_13','S_12', & ! 6 fields  (I)
        'eps_vM_begin', 'eps_vM_end', 'D_vM', 'S_vM', 'dW', 'M' & ! 6 fields
        ]

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
            do increment = 1, size(this%steps(step)%increments)
                  associate(v => this%steps(step)%increments(increment))
                      von_mises_strain_rate = strain_tensor_to_von_mises(v%velocity_gradient)  !Small strain assumption

                      l_voigt = tensor_to_unscaled_voigt(v%velocity_gradient)
                      write(iounit, fmt = 710, iostat = ierr) &
                          step, increment, &            ! 2 fields
                          v%velocity_gradient, &         ! 9 fields: velocity gradient
                          tensor_to_unscaled_voigt(v%velocity_gradient), &         ! 6 fields: rate for deformation tensor (strain rate)
                          tensor_to_spin(v%velocity_gradient), &         ! 3 fields: spin tensor
                          normalize(tensor_to_unscaled_voigt(v%velocity_gradient)), &         ! 6 fields: strain mode
                          tensor_to_unscaled_voigt(v%stress), &         ! 6 fields: deviatoric stress
                          acc_von_mises_strain, &
                          acc_von_mises_strain + von_mises_strain_rate, &
                          von_mises_strain_rate, &
                          sqrt(3._DP/2._DP)*norm2(v%stress), &
                          v%velocity_gradient .dot. v%stress, &
                          v%taylor_factor

                      acc_von_mises_strain = acc_von_mises_strain + von_mises_strain_rate !Assumes 1s time step
                  end associate
                  if (ierr /= 0) return
            enddo
        enddo
        info = VEF_OK

        ! Formats for the output file
        710 format(1X, 2(I18, 1X), 38(ES18.9E3, 1X))
    end function
end module
