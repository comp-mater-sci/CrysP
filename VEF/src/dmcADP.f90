!> Arbitrary Deformation Path strain-(rate) driven simulations
module dmcADP
    use base_defs
    use conversions
    use criConfigReader
    use dmcResultFileOutput
    use dmcStrainDrivenStep
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

    !> Outputs collected by the simulation run
    type:: ADPOutputData
        type(StepOutput), dimension(:), allocatable:: steps
    end type

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
        ! For the time being, only default solver configuration is accepted for this module.
        if (.not. default_solver_config) return

        ! Read the module-specific config
        if (.not. readValue(cnfunit, n_steps)) return
        !
        if (n_steps < 1) then
            info = VEF_ERROR
            return
        end if

        allocate(this%steps(n_steps))

        do i = 1, n_steps
            ! Read the step definition and convert it into
            !  StrainDrivenStep object step
            if (.not. readValue(cnfunit, tmp_deformation_rate)) return
            if (.not. readValue(cnfunit, step_size)) return

            if (norm2(tmp_deformation_rate) < TOLERANCE) &
                call log_error(MOD_NAME, 'readconfig', ERR_IO, 'Strain mode must not be 0')
            tmp_deformation_rate = tmp_deformation_rate/norm2(tmp_deformation_rate)*step_size

            if (.not. readValue(cnfunit, this%steps(i)%update_state)) return
            !
            this%steps(i)%velocity_gradient = tmp_deformation_rate
            this%steps(i)%output_state = this%output%outputRequest
        enddo
        info = VEF_OK
    end function

    !> Run the simulation
    subroutine ADPModule_run(this, info)
        class(ADPModule), intent(inout)  :: this
        integer, intent(out)             :: info
        !
        type(ADPOutputData):: output
        integer:: iounit, i_step, n_steps

        ! Open output file
        info = this%openOutputFile('.adp',iounit)
        if (info /= VEF_OK) return
        !
        ! Run the simulation
        info = VEF_ERROR
        if (allocated(this%steps)) then
            n_steps = size(this%steps)
        else
            return
        end if
        !
        ! Storage for the calculated output
        allocate(output%steps(n_steps))
        !
        ! Main loop over the steps
        do i_step = 1, n_steps
            associate(step => this%steps(i_step), &
                      step_output => output%steps(i_step))
                ! Execute the step
                info = step%execute(step_output)
                if (info /= VEF_OK) return
                ! Output the results
                info = this%fileOutput(iounit, output, header=(i_step == 1), step_id = i_step)
                if (info /= VEF_OK) return
            end associate
        enddo
    end subroutine

    !> Write out results to the output file
    integer function ADPModule_fileOutput(this, iounit, output, header, step_id) result(info)
        class(ADPModule), intent(in)                 :: this
        integer, intent(in)                          :: iounit !< Output IO unit
        type(ADPOutputData), intent(in), optional     :: output !< Data to be written out
        logical, intent(in), optional                 :: header !< Header to be written out
        integer, intent(in), optional                 :: step_id
        !
        integer:: step, increment, ierr, n_steps, first_step, last_step, n_increments
        real(DP):: l_voigt(6)
        !
        integer, parameter:: ncolumn_labels = 2+9+3*6+3+7, column_width = 18
        character(len = column_width), dimension(ncolumn_labels):: column_names = [character(len = column_width) :: &
        'step', 'increment', & ! 2 fields
        'L_11','L_21','L_31','L_12','L_22','L_32','L_13','L_23','L_33',  & ! 9 fields  (I)
        'D_11','D_22','D_33','D_23','D_13','D_12', & ! 6 fields  (I)
        'O_12','O_23','O_13', & ! 3 fields  (I)
        'A_11','A_22','A_33','A_23','A_13','A_12', & ! 6 fields  (I)
        'S_11','S_22','S_33','S_23','S_13','S_12', & ! 6 fields  (I)
        'eps_vM_begin', 'eps_vM_end', 'D_vM', 'S_vM', 'dW', 'M-factor', 'gamma' & ! 7 fields
        ]
        !
        info = VEF_ERROR
        if (optionalDefault(header, .false.)) then
            ! Write column numbers
            info = writeColumnNumbers(iounit, size(column_names), [column_width] )
            if (info /= VEF_OK) return
            ! Write column labels
            info = writeColumnNames(iounit, column_names, [column_width] )
            if (info /= VEF_OK) return
        endif
        !
        if (present(output)) then
            n_steps = 0
            if (allocated(output%steps)) &
                n_steps = size(output%steps)

            first_step = optionalDefault(step_id, 1)
            last_step = optionalDefault(step_id, n_steps)

            if (first_step < 1 .or. last_step > n_steps) then
                info = VEF_ERROR
                return
            end if
            !
            info = VEF_ERROR
            !
            ! Write the data
            do step = first_step, last_step
                associate(step_output => output%steps(step))
                    n_increments = 0
                    if (allocated(step_output%increments)) &
                        n_increments = size(step_output%increments)

                    do increment = 1, n_increments
                          associate(v => step_output%increments(increment))
                              l_voigt = tensor_to_unscaled_voigt(v%L)
                              write(iounit, fmt = 710, iostat = ierr) &
                                          step, increment, &            ! 2 fields
                                          v%L, &         ! 9 fields: velocity gradient
                                          tensor_to_unscaled_voigt(v%L), &         ! 6 fields: rate for deformation tensor (strain rate)
                                          tensor_to_spin(v%L), &         ! 3 fields: spin tensor
                                          normalize(tensor_to_unscaled_voigt(v%L)), &         ! 6 fields: strain mode
                                          tensor_to_unscaled_voigt(v%S), &         ! 6 fields: deviatoric stress
                                          v%vm_strain_begin, &
                                          v%vm_strain_end, &
                                          v%vMeqStrainRate, &
                                          v%vm_stress, &
                                          v%plastic_work_inc, &
                                          v%taylor_factor
                          end associate
                          if (ierr /= 0) return
                    enddo
                end associate
            enddo
            info = VEF_OK
        endif

        ! Formats for the output file
        710 format(1X, 2(I18, 1X), 38(ES18.9E3, 1X))
    !
    end function
end module
