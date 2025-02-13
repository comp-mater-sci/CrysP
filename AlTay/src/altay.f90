module altay
    use simulation
    use altayConfig
    use micro
    use logging
    use parameters
    use grain_module
    use file_io
    use parameters
    use meso

    implicit none

    character(*), parameter, private:: MOD_NAME = 'altay'

contains

    !>@Brief Initialize AlTay.
    !>@Details This subroutine must be called prior to any call to other module subroutines.
    subroutine initAltay(cnf, info)
        type(altayConfigData), target, intent(inout)    :: cnf      !< configuration data
        integer, intent(out)                 :: info     !< exit code (altay_OK on success)
        character(*), parameter:: PROC_NAME = 'initAltay'
        real(DP), dimension(:,:), allocatable:: orientations
        type(Parameter), dimension(:), allocatable, target:: params
        type(Grain), dimension(:), allocatable:: grains
        class(Cluster), dimension(:), allocatable:: clusters
        type(Parameter), pointer:: param_ptr
        type(PhaseDescriptor):: phase_

        integer:: ierr, cluster_size

        ierr = 0
        !Set the singleton object to the cnf
        acnf = cnf

        cluster_size    = cnf%simul_init%NGR

        !Get the initial texture
        orientations = read_texture(trim(cnf%texture_input_fname))

        params = meso_get_parameters(cluster_size)
        if (cluster_size == 2) then
            param_ptr  => params .find. "Boundaries"
            param_ptr = read_boundaries(cnf%micros_fname)
        end if

        ! Open output files
        call open_output_files(cnf, info)
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Cannot open output files.')

        !Initialize altay modules
        phase_%model_id = cnf%hardening_model_id
        phase_%deformation_mechanism = cnf%deformation_mechanism
        phase_%parameters = cnf%hardening_parameters
        phase_%orientations = orientations

        call micro_init([phase_], grains)
        call meso_init(cnf%simul_init%ngr, grains, params, clusters)
        call macro_init(clusters)

        info = VEF_OK
    end subroutine

    !> Finalizes the module and releases the resources.
    subroutine finalizeAltay(info)
        integer, intent(out)                 :: info     !< exit code (0 on success)

        ! Close all units.
        close(IMP5)
        call simulation_finalize()
        if (allocated(astate%simulCalls)) then
              deallocate(astate%simulCalls)
              astate%nSimulCalls = 0
        endif
        info = 0
    end subroutine

    !> Initialization of input and output data for the steps.
    subroutine initStepData(nsteps, steps, info)
        integer, intent(in)                  :: nsteps   !< Number of steps to be created
        type(altayStateData), intent(out)    :: steps    !< Definiton of the steps.
        integer, intent(out)                 :: info     !< Exit code: 0 on success
        integer:: ierr

        info = 1
        if (nsteps <= 0) return
        allocate(steps%simulCalls(nsteps), stat = ierr)
        steps%nSimulCalls = nsteps
        steps%this = 0
        ! No need to specifically initialize other components,
        ! since there are initializers provided in the datatype.
        info = ierr
    end subroutine

    !> Run the AlTay for the set of steps
    subroutine runSteps(steps, info)
        type(altayStateData), intent(inout)        :: steps !< Definiton of the steps.
        integer, intent(out)                       :: info  !< Exit code: 0 on success.
        integer:: i
        logical:: input_ok
        real(DP):: velocity_gradient(3, 3)

        ! Validate input
        info = VEF_ERROR
        input_ok = .false.
        if (allocated(steps%simulCalls)) input_ok = (size(steps%simulCalls) == steps%nSimulCalls)
        if (.not. input_ok) return
        ! Assign steps with astate
        astate = steps

        do i = 1, steps%nSimulCalls
            steps%this = i
            velocity_gradient = steps%simulcalls(i)%input%dgf

            call simulation_run(velocity_gradient)

            if (steps%simulCalls(i)%input%do_output_final) call outputCurrentState(info)
        enddo

        info = VEF_OK
    end subroutine

    function altay_get_stress_state(velocity_gradient) result(stress_state)
        real(DP), dimension(3, 3), intent(in):: velocity_gradient
        real(DP), dimension(3, 3):: stress_state

        stress_state = get_stress(velocity_gradient)
    end function

    !> Write out the current state variables.
    !> The call may involve IO units: IMP1 (CUR file)
    !> Appropriate control fields in acnf%output_config are checked to decide if the data have to
    !> be actually written to corresponding IO units.
    subroutine outputCurrentState(info)
        integer, intent(out)           :: info

        info = VEF_OK
        if (acnf%output_config%nfile == 1) call output_current_state(IMP1)
        if (info /= 0) return
    end subroutine
end module
