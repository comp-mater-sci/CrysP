module altay
    use iso_c_binding
    use base_defs
    use conversions
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

        integer:: ierr, &
                  meso_model_id

        ierr = 0
        !Set the singleton object to the cnf
        acnf = cnf

        meso_model_id    = cnf%model_id

        !Get the initial texture
        orientations = read_texture(trim(cnf%texture_input_fname))

        params = meso_get_parameters(meso_model_id)
        if (meso_model_id == 2) then
            param_ptr  => params .find. "Boundaries"
            param_ptr = read_boundaries(cnf%micros_fname)
        end if

        ! Open output files
        call open_output_files(cnf, info)
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Cannot open output files.')

        !Even though the back-end logic can handle n phases, the current I/O structure only sopports 1 phase. Therefore, wrap the
        !description of this one phase in a phase descriptor and pass it as a 1-element list to micro_init
        phase_%model_id = cnf%hardening_model_id
        phase_%deformation_mechanism = cnf%deformation_mechanism
        phase_%parameters = cnf%hardening_parameters
        phase_%orientations = orientations

        !Initialize altay modules
        call micro_init([phase_], grains)
        call meso_init(meso_model_id, grains, params, clusters)
        call macro_init(clusters)

        info = VEF_OK
    end subroutine

    !> Finalizes the module and releases the resources.
    subroutine finalizeAltay(info)
        integer, intent(out)                 :: info     !< exit code (0 on success)

        call simulation_finalize()
        info = 0
    end subroutine

    !> Run the AlTay for the set of steps
    subroutine deformation_step(velocity_gradient, stress, taylor_factor)
        real(DP), dimension(3,3), intent(in):: velocity_gradient
        real(DP), dimension(3,3), intent(out):: stress
        real(DP), intent(out):: taylor_factor

        call simulation_run(velocity_gradient, stress, taylor_factor)
        if (acnf%nfile == 1) &
            call output_current_state()
    end subroutine

    function altay_get_stress_state(velocity_gradient) result(stress_state)
        real(DP), dimension(3, 3), intent(in):: velocity_gradient
        real(DP), dimension(3, 3):: stress_state

        stress_state = get_stress(velocity_gradient)
    end function

    !Result rounded to 9 digits
    subroutine altay_get_stress_state_c(strain_rate, stress_state) bind(C)
        real(C_DOUBLE), dimension(5), intent(in):: strain_rate
        real(C_DOUBLE), dimension(5), intent(out):: stress_state

        !Transpose both input and output because C is row major and Fortran column major.
        stress_state = anint(tensor_to_deviatoric(get_stress(deviatoric_to_tensor(strain_rate)))/TOLERANCE) * TOLERANCE
    end subroutine
end module
