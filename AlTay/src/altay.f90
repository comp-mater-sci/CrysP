module altay
    use iso_c_binding
    use base_defs
    use conversions
    use simulation
    use micro
    use logging
    use parameters
    use grain_module
    use parameters
    use meso

    implicit none

contains

    !>@Brief Initialize AlTay.
    !>@Details This subroutine must be called prior to any call to other module subroutines.
    subroutine altay_init(meso_model_id, meso_params, phases)
        integer, intent(in):: meso_model_id
        type(Parameter), dimension(:), intent(in):: meso_params
        type(PhaseDescriptor), dimension(:), intent(in):: phases

        type(Grain), dimension(:), allocatable:: grains_
        class(Cluster), dimension(:), allocatable:: clusters_

        call micro_init(phases, grains_)
        call meso_init(meso_model_id, grains_, meso_params, clusters_)
        call macro_init(clusters_)
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
