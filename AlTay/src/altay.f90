!> Top-level AlTay Module
!>
!> Provides inerface to callers and formats data to be used in underlying modules.
!> Only module in AlTay where all procedures are guaranteed to sanitize their input.
module altay
    use iso_c_binding
    use base_defs
    use conversions
    use macro
    use micro
    use logging
    use parameters
    use grain_module
    use parameters
    use meso
    use incrementation

    implicit none

    private
    public:: altay_init, &
             finalizeAltay, &
             altay_strain_driven_deformation, &
             altay_stress_driven_deformation, &
             altay_simulate_stress_mode, &
             altay_simulate_strain_mode

    character(*), parameter:: MOD_NAME = 'altay'

contains

    !> Initialize AlTay.
    !>
    !> This subroutine must be called prior to any call to other module subroutines.
    !> Sanitization of the input parameters is propagated to lower-level initialization procedures
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

        call macro_finalize()
        info = 0
    end subroutine

    !> Run the AlTay for the set of steps
    subroutine altay_strain_driven_deformation(velocity_gradient, target_vm_strain, increments)
        real(DP), dimension(3,3), intent(in):: velocity_gradient !! Assumed not to contain volumetric component.
        real(DP), intent(in)::                 target_vm_strain  !! Total von mises equivalent true strain to be reached.
        type(StrainIncrement), dimension(:), allocatable, intent(out):: increments !! List of increments of the deformation.

        character(*), parameter:: PROC_NAME = 'altay_strain_driven_deformation'

        if (abs(math_trace33(velocity_gradient)) > TOLERANCE) &
            call log_error(MOD_NAME, PROC_NAME, ERR_ARG, 'Volumetric deformation is not allowed.')
        if (target_vm_strain < TOLERANCE) &
            call log_error(MOD_NAME, PROC_NAME, ERR_ARG, 'Target von mises equivalent strain must be larger than 0.')

        call macro_deform(velocity_gradient, target_vm_strain, increments)
    end subroutine

    subroutine altay_stress_driven_deformation(target_stress_mode, target_vm_strain, increments)
        real(DP), dimension(5), intent(in):: target_stress_mode
        real(DP), intent(in):: target_vm_strain
        type(StressIncrement), dimension(:), allocatable, intent(out):: increments

        character(*), parameter:: PROC_NAME = 'altay_stress_driven_deformation'

        if (abs(norm2(target_stress_mode) - 1._DP) > TOLERANCE) &
            call log_error(MOD_NAME, PROC_NAME, ERR_ARG, 'Norm of stress mode must be 1.')
        if (target_vm_strain < TOLERANCE) &
            call log_error(MOD_NAME, PROC_NAME, ERR_ARG, 'Target von mises equivalent strain must be larger tham 0.')

        call macro_stress_driven_deformation(target_stress_mode, &
                                             target_vm_strain, &
                                             increments)
    end subroutine

    !> Calculate strain mode and stress corresponding to a desired stress mode.
    !>
    !> Uses iterative search to find an accurate match for the strain mode and stress state corresponding to the d  esired stress mode.
    !> An accurate initial guess for the strain mode should be provided to improve convergence and performance. The residual of the search is returned to provide
    !> an estimation of search accuracy.
    subroutine altay_simulate_stress_mode(target_stress_mode, strain_mode, stress, residual)
        real(DP), dimension(5), intent(in)::  target_stress_mode   !! Intended stress mode
        real(DP), dimension(5), intent(inout):: strain_mode !! On entry, contains an initial guess of the strain mode matching the
                                                            !! target stress mode. On exit, contains the actual strain mode.
        real(DP), dimension(5), intent(out):: stress        !! Actual stress state found.
        real(DP), dimension(5), intent(out):: residual      !! Residual of the the search.

        character(*), parameter:: PROC_NAME = 'altay_simulate_stress_mode'

        if (abs(norm2(target_stress_mode) - 1._DP) > TOLERANCE) &
            call log_error(MOD_NAME, PROC_NAME, ERR_ARG, 'Norm of stress mode must be 1.')

        call macro_simulate_stress_mode(target_stress_mode, &
                                        strain_mode, &
                                        stress, &
                                        residual)
    end subroutine

    !> Calculate stress state corresponding to a given strain mode.
    subroutine altay_simulate_strain_mode(strain_mode, stress)
        real(DP), dimension(5), intent(in)::    strain_mode !! Deviatoric strain mode
        real(DP), dimension(5), intent(out)::   stress      !! Stress state corresponding to the strain mode.

        call macro_simulate_strain_mode(strain_mode, stress)
    end subroutine
end module
