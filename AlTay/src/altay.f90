!> Top-level AlTay Module
!>
!> Provides inerface to callers and formats data to be used in underlying modules.
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
    use deformation
    use incrementation

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

        call macro_finalize()
        info = 0
    end subroutine

    !> Run the AlTay for the set of steps
    subroutine altay_deform(velocity_gradient, total_strain, increments)
        real(DP), dimension(3,3), intent(in):: velocity_gradient !! Assumed not to contain volumetric component.
        real(DP), intent(in)::                 total_strain      !! Total von mises equivalent true strain to be reached.
        type(StrainIncrement), dimension(:), allocatable, intent(out):: increments !! List of increments of the deformation.

        call macro_deform(velocity_gradient, total_strain, increments)
    end subroutine

    !> Calculate strain mode and stress corresponding to a desired stress mode.
    !>
    !> Uses iterative search to find an accurate match for the strain mode and stress state corresponding to the d  esired stress mode.
    !> An accurate initial guess for the strain mode should be provided to improve convergence and performance. The residual of the search is returned to provide
    !> an estimation of search accuracy.
    subroutine altay_simulate_stress_mode(stress_mode, strain_mode, stress, residual)
        real(DP), dimension(5), intent(in)::  stress_mode   !! Intended stress mode
        real(DP), dimension(5), intent(inout):: strain_mode !! On entry, contains an initial guess of the strain m  ode matching the
                                                            !! target stress mode. On exit, contains the actual st  rain mode.
        real(DP), dimension(5), intent(out):: stress        !! Actual stress state found.
        real(DP), dimension(5), intent(out):: residual      !! Residual of the the search.

        call macro_simulate_stress_mode(stress_mode, strain_mode, stress, residual)
    end subroutine

    !> Calculate stress state corresponding to a given strain mode.
    subroutine altay_simulate_strain_mode(strain_mode, stress)
        real(DP), dimension(5), intent(in)::    strain_mode !! Deviatoric strain mode
        real(DP), dimension(5), intent(out)::   stress      !! Stress state corresponding to the strain mode.

        call macro_simulate_strain_mode(strain_mode, stress)
    end subroutine
end module
