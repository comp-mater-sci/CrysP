!> Top-level libCrysP Module
!>
!> Provides inerface to callers and formats data to be used in underlying modules.
!> Only module in libCrysP where all procedures are guaranteed to sanitize their input.
module libcrysp
    use iso_c_binding
    use base_defs
    use conversions
    use macro
    use meso
    use micro
    use logging
    use crysp_grain
    use incrementation
    use crysp_serialization
    use crysp_material

    implicit none

    public

    character(*), parameter, private:: MOD_NAME = 'crysp'

contains

    !> Initialize a new material state.
    !>
    !> Initializes all data structures associated to a meterial state and assembles them into a Material object.
    !> An initialized Material object is needed for all other calls to libCrysP
    !> Sanitization of the input parameters is propagated to lower-level initialization procedures
    subroutine crysp_new_material(phase_sizes, orientations, deformation_mechanisms, hardening_model_ids, hardening_params, meso_model_id, meso_params, material)
        integer(C_INT), dimension(:), intent(in):: phase_sizes !! Amount of grains belonging to each phase.
        real(C_DOUBLE), dimension(3,sum(phase_sizes)), intent(in):: orientations !! List of Euler angle triplets in bunge convention representing all grains.
        integer(C_INT), dimension(size(phase_sizes)), intent(in):: deformation_mechanisms !! ID of slip system family to use for each phase. Must
                                                                          !! exist in the list in [[micro]]

        integer(C_INT), dimension(size(phase_sizes)), intent(in):: hardening_model_ids !! ID of the hardening model of
                                                                                                  !! each phase, in the same order as deformation_mechanisms.
                                                                                                   !! Must exist in the list in [[micro]]
        type(Parameter), dimension(:), intent(in):: hardening_params !! list containing the parameter lists of the hardening model of each phase,
                                                                         !! in the smae order as deformation_mechanisms

        integer(C_INT), intent(in):: meso_model_id                       !! ID of the meso model. Must exist in the enum list in [[meso]]
        type(Parameter), dimension(:), intent(in):: meso_params !! List containing values for the parameters of the
                                                                      !!selected meso model. Order and types must correspond to meso_get_parameters(meso_model_id)

        type(Parameter), dimension(:), allocatable, intent(out):: material              !! The initialized material state

        type(Grain), allocatable:: grains_(:)


        call micro_init(phase_sizes, orientations, deformation_mechanisms, hardening_model_ids, hardening_params, material%phases, grains_)
        call meso_init(meso_model_id, grains_, meso_params, material%meso_model, material%clusters)
    end subroutine

    !> Apply a strain-driven deformation to a material
    subroutine crysp_strain_driven_deformation(mat, velocity_gradient, target_vm_strain, increments)
        type(Material), target, intent(inout):: mat
        real(DP), dimension(3,3), intent(in):: velocity_gradient !! Assumed not to contain volumetric component.
        real(DP), intent(in)::                 target_vm_strain  !! Total von mises equivalent true strain to be reached.
        type(StrainIncrement), dimension(:), allocatable, intent(out):: increments !! List of increments of the deformation.

        character(*), parameter:: PROC_NAME = 'crysp_strain_driven_deformation'

        if (abs(math_trace33(velocity_gradient)) > TOLERANCE) &
            call log_error(MOD_NAME, PROC_NAME, ERR_ARG, 'Volumetric deformation is not allowed.')
        if (target_vm_strain < TOLERANCE) &
            call log_error(MOD_NAME, PROC_NAME, ERR_ARG, 'Target von mises equivalent strain must be larger than 0.')

        call macro_strain_driven_deformation(mat, velocity_gradient, target_vm_strain, increments)
    end subroutine

    !> Apply a stress-driven deformtation to a material.
    subroutine crysp_stress_driven_deformation(mat, target_stress_mode, target_vm_strain, increments)
        type(Material), target, intent(inout):: mat
        real(DP), dimension(5), intent(in):: target_stress_mode
        real(DP), intent(in):: target_vm_strain
        type(StressIncrement), dimension(:), allocatable, intent(out):: increments

        character(*), parameter:: PROC_NAME = 'crysp_stress_driven_deformation'

        if (abs(norm2(target_stress_mode) - 1._DP) > TOLERANCE) &
            call log_error(MOD_NAME, PROC_NAME, ERR_ARG, 'Norm of stress mode must be 1.')
        if (target_vm_strain < TOLERANCE) &
            call log_error(MOD_NAME, PROC_NAME, ERR_ARG, 'Target von mises equivalent strain must be strictly positive.')

        call macro_stress_driven_deformation(mat, target_stress_mode, target_vm_strain, increments)
    end subroutine

    !> Calculate strain mode and stress corresponding to a desired stress mode.
    !>
    !> Uses iterative search to find an accurate match for the strain mode and stress state corresponding to the d  esired stress mode.
    !> An accurate initial guess for the strain mode should be provided to improve convergence and performance. The residual of the search is returned to provide
    !> an estimation of search accuracy.
    subroutine crysp_simulate_stress_mode(mat, target_stress_mode, strain_mode, stress, residual)
        type(Material), target, intent(inout):: mat !! Material state.
        real(DP), dimension(5), intent(in)::  target_stress_mode   !! Intended stress mode
        real(DP), dimension(5), intent(inout):: strain_mode !! On entry, contains an initial guess of the strain mode matching the
                                                            !! target stress mode. On exit, contains the actual strain mode.
        real(DP), dimension(5), intent(out):: stress        !! Actual stress state found.
        real(DP), dimension(5), intent(out):: residual      !! Residual of the the search.

        character(*), parameter:: PROC_NAME = 'crysp_simulate_stress_mode'

        if (abs(norm2(target_stress_mode) - 1._DP) > TOLERANCE) &
            call log_error(MOD_NAME, PROC_NAME, ERR_ARG, 'Norm of stress mode must be 1.')

        call macro_simulate_stress_mode(mat, target_stress_mode, strain_mode, stress, residual)
    end subroutine

    !> Calculate stress state corresponding to a given strain mode.
    subroutine crysp_simulate_strain_mode(mat, strain_mode, stress)
        type(Material), target, intent(inout):: mat !! Material state
        real(DP), dimension(5), intent(in)::    strain_mode !! Deviatoric strain mode
        real(DP), dimension(5), intent(out)::   stress      !! Stress state corresponding to the strain mode.

        call macro_simulate_strain_mode(mat, strain_mode, stress)
    end subroutine
end module
