!> Top-level libCrysP Module
!>
!> Provides inerface to callers and formats data to be used in underlying modules.
!> Only module in libCrysP where all procedures are guaranteed to sanitize their input.
!>
!> Every entry point returns a [[crysp_status:Status]] by value and never terminates the process: libCrysP may be embedded in a
!> long-lived host such as a web server, where a bad request must not take the host down with it. A caller checks `stat%code`
!> against `CRYSP_OK` and, on failure, reads the NUL-terminated `mod_name`, `proc_name` and `message` buffers for the detail.
module libcrysp
    use iso_c_binding
    use base_defs
    use conversions
    use macro
    use meso
    use micro
    use crysp_status
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
    !> Initializes all data structures associated to a material state and assembles them into a Material object.
    !> An initialized Material object is needed for all other calls to libCrysP
    !> Sanitization of the input parameters is propagated to lower-level initialization procedures
    function crysp_new_material(phase_sizes, orientations, deformation_mechanisms, hardening_model_ids, hardening_params, meso_model_id, meso_params, mat) bind(C) result(stat)
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

        type(Parameter), dimension(:), allocatable, intent(out):: mat                  !! The initialized material state
        type(Status):: stat                                                            !! `CRYSP_OK`, or a failure describing which
                                                                                       !! of the supplied IDs was rejected.

        type(Grain), allocatable:: grains_(:)
        type(Material), target:: mat_


        stat = micro_init(phase_sizes, orientations, deformation_mechanisms, hardening_model_ids, hardening_params, mat_%phases, grains_)
        if (failed(stat)) return

        stat = meso_init(meso_model_id, grains_, meso_params, mat_%meso_model, mat_%clusters)
        if (failed(stat)) return

        mat = mat_%serialize()
    end function

    !> Deform a material according to a prescribed velocity gradient.
    !>
    !> The material has to be pre-initialized via crysp_new_material
    !> The velocity gradient must not contain a volumetric component (sum(trace) == )
    !> The velocity gradient is applied until a given von-mises equivalent true strain is reached.
    !> The incrementation process is iterative, so a deviation of the target strain level in the order of 10^-9 is expected.
    function crysp_strain_driven_deformation(mat, velocity_gradient, target_vm_strain, increments) bind(C) result(stat)
        type(Parameter), dimension(:), allocatable, intent(inout):: mat
        real(C_DOUBLE), dimension(3,3), intent(in):: velocity_gradient !! Assumed not to contain volumetric component.
        real(C_DOUBLE), intent(in)::                 target_vm_strain  !! Total von mises equivalent true strain to be reached.
        type(StrainIncrement), dimension(:), allocatable, intent(out):: increments !! Reporting of individual deformation steps that
                                                                                   !! were applied. See [[strainIncrement]] for
                                                                                   !! details.
        type(Status):: stat                                                        !! `CRYSP_OK`, `CRYSP_ERR_ARG` for a rejected input,
                                                                                   !! or a failure raised during the simulation.

        character(*), parameter:: PROC_NAME = 'crysp_strain_driven_deformation'

        type(Material), target:: mat_

        if (abs(math_trace33(velocity_gradient)) > TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Volumetric deformation is not allowed.')
            return
        end if
        if (target_vm_strain < TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Target von mises equivalent strain must be larger than 0.')
            return
        end if

        call mat_%deserialize(mat)

        stat = macro_strain_driven_deformation(mat_, velocity_gradient, target_vm_strain, increments)
        if (failed(stat)) return

        mat = mat_%serialize()
    end function

    !> Deform a material based on a stress mode.
    !>
    !> The material has to be pre-initialized via crysp_new_material
    !> The stress mode is applied until a given von-mises equivalent true strain is reached.
    !> The stress mode is iteratively matched to the appropriate strain mode. This process is inexact, so a deviation of 10^-2
    !> between the requested stress mode and the actually applied stress mode is expected.
    !> The incrementation process is iterative, so a deviation of the target strain level in the order of 10^-9 is expected.
    function crysp_stress_driven_deformation(mat, target_stress_mode, target_vm_strain, stress_increments, strain_increments) bind(C) result(stat)
        type(Parameter), dimension(:), allocatable, intent(inout):: mat                   !! Initialized material
        real(C_DOUBLE), dimension(5), intent(in):: target_stress_mode                     !! Norm must be 1
        real(C_DOUBLE), intent(in):: target_vm_strain                                     !! Target true von mises strain
        type(StressIncrement), dimension(:), allocatable, intent(out):: stress_increments !! Step-wise reporting of the mapping
                                                                                          !! between requested stress mode and strain mode.
                                                                                          !! See [[stressincrement]] for details.
        type(StrainIncrement), dimension(:), allocatable, intent(out):: strain_increments !! Reporting of individual deformation steps that
                                                                                          !! were applied. See [[strainIncrement]] for
                                                                                          !! details.
        type(Status):: stat                                                               !! `CRYSP_OK`, `CRYSP_ERR_ARG` for a rejected
                                                                                          !! input, or a simulation failure.

        character(*), parameter:: PROC_NAME = 'crysp_stress_driven_deformation'

        type(Material), target:: mat_

        if (abs(norm2(target_stress_mode) - 1._DP) > TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Norm of stress mode must be 1.')
            return
        end if
        if (target_vm_strain < TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Target von mises equivalent strain must be strictly positive.')
            return
        end if

        call mat_%deserialize(mat)

        stat = macro_stress_driven_deformation(mat_, target_stress_mode, target_vm_strain, stress_increments, strain_increments)
        if (failed(stat)) return

        mat = mat_%serialize()
    end function

    !> Calculate strain mode and stress corresponding to a desired stress mode.
    !>
    !> The material has to be pre-initialized via crysp_new_material
    !> Uses iterative search to find an accurate match for the strain mode and stress state corresponding to the desired stress mode.
    !> An accurate initial guess for the strain mode should be provided to improve convergence and performance. The residual of the search is returned to provide
    !> an estimation of search accuracy.
    function crysp_simulate_stress_mode(mat, target_stress_mode, strain_mode, stress, residual) bind(C) result(stat)
        type(Parameter), dimension(:), allocatable, intent(inout):: mat !! Material state.
        real(C_DOUBLE), dimension(5), intent(in)::  target_stress_mode   !! Intended stress mode
        real(C_DOUBLE), dimension(5), intent(inout):: strain_mode !! On entry, contains an initial guess of the strain mode matching the
                                                            !! target stress mode. On exit, contains the actual strain mode.
        real(C_DOUBLE), dimension(5), intent(out):: stress        !! Actual stress state found.
        real(C_DOUBLE), dimension(5), intent(out):: residual      !! Residual of the the search.
        type(Status):: stat                                       !! `CRYSP_OK`, `CRYSP_ERR_ARG` for a rejected input, or a
                                                                  !! simulation failure.

        character(*), parameter:: PROC_NAME = 'crysp_simulate_stress_mode'

        type(Material), target:: mat_

        if (abs(norm2(target_stress_mode) - 1._DP) > TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Norm of stress mode must be 1.')
            return
        end if

        call mat_%deserialize(mat)

        stat = macro_simulate_stress_mode(mat_, target_stress_mode, strain_mode, stress, residual)
        if (failed(stat)) return

        mat = mat_%serialize()
    end function

    !> Calculate stress state corresponding to a given strain mode.
    function crysp_simulate_strain_mode(mat, strain_mode, stress) bind(C) result(stat)
        type(Parameter), dimension(:), allocatable, intent(inout):: mat !! Material state
        real(C_DOUBLE), dimension(5), intent(in)::    strain_mode !! Deviatoric strain mode
        real(C_DOUBLE), dimension(5), intent(out)::   stress      !! Stress state corresponding to the strain mode.
        type(Status):: stat                                       !! `CRYSP_OK`, or a failure raised during the simulation.

        type(Material), target:: mat_

        call mat_%deserialize(mat)

        stat = macro_simulate_strain_mode(mat_, strain_mode, stress)
        if (failed(stat)) return

        mat = mat_%serialize()
    end function
end module
