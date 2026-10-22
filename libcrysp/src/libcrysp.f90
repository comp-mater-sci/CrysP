!> Libcrysp Fortran interface.
!>
!> Provides interface to callers and formats data to be used in underlying modules.
!> Only module in libcrysp where all procedures are guaranteed to sanitize their input.
!>
!> Every entry point returns a [[crysp_status:Status]] by value and never terminates the process: libCrysP may be embedded in a
!> long-lived host such as a web server, where a bad request must not take the host down with it. A caller checks `stat%code`
!> against `CRYSP_OK` and, on failure, reads `mod_name`, `proc_name` and `message` for the detail.
module libcrysp
    use iso_c_binding
    use base_defs
    use conversions
    use macro
    use meso
    use crysp_micro
    use crysp_status
    use crysp_grain
    use incrementation
    use crysp_parameters
    use crysp_material

    implicit none

    public

    character(*), parameter, private:: MOD_NAME = 'crysp'

contains

    !> List the mesoscopic models libCrysP supports.
    !>
    !> One descriptor per model. The IDs are those accepted as `meso_model_id` by [[crysp_new_material]].
    function crysp_get_meso_models(model_descs) result(stat)
        type(ModelDescriptor), dimension(:), allocatable, intent(out):: model_descs   !! Descriptor for each supported meso model
        type(Status):: stat                                                           !! Return status. Should always be OK

        integer:: i
        class(MesoModel), allocatable:: model

        allocate(model_descs(size(SUPPORTED_MESO_MODELS)))

        do i = 1, size(SUPPORTED_MESO_MODELS)
            stat = meso_get_model(SUPPORTED_MESO_MODELS(i), model)
            if (failed(stat)) return

            model_descs(i) = model%describe()
        end do
    end function

    !> List the microscopic (hardening) models libCrysP supports.
    !>
    !> One descriptor per model. The IDs are those accepted as `hardening_model_ids` by [[crysp_new_material]].
    function crysp_get_micro_models(model_descs) result(stat)
        type(ModelDescriptor), dimension(:), allocatable, intent(out):: model_descs   !! Descriptor for each supported micro model
        type(Status):: stat                                                           !! Return status. Should always be OK

        integer:: i
        class(ConstitutiveModel), allocatable:: model

        allocate(model_descs(size(SUPPORTED_HARDENING_MODELS)))

        do i = 1, size(SUPPORTED_HARDENING_MODELS)
            stat = micro_get_model(SUPPORTED_HARDENING_MODELS(i), model)
            if (failed(stat)) return

            model_descs(i) = model%describe()
        end do
    end function

    !> Initialize a new material state.
    !>
    !> Initializes all data structures associated to a material state and assembles them into a Material object.
    !> An initialized Material object is needed for all other calls to libCrysP
    !> Sanitization of the input parameters is propagated to lower-level initialization procedures
    function crysp_new_material(meso_data, phase_data, mat) result(stat)
        type(ModelData):: meso_data
        type(PhaseData), dimension(:), intent(in):: phase_data
        type(Material), target, intent(out):: mat
        type(Status):: stat

        type(Grain), allocatable:: grains_(:)

        stat =  micro_init(phase_data, mat%phases, grains_)
        if (.not. failed(stat)) stat = meso_init(meso_data, grains_, mat%meso_model, mat%clusters)
    end function


    !> Deform a material according to a prescribed velocity gradient.
    !>
    !> The material has to be pre-initialized via crysp_new_material
    !> The velocity gradient must not contain a volumetric component (sum(trace) == )
    !> The velocity gradient is applied until a given von-mises equivalent true strain is reached.
    !> The incrementation process is iterative, so a deviation of the target strain level in the order of 10^-9 is expected.
    function crysp_strain_driven_deformation(mat, velocity_gradient, target_vm_strain, increments) result(stat)
        type(Material), target, intent(inout):: mat
        real(DP), dimension(3,3), intent(in):: velocity_gradient !! Assumed not to contain volumetric component.
        real(DP), intent(in)::                 target_vm_strain  !! Total von mises equivalent true strain to be reached.
        type(StrainIncrement), dimension(:), allocatable, intent(out):: increments !! Reporting of individual deformation steps
                                                                                       !! that were applied. See
                                                                                       !! [[StrainIncrementData]] for details.
        type(Status):: stat                                                        !! `CRYSP_OK`, `CRYSP_ERR_ARG` for a rejected input,
                                                                                   !! or a failure raised during the simulation.

        character(*), parameter:: PROC_NAME = 'crysp_strain_driven_deformation'

        if (abs(math_trace33(velocity_gradient)) > TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Volumetric deformation is not allowed.')
            return
        end if
        if (target_vm_strain < TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Target von mises equivalent strain must be larger than 0.')
            return
        end if

        stat = macro_strain_driven_deformation(mat, velocity_gradient, target_vm_strain, increments)
    end function

    !> Deform a material based on a stress mode.
    !>
    !> The material has to be pre-initialized via crysp_new_material
    !> The stress mode is applied until a given von-mises equivalent true strain is reached.
    !> The stress mode is iteratively matched to the appropriate strain mode. This process is inexact, so a deviation of 10^-2
    !> between the requested stress mode and the actually applied stress mode is expected.
    !> The incrementation process is iterative, so a deviation of the target strain level in the order of 10^-9 is expected.
    function crysp_stress_driven_deformation(mat, target_stress_mode, target_vm_strain, increments) result(stat)
        type(Material), target, intent(inout):: mat                                            !! Initialized material
        real(DP), dimension(5), intent(in):: target_stress_mode                     !! Norm must be 1
        real(DP), intent(in):: target_vm_strain                                     !! Target true von mises strain
        type(StressIncrement), dimension(:), allocatable, intent(out):: increments !! Step-wise reporting of the mapping
                                                                                          !! between requested stress mode and strain
                                                                                          !! mode. See [[StressIncrementData]].
        type(Status):: stat                                                               !! `CRYSP_OK`, `CRYSP_ERR_ARG` for a rejected
                                                                                          !! input, or a simulation failure.

        character(*), parameter:: PROC_NAME = 'crysp_stress_driven_deformation'


        if (abs(norm2(target_stress_mode) - 1._DP) > TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Norm of stress mode must be 1.')
            return
        end if
        if (target_vm_strain < TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Target von mises equivalent strain must be strictly positive.')
            return
        end if

        stat = macro_stress_driven_deformation(mat, target_stress_mode, target_vm_strain, increments)
    end function

    !> Calculate strain mode and stress corresponding to a desired stress mode.
    !>
    !> The material has to be pre-initialized via crysp_new_material
    !> Uses iterative search to find an accurate match for the strain mode and stress state corresponding to the desired stress mode.
    !> An accurate initial guess for the strain mode should be provided to improve convergence and performance. The residual of the search is returned to provide
    !> an estimation of search accuracy.
    function crysp_simulate_stress_mode(mat, target_stress_mode, strain_mode, stress, residual) result(stat)
        type(Material), target, intent(inout):: mat                          !! Material state.
        real(DP), dimension(5), intent(in)::  target_stress_mode   !! Intended stress mode
        real(DP), dimension(5), intent(inout):: strain_mode !! On entry, contains an initial guess of the strain mode matching the
                                                            !! target stress mode. On exit, contains the actual strain mode.
        real(DP), dimension(5), intent(out):: stress        !! Actual stress state found.
        real(DP), dimension(5), intent(out):: residual      !! Residual of the the search.
        type(Status):: stat                                       !! `CRYSP_OK`, `CRYSP_ERR_ARG` for a rejected input, or a
                                                                  !! simulation failure.

        character(*), parameter:: PROC_NAME = 'crysp_simulate_stress_mode'

        if (abs(norm2(target_stress_mode) - 1._DP) > TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Norm of stress mode must be 1.')
            return
        end if

        stat = macro_simulate_stress_mode(mat, target_stress_mode, strain_mode, stress, residual)
    end function

    !> Calculate stress state corresponding to a given strain mode.
    function crysp_simulate_strain_mode(mat, strain_mode, stress) result(stat)
        type(Material), target, intent(inout):: mat                          !! Material state
        real(DP), dimension(5), intent(in)::    strain_mode !! Deviatoric strain mode
        real(DP), dimension(5), intent(out)::   stress      !! Stress state corresponding to the strain mode.
        type(Status):: stat                                       !! `CRYSP_OK`, or a failure raised during the simulation.

        stat = macro_simulate_strain_mode(mat, strain_mode, stress)
    end function
end module
