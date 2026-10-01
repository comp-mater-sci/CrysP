!> Libcrysp Fortran interface.
!>
!> Provides interface to callers and formats data to be used in underlying modules.
!> Only module in libcrysp where all procedures are guaranteed to sanitize their input.
!>
!> Every entry point that can fail returns a [[crysp_status:Status]] by value and never terminates the process: libCrysP may be embedded in a
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
    !> Largest angle (rad) between two stress modes for which the strain mode of one is a good initial guess for the other.
    real(DP), parameter, private:: MAX_GUESS_ANGLE = 0.1_DP

contains

    !> List the slip system sets libCrysP supports.
    !>
    !> The IDs are those accepted as `deformation_mechanism` by [[crysp_new_material]].
    function crysp_get_slip_systems() result(slip_system_descs)
        type(SlipSystemDescriptor), dimension(:), allocatable:: slip_system_descs !! Descriptor for each supported slip system set

        slip_system_descs = micro_get_slip_systems()
    end function

    !> List the mesoscopic models libCrysP supports.
    !>
    !> The IDs are those accepted as `meso_model_id` by [[crysp_new_material]].
    function crysp_get_meso_models() result(model_descs)
        type(ModelDescriptor), dimension(:), allocatable:: model_descs   !! Descriptor for each supported meso model

        integer:: i
        type(Status):: stat
        class(MesoModel), allocatable:: model

        allocate(model_descs(size(SUPPORTED_MESO_MODELS)))

        do i = 1, size(SUPPORTED_MESO_MODELS)
            stat = meso_get_model(SUPPORTED_MESO_MODELS(i), model)
            model_descs(i) = model%describe()
        end do
    end function

    !> List the microscopic (hardening) models libCrysP supports.
    !>
    !> The IDs are those accepted as `hardening_model_ids` by [[crysp_new_material]].
    function crysp_get_micro_models() result(model_descs)
        type(ModelDescriptor), dimension(:), allocatable:: model_descs   !! Descriptor for each supported micro model

        integer:: i
        type(Status):: stat
        class(ConstitutiveModel), allocatable:: model

        allocate(model_descs(size(SUPPORTED_HARDENING_MODELS)))

        do i = 1, size(SUPPORTED_HARDENING_MODELS)
            stat = micro_get_model(SUPPORTED_HARDENING_MODELS(i), model)
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
        type(Increment), dimension(:), allocatable, intent(out):: increments !! Reporting of individual deformation steps
                                                                             !! that were applied. See
                                                                             !! [[Increment]] for details.
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
        type(Increment), dimension(:), allocatable, intent(out):: increments !! Step-wise reporting of the mapping
                                                                             !! between requested stress mode and strain
                                                                             !! mode. See [[Increment]].
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

    !> Calculate strain modes and stresses corresponding to a set of desired stress modes.
    !>
    !> The material has to be pre-initialized via crysp_new_material
    !> Each mode is simulated using iterative search to find an accurate match for the strain mode and stress state corresponding
    !> to the desired stress mode. The residual of each search is returned to provide an estimation of search accuracy.
        !> All modes are validated before any is simulated. On a simulation failure, the outputs of the modes after the failing one are
    !> undefined.
    function crysp_simulate_stress_modes(mat, target_stress_modes, strain_modes, stresses, residuals) result(stat)
        type(Material), target, intent(inout):: mat              !! Material state.
        real(DP), dimension(:,:), intent(in)::  target_stress_modes  !! 5 x n: intended stress modes, each with norm 1.
        real(DP), dimension(:,:), intent(out):: strain_modes     !! 5 x n: strain modes found.
        real(DP), dimension(:,:), intent(out):: stresses         !! 5 x n: actual stress states found.
        real(DP), dimension(:,:), intent(out):: residuals        !! 5 x n: residuals of the searches.
        type(Status):: stat                                      !! `CRYSP_OK`, `CRYSP_ERR_DIMS` for mismatched shapes,
                                                                 !! `CRYSP_ERR_ARG` for a rejected input, or a simulation failure.

        character(*), parameter:: PROC_NAME = 'crysp_simulate_stress_modes'

        integer:: i, &
                   closest

        if (size(target_stress_modes, 1) /= 5 &
                .or. any(shape(strain_modes) /= shape(target_stress_modes)) &
                .or. any(shape(stresses) /= shape(target_stress_modes)) &
                .or. any(shape(residuals) /= shape(target_stress_modes))) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_DIMS, 'All mode arrays must have shape 5 x n.')
            return
        end if
        do i = 1, size(target_stress_modes, 2)
            if (abs(norm2(target_stress_modes(:,i)) - 1._DP) > TOLERANCE) then
                stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Norm of stress mode ' // int_to_str(i) // ' must be 1.')
                return
            end if
        end do

        do i = 1, size(target_stress_modes, 2)
            strain_modes(:,i) = target_stress_modes(:,i)
            ! From 2nd mode onwards, we can use the result of the closest previously calculated mode as initial guess, if it is
            ! closer than MAX_GUESS_ANGLE to the mode to be simulated.
            if (i > 1) then
                closest = maxloc(matmul(target_stress_modes(:,i), target_stress_modes(:,1:i-1)), 1)
                if (dot_product(target_stress_modes(:,i), target_stress_modes(:,closest)) >= cos(MAX_GUESS_ANGLE)) &
                    strain_modes(:,i) = strain_modes(:,closest)
            end if

            stat = macro_simulate_stress_mode(mat, target_stress_modes(:,i), strain_modes(:,i), stresses(:,i), residuals(:,i))
            if (failed(stat)) return
        end do
    end function

    !> Calculate the stress states corresponding to a set of strain modes.
    !>
    !> Each mode is simulated independently. On a simulation failure, the outputs of the modes after the failing one are undefined.
    function crysp_simulate_strain_modes(mat, strain_modes, stresses) result(stat)
        type(Material), target, intent(inout):: mat           !! Material state
        real(DP), dimension(:,:), intent(in)::  strain_modes  !! 5 x n: deviatoric strain modes.
        real(DP), dimension(:,:), intent(out):: stresses      !! 5 x n: stress state corresponding to each strain mode.
        type(Status):: stat                                   !! `CRYSP_OK`, `CRYSP_ERR_DIMS` for mismatched shapes, or a
                                                              !! failure raised during the simulation.

        character(*), parameter:: PROC_NAME = 'crysp_simulate_strain_modes'

        integer:: i

        if (size(strain_modes, 1) /= 5 .or. any(shape(stresses) /= shape(strain_modes))) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_DIMS, 'All mode arrays must have shape 5 x n.')
            return
        end if

        do i = 1, size(strain_modes, 2)
            stat = macro_simulate_strain_mode(mat, strain_modes(:,i), stresses(:,i))
            if (failed(stat)) return
        end do
    end function
end module
