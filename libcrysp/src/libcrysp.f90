!> Top-level libCrysP Module
!>
!> Provides inerface to callers and formats data to be used in underlying modules.
!> Only module in libCrysP where all procedures are guaranteed to sanitize their input.
!>
!> Every entry point returns a [[crysp_status:Status]] by value and never terminates the process: libCrysP may be embedded in a
!> long-lived host such as a web server, where a bad request must not take the host down with it. A caller checks `stat%code`
!> against `CRYSP_OK` and, on failure, reads the NUL-terminated `mod_name`, `proc_name` and `message` buffers for the detail.
!>
!> The interface is the same for Fortran and C callers. A material lives in libCrysP's memory and is referred to by a
!> [[MaterialHandle]]: it is created by [[crysp_new_material]] or [[crysp_deserialize_material]], and must be released with
!> [[crysp_destroy_material]]. The contents of a material are private to libCrysP. What callers need from it is exposed through
!> dedicated calls, such as [[crysp_get_texture]]. A material only needs to be serialized to move it out of the process, e.g. to
!> hand it to a client of a stateless web API.
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
    private:: dereference_material, &
             split_strain_increments

    character(*), parameter, private:: MOD_NAME = 'crysp'

    !> Reference to a material owned by libCrysP.
    !>
    !> A handle is a plain pointer: copying it does not copy the material, and every copy is invalid once the material is freed.
    !> A material must not be used by two threads at the same time.
    !> The material has to be freed manually!
    type, bind(C):: MaterialHandle
        type(C_PTR):: ptr
    end type

    !> One step of a deformation, as reported to callers.
    !>
    !> The C-compatible counterpart of [[incrementation:StrainIncrement]]: same fields, except that the texture is reported
    !> separately because an interoperable type cannot hold one texture per grain. See [[crysp_strain_driven_deformation]].
    type, bind(C):: StrainIncrementData
        real(C_DOUBLE):: duration
        real(C_DOUBLE), dimension(3,3):: deformation_gradient !! Technically redundant but saves a lot of computation
        real(C_DOUBLE):: vm_strain                            !! Von mises true strain. Technically redundant but saves a lot of computation
        real(C_DOUBLE), dimension(3,3):: stress
        real(C_DOUBLE):: taylor_factor
    end type

    !> One step of the mapping between a requested stress mode and the strain mode applied to reach it, as reported to callers.
    !>
    !> The C-compatible counterpart of [[incrementation:StressIncrement]]: the strain increments it holds are reported as one flat
    !> list, so `n_strain_increments` says how many of them belong to this stress increment. See
    !> [[crysp_stress_driven_deformation]].
    type, bind(C):: StressIncrementData
        real(C_DOUBLE), dimension(5):: strain_rate
        real(C_DOUBLE), dimension(5):: residual
        integer(C_INT):: n_strain_increments !! Number of consecutive strain increments belonging to this stress increment.
    end type

    !> General information about a model supported by libCrysP, as reported to callers.
    !>
    !> Returned by [[crysp_get_meso_models]] and [[crysp_get_micro_models]], which also report the inputs the descriptors refer
    !> to. The text buffers are NUL-terminated and truncated to fit; a name or description longer than its buffer loses its tail,
    !> never its terminator.
    type, bind(C):: ModelDescriptorCompat
        integer(C_INT):: id                                   !! ID identifying this model in the call it was obtained from.
        character(kind=C_CHAR), dimension(NAME_LEN):: name    !! Short name of the model. NUL-terminated.
        character(kind=C_CHAR), dimension(DESC_LEN):: description !! What the model does. NUL-terminated.
        integer(C_INT):: n_inputs                             !! Number of consecutive inputs belonging to this model.
    end type

contains

    !> List the mesoscopic models libCrysP supports.
    !>
    !> One descriptor per model. The IDs are those accepted as `meso_model_id` by [[crysp_new_material]].
    !>
    !> The inputs of all models are reported as one flat list, because an interoperable type cannot hold a variable number of
    !> them: `n_inputs` says how many consecutive entries of `inputs` belong to each descriptor, in descriptor order.
    function crysp_get_meso_models(model_descs, inputs) bind(C) result(stat)
        type(ModelDescriptorCompat), dimension(:), allocatable, intent(out):: model_descs !! Descriptor for each supported meso model
        type(Input), dimension(:), allocatable, intent(out):: inputs                !! Inputs of all models, concatenated.
        type(Status):: stat                                                         !! Return status. Should always be OK

        integer:: i, &
                  n_models
        integer, dimension(:), allocatable:: ids
        class(MesoModel), allocatable:: model
        type(ModelDescriptor):: desc

        ids = meso_get_model_ids()
        n_models = size(ids)

        allocate(model_descs(n_models))
        allocate(inputs(0))

        do i = 1, n_models
            stat = meso_get_model(ids(i), model)
            if (failed(stat)) return

            !Count and data must come from the same descriptor, so that n_inputs always matches what was appended.
            desc = model%describe()

            model_descs(i) = ModelDescriptorCompat(int(desc%id, C_INT), &
                                                   to_c_string(desc%name, NAME_LEN), &
                                                   to_c_string(desc%description, DESC_LEN), &
                                                   int(size(desc%inputs), C_INT))
            inputs = [inputs, desc%inputs]
        end do
    end function

    !> List the microscopic (hardening) models libCrysP supports.
    !>
    !> One descriptor per model. The IDs are those accepted as `hardening_model_ids` by [[crysp_new_material]].
    !>
    !> The inputs of all models are reported as one flat list, because an interoperable type cannot hold a variable number of
    !> them: `n_inputs` says how many consecutive entries of `inputs` belong to each descriptor, in descriptor order.
    function crysp_get_micro_models(model_descs, inputs) bind(C) result(stat)
        type(ModelDescriptorCompat), dimension(:), allocatable, intent(out):: model_descs !! Descriptor for each supported micro model
        type(Input), dimension(:), allocatable, intent(out):: inputs                !! Inputs of all models, concatenated.
        type(Status):: stat                                                         !! Return status. Should always be OK

        integer:: i, &
                  n_models
        integer, dimension(:), allocatable:: ids
        class(ConstitutiveModel), allocatable:: model
        type(ModelDescriptor):: desc

        ids = micro_get_model_ids()
        n_models = size(ids)

        allocate(model_descs(n_models))
        allocate(inputs(0))

        do i = 1, n_models
            stat = micro_get_model(ids(i), model)
            if (failed(stat)) return

            !Count and data must come from the same descriptor, so that n_inputs always matches what was appended.
            desc = model%describe()

            model_descs(i) = ModelDescriptorCompat(int(desc%id, C_INT), &
                                                   to_c_string(desc%name, NAME_LEN), &
                                                   to_c_string(desc%description, DESC_LEN), &
                                                   int(size(desc%inputs), C_INT))
            inputs = [inputs, desc%inputs]
        end do
    end function

    !> Initialize a new material state.
    !>
    !> Initializes all data structures associated to a material state and assembles them into a Material object.
    !> An initialized Material object is needed for all other calls to libCrysP
    !> Sanitization of the input parameters is propagated to lower-level initialization procedures
    !>
    !> @note
    !> The arrays sized by `phase_sizes` are assumed-size rather than explicit-shape. IFX (as of 2026.1) evaluates specification
    !> expressions of a `bind(C)` procedure before its assumed-shape dummies are set up, so any expression involving `phase_sizes`
    !> there reads an undefined descriptor.
    !> @endnote
    function crysp_new_material(phase_sizes, orientations, deformation_mechanisms, hardening_model_ids, hardening_params, meso_model_id, meso_params, mat) bind(C) result(stat)
        integer(C_INT), dimension(:), intent(in):: phase_sizes !! Amount of grains belonging to each phase.
        real(C_DOUBLE), dimension(3,*), intent(in)::           orientations !! List of Euler angle triplets in bunge convention representing all grains.
        integer(C_INT), dimension(*), intent(in)::                 deformation_mechanisms !! ID of slip system family to use for each phase. Must
                                                                          !! exist in the list in [[micro]]

        integer(C_INT), dimension(*), intent(in)::                 hardening_model_ids !! ID of the hardening model of
                                                                                                  !! each phase, in the same order as deformation_mechanisms.
                                                                                                   !! Must exist in the list in [[micro]]
        type(Parameter), dimension(:), intent(in):: hardening_params !! list containing the parameter lists of the hardening model of each phase,
                                                                         !! in the smae order as deformation_mechanisms

        integer(C_INT), intent(in):: meso_model_id                       !! ID of the meso model. Must exist in the enum list in [[meso]]
        type(Parameter), dimension(:), intent(in):: meso_params !! List containing values for the parameters of the
                                                                      !!selected meso model. Order and types must correspond to meso_get_parameters(meso_model_id)

        type(MaterialHandle), intent(out):: mat                                        !! The initialized material. Null on failure.
        type(Status):: stat                                                            !! `CRYSP_OK`, or a failure describing which
                                                                                       !! of the supplied IDs was rejected.

        type(Grain), allocatable:: grains_(:)
        type(Material), pointer:: mat_

        mat%ptr = C_NULL_PTR
        allocate(mat_)

        stat = micro_init(phase_sizes, orientations, deformation_mechanisms, hardening_model_ids, hardening_params, mat_%phases, grains_)
        if (.not. failed(stat)) stat = meso_init(meso_model_id, grains_, meso_params, mat_%meso_model, mat_%clusters)
        if (failed(stat)) then
            deallocate(mat_)
            return
        end if

        mat%ptr = c_loc(mat_)
    end function

    !> Release a material.
    !>
    !> The handle is null afterwards. Destroying a null handle does nothing.
    subroutine crysp_destroy_material(mat) bind(C)
        type(MaterialHandle), intent(inout):: mat

        type(Material), pointer:: mat_

        if (.not. c_associated(mat%ptr)) return

        call c_f_pointer(mat%ptr, mat_)
        deallocate(mat_)
        mat%ptr = C_NULL_PTR
    end subroutine

    !> Serialize a material, e.g. to move it out of the process.
    !>
    !> Serializing moves the material into the parameters: it is destroyed and the handle is null afterwards. To keep using it,
    !> restore it with [[crysp_deserialize_material]].
    function crysp_serialize_material(mat, params) bind(C) result(stat)
        type(MaterialHandle), intent(inout):: mat
        type(Parameter), dimension(:), allocatable, intent(out):: params !! The serialized material state.
        type(Status):: stat                                              !! `CRYSP_OK`, or `CRYSP_ERR_ARG` for a null handle.

        type(Material), pointer:: mat_

        stat = dereference_material(mat, mat_)
        if (failed(stat)) return

        params = mat_%serialize()
        call crysp_destroy_material(mat)
    end function

    !> Rebuild a material from its serialized state.
    !>
    !> Reading consumes the parameters, see [[crysp_serialization]].
    !>
    !> @warning
    !> The parameters are not validated: a list that was not produced by [[crysp_serialize_material]] is undefined behavior. See
    !> the note in [[crysp_serialization]].
    !> @endwarning
    function crysp_deserialize_material(params, mat) bind(C) result(stat)
        type(Parameter), dimension(:), intent(in):: params !! Serialized material state.
        type(MaterialHandle), intent(out):: mat            !! The restored material.
        type(Status):: stat                                !! `CRYSP_OK`.

        type(Material), pointer:: mat_

        allocate(mat_)
        call mat_%deserialize(params)
        mat%ptr = c_loc(mat_)
    end function

    !> Get the current texture of a material.
    !>
    !> One texture point per grain, in the order in which the grains were passed to [[crysp_new_material]], i.e. grouped by phase.
    !> The Euler angles have the convention and units of the orientations passed there. The phase ID is the index of the grain's phase
    !> in `phase_sizes`.
    function crysp_get_texture(mat, texture) bind(C) result(stat)
        type(MaterialHandle), intent(in):: mat
        type(TexturePoint), dimension(:), allocatable, intent(out):: texture !! One texture point per grain.
        type(Status):: stat                                                  !! `CRYSP_OK`, or `CRYSP_ERR_ARG` for a null handle.

        type(Material), pointer:: mat_

        stat = dereference_material(mat, mat_)
        if (failed(stat)) return

        texture = mat_%get_texture()
    end function

    !> Deform a material according to a prescribed velocity gradient.
    !>
    !> The material has to be pre-initialized via crysp_new_material
    !> The velocity gradient must not contain a volumetric component (sum(trace) == )
    !> The velocity gradient is applied until a given von-mises equivalent true strain is reached.
    !> The incrementation process is iterative, so a deviation of the target strain level in the order of 10^-9 is expected.
    function crysp_strain_driven_deformation(mat, velocity_gradient, target_vm_strain, increments, textures) bind(C) result(stat)
        type(MaterialHandle), intent(in):: mat
        real(C_DOUBLE), dimension(3,3), intent(in):: velocity_gradient !! Assumed not to contain volumetric component.
        real(C_DOUBLE), intent(in)::                 target_vm_strain  !! Total von mises equivalent true strain to be reached.
        type(StrainIncrementData), dimension(:), allocatable, intent(out):: increments !! Reporting of individual deformation steps
                                                                                       !! that were applied. See
                                                                                       !! [[StrainIncrementData]] for details.
        type(TexturePoint), dimension(:,:), allocatable, intent(out):: textures    !! Texture after each increment, shaped
                                                                                   !! (grains, increments).
        type(Status):: stat                                                        !! `CRYSP_OK`, `CRYSP_ERR_ARG` for a rejected input,
                                                                                   !! or a failure raised during the simulation.

        character(*), parameter:: PROC_NAME = 'crysp_strain_driven_deformation'

        type(Material), pointer:: mat_
        type(StrainIncrement), dimension(:), allocatable:: increments_

        stat = dereference_material(mat, mat_)
        if (failed(stat)) return

        if (abs(math_trace33(velocity_gradient)) > TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Volumetric deformation is not allowed.')
            return
        end if
        if (target_vm_strain < TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Target von mises equivalent strain must be larger than 0.')
            return
        end if

        stat = macro_strain_driven_deformation(mat_, velocity_gradient, target_vm_strain, increments_)
        if (failed(stat)) return

        call split_strain_increments(increments_, increments, textures)
    end function

    !> Deform a material based on a stress mode.
    !>
    !> The material has to be pre-initialized via crysp_new_material
    !> The stress mode is applied until a given von-mises equivalent true strain is reached.
    !> The stress mode is iteratively matched to the appropriate strain mode. This process is inexact, so a deviation of 10^-2
    !> between the requested stress mode and the actually applied stress mode is expected.
    !> The incrementation process is iterative, so a deviation of the target strain level in the order of 10^-9 is expected.
    function crysp_stress_driven_deformation(mat, target_stress_mode, target_vm_strain, stress_increments, strain_increments, &
                                             textures) bind(C) result(stat)
        type(MaterialHandle), intent(in):: mat                                            !! Initialized material
        real(C_DOUBLE), dimension(5), intent(in):: target_stress_mode                     !! Norm must be 1
        real(C_DOUBLE), intent(in):: target_vm_strain                                     !! Target true von mises strain
        type(StressIncrementData), dimension(:), allocatable, intent(out):: stress_increments !! Step-wise reporting of the mapping
                                                                                          !! between requested stress mode and strain
                                                                                          !! mode. See [[StressIncrementData]].
        type(StrainIncrementData), dimension(:), allocatable, intent(out):: strain_increments !! The deformation steps that were
                                                                                          !! applied, for all stress increments.
                                                                                          !! See [[StrainIncrementData]].
        type(TexturePoint), dimension(:,:), allocatable, intent(out):: textures           !! Texture after each strain increment,
                                                                                          !! shaped (grains, strain increments).
        type(Status):: stat                                                               !! `CRYSP_OK`, `CRYSP_ERR_ARG` for a rejected
                                                                                          !! input, or a simulation failure.

        character(*), parameter:: PROC_NAME = 'crysp_stress_driven_deformation'

        type(Material), pointer:: mat_
        type(StressIncrement), dimension(:), allocatable:: increments_
        type(StrainIncrement), dimension(:), allocatable:: strain_increments_
        integer:: i, offset

        stat = dereference_material(mat, mat_)
        if (failed(stat)) return

        if (abs(norm2(target_stress_mode) - 1._DP) > TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Norm of stress mode must be 1.')
            return
        end if
        if (target_vm_strain < TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Target von mises equivalent strain must be strictly positive.')
            return
        end if

        stat = macro_stress_driven_deformation(mat_, target_stress_mode, target_vm_strain, increments_)
        if (failed(stat)) return

        !Flatten the strain increments of all stress increments into one list, recording how many belong to each.
        allocate(stress_increments(size(increments_)))
        allocate(strain_increments_(sum([(size(increments_(i)%strain_increments), i = 1, size(increments_))])))
        offset = 0
        do i = 1, size(increments_)
            associate (inc => increments_(i))
                stress_increments(i) = StressIncrementData(inc%strain_rate, inc%residual, size(inc%strain_increments))
                strain_increments_(offset+1:offset+size(inc%strain_increments)) = inc%strain_increments
                offset = offset + size(inc%strain_increments)
            end associate
        end do

        call split_strain_increments(strain_increments_, strain_increments, textures)
    end function

    !> Split strain increments into their C-compatible fields and the textures they carry.
    subroutine split_strain_increments(source, increments, textures)
        type(StrainIncrement), dimension(:), intent(in):: source
        type(StrainIncrementData), dimension(:), allocatable, intent(out):: increments
        type(TexturePoint), dimension(:,:), allocatable, intent(out):: textures !! Shaped (grains, increments).

        integer:: i, n_grains

        n_grains = 0
        if (size(source) > 0) n_grains = size(source(1)%texture)

        allocate(increments(size(source)))
        allocate(textures(n_grains, size(source)))
        do i = 1, size(source)
            associate (inc => source(i))
                increments(i) = StrainIncrementData(inc%duration, inc%deformation_gradient, inc%vm_strain, inc%stress, &
                                                    inc%taylor_factor)
                textures(:,i) = inc%texture
            end associate
        end do
    end subroutine

    !> Calculate strain mode and stress corresponding to a desired stress mode.
    !>
    !> The material has to be pre-initialized via crysp_new_material
    !> Uses iterative search to find an accurate match for the strain mode and stress state corresponding to the desired stress mode.
    !> An accurate initial guess for the strain mode should be provided to improve convergence and performance. The residual of the search is returned to provide
    !> an estimation of search accuracy.
    function crysp_simulate_stress_mode(mat, target_stress_mode, strain_mode, stress, residual) bind(C) result(stat)
        type(MaterialHandle), intent(in):: mat                          !! Material state.
        real(C_DOUBLE), dimension(5), intent(in)::  target_stress_mode   !! Intended stress mode
        real(C_DOUBLE), dimension(5), intent(inout):: strain_mode !! On entry, contains an initial guess of the strain mode matching the
                                                            !! target stress mode. On exit, contains the actual strain mode.
        real(C_DOUBLE), dimension(5), intent(out):: stress        !! Actual stress state found.
        real(C_DOUBLE), dimension(5), intent(out):: residual      !! Residual of the the search.
        type(Status):: stat                                       !! `CRYSP_OK`, `CRYSP_ERR_ARG` for a rejected input, or a
                                                                  !! simulation failure.

        character(*), parameter:: PROC_NAME = 'crysp_simulate_stress_mode'

        type(Material), pointer:: mat_

        stat = dereference_material(mat, mat_)
        if (failed(stat)) return

        if (abs(norm2(target_stress_mode) - 1._DP) > TOLERANCE) then
            stat = fail(MOD_NAME, PROC_NAME, CRYSP_ERR_ARG, 'Norm of stress mode must be 1.')
            return
        end if

        stat = macro_simulate_stress_mode(mat_, target_stress_mode, strain_mode, stress, residual)
    end function

    !> Calculate stress state corresponding to a given strain mode.
    function crysp_simulate_strain_mode(mat, strain_mode, stress) bind(C) result(stat)
        type(MaterialHandle), intent(in):: mat                          !! Material state
        real(C_DOUBLE), dimension(5), intent(in)::    strain_mode !! Deviatoric strain mode
        real(C_DOUBLE), dimension(5), intent(out)::   stress      !! Stress state corresponding to the strain mode.
        type(Status):: stat                                       !! `CRYSP_OK`, or a failure raised during the simulation.

        type(Material), pointer:: mat_

        stat = dereference_material(mat, mat_)
        if (failed(stat)) return

        stat = macro_simulate_strain_mode(mat_, strain_mode, stress)
    end function

    !> Get the material a handle refers to.
    !>
    !> Only a null handle is detected. A handle to a freed material is undefined behavior.
    function dereference_material(mat, mat_) result(stat)
        type(MaterialHandle), intent(in):: mat
        type(Material), pointer, intent(out):: mat_
        type(Status):: stat

        mat_ => null()
        if (.not. c_associated(mat%ptr)) then
            stat = fail(MOD_NAME, 'dereference_material', CRYSP_ERR_ARG, 'Material handle is null.')
            return
        end if

        call c_f_pointer(mat%ptr, mat_)
    end function
end module
