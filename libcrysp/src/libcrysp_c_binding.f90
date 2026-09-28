module libcrysp_c_binding
    use iso_c_binding
    use libcrysp

    implicit none

    private
    public:: crysp_get_meso_models_c, &
             crysp_get_micro_models_c, &
             crysp_new_material_c, &
             crysp_destroy_material_c, &
             crysp_serialize_material_c, &
             crysp_deserialize_material_c, &
             crysp_get_texture_c, &
             crysp_strain_driven_deformation_c, &
             crysp_stress_driven_deformation_c, &
             crysp_simulate_stress_mode_c, &
             crysp_simulate_strain_mode_c

contains

    !> C binding of [[libcrysp:crysp_get_meso_models]]. See [[ModelDescriptor_C]] for what the caller has to release.
    function crysp_get_meso_models_c(model_descs) bind(C, name='crysp_get_meso_models') result(stat)
        type(ModelDescriptor_C), dimension(:), allocatable, intent(out):: model_descs
        type(Status_C):: stat

        type(ModelDescriptor), dimension(:), allocatable:: model_descs_

        stat = crysp_get_meso_models(model_descs_)
        if (stat%code /= CRYSP_OK) return

        model_descs = to_model_descriptor_c(model_descs_)
    end function

    !> C binding of [[libcrysp:crysp_get_micro_models]]. See [[ModelDescriptor_C]] for what the caller has to release.
    function crysp_get_micro_models_c(model_descs) bind(C, name='crysp_get_micro_models') result(stat)
        type(ModelDescriptor_C), dimension(:), allocatable, intent(out):: model_descs
        type(Status_C):: stat

        type(ModelDescriptor), dimension(:), allocatable:: model_descs_

        stat = crysp_get_micro_models(model_descs_)
        if (stat%code /= CRYSP_OK) return

        model_descs = to_model_descriptor_c(model_descs_)
    end function

    !> C binding of [[libcrysp:crysp_new_material]].
    !>
    !> @note
    !> The arrays sized by `phase_sizes` are assumed-size rather than explicit-shape. IFX (as of 2026.1) evaluates specification
    !> expressions of a `bind(C)` procedure before its assumed-shape dummies are set up, so any expression involving `phase_sizes`
    !> there reads an undefined descriptor.
    !> @endnote
    function crysp_new_material_c(phase_sizes, orientations, deformation_mechanisms, hardening_model_ids, hardening_params, &
                                  meso_model_id, meso_params, mat) bind(C, name='crysp_new_material') result(stat)
        integer(C_INT), dimension(:), intent(in):: phase_sizes
        real(C_DOUBLE), dimension(3,*), intent(in):: orientations
        integer(C_INT), dimension(*), intent(in):: deformation_mechanisms
        integer(C_INT), dimension(*), intent(in):: hardening_model_ids
        type(Parameter_C), dimension(:), intent(in):: hardening_params
        integer(C_INT), intent(in):: meso_model_id
        type(Parameter_C), dimension(:), intent(in):: meso_params
        type(MaterialHandle), intent(out):: mat
        type(Status_C):: stat

        stat = crysp_new_material(phase_sizes, orientations, deformation_mechanisms, hardening_model_ids, &
                                  parameters_from_c(hardening_params), meso_model_id, &
                                  parameters_from_c(meso_params), mat)
    end function

    !> C binding of [[libcrysp:crysp_strain_driven_deformation]].
    function crysp_strain_driven_deformation_c(mat, velocity_gradient, target_vm_strain, increments, textures) &
            bind(C, name='crysp_strain_driven_deformation') result(stat)
        type(MaterialHandle), intent(in):: mat
        real(C_DOUBLE), dimension(3,3), intent(in):: velocity_gradient
        real(C_DOUBLE), intent(in):: target_vm_strain
        type(StrainIncrementData), dimension(:), allocatable, intent(out):: increments
        type(TexturePoint), dimension(:,:), allocatable, intent(out):: textures
        type(Status_C):: stat

        stat = crysp_strain_driven_deformation(mat, velocity_gradient, target_vm_strain, increments, textures)
    end function

    !> C binding of [[libcrysp:crysp_stress_driven_deformation]].
    function crysp_stress_driven_deformation_c(mat, target_stress_mode, target_vm_strain, stress_increments, strain_increments, &
                                               textures) bind(C, name='crysp_stress_driven_deformation') result(stat)
        type(MaterialHandle), intent(in):: mat
        real(C_DOUBLE), dimension(5), intent(in):: target_stress_mode
        real(C_DOUBLE), intent(in):: target_vm_strain
        type(StressIncrementData), dimension(:), allocatable, intent(out):: stress_increments
        type(StrainIncrementData), dimension(:), allocatable, intent(out):: strain_increments
        type(TexturePoint), dimension(:,:), allocatable, intent(out):: textures
        type(Status_C):: stat

        stat = crysp_stress_driven_deformation(mat, target_stress_mode, target_vm_strain, stress_increments, &
                                               strain_increments, textures)
    end function

    !> C binding of [[libcrysp:crysp_simulate_stress_mode]].
    function crysp_simulate_stress_mode_c(mat, target_stress_mode, strain_mode, stress, residual) &
            bind(C, name='crysp_simulate_stress_mode') result(stat)
        type(MaterialHandle), intent(in):: mat
        real(C_DOUBLE), dimension(5), intent(in):: target_stress_mode
        real(C_DOUBLE), dimension(5), intent(inout):: strain_mode
        real(C_DOUBLE), dimension(5), intent(out):: stress
        real(C_DOUBLE), dimension(5), intent(out):: residual
        type(Status_C):: stat

        stat = crysp_simulate_stress_mode(mat, target_stress_mode, strain_mode, stress, residual)
    end function

    !> C binding of [[libcrysp:crysp_simulate_strain_mode]].
    function crysp_simulate_strain_mode_c(mat, strain_mode, stress) bind(C, name='crysp_simulate_strain_mode') result(stat)
        type(MaterialHandle), intent(in):: mat
        real(C_DOUBLE), dimension(5), intent(in):: strain_mode
        real(C_DOUBLE), dimension(5), intent(out):: stress
        type(Status_C):: stat

        stat = crysp_simulate_strain_mode(mat, strain_mode, stress)
    end function
end module
