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


    type, bind(C):: StrainIncrement_C
        real(C_DOUBLE):: duration
        real(C_DOUBLE), dimension(3,3):: deformation_gradient
        real(C_DOUBLE):: vm_strain
        real(C_DOUBLE), dimension(3,3):: stress
        real(C_DOUBLE):: taylor_factor
        integer(C_INT):: n_grains
        type(C_PTR):: texture
    end type

    type, bind(C):: StressIncrement_C
        real(C_DOUBLE), dimension(5):: strain_rate
        real(C_DOUBLE), dimension(5):: residual
        integer(C_INT):: n_strain_increments
        type(C_PTR):: strain_increments
    end type

    type, bind(C):: Input_C
        character(C_CHAR), dimension(NAME_LEN):: name
        character(C_CHAR), dimension(DESC_LEN):: description
        integer(C_INT):: type
        type(Parameter_C):: lower_bound
        logical(C_BOOL):: lower_bound_inclusive
        type(Parameter_C):: upper_bound
        logical(C_BOOL):: upper_bound_inclusive
        type(Parameter_C):: default_value
    end type

    type, bind(C):: StateDescriptor_C
        character(C_CHAR), dimension(NAME_LEN):: name
        character(C_CHAR), dimension(DESC_LEN):: description
        integer(C_INT):: type
    end type

    !> General information about a model supported by libCrysP, as reported to C callers.
    !>
    !> The C-compatible counterpart of [[crysp_model:ModelDescriptor]]. Each list is a malloc'ed array of `n_...` elements, or
    !> null if it is empty, and is released by the caller with `free()`. The bounds and default value of each input are owned by
    !> the caller as well: see [[Parameter_C]].
    type, bind(C):: ModelDescriptor_C
        integer(C_INT):: id
        character(C_CHAR), dimension(NAME_LEN):: name
        character(C_CHAR), dimension(DESC_LEN):: description
        integer(C_INT):: n_inputs
        type(C_PTR):: inputs
        integer(C_INT):: n_model_state
        type(C_PTR):: model_state
        integer(C_INT):: n_element_state
        type(C_PTR):: element_state
    end type

    type, bind(C):: ModelData_C
        integer(C_INT):: id
        type(C_PTR):: parameters
    end type

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

    !> C binding of [[libcrysp:crysp_destroy_material]].
    subroutine crysp_destroy_material_c(mat) bind(C, name='crysp_destroy_material')
        type(MaterialHandle), intent(inout):: mat

        call crysp_destroy_material(mat)
    end subroutine

    !> C binding of [[libcrysp:crysp_serialize_material]]. The caller owns the data of the parameters: see [[Parameter_C]].
    function crysp_serialize_material_c(mat, params) bind(C, name='crysp_serialize_material') result(stat)
        type(MaterialHandle), intent(inout):: mat
        type(Parameter_C), dimension(:), allocatable, intent(out):: params
        type(Status_C):: stat

        integer:: i
        type(Parameter), dimension(:), allocatable:: params_

        stat = crysp_serialize_material(mat, params_)
        if (stat%code /= CRYSP_OK) return

        allocate(params(size(params_)))
        do i = 1, size(params_)
            params(i) = params_(i)
        end do
    end function

    !> C binding of [[libcrysp:crysp_deserialize_material]].
    function crysp_deserialize_material_c(params, mat) bind(C, name='crysp_deserialize_material') result(stat)
        type(Parameter_C), dimension(:), intent(in):: params
        type(MaterialHandle), intent(out):: mat
        type(Status_C):: stat

        stat = crysp_deserialize_material(parameters_from_c(params), mat)
    end function

    !> C binding of [[libcrysp:crysp_get_texture]].
    function crysp_get_texture_c(mat, texture) bind(C, name='crysp_get_texture') result(stat)
        type(MaterialHandle), intent(in):: mat
        type(TexturePoint), dimension(:), allocatable, intent(out):: texture
        type(Status_C):: stat

        stat = crysp_get_texture(mat, texture)
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


    !> Convert a list of C-compatible parameters to parameters. The data is copied: see [[parameter_from_c]].
    function parameters_from_c(params_c) result(params)
        type(Parameter_C), dimension(:), intent(in):: params_c
        type(Parameter), dimension(:), allocatable:: params

        integer:: i

        allocate(params(size(params_c)))
        do i = 1, size(params_c)
            params(i) = params_c(i)
        end do
    end function

    !> Convert a model descriptor to its C-compatible counterpart. See [[ModelDescriptor_C]] for who owns what.
    impure elemental function to_model_descriptor_c(desc) result(desc_c)
        type(ModelDescriptor), intent(in):: desc
        type(ModelDescriptor_C):: desc_c

        desc_c%id = int(desc%id, C_INT)
        desc_c%name = string_to_chars(desc%name, size(desc_c%name))
        desc_c%description = string_to_chars(desc%description, size(desc_c%description))
        desc_c%n_inputs = int(size(desc%inputs), C_INT)
        desc_c%inputs = to_inputs_c(desc%inputs)
        desc_c%n_model_state = int(size(desc%model_state), C_INT)
        desc_c%model_state = to_state_descriptors_c(desc%model_state)
        desc_c%n_element_state = int(size(desc%element_state), C_INT)
        desc_c%element_state = to_state_descriptors_c(desc%element_state)
    end function

    !> Copy a list of inputs into a malloc'ed array of their C-compatible counterparts. Null if the list is empty.
    !>
    !> The bounds and default values are copied as well: the result owns them.
    function to_inputs_c(inputs) result(ptr)
        type(Input), dimension(:), intent(in):: inputs
        type(C_PTR):: ptr

        integer:: i
        type(Input_C):: mold
        type(Input_C), dimension(:), pointer:: inputs_c

        ptr = C_NULL_PTR
        if (size(inputs) == 0) return

        ptr = malloc(int(size(inputs), C_SIZE_T) * c_sizeof(mold))
        call c_f_pointer(ptr, inputs_c, [size(inputs)])
        do i = 1, size(inputs)
            associate (input_ => inputs(i), input_c => inputs_c(i))
                input_c%name = string_to_chars(input_%name, size(input_c%name))
                input_c%description = string_to_chars(input_%description, size(input_c%description))
                input_c%type = int(input_%type, C_INT)
                input_c%lower_bound = input_%lower_bound
                input_c%lower_bound_inclusive = logical(input_%lower_bound_inclusive, C_BOOL)
                input_c%upper_bound = input_%upper_bound
                input_c%upper_bound_inclusive = logical(input_%upper_bound_inclusive, C_BOOL)
                input_c%default_value = input_%default_value
            end associate
        end do
    end function

    !> Copy a list of state descriptors into a malloc'ed array of their C-compatible counterparts. Null if the list is empty.
    function to_state_descriptors_c(states) result(ptr)
        type(StateDescriptor), dimension(:), intent(in):: states
        type(C_PTR):: ptr

        integer:: i
        type(StateDescriptor_C):: mold
        type(StateDescriptor_C), dimension(:), pointer:: states_c

        ptr = C_NULL_PTR
        if (size(states) == 0) return

        ptr = malloc(int(size(states), C_SIZE_T) * c_sizeof(mold))
        call c_f_pointer(ptr, states_c, [size(states)])
        do i = 1, size(states)
            states_c(i)%name = string_to_chars(states(i)%name, size(states_c(i)%name))
            states_c(i)%description = string_to_chars(states(i)%description, size(states_c(i)%description))
            states_c(i)%type = int(states(i)%type, C_INT)
        end do
    end function

end module
