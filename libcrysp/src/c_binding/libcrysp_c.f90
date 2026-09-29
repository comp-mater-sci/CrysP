module libcrysp_c_binding
    use iso_c_binding
    use libcrysp
    use crysp_utils_c
    use crysp_macro_c

    implicit none

    private
    public:: crysp_get_meso_models_c, &
             crysp_get_micro_models_c, &
             crysp_new_material_c, &
             crysp_strain_driven_deformation_c, &
             crysp_stress_driven_deformation_c, &
             crysp_simulate_stress_mode_c, &
             crysp_simulate_strain_mode_c

contains

    !> C binding of [[libcrysp:crysp_get_meso_models]].
    !>
    !> Ownership of allocated buffers is passed to caller.
    function crysp_get_meso_models_c(n_models, model_descs) bind(C, name='crysp_get_meso_models') result(stat)
        integer(C_INT), intent(out):: n_models
        type(C_PTR), intent(out):: model_descs !! n_models * type(ModelDescriptor_C)
        type(Status_C):: stat

        integer:: i
        type(ModelDescriptor), dimension(:), allocatable:: model_descs_
        type(ModelDescriptor_C):: mold
        type(ModelDescriptor_C), dimension(:), pointer:: descs_ptr

        n_models = 0
        model_descs = C_NULL_PTR

        stat = crysp_get_meso_models(model_descs_)

        n_models = int(size(model_descs_), C_INT)
        model_descs = malloc(int(size(model_descs_), C_SIZE_T) * c_sizeof(mold))
        call c_f_pointer(model_descs, descs_ptr, [size(model_descs_)])
        do i = 1, size(model_descs_)
            descs_ptr(i) = model_descs_(i)
        end do
    end function

    !> C binding of [[libcrysp:crysp_get_micro_models]].
    !>
    !> Ownership of allocated buffers is passed to caller.
    function crysp_get_micro_models_c(n_models, model_descs) bind(C, name='crysp_get_micro_models') result(stat)
        integer(C_INT), intent(out):: n_models
        type(C_PTR), intent(out):: model_descs !! n_models * type(ModelDescriptor_C)
        type(Status_C):: stat

        integer:: i
        type(ModelDescriptor), dimension(:), allocatable:: model_descs_
        type(ModelDescriptor_C):: mold
        type(ModelDescriptor_C), dimension(:), pointer:: descs_ptr

        n_models = 0
        model_descs = C_NULL_PTR

        stat = crysp_get_micro_models(model_descs_)

        n_models = int(size(model_descs_), C_INT)
        model_descs = malloc(int(size(model_descs_), C_SIZE_T) * c_sizeof(mold))
        call c_f_pointer(model_descs, descs_ptr, [size(model_descs_)])
        do i = 1, size(model_descs_)
            descs_ptr(i) = model_descs_(i)
        end do
    end function

    !> C binding of [[libcrysp:crysp_new_material]].
    !>
    !> The input is consumed: all allocated buffers in `meso_model_data` and `phase_data` are freed, also on failure, so they must
    !> have been malloc'ed and are dangling afterwards. `phase_data` itself is not freed.
    !> On success, ownership of all allocated buffers in `mat` is passed to the caller, see [[Material_C]]. On failure, `mat` is
    !> undefined and must not be freed.
    function crysp_new_material_c(meso_model_data, n_phases, phase_data, mat) bind(C, name='crysp_new_material') result(stat)
        type(ModelData_C), intent(in):: meso_model_data
        integer(C_INT), value, intent(in):: n_phases
        type(PhaseData_C), dimension(n_phases), intent(in):: phase_data
        type(Material_C), intent(out):: mat
        type(Status_C):: stat

        integer:: i
        type(ModelData):: meso_model_data_fort
        type(PhaseData), dimension(n_phases):: phase_data_fort
        type(Material), target:: mat_fort

        meso_model_data_fort = meso_model_data
        do i=1,n_phases
            phase_data_fort(i) = phase_data(i)
        end do

        stat = crysp_new_material(meso_model_data_fort, phase_data_fort, mat_fort)
        if (stat%code /= CRYSP_OK) return
        mat = mat_fort
    end function

    !> C binding of [[libcrysp:crysp_strain_driven_deformation]].
    !>
    !> `mat` is replaced by the deformed material: all allocated buffers it held on entry are freed, and ownership of the new ones
    !> is passed to the caller, see [[Material_C]]. This also happens on failure, where `mat` holds the state reached when the
    !> failure occurred, so the caller always owns a valid material.
    !> On success, `incs` is a malloc'ed array of `n_incs` increments whose buffers are owned by the caller. On failure, `n_incs`
    !> is 0 and `incs` is null.
    function crysp_strain_driven_deformation_c(mat, velocity_gradient, target_vm_strain, n_incs, incs) &
            bind(C, name='crysp_strain_driven_deformation') result(stat)
        type(Material_C), intent(inout):: mat
        real(C_DOUBLE), dimension(3,3), intent(in):: velocity_gradient
        real(C_DOUBLE), value, intent(in):: target_vm_strain
        integer(C_INT), intent(out):: n_incs
        type(C_PTR), intent(out):: incs !! n_incs * type(StrainIncrement_C)
        type(Status_C):: stat

        integer:: i
        type(Material), target:: mat_fort
        type(StrainIncrement), dimension(:), allocatable:: incs_fort
        type(StrainIncrement_C):: mold
        type(StrainIncrement_C), dimension(:), pointer:: incs_ptr

        n_incs = 0
        incs = C_NULL_PTR

        mat_fort = mat
        stat = crysp_strain_driven_deformation(mat_fort, velocity_gradient, target_vm_strain, incs_fort)
        if (stat%code /= CRYSP_OK) return

        mat = mat_fort
        n_incs = int(size(incs_fort), C_INT)
        incs = malloc(int(size(incs_fort), C_SIZE_T) * c_sizeof(mold))
        call c_f_pointer(incs, incs_ptr, [size(incs_fort)])
        do i = 1, size(incs_fort)
            incs_ptr(i) = incs_fort(i)
        end do
    end function

    !> C binding of [[libcrysp:crysp_stress_driven_deformation]].
    !>
    !> `mat` is replaced by the deformed material: all allocated buffers it held on entry are freed, and ownership of the new ones
    !> is passed to the caller, see [[Material_C]]. This also happens on failure, where `mat` holds the state reached when the
    !> failure occurred, so the caller always owns a valid material.
    !> On success, `incs` is a malloc'ed array of `n_incs` increments whose buffers are owned by the caller. On failure, `n_incs`
    !> is 0 and `incs` is null.
    function crysp_stress_driven_deformation_c(mat, target_stress_mode, target_vm_strain, n_incs, incs) &
            bind(C, name='crysp_stress_driven_deformation') result(stat)
        type(Material_C), intent(inout):: mat
        real(C_DOUBLE), dimension(5), intent(in):: target_stress_mode
        real(C_DOUBLE), value, intent(in):: target_vm_strain
        integer(C_INT), intent(out):: n_incs
        type(C_PTR), intent(out):: incs !! n_incs * type(StressIncrement_C)
        type(Status_C):: stat

        integer:: i
        type(Material), target:: mat_fort
        type(StressIncrement), dimension(:), allocatable:: incs_fort
        type(StressIncrement_C):: mold
        type(StressIncrement_C), dimension(:), pointer:: incs_ptr

        n_incs = 0
        incs = C_NULL_PTR

        mat_fort = mat
        stat = crysp_stress_driven_deformation(mat_fort, target_stress_mode, target_vm_strain, incs_fort)
        mat = mat_fort
        if (stat%code /= CRYSP_OK) return

        n_incs = int(size(incs_fort), C_INT)
        incs = malloc(int(size(incs_fort), C_SIZE_T) * c_sizeof(mold))
        call c_f_pointer(incs, incs_ptr, [size(incs_fort)])
        do i = 1, size(incs_fort)
            incs_ptr(i) = incs_fort(i)
        end do
    end function

    !> C binding of [[libcrysp:crysp_simulate_stress_mode]].
    !>
    !> `mat` is replaced by the material after the simulation: all allocated buffers it held on entry are freed, and ownership of
    !> the new ones is passed to the caller, see [[Material_C]]. This also happens on failure, so the caller always owns a valid
    !> material.
    function crysp_simulate_stress_mode_c(mat, target_stress_mode, strain_mode, stress, residual) &
            bind(C, name='crysp_simulate_stress_mode') result(stat)
        type(Material_C), intent(inout):: mat
        real(C_DOUBLE), dimension(5), intent(in):: target_stress_mode
        real(C_DOUBLE), dimension(5), intent(inout):: strain_mode
        real(C_DOUBLE), dimension(5), intent(out):: stress
        real(C_DOUBLE), dimension(5), intent(out):: residual
        type(Status_C):: stat

        type(Material), target:: mat_fort

        mat_fort = mat
        stat = crysp_simulate_stress_mode(mat_fort, target_stress_mode, strain_mode, stress, residual)
        mat = mat_fort
    end function

    !> C binding of [[libcrysp:crysp_simulate_strain_mode]].
    !>
    !> `mat` is replaced by the material after the simulation: all allocated buffers it held on entry are freed, and ownership of
    !> the new ones is passed to the caller, see [[Material_C]]. This also happens on failure, so the caller always owns a valid
    !> material.
    function crysp_simulate_strain_mode_c(mat, strain_mode, stress) bind(C, name='crysp_simulate_strain_mode') result(stat)
        type(Material_C), intent(inout):: mat
        real(C_DOUBLE), dimension(5), intent(in):: strain_mode
        real(C_DOUBLE), dimension(5), intent(out):: stress
        type(Status_C):: stat

        type(Material), target:: mat_fort

        mat_fort = mat
        stat = crysp_simulate_strain_mode(mat_fort, strain_mode, stress)
        mat = mat_fort
    end function
end module
