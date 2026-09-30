!> C binding of the libCrysP entry points.
!>
!> Thin wrappers that convert their arguments and call the Fortran counterparts in [[libcrysp]], which documents what they do.
!> The C-specific contract (ownership, allocation conventions, ...) is documented in `src/c_binding/libcrysp.h`.
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
        call destroy(mat)
        stat = crysp_strain_driven_deformation(mat_fort, velocity_gradient, target_vm_strain, incs_fort)
        mat = mat_fort
        if (stat%code /= CRYSP_OK) return

        n_incs = int(size(incs_fort), C_INT)
        incs = malloc(int(size(incs_fort), C_SIZE_T) * c_sizeof(mold))
        call c_f_pointer(incs, incs_ptr, [size(incs_fort)])
        do i = 1, size(incs_fort)
            incs_ptr(i) = incs_fort(i)
        end do
    end function

    !> C binding of [[libcrysp:crysp_stress_driven_deformation]].
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
        call destroy(mat)
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
    function crysp_simulate_stress_mode_c(mat, target_stress_mode, strain_mode, stress, residual) &
            bind(C, name='crysp_simulate_stress_mode') result(stat)
        type(Material_C), intent(in):: mat
        real(C_DOUBLE), dimension(5), intent(in):: target_stress_mode
        real(C_DOUBLE), dimension(5), intent(inout):: strain_mode
        real(C_DOUBLE), dimension(5), intent(out):: stress
        real(C_DOUBLE), dimension(5), intent(out):: residual
        type(Status_C):: stat

        type(Material), target:: mat_fort

        mat_fort = mat
        stat = crysp_simulate_stress_mode(mat_fort, target_stress_mode, strain_mode, stress, residual)
    end function

    !> C binding of [[libcrysp:crysp_simulate_strain_mode]].
    function crysp_simulate_strain_mode_c(mat, strain_mode, stress) bind(C, name='crysp_simulate_strain_mode') result(stat)
        type(Material_C), intent(in):: mat
        real(C_DOUBLE), dimension(5), intent(in):: strain_mode
        real(C_DOUBLE), dimension(5), intent(out):: stress
        type(Status_C):: stat

        type(Material), target:: mat_fort

        mat_fort = mat
        stat = crysp_simulate_strain_mode(mat_fort, strain_mode, stress)
    end function
end module
