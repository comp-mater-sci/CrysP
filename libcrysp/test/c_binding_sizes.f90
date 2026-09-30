!> Sizes of the `bind(C)` types as the library sees them, for `test_c_binding.c` to check `libcrysp.h` against.
!>
!> The order must match `FORTRAN_TYPES` in `test_c_binding.c`.
subroutine crysp_test_sizes(sizes) bind(C, name='crysp_test_sizes')
    use iso_c_binding
    use crysp_utils_c
    use crysp_micro_c
    use crysp_meso_c
    use crysp_macro_c

    implicit none

    integer(C_SIZE_T), dimension(15), intent(out):: sizes

    type(Status_C):: stat
    type(Parameter_C):: param
    type(Input_C):: inp
    type(StateDescriptor_C):: state_desc
    type(ModelDescriptor_C):: model_desc
    type(ModelData_C):: model_data
    type(PhaseData_C):: phase_data
    type(ConstitutiveModel_C):: cm
    type(HardeningState_C):: hard_stat
    type(Grain_C):: gr
    type(Cluster_C):: clstr
    type(Material_C):: mat
    type(StrainIncrement_C):: strain_inc
    type(StressIncrement_C):: stress_inc
    type(SlipSystemDescriptor_C):: slip_desc

    sizes = [c_sizeof(stat), c_sizeof(param), c_sizeof(inp), c_sizeof(state_desc), c_sizeof(model_desc), &
             c_sizeof(model_data), c_sizeof(phase_data), c_sizeof(cm), c_sizeof(hard_stat), c_sizeof(gr), c_sizeof(clstr), &
             c_sizeof(mat), c_sizeof(strain_inc), c_sizeof(stress_inc), c_sizeof(slip_desc)]
end subroutine
