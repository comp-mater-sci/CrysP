!> Top-level module of the macro layer
!>
!> Assimilates all the logic from the underlying layers and performs operattions on the mat as a whole.
!> @note
!> This module can be seen as the Fortran interface of libcrysp. If you wish to embed libcrysp into another Fortran project, it may
!> be more convenient to compile libcrysp without the top-level libcrysp module and interface with the library through this module.
!> Doing this allows you to interface with the library through the much richer Fortran interface and avoids many conversions needed
!> for the C interface implemented in the top-level libcrysp module.
!> If you decide to do this, make sure to sanitize the inputs of these routines because that is normally done in the libcrysp
!> module and this module expects to get proper inputs.
!> @endnote

module macro
    use base_defs
    use math_utils
    use crysp_cluster
    use meso
    use micro
    use incrementation
    use deformation
    use mode
    use conversions
    use crysp_meso_model

    implicit none
    public

    character(*), parameter, private:: MOD_NAME = 'macro'

contains

    subroutine macro_simulate_stress_mode(mat, stress_mode, strain_mode, stress, residual)
        type(Material), intent(inout):: mat
        real(DP), dimension(5), intent(in)::  stress_mode
        real(DP), dimension(5), intent(inout):: strain_mode

        real(DP), dimension(5), intent(out):: stress
        real(DP), dimension(5), intent(out):: residual

        call simulate_stress_mode(mat%cp_model, mat%clusters, stress_mode, strain_mode, stress, residual)
    end subroutine

    subroutine macro_simulate_strain_mode(mat, strain_mode, stress)
        type(Material), intent(inout):: mat
        real(DP), dimension(5), intent(in)::  strain_mode
        real(DP), dimension(5), intent(out):: stress

        call simulate_strain_mode(mat%cp_model, mat%clusters, strain_mode, stress)
    end subroutine

    subroutine macro_stress_driven_deformation(mat, target_stress_mode, target_vm_strain, increments)
        type(Material), target, intent(inout):: mat
        real(DP), dimension(5), intent(in):: target_stress_mode
        real(DP), intent(in):: target_vm_strain
        type(StressIncrement), dimension(:), allocatable, intent(out):: increments

        integer:: prob_rem_incs
        real(DP):: strain_rate(5), &
                   cur_vm_strain, &
                   next_vm_strain, &
                   stress(5), &
                   residual(5), &
                   def_grad(3,3)

        type(StrainIncrement), allocatable:: strain_incs(:)
        type(StressIncrementFactory):: incs

        cur_vm_strain = 0._DP
        def_grad = UNIT_MATRIX_3X3
        strain_rate = target_stress_mode

        do while (cur_vm_strain < target_vm_strain - TOLERANCE)
            call simulate_stress_mode(mat%cp_model, mat%clusters, target_stress_mode, strain_rate, stress, residual)
            strain_incs = deform(mat%cp_model, &
                                 mat%clusters, &
                                 deviatoric_to_tensor(strain_rate), &
                                 target_vm_strain - cur_vm_strain, &
                                 target_stress_mode)

            def_grad = matmul(strain_incs(size(strain_incs))%deformation_gradient, def_grad)
            next_vm_strain = stretch_to_von_mises_true_strain(def_grad)
            !Add one for current increment and to make sure padding in incrementation is positive
            prob_rem_incs = ceiling((target_vm_strain - cur_vm_strain)/(next_vm_strain - cur_vm_strain))
            cur_vm_strain = next_vm_strain
            call incs%add(StressIncrement(strain_rate, residual, strain_incs), prob_rem_incs)
        end do

        increments = incs%get()
    end subroutine

    subroutine macro_strain_driven_deformation(mat, velocity_gradient, target_vm_strain, increments)
        type(Material), target, intent(inout):: mat
        real(DP), dimension(3,3), intent(in):: velocity_gradient
        real(DP), intent(in)::                 target_vm_strain
        type(StrainIncrement), dimension(:), allocatable, intent(out):: increments

        increments = deform(mat%cp_model, mat%clusters, velocity_gradient, target_vm_strain)
    end subroutine
end module
