!> Top-level module of the macro layer
!>
!> Assimilates all the logic from the underlying layers and performs operattions on the material as a whole.
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
    use crysp_micro
    use incrementation
    use deformation
    use mode
    use conversions
    use crysp_meso_model
    use crysp_material
    use crysp_status

    implicit none
    public

    character(*), parameter, private:: MOD_NAME = 'macro'

contains

    function macro_simulate_stress_mode(mat, stress_mode, strain_mode, stress, residual) result(stat)
        type(Material), intent(inout):: mat
        real(DP), dimension(5), intent(in)::  stress_mode
        real(DP), dimension(5), intent(inout):: strain_mode

        real(DP), dimension(5), intent(out):: stress
        real(DP), dimension(5), intent(out):: residual
        type(Status):: stat

        stat = simulate_stress_mode(mat%meso_model, mat%clusters, stress_mode, strain_mode, stress, residual)
    end function

    function macro_simulate_strain_mode(mat, strain_mode, stress) result(stat)
        type(Material), intent(inout):: mat
        real(DP), dimension(5), intent(in)::  strain_mode
        real(DP), dimension(5), intent(out):: stress
        type(Status):: stat

        stat = simulate_strain_mode(mat%meso_model, mat%clusters, strain_mode, stress)
    end function

    function macro_stress_driven_deformation(mat, target_stress_mode, target_vm_strain, increments) result(stat)
        type(Material), target, intent(inout):: mat
        real(DP), dimension(5), intent(in):: target_stress_mode
        real(DP), intent(in):: target_vm_strain
        type(Increment), dimension(:), allocatable, intent(out):: increments
        type(Status):: stat

        integer:: prob_rem_incs, &
                  i, &
                  n_incs
        real(DP):: strain_rate(5), &
                   cur_vm_strain, &
                   next_vm_strain, &
                   stress(5), &
                   residual(5), &
                   def_grad(3,3)

        type(Increment), allocatable:: incs(:)
        type(IncrementFactory):: inc_fact


        cur_vm_strain = 0._DP
        def_grad = UNIT_MATRIX_3X3
        strain_rate = target_stress_mode

        do while (cur_vm_strain < target_vm_strain - TOLERANCE)
            stat = simulate_stress_mode(mat%meso_model, mat%clusters, target_stress_mode, strain_rate, stress, residual)
            if (failed(stat)) return

            stat = deform(mat, &
                          deviatoric_to_tensor(strain_rate), &
                          target_vm_strain - cur_vm_strain, &
                          incs, &
                          target_stress_mode)
            if (failed(stat)) return


            n_incs = size(incs)

            !Deform only sees the current deformation step,
            !so multiply deformation gradient of all increments with pre-existing deformation
            do i=1,n_incs
                incs(i)%deformation_gradient = matmul(incs(i)%deformation_gradient, def_grad)
                incs(i)%vm_strain = stretch_to_von_mises_true_strain(incs(i)%deformation_gradient)
            end do

            def_grad = incs(n_incs)%deformation_gradient
            next_vm_strain = incs(n_incs)%vm_strain
            !Add one for current increment and to make sure padding in incrementation is positive
            prob_rem_incs = ceiling((target_vm_strain - cur_vm_strain)/(next_vm_strain - cur_vm_strain)) * n_incs
            cur_vm_strain = next_vm_strain
            call inc_fact%add(incs, prob_rem_incs)
        end do

        increments = inc_fact%get()
    end function

    function macro_strain_driven_deformation(mat, velocity_gradient, target_vm_strain, increments) result(stat)
        type(Material), target, intent(inout):: mat
        real(DP), dimension(3,3), intent(in):: velocity_gradient
        real(DP), intent(in)::                 target_vm_strain
        type(Increment), dimension(:), allocatable, intent(out):: increments
        type(Status):: stat

        stat = deform(mat, velocity_gradient, target_vm_strain, increments)
    end function
end module
