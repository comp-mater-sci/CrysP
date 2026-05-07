module macro
    use base_defs
    use cluster_module
    use mode
    use deformation
    use meso
    use incrementation
    use conversions

    implicit none
    private

    class(Cluster), dimension(:), allocatable:: clusters

    character(*), parameter:: MOD_NAME = 'Simul'

    public:: macro_init, &
             macro_finalize, &
             macro_simulate_stress_mode, &
             macro_simulate_strain_mode, &
             macro_deform, &
             macro_stress_driven_deformation, &
             clusters

contains

    ! initialization call
    subroutine macro_init(clstrs)
        class(Cluster), dimension(:), allocatable, intent(inout):: clstrs

        call move_alloc(clstrs, clusters)
    end subroutine

    subroutine macro_simulate_stress_mode(stress_mode, strain_mode, stress, residual)
        real(DP), dimension(5), intent(in)::  stress_mode
        real(DP), dimension(5), intent(inout):: strain_mode

        real(DP), dimension(5), intent(out):: stress
        real(DP), dimension(5), intent(out):: residual

        call simulate_stress_mode(clusters, stress_mode, strain_mode, stress, residual)
    end subroutine

    subroutine macro_simulate_strain_mode(strain_mode, stress)
        real(DP), dimension(5), intent(in)::  strain_mode
        real(DP), dimension(5), intent(out):: stress

        call simulate_strain_mode(clusters, strain_mode, stress)
    end subroutine

    subroutine macro_stress_driven_deformation(target_stress_mode, target_vm_strain, increments)
        real(DP), dimension(5), intent(in):: target_stress_mode
        real(DP), intent(in):: target_vm_strain
        type(StressIncrement), dimension(:), allocatable, intent(out):: increments

        real(DP):: strain_rate(5), &
                   cur_vm_strain, &
                   stress(5), &
                   residual(5)
        type(StrainIncrement), dimension(:), allocatable:: strain_incs
        type(IncrementListBuilder):: inc_factory

        cur_vm_strain = 0._DP

        do while (cur_vm_strain < target_vm_strain - TOLERANCE)
            call simulate_stress_mode(clusters, target_stress_mode, strain_rate, stress, residual)
            strain_incs = deform(clusters, &
                                 deviatoric_to_tensor(strain_rate), &
                                 target_vm_strain - cur_vm_strain, &
                                 target_stress_mode)

            cur_vm_strain = cur_vm_strain + sum(strain_incs%vm_strain)

            call inc_factory%add(StressIncrement(strain_rate, &
                                                 residual, &
                                                 strain_incs))
        end do

        increments = inc_factory%get_stress_increments()
    end subroutine

    subroutine macro_deform(velocity_gradient, total_strain, increments)
        real(DP), dimension(3,3), intent(in):: velocity_gradient
        real(DP), intent(in)::                 total_strain
        type(StrainIncrement), dimension(:), allocatable, intent(out):: increments

        increments = deform(clusters, velocity_gradient, total_strain)
    end subroutine

    subroutine macro_finalize()
        call meso_finalize()
        deallocate(clusters)
    end subroutine
end module
