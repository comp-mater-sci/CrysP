module macro
    use base_defs
    use cluster_module
    use mode
    use deformation
    use meso

    implicit none
    private

    class(Cluster), dimension(:), allocatable:: clusters

    character(*), parameter:: MOD_NAME = 'Simul'


    public:: macro_init, &
             macro_finalize, &
             macro_simulate_stress_mode, &
             macro_simulate_strain_mode, &
             macro_deform, &
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

    subroutine macro_deform(velocity_gradient, total_strain, increments)
        real(DP), dimension(3,3), intent(in):: velocity_gradient
        real(DP), intent(in)::                 total_strain
        type(Increment), dimension(:), allocatable, intent(out):: increments

        call deform(clusters, velocity_gradient, total_strain, increments)
    end subroutine

    subroutine macro_finalize()
        call meso_finalize()
        deallocate(clusters)
    end subroutine
end module
