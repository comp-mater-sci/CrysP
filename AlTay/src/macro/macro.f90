module macro
    use base_defs
    use cluster_module
    use stress_mode
    use deformation
    use meso

    implicit none
    private

    class(Cluster), dimension(:), allocatable:: clusters

    character(*), parameter:: MOD_NAME = 'Simul'


    public:: macro_init, &
             macro_finalize, &
             macro_get_stress, &
             macro_deform, &
             clusters
    contains

    ! initialization call
    subroutine macro_init(clstrs)
        class(Cluster), dimension(:), allocatable, intent(inout):: clstrs

        call move_alloc(clstrs, clusters)
    end subroutine

    function macro_get_stress(velocity_gradient) result(stress)
        real(DP), dimension(3,3), intent(in):: velocity_gradient
        real(DP), dimension(3,3):: stress

        stress = get_stress(clusters, velocity_gradient)
    end function

    subroutine macro_deform(velocity_gradient, stress, taylor_factor)
        real(DP), dimension(3,3), intent(in):: velocity_gradient
        real(DP), dimension(3,3), intent(out):: stress
        real(DP), intent(out):: taylor_factor

        call deform(clusters, velocity_gradient, stress, taylor_factor)
    end subroutine

    subroutine macro_finalize()
        call meso_finalize()
        deallocate(clusters)
    end subroutine
end module
