module stress_mode
     use base_defs
     use cluster_module
     use omp_lib
     use meso

    implicit none

    private
    public:: get_stress

contains

    function get_stress(clusters, velocity_gradient) result(homogenized_stress)
        class(Cluster), dimension(:), intent(inout):: clusters
        real(DP), dimension(3, 3), intent(in):: velocity_gradient
        real(DP), dimension(3, 3):: homogenized_stress

        integer:: i
        real(DP):: total_weight

        homogenized_stress = 0._DP
        total_weight = 0._DP

        !$OMP PARALLEL SHARED(clusters, velocity_gradient)
            !$OMP DO SCHEDULE(DYNAMIC, 1) REDUCTION (+:total_weight, homogenized_stress)
                do i = 1, size(clusters)
                    homogenized_stress = homogenized_stress+meso_get_stress(clusters(i), velocity_gradient) * clusters(i)%weight
                    total_weight = total_weight+clusters(i)%weight
                end do
            !$OMP END DO
        !$OMP END PARALLEL

        homogenized_stress = homogenized_stress/total_weight
    end function
end module
