module cluster_module
    use utils
    use grain_module

    implicit none

    type Cluster
        type(Grain), dimension(:), allocatable:: grains
        integer, dimension(:), allocatable:: ind_basis_systems
        real(DP):: weight                                    !> Measure of importance of the cluster with respect to the whole microstructure
        real(DP), dimension(3, 3):: boundary_reference_frame !> Rotation matrix for boundary frame in ACTIVE notation (for performance)
        real(DP), dimension(:), allocatable:: imposed_strain, &
                                              slip_rates, &
                                              overstress, &
                                              rss
        real(DP), dimension(:,:), allocatable:: taylor_coeffs, &
                                                spin_coeffs, &
                                                crss, &
                                                inverse_basis
    end type
end module
