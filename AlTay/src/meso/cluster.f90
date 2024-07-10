module cluster_module
    use utils
    use grain_module

    implicit none

    type Cluster
        type(Grain), dimension(:), pointer:: grains
        real(DP):: weight
        real(DP), dimension(:,:), allocatable:: inverse_basis, &
                                                taylor_coeffs, &
                                                spin_coeffs
        real(DP), dimension(:,:,:), allocatable:: spin_coeffs_relaxations
        integer, dimension(:), allocatable:: ind_basis_systems
    end type
end module
