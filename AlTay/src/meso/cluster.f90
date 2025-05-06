!> Module defining the abstract cluster type, which serves as the fundamental abstraction of the mesoscopic level.
!>
!> @note
!> This module is needed because defining the cluster type in the top-level meso module would create a circular dependency.
!> @endnote
module cluster_module
    use utils
    use grain_module

    implicit none

    public

    !> Unit of abstraction at the mesoscopic level.
    !>
    !> The cluster can be seen as the atomistic material unit with respect to the response to an applied deformation.
    !> Mesoscopic models are expected to extend this type and add fields for any cluster-specific state they need.
    type, abstract:: Cluster
        type(Grain), dimension(:), allocatable:: grains !! List of grains making up this cluster.
        real(DP)                              :: weight !! Measure of importance of the cluster with respect to the whole microstructure
    end type
end module

