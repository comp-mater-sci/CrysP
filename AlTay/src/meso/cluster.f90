!> Module defining the abstract cluster type, which serves as the fundamental abstraction of the mesoscopic level.
!>
!> @note
!> The cluster type can not be defined in the top-level meso module as this would create a circular dependency.
!> @endnote
module cluster_module
    use utils
    use grain_module

    implicit none

    public

    !> Unit of abstraction at the mesoscopic level. Consists of one or more grains.
    !>
    !> Mesoscopic models are expected to extend this type and add fields for any cluster-specific state they need.
    type, abstract:: Cluster
        type(Grain), dimension(:), allocatable:: grains !! List of grains making up this cluster.
        real(DP)                              :: weight !! Measure of importance of the cluster with respect to the whole microstructure
    end type
end module

