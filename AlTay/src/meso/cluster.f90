!> Module defining the abstract cluster type, which serves as the fundamental abstraction of the mesoscopic level.
!> Note the cluster type can not be defined in the top-level meso module as this would create a circular dependency.
module cluster_module
    use utils
    use grain_module

    implicit none

    public

    !> Unit of abstraction at the mesoscopic level. Consists of one or more grains. The concrete subtype determines how these are
    !stored.
    type, abstract:: Cluster
        type(Grain), dimension(:), allocatable:: grains
        real(DP):: weight      !> Measure of importance of the cluster with respect to the whole microstructure
    end type
end module

