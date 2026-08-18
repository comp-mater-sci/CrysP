!> Module defining the abstract cluster type, which serves as the fundamental abstraction of the mesoscopic level.
!>
!> @note
!> This module is needed because defining the cluster type in the top-level meso module would create a circular dependency.
!> @endnote
module crysp_cluster
    use base_defs
    use crysp_grain
    use crysp_serialization

    implicit none

    private
    public:: ClusterState, &
             Cluster

    type, extends(State):: ClusterState
        real(DP):: weight = 1._DP !! Measure of importance of the cluster with respect to the whole microstructure
    contains
        procedure:: serialize => cluster_serialize
        procedure:: deserialize => cluster_deserialize
    end type

    !> Unit of abstraction at the mesoscopic level.
    !>
    !> The cluster can be seen as the atomistic material unit with respect to the response to an applied deformation.
    !> Mesoscopic models are expected to extend this type and add fields for any cluster-specific state they need.
    type, abstract:: Cluster
        type(Grain), dimension(:), allocatable:: grains !! List of grains making up this cluster.
        type(ClusterState):: state
    end type

contains

    pure function cluster_serialize(this) result(params)
        class(ClusterState), intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        params = [serialize(this%weight)]
    end function

    function cluster_deserialize(this, params) result(params_)
        class(ClusterState), intent(out):: this
        type(Parameter), dimension(:), intent(in):: params
        type(Parameter), dimension(:), allocatable:: params_

        this%weight = params(1)
        params_ = params .pop. 1
    end function

end module

