!> Module defining the abstract cluster type, which serves as the fundamental abstraction of the mesoscopic level.
!>
!> @note
!> This module is needed because defining the cluster type in the top-level meso module would create a circular dependency.
!> @endnote
module crysp_cluster
    use base_defs
    use crysp_grain
    use crysp_constitutive_model
    use crysp_parameters
    use crysp_model
    implicit none

    private
    public:: Cluster

    !> Unit of abstraction at the mesoscopic level.
    !>
    !> The cluster can be seen as the atomistic material unit with respect to the response to an applied deformation.
    !> Mesoscopic models extend this type and add fields for any cluster-specific state they need.
    !> @note
    !> This type is deliberately NOT abstract, even though it is only ever used as a base for concrete clusters, because concrete
    !> clusters call their parent's procedures via `this%Cluster%...`, and the Fortran standard only permits such a type-bound
    !> procedure reference when the parent part-name is not of abstract type (F2018 19.4.5).
    !> @endnote
    type, extends(State):: Cluster
        type(Grain), dimension(:), allocatable:: grains !! List of grains making up this cluster.
        real(DP):: weight !! Measure of importance of the cluster with respect to the whole microstructure
    contains
        procedure:: serialize => cluster_serialize
        procedure:: deserialize => cluster_deserialize
    end type

contains

    !> Base of the serialization chain for clusters. Serialization only covers state added by concrete clusters; the grains and
    !> weight are common to all clusters, so there is nothing to serialize here.
    !>
    !> The result is allocated empty rather than left unallocated so that extensions can append to it unconditionally.
    function cluster_serialize(this) result(params)
        class(Cluster), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        allocate(params(0))
    end function

    !> Counterpart of [[cluster_serialize]]: there is nothing to restore here.
    subroutine cluster_deserialize(this, params)
        class(Cluster), target, intent(inout):: this
        type(Parameter), dimension(:), intent(in):: params
    end subroutine
end module
