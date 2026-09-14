!> Module defining the abstract cluster type, which serves as the fundamental abstraction of the mesoscopic level.
!>
!> @note
!> This module is needed because defining the cluster type in the top-level meso module would create a circular dependency.
!> @endnote
module crysp_cluster
    use base_defs
    use crysp_grain
    use constitutive_model
    use crysp_serialization

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
    type:: Cluster
        type(Grain), dimension(:), allocatable:: grains !! List of grains making up this cluster.
        real(DP):: weight !! Measure of importance of the cluster with respect to the whole microstructure
    contains
        procedure:: size => cluster_size
        procedure:: serialize => cluster_serialize
        procedure:: deserialize => cluster_deserialize
    end type

contains

    pure function cluster_size(this) result(n)
        class(Cluster), intent(in):: this
        integer:: n

        integer:: i

        n = 1
        do i=1,size(this%grains)
            n = n + this%grains(i)%size()
        end do
        n = n + 1
    end function

    function cluster_serialize(this, phases) result(params)
        class(Cluster), target, intent(in):: this
        type(Phase), dimension(:), target, intent(in):: phases
        type(Parameter), dimension(:), allocatable:: params

        integer:: i, &
                  offset, &
                  size_grain
        type(Parameter), dimension(:), allocatable:: grain

        allocate(params(this%size()))

        params(1) = size(this%grains)
        offset = 1
        do i=1,size(this%grains)
            grain = this%grains(i)%serialize(phases)
            size_grain = size(grain)
            params(offset+1:offset+size_grain) = grain
            offset = offset + size_grain
        end do
        params(offset+1) = this%weight
    end function

    subroutine cluster_deserialize(this, params, phases)
        class(Cluster), target, intent(out):: this
        type(Parameter), dimension(:), intent(in):: params
        type(Phase), dimension(:), target, intent(in):: phases

        integer:: i, &
                  offset, &
                  size_grain, &
                  n_grains

        n_grains = params(1)
        allocate(this%grains(n_grains))

        offset=1
        do i=1,n_grains
            call this%grains(i)%deserialize(params(offset+1:), phases)
            offset = offset + this%grains(i)%size()
        end do

        this%weight = params(offset+1)
    end subroutine
end module
