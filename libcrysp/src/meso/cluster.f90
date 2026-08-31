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
    public:: Cluster

    !> Unit of abstraction at the mesoscopic level.
    !>
    !> The cluster can be seen as the atomistic material unit with respect to the response to an applied deformation.
    !> Mesoscopic models are expected to extend this type and add fields for any cluster-specific state they need.
    type, abstract:: Cluster
        type(Grain), dimension(:), allocatable:: grains !! List of grains making up this cluster.
        real(DP):: weight = 1._DP !! Measure of importance of the cluster with respect to the whole microstructure
    contains
        size => cluster_size
        serialize => cluster_serialize
        deserialize => cluster_deserialize
    end type

contains

    pure function cluster_size(this) result(size)
        class(Cluster), intent(in):: this
        integer:: size

        integer:: i

        size = 1
        do i,size(this%grains)
            size = size + this%grains(i)%size()
        end do
        size = size + 1
    end function

    pure function cluster_serialize(this, phases) result(params)
        class(Cluster), target, intent(in):: this
        class(ConstitutiveModel), dimension(:), target, intent(in):: phases
        type(Parameter), dimension(this%size()):: params

        integer:: i, &
                  offset, &
                  size_grain

        params(1) = serialize(size(this%grains))
        offset = 1
        do i=1,size(this%grains)
            size_grain = this%grains(i)%size()
            params(offset+1:offset+size_grain) = this%grains(i)%serialize(phases)
            offset = offset + size_grain
        end do
        params(offset+1) = this%weight
    end function

    pure subroutine cluster_deserialize(this, params, phases)
        class(Cluster), target, intent(out):: this
        type(Parameter), dimension(this%size()), intent(in):: params
        class(ConstitutiveModel), dimension(:), target, intent(in):: phases

        integer:: i, &
                  offset, &
                  size_grain, &
                  n_grains

        n_grains = params(1)
        allocate(this%grains(n_grains))

        offset=0
        do i=1,n_grains
            call this%grains(i)%deserialize(params, phases)
        end do


        this%weight = params(1)
    end function

end module

