!> Module defining the abstract cluster type, which serves as the fundamental abstraction of the mesoscopic level.
!>
!> @note
!> This module is needed because defining the cluster type in the top-level meso module would create a circular dependency.
!> @endnote
module crysp_cluster
    use base_defs
    use crysp_grain
    use constitutive_model
    use crysp_parameters
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
        procedure:: serialize => cluster_serialize
        procedure:: deserialize => cluster_deserialize
    end type

    type, bind(C):: Cluster_C
        integer(C_INT):: n_grains
        type(C_PTR):: grains      !! n_grains * type(Grain_C)
        real(C_DOUBLE):: weight
    end type

    interface assignment(=)
        module procedure cluster_to_c, cluster_from_c
    end interface

contains

    subroutine cluster_to_c(clstr_c, clstr)
        type(Cluster_C), intent(out):: clstr_c
        type(Cluster), intent(in):: clstr

    end subroutine

    subroutine cluster_from_c(clstr, clstr_c)
        type(Cluster), intent(out):: clstr
        type(Cluster_C), intent(in):: clstr_c

    end subroutine

end module
