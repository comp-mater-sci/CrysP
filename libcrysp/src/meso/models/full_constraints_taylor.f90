!> Traditional full constraints Taylor theory.
!>
!> Assumes all grains deform identically.

module full_constraints_taylor
    use base_defs
    use math_utils
    use conversions
    use cluster_module
    use logging
    use taylor_ambiguity
    use micro
    use simplex
    use parameters
    use crysp_meso_model

    implicit none

    private
    public:: TaylorModel

    character(*), parameter:: MOD_NAME = "full_constraints_taylor"

    !> Cluster-specific state needed for full constraints Taylor simulations.
    !>
    !> A Taylor cluster only holds 1 grain.
    type, extends(Cluster):: TaylorCluster
        integer, dimension(5):: ind_basis_systems
        real(DP), dimension(5, 5):: inverse_basis
    end type

    !> Model for full constraints Taylor simulations.
    type, extends(MesoModel):: TaylorModel
    contains
        procedure, nopass:: get_name => fctaylor_get_name
        procedure, nopass:: get_description => fctaylor_get_description
        procedure, nopass:: get_signature => fctaylor_get_signature
        procedure, nopass:: get_parameters => fctaylor_get_parameters
        procedure:: init       => fctaylor_init       !! Inherited from [[MesoModel]]
        procedure:: get_stress => fctaylor_get_stress !! Inherited from [[MesoModel]]
        procedure:: apply_step => fctaylor_deform     !! Inherited from [[MesoModel]]
    end type

contains

    pure function fctaylor_get_name() result(name)
        character(:), allocatable:: name

        name = "Full-Constraints Taylor"
    end function

    pure function fctaylor_get_description() result(description)
        character(:), allocatable:: description

        description = "Each grain is forced to deform exactly like the material as a whole."
    end function

    pure function fctaylor_get_signature() result(signature)
        integer, dimension(:), allocatable:: signature

        allocate(signature(0))
    end function

    pure function fctaylor_get_parameters() result(params)
         type(ParameterDescriptor), dimension(:), allocatable:: params

         allocate(params(0))
    end function

    !> Convert the type of a provided generic cluster to TaylorCluster
    !>
    !> This is the closest Fortran can get to proper type casting.
    !> Useful for accessing TaylorCluster-specific fields without the boilerplate of type selection and error handling in
    !> each calling procedure.
    !> If the input cluster is not a TaylorCluster, the routine crashes the program.
    !> If the input cluster is indeed a TaylorCluster, a pointer to this cluster of type TaylorCluster is returned.
    function to_taylor_cluster(cluster_) result(ptr)
        class(Cluster), target, intent(in):: cluster_   !! Generic input cluster
        type(TaylorCluster), pointer:: ptr              !! Pointer to input cluster of type TaylorCluster

        select type (cluster_)
            type is (TaylorCluster)
                ptr => cluster_
            class default
                call log_error(MOD_NAME, 'to_taylor_cluster', ERR_TYPE)
        end select
    end function

    !> See [[MesoModel:Init]]
    subroutine fctaylor_init(this, grains, params, clusters)
        class(TaylorModel), intent(inout):: this
        type(Grain), dimension(:), intent(in):: grains
        type(Parameter), dimension(:), intent(in):: params
        class(Cluster), dimension(:), allocatable, intent(out):: clusters

        real(DP), dimension(5, size(grains(1)%model%taylor_coeffs, 2)):: taylor_coeffs
        integer:: i

        allocate(TaylorCluster:: clusters(size(grains)))

        !Must check type even though we just allocated due to Fortran semantics.
        select type (clusters)
            type is (TaylorCluster)
                do i = 1, size(clusters)
                    clusters(i)%grains = [grains(i)]
                    clusters(i)%weight = 1._DP
                    clusters(i)%ind_basis_systems = grains(1)%model%basis
                    taylor_coeffs = clusters(i)%grains(1)%model%taylor_coeffs
                    clusters(i)%inverse_basis = invert(taylor_coeffs(:,clusters(i)%ind_basis_systems))
                end do
        end select
    end subroutine

    !> See [[MesoModel:get_stress]]
    function fctaylor_get_stress(this, cluster_, v_grad) result(stress)
        class(TaylorModel), intent(in):: this
        class(Cluster), target, intent(inout):: cluster_
        real(DP), dimension(3, 3), intent(in):: v_grad
        real(DP), dimension(3, 3):: stress

        real(DP), dimension(size(cluster_%grains(1)%model%taylor_coeffs, 2)):: slip_rates, &
                                                                     rss
        real(DP), dimension(5):: stress_cluster
        type(TaylorCluster), pointer:: cluster_ptr

        cluster_ptr => to_taylor_cluster(cluster_)

        associate (grain_ => cluster_ptr%grains(1))
            call simplex_solve(grain_%model%taylor_coeffs, &
                               tensor_to_deviatoric(v_grad .toframe. grain_%orientation), &
                               grain_%state%crss, &
                               cluster_ptr%inverse_basis, &
                               cluster_ptr%ind_basis_systems, &
                               slip_rates, &
                               stress_cluster, &
                               rss)
            stress = deviatoric_to_tensor(stress_cluster) .fromframe. grain_%orientation
        end associate
    end function

    !> See [[MesoModel:apply_step]]
    subroutine fctaylor_deform(this, cluster_, velocity_gradient, time, stress, slip)
        class(TaylorModel), intent(in):: this
        class(Cluster), target, intent(inout):: cluster_
        real(DP), dimension(3,3), intent(in):: velocity_gradient
        real(DP), intent(in):: time
        real(DP), dimension(3, 3), intent(out):: stress
        real(DP), intent(out):: slip

        integer::                   n_systems, &
                                    n_active_simplex
        real(DP), dimension(size(cluster_%grains(1)%model%taylor_coeffs, 2)):: slip_rates, &
                                                                               rss
        real(DP):: stress_cluster(5), &
                   imposed_strain_rate(5), &
                   lattice_spin(3,3), &
                   v_grad_grain(3,3)
        integer, dimension(:), allocatable:: ind_active_slip_systems
        type(TaylorCluster), pointer:: cluster_ptr

        cluster_ptr => to_taylor_cluster(cluster_)

        associate(grain_=>cluster_ptr%grains(1))

            n_systems = size(grain_%model%taylor_coeffs, 2)

            imposed_strain_rate = tensor_to_deviatoric(velocity_gradient .toframe. grain_%orientation)

            call simplex_solve(grain_%model%taylor_coeffs, &
                               imposed_strain_rate, &
                               grain_%state%crss, &
                               cluster_ptr%inverse_basis, &
                               cluster_ptr%ind_basis_systems, &
                               slip_rates, &
                               stress_cluster, &
                               rss)

            stress = deviatoric_to_tensor(stress_cluster) .fromframe. grain_%orientation

            call  assess_slip_system_activity(grain_, rss, slip_rates, n_active_simplex, ind_active_slip_systems)

            if (allocated(ind_active_slip_systems)) then
                slip_rates = resolve_taylor_ambiguity(ind_active_slip_systems, &
                    rss(ind_active_slip_systems), &
                    imposed_strain_rate, &
                    grain_%model%taylor_coeffs, &
                    n_active_simplex)
            end if

            slip = sum(abs(slip_rates))

            call grain_%deform(velocity_gradient, time, slip_rates, stress)
        end associate
    end subroutine
end module
