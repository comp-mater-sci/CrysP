module full_constraints_taylor
    use utils
    use cluster_module
    use logging
    use taylor_ambiguity
    use micro
    use simplex
    use parameters
    use meso_model

    implicit none

    private
    public:: TaylorModel

    character(*), parameter:: MOD_NAME = "full_constraints_taylor"
    integer, dimension(5), parameter::   INITIAL_BASIS_SYSTEMS_FCC(5) = [2, 5, 6, 7, 8], &
                                       INITIAL_BASIS_SYSTEMS_BCC(5) = [1, 2, 4, 5, 7]

    type, extends(Cluster):: TaylorCluster
        integer, dimension(5):: ind_basis_systems
        real(DP), dimension(5, 5):: inverse_basis
    end type

    type, extends(MesoModel):: TaylorModel
    contains
        procedure:: init => full_constraints_taylor_init
        procedure:: get_stress => full_constraints_taylor_get_stress
        procedure:: apply_step => full_constraints_taylor_deform
    end type

contains

    !>@Brief Convert the type of a provided generic cluster to TaylorCluster
    !>@Details This is the closest Fortran can get to proper type casting.
    !>         Useful for accessing TaylorCluster-specific fields without the boilerplate of type selection and error handling in
    !>         each calling procedure.
    !>         If the input cluster is not a TaylorCluster, the routine crashes the program.
    !>@Return  If the input cluster is indeed a TaylorCluster, a pointer to this cluster of type TaylorCluster is returned.
    function to_taylor_cluster(cluster_) result(ptr)
        class(Cluster), target, intent(in):: cluster_   !> Generic input cluster
        type(TaylorCluster), pointer:: ptr              !> Pointer to input cluster of type TaylorCluster

        select type (cluster_)
            type is (TaylorCluster)
                ptr => cluster_
            class default
                call log_error(MOD_NAME, 'to_taylor_cluster', ERR_TYPE)
        end select
    end function

    !>@Brief See 'meso_model_init'.
    subroutine full_constraints_taylor_init(this, grains, params, clusters)
        class(TaylorModel), intent(inout):: this
        type(Grain), dimension(:), allocatable, intent(in):: grains
        type(Parameter), dimension(:), intent(in):: params
        class(Cluster), dimension(:), allocatable, intent(out):: clusters

        real(DP), dimension(5, size(grains(1)%slip_systems)):: taylor_coeffs
        integer:: i

        allocate(TaylorCluster:: clusters(size(grains)))

        !Must check type even though we just allocated due to Fortran semantics.
        select type (clusters)
            type is (TaylorCluster)
                do i = 1, size(clusters)
                    clusters(i)%grains = [grains(i)]
                    clusters(i)%weight = 1._DP
                    clusters(i)%ind_basis_systems = merge(INITIAL_BASIS_SYSTEMS_FCC, INITIAL_BASIS_SYSTEMS_BCC, size(grains(1)%slip_systems) == 12)
                    taylor_coeffs = clusters(i)%grains(1)%get_taylor_coeffs()
                    clusters(i)%inverse_basis = invert(taylor_coeffs(:,clusters(i)%ind_basis_systems))
                end do
        end select
    end subroutine

    pure function get_taylor_coeffs(taylor_cluster) result(coeffs)
        type(TaylorCluster), intent(in):: taylor_cluster
        real(DP), dimension(5, size(taylor_cluster%grains(1)%slip_systems)):: coeffs

        integer:: i

        do i = 1, size(taylor_cluster%grains(1)%slip_systems)
            coeffs(:,i) = taylor_cluster%grains(1)%slip_systems(i)%taylor_coeffs
        end do
    end function

    pure function get_crss(taylor_cluster) result(crss)
        type(TaylorCluster), intent(in):: taylor_cluster
        real(DP), dimension(2, size(taylor_cluster%grains(1)%slip_systems)):: crss

        integer:: i

        do i = 1, size(taylor_cluster%grains(1)%slip_systems)
            crss(:,i) = taylor_cluster%grains(1)%slip_systems(i)%crss
        end do
    end function

    !>@Brief See 'meso_model_get_stress'.
    function full_constraints_taylor_get_stress(this, cluster_, v_grad) result(stress)
        class(TaylorModel), intent(in):: this
        class(Cluster), target, intent(inout):: cluster_                    !> Intent(inout) because simplex modifies inverse basis
        real(DP), dimension(3, 3), intent(in):: v_grad
        real(DP), dimension(3, 3):: stress

        real(DP), dimension(size(cluster_%grains(1)%slip_systems)):: slip_rates, &
                                                                     rss
        real(DP), dimension(5):: stress_cluster
        type(TaylorCluster), pointer:: cluster_ptr

        cluster_ptr => to_taylor_cluster(cluster_)

        call simplex_solve(get_taylor_coeffs(cluster_ptr), &
                           convert_stress_strain_space(v_grad .toframe. cluster_ptr%grains(1)%orientation), &
                           get_crss(cluster_ptr), &
                           cluster_ptr%inverse_basis, &
                           cluster_ptr%ind_basis_systems, &
                           slip_rates, &
                           stress_cluster, &
                           rss)
        stress = convert_stress_strain_space(stress_cluster) .fromframe. cluster_%grains(1)%orientation
    end function

    subroutine full_constraints_taylor_prepare_deformation(this, v_grad)
        class(TaylorModel), intent(inout):: this
        real(DP), dimension(3, 3), intent(in):: v_grad !> Velocity gradient used for the next deformation step(s)

        this%velocity_gradient = v_grad
        this%imposed_spin_rate = antisymmetric_part(this%velocity_gradient)
    end subroutine

    !>@Brief See 'meso_model_apply_step'.
    subroutine full_constraints_taylor_deform(this, cluster_, index_cluster, stress, slip)
        class(TaylorModel), intent(in):: this
        class(Cluster), target, intent(inout):: cluster_
        integer, intent(in):: index_cluster
        real(DP), dimension(3, 3), intent(out):: stress
        real(DP), intent(out):: slip

        real(DP)::                  orientation_increment(3, 3), &
                                    taylor_coeffs(5, size(cluster_%grains(1)%slip_systems))
        integer::                   n_systems, &
                                    n_active_simplex
        real(DP), dimension(size(cluster_%grains(1)%slip_systems)):: slip_rates, &
                                                     rss
        real(DP), dimension(5):: stress_cluster, &
                                 imposed_strain_rate
        integer, dimension(:), allocatable:: ind_active_slip_systems
        type(TaylorCluster), pointer:: cluster_ptr

        cluster_ptr => to_taylor_cluster(cluster_)

        associate(grain_=>cluster_ptr%grains(1))
            n_systems = size(grain_%slip_systems)

            imposed_strain_rate = convert_stress_strain_space(this%velocity_gradient .toframe. grain_%orientation)
            taylor_coeffs = get_taylor_coeffs(cluster_ptr)

            call simplex_solve(taylor_coeffs, &
                               imposed_strain_rate, &
                               get_crss(cluster_ptr), &
                               cluster_ptr%inverse_basis, &
                               cluster_ptr%ind_basis_systems, &
                               slip_rates, &
                               stress_cluster, &
                               rss)

            stress = convert_stress_strain_space(stress_cluster) .fromframe. grain_%orientation



            call  assess_slip_system_activity(grain_, rss, slip_rates, n_active_simplex, ind_active_slip_systems)

            if (allocated(ind_active_slip_systems)) then
                slip_rates = resolve_taylor_ambiguity(ind_active_slip_systems, &
                    rss(ind_active_slip_systems), &
                    imposed_strain_rate, &
                    taylor_coeffs, &
                    n_active_simplex)
            end if

            slip = sum(abs(slip_rates))
            grain_%sum_slip = grain_%sum_slip+slip

            !Update hardening model state
            call micro_update_state(index_cluster, 1._DP, slip_rates)
            call grain_%set_crss(micro_get_crss(index_cluster, grain_%sum_slip))

            orientation_increment = UNIT_MATRIX_3X3 &
                                    +(this%imposed_spin_rate .toframe. grain_%orientation) &                !>Change of reference frame
                                    -convert_spin(matmul(grain_%get_spin_coeffs(), slip_rates))    !>Spin induced by activation of slip systems
            grain_%orientation = matmul(orientation_increment, grain_%orientation)
        end associate
    end subroutine
end module
