module full_constraints_taylor
    use utils
    use cluster_module
    use logging
    use taylor_ambiguity
    use hardening

    implicit none

    private
    public:: MesoModelTaylor

    character(*), parameter:: MOD_NAME = "full_constraints_taylor"
    integer, dimension(5), parameter::   INITIAL_BASIS_SYSTEMS_FCC(5) = [2, 5, 6, 7, 8], &
                                       INITIAL_BASIS_SYSTEMS_BCC(5) = [1, 2, 4, 5, 7]

    type, extends(Cluster):: TaylorCluster
        integer, dimension(5):: ind_basis_systems
        real(DP), dimension(5, 5):: inverse_basis
    end type

    real(DP), dimension(3, 3):: imposed_spin_rate

contains

    !>@Brief Attempt to convert a generic cluster to a taylor cluster
    !>@Details If the provided cluster pointer is of type TaylorCluster, an equivalent pointer of type taylor_cluster is reterned.
    !If not, the program crashes. As such, this procedure acts as a safe type cast.
    function to_taylor_cluster(cluster_ptr) result(taylor_cluster_ptr)
       class(Cluster), pointer, intent(in):: cluster_ptr
       type(TaylorCluster), pointer:: taylor_cluster_ptr

       select type (cluster_ptr)
            type is (TaylorCluster)
                taylor_cluster_ptr = cluster_ptr
            class default
                call log_error(MOD_NAME, 'to_taylor_cluster', ERR_TYPE, "Cluster is not a TaylorCluster!")
       end select
    end function

    !>@Brief See 'meso_model_init'.
    function full_constraints_taylor_init(orientations, deformation_mechanism) result(clusters)
        real(DP), dimension(:,:), intent(in):: orientations
        integer, dimension(:,:,:), intent(in):: deformation_mechanism
        class(Cluster), dimension(:), pointer, contiguous:: clusters

        type(TaylorCluster), dimension(:), allocatable, target:: taylor_clusters
        real(DP), dimension(5, size(deformation_mechanism, 3)):: taylor_coeffs
        integer:: i

        allocate(taylor_clusters(size(orientations, 2)))
        do i = 1, size(taylor_clusters)
            allocate(taylor_clusters(i)%grains(1))
            taylor_clusters(i)%weight = 1._DP
            taylor_clusters(i)%ind_basis_systems = merge(INITIAL_BASIS_SYSTEMS_FCC, INITIAL_BASIS_SYSTEMS_BCC, size(deformation_mechanism, 3) == 12)
            call taylor_clusters(i)%grains(1)%init(deformation_mechanism, orientations(:,i))
            taylor_coeffs = taylor_clusters(i)%grains(1)%get_taylor_coeffs()
            taylor_clusters(i)%inverse_basis = invert(taylor_coeffs(:,taylor_clusters(i)%ind_basis_systems))
            call taylor_clusters(i)%grains(i)%set_crss(hardening_get_crss(i, 0._DP))
        end do

        clusters => taylor_clusters
    end function

    pure function get_taylor_coeffs(taylor_cluster_ptr) result(coeffs)
        type(TaylorCluster), intent(in):: taylor_cluster_ptr
        real(DP), dimension(5, size(taylor_cluster_ptr%grains(1)%slip_systems)):: coeffs

        integer:: i

        do i = 1, size(taylor_cluster_ptr%grains(1)%slip_systems)
            coeffs(:,i) = taylor_cluster_ptr%grains(1)%slip_systems(i)%taylor_coeffs
        end do
    end function

    pure function get_crss(taylor_cluster_ptr) result(crss)
        type(TaylorCluster), intent(in):: taylor_cluster_ptr
        real(DP), dimension(2, size(taylor_cluster_ptr%grains(1)%slip_systems)):: crss

        integer:: i

        do i = 1, size(taylor_cluster_ptr%grains(1)%slip_systems)
            crss(:,i) = taylor_cluster_ptr%grains(1)%slip_systems(i)%crss
        end do
    end function

    !>@Brief See 'meso_model_get_stress'.
    function full_constraints_taylor_get_stress(cluster_ptr, velocity_gradient) result(stress)
        class(Cluster), pointer, intent(in):: cluster_ptr
        real(DP), dimension(3, 3), intent(in):: velocity_gradient
        real(DP), dimension(3, 3):: stress

        real(DP), dimension(size(cluster_ptr%grains(1)%slip_systems)):: slip_rates, &
                                                                        overstress, &
                                                                        rss
        real(DP), dimension(5):: stress_cluster
        type(TaylorCluster), pointer:: taylor_cluster_ptr

        !Convert cluster_ptr to type TaylorCluster to access model-specific fields
        taylor_cluster_ptr = to_taylor_cluster(cluster_ptr)

        call simplex_solve(get_taylor_coeffs(taylor_cluster_ptr), &
                           convert_stress_strain_space(velocity_gradient .toframe. taylor_cluster_ptr%grains(1)%orientation), &
                           get_crss(taylor_cluster_ptr), &
                           taylor_cluster_ptr%inverse_basis, &
                           taylor_cluster_ptr%ind_basis_systems, &
                           slip_rates, &
                           stress_cluster, &
                           rss, &
                           overstress)

        stress = convert_stress_strain_space(stress_cluster)
    end function

    subroutine full_constraints_taylor_prepare_deformation(velocity_gradient)
        real(DP), dimension(3, 3), intent(in):: velocity_gradient

        imposed_spin_rate = antisymmetric_part(velocity_gradient)
    end subroutine

    !>@Brief See 'meso_model_apply_step'.
    subroutine full_constraints_taylor_deform(cluster_ptr, index_cluster, stress, slip)
        class(Cluster), pointer, intent(in):: cluster_ptr
        integer, intent(in):: index_cluster
        real(DP), dimension(3, 3), intent(out):: stress
        real(DP), intent(out):: slip

        real(DP)::                  orientation_increment(3, 3), &
                                    taylor_coeffs(5, size(cluster_ptr%grains(1)%slip_systems))
        integer::                   i, &
                                    n_systems, &
                                    n_overstressed_slip_systems, &
                                    n_active_simplex, &
                                    ind_overstressed_slip_systems(8)  ! Theoretical maximum of overstressed systems is 8
        real(DP), dimension(size(cluster_ptr%grains(1)%slip_systems)):: slip_rates, &
                                                     overstress, &
                                                     rss
        real(DP), dimension(5):: stress_cluster, &
                                 imposed_strain_rate

        type(Grain), pointer::      grain_ptr
        type(TaylorCluster), pointer:: taylor_cluster_ptr
        character(*), parameter::   PROC_NAME = 'full_constraints_taylor_apply_step'

        taylor_cluster_ptr = to_taylor_cluster(cluster_ptr)
        grain_ptr => cluster_ptr%grains(1)
        n_systems = size(grain_ptr%slip_systems)

        imposed_strain_rate = convert_stress_strain_space(velocity_gradient .toframe. grain_ptr%orientation)
        taylor_coeffs = get_taylor_coeffs(taylor_cluster_ptr)

        call simplex_solve(taylor_coeffs, &
                           imposed_strain_rate, &
                           get_crss(taylor_cluster_ptr), &
                           taylor_cluster_ptr%inverse_basis, &
                           taylor_cluster_ptr%ind_basis_systems, &
                           slip_rates, &
                           stress_cluster, &
                           rss, &
                           overstress)

        stress = convert_stress_strain_space(stress_cluster)

        !Determine if taylor ambiguity may be occuring. While we are iterating over the slip systems, might as well prepare for
        !resolving it
        n_overstressed_slip_systems = 0
        n_active_simplex = 0
        do i = 1, n_systems
            if (abs(overstress(i)) < TOLERANCE) then
                n_overstressed_slip_systems = n_overstressed_slip_systems+1
                ind_overstressed_slip_systems(n_overstressed_slip_systems) =i
                if (slip_rates(i) > TOLERANCE) &
                    n_active_simplex = n_active_simplex+1
            end if
        enddo

        !Check if the number of overstressed slip systems is within theoretical bounds.
        if (n_overstressed_slip_systems > 8) then
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Too many active slip systems.')
        elseif (n_overstressed_slip_systems == 0) then
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'No active slip systems found.')
        endif

        !If any of the overstressed slip systems has 0 slip, taylor ambiguity may be occuring.
        if (n_overstressed_slip_systems > n_active_simplex) then
            !Determine strain absorbed by slip systems (imposed strain-relaxations)

            slip_rates = resolve_taylor_ambiguity(ind_overstressed_slip_systems(1:n_overstressed_slip_systems), &
                rss(ind_overstressed_slip_systems(1:n_overstressed_slip_systems)), &
                imposed_strain_rate, &
                grain_ptr%get_taylor_coeffs(), &
                n_active_simplex)
        end if

        slip = sum(abs(slip_rates))
        grain_ptr%sum_slip = grain_ptr%sum_slip+slip

        !Update hardening model state
        call hardening_update_state(index_cluster, 1._DP, slip_rates)
        call grain_ptr%set_crss(hardening_get_crss(index_cluster, grain_ptr%sum_slip))

        orientation_increment = UNIT_MATRIX_3X3 &
                                +(imposed_spin_rate .toframe. grain_ptr%orientation) &                !>Change of reference frame
                                -convert_spin(matmul(grain_ptr%get_spin_coeffs(), slip_rates))    !>Spin induced by activation of slip systems
        grain_ptr%orientation = matmul(orientation_increment, grain_ptr%orientation)
    end subroutine
end module
