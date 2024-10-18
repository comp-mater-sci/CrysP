module full_constraints_taylor
    use utils
    use cluster_module
    use logging
    use taylor_ambiguity
    use hardening
    use simplex

    implicit none

    private
    public:: full_constraints_taylor_init, &
             full_constraints_taylor_prepare_deformation, &
             full_constraints_taylor_get_stress, &
             full_constraints_taylor_deform

    character(*), parameter:: MOD_NAME = "full_constraints_taylor"
    integer, dimension(5), parameter::   INITIAL_BASIS_SYSTEMS_FCC(5) = [2, 5, 6, 7, 8], &
                                       INITIAL_BASIS_SYSTEMS_BCC(5) = [1, 2, 4, 5, 7]

    type, extends(Cluster):: TaylorCluster
        integer, dimension(5):: ind_basis_systems
        real(DP), dimension(5, 5):: inverse_basis
    end type

    real(DP), dimension(3, 3):: imposed_spin_rate
    real(DP), dimension(3, 3):: velocity_gradient

contains

    !>@Brief Attempt to convert a generic cluster to a taylor cluster
    !>@Details If the provided cluster pointer is of type TaylorCluster, an equivalent pointer of type taylor_cluster is reterned.
    !If not, the program crashes. As such, this procedure acts as a safe type cast.
    function to_taylor_cluster(cluster_ptr) result(taylor_cluster_ptr)
       class(Cluster), target, intent(in):: cluster_ptr
       type(TaylorCluster), pointer:: taylor_cluster_ptr !> Pointer to the input cluster but with type TaylorCluster
                                                         !Note that we must use a pointer to make sure we are referencing the exact
                                                         !same cluster struct as the input.

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
        class(Cluster), dimension(:), allocatable, target:: clusters

        type(TaylorCluster), pointer:: taylor_cluster_ptr
        real(DP), dimension(5, size(deformation_mechanism, 3)):: taylor_coeffs
        integer:: i

        allocate(TaylorCluster:: clusters(size(orientations, 2)))
        select type (clusters)  ! Unfortunately Fortran does not allow a cleaner notation
            type is (TaylorCluster)
                do i = 1, size(clusters)
                    allocate(clusters(i)%grains(1))
                    clusters(i)%weight = 1._DP
                    clusters(i)%ind_basis_systems = merge(INITIAL_BASIS_SYSTEMS_FCC, INITIAL_BASIS_SYSTEMS_BCC, size(deformation_mechanism, 3) == 12)
                    call clusters(i)%grains(1)%init(deformation_mechanism, orientations(:,i))
                    taylor_coeffs = clusters(i)%grains(1)%get_taylor_coeffs()
                    clusters(i)%inverse_basis = invert(taylor_coeffs(:,clusters(i)%ind_basis_systems))
                    call clusters(i)%grains(1)%set_crss(hardening_get_crss(i, 0._DP))
                end do
        end select
    end function

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
    function full_constraints_taylor_get_stress(cluster_, v_grad) result(stress)
        class(Cluster), intent(inout):: cluster_                    !> Intent(inout) because simplex modifies inverse basis
        real(DP), dimension(3, 3), intent(in):: v_grad
        real(DP), dimension(3, 3):: stress

        real(DP), dimension(size(cluster_%grains(1)%slip_systems)):: slip_rates, &
                                                                        overstress, &
                                                                        rss
        real(DP), dimension(5):: stress_cluster

        select type(cluster_)
            type is (TaylorCluster)
                call simplex_solve(get_taylor_coeffs(cluster_), &
                                   convert_stress_strain_space(v_grad .toframe. cluster_%grains(1)%orientation), &
                                   get_crss(cluster_), &
                                   cluster_%inverse_basis, &
                                   cluster_%ind_basis_systems, &
                                   slip_rates, &
                                   stress_cluster, &
                                   rss, &
                                   overstress)
                stress = convert_stress_strain_space(stress_cluster) .fromframe. cluster_%grains(1)%orientation
        end select
    end function

    subroutine full_constraints_taylor_prepare_deformation(v_grad)
        real(DP), dimension(3, 3), intent(in):: v_grad !> Velocity gradient used for the next deformation step(s)

        velocity_gradient = v_grad
        imposed_spin_rate = antisymmetric_part(velocity_gradient)
    end subroutine

    !>@Brief See 'meso_model_apply_step'.
    subroutine full_constraints_taylor_deform(cluster_, index_cluster, stress, slip)
        class(Cluster), target, intent(inout):: cluster_
        integer, intent(in):: index_cluster
        real(DP), dimension(3, 3), intent(out):: stress
        real(DP), intent(out):: slip

        real(DP)::                  orientation_increment(3, 3), &
                                    taylor_coeffs(5, size(cluster_%grains(1)%slip_systems))
        integer::                   i, &
                                    n_systems, &
                                    n_overstressed_slip_systems, &
                                    n_active_simplex, &
                                    ind_overstressed_slip_systems(8)  ! Theoretical maximum of overstressed systems is 8
        real(DP), dimension(size(cluster_%grains(1)%slip_systems)):: slip_rates, &
                                                     overstress, &
                                                     rss
        real(DP), dimension(5):: stress_cluster, &
                                 imposed_strain_rate

        type(Grain), pointer::      grain_ptr
        character(*), parameter::   PROC_NAME = 'full_constraints_taylor_apply_step'

        select type (cluster_)
            type is (TaylorCluster)
                grain_ptr => cluster_%grains(1)
                n_systems = size(grain_ptr%slip_systems)

                imposed_strain_rate = convert_stress_strain_space(velocity_gradient .toframe. grain_ptr%orientation)
                taylor_coeffs = get_taylor_coeffs(cluster_)

                !print *, "Taylor coeffs: "
                !print '(5f6.2)', taylor_coeffs
                !print *, "Imposed strain rate: "
                !print '(5f6.2)', imposed_strain_rate
                !print *, "CRSS: "
                !print '(2f6.2)', get_crss(cluster_)

                call simplex_solve(taylor_coeffs, &
                                   imposed_strain_rate, &
                                   get_crss(cluster_), &
                                   cluster_%inverse_basis, &
                                   cluster_%ind_basis_systems, &
                                   slip_rates, &
                                   stress_cluster, &
                                   rss, &
                                   overstress)

                stress = convert_stress_strain_space(stress_cluster) .fromframe. grain_ptr%orientation

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
                        taylor_coeffs, &
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
        end select
    end subroutine
end module
