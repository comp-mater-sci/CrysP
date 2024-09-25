module taylor
    use utils
    use hardening
    use taylor_ambiguity
    use altayConfig, only: astate
    use logging
    use simplex
    use slip_systems
    use grain_module
    use cluster_module

    implicit none

    private
    public ::   taylor_init, &
                get_stress_state, &
                apply_deformation_step, &
                update_cluster_state, &
                cluster_weight

    !Initial values for the inverse basis and basis systems.

    character(*), parameter:: MOD_NAME = 'taylor'

contains

    function taylor_init(deformation_mechanism, cluster_size, initial_deformation_gradient, orientations, boundaries) result(clusters)
        integer, intent(in):: cluster_size
        type(Cluster), dimension(:), pointer, contiguous:: clusters
        integer, dimension(:,:,:), intent(in):: deformation_mechanism
        real(DP), dimension(3, 3), intent(in):: initial_deformation_gradient
        real(DP), dimension(:,:), allocatable, intent(in):: orientations, &
                                                            boundaries
        integer:: i, j, k, &
                  n_slip_systems_grain

        n_slip_systems_grain = size(deformation_mechanism, 3)

        if (cluster_size == 1) then
            allocate(clusters(size(orientations, 2)))
            do i = 1, size(clusters)
                call clusters(i)%init(orientations(:,i:i), deformation_mechanism)

                do j = 1, cluster_size
                    call clusters(i)%grains(j)%set_crss(hardening_get_crss((i-1)*cluster_size+j, clusters(i)%grains(j)%sum_slip))
                enddo
            end do
        else
            allocate(clusters(size(orientations, 2)/2))
            j = 1
            do i = 1, size(clusters)
                call clusters(i)%init(orientations(:,2*i-1:2*i), deformation_mechanism, boundaries(:,j), initial_deformation_gradient)
                j = merge(1, j+1, j == size(boundaries, 2))

                do k = 1, cluster_size
                    call clusters(i)%grains(k)%set_crss(hardening_get_crss((i-1)*cluster_size+k, clusters(i)%grains(k)%sum_slip))
                enddo

                call clusters(i)%update_relaxations(initial_deformation_gradient)
                clusters(i)%weight = cluster_weight(clusters(i), initial_deformation_gradient)
            end do
        end if
    end function

    subroutine get_stress_state(cluster_ptr, velocity_gradient)
        type(Cluster), pointer, intent(in):: cluster_ptr
        real(DP), dimension(3, 3), intent(in):: velocity_gradient
        real(DP), dimension(cluster_ptr%n_systems):: slip_rates, &
                                                     overstress, &
                                                     rss
        integer::                   cluster_size, &
                                    i
        real(DP), dimension(5*size(cluster_ptr%grains)):: stress_cluster

        cluster_size = size(cluster_ptr%grains)

        call cluster_ptr%set_imposed_strain_rate(velocity_gradient)

        call simplex_solve(cluster_ptr%get_taylor_coeffs(), cluster_ptr%get_imposed_strain(), cluster_ptr%get_crss(), cluster_ptr%inverse_basis, &
        cluster_ptr%ind_basis_systems, slip_rates, stress_cluster, rss, overstress)

        call cluster_ptr%set_slip_rates(slip_rates)
        call cluster_ptr%set_rss(rss)

        do i = 1, cluster_size
            cluster_ptr%grains(i)%stress = convert_stress_strain_space(stress_cluster(5*(i-1)+1:5*i)) .fromframe. cluster_ptr%grains(i)%orientation
        end do
    end subroutine

    subroutine apply_deformation_step(cluster_ptr, imposed_spin, index_cluster, deformation_gradient, velocity_gradient)
        type(Cluster), pointer, intent(in):: cluster_ptr
        integer, intent(in)::       index_cluster
        real(DP), dimension(3, 3), intent(in)::  imposed_spin
        real(DP), dimension(3, 3), intent(in)::  deformation_gradient
        real(DP), dimension(3, 3), intent(in)::  velocity_gradient
        real(DP)::                  orientation_increment(3, 3), &
                                    strain_grain(5), &
                                    strain_relaxations(5), &
                                    sum_slip
        integer::                   i, j, cluster_size, &
                                    n_overstressed_slip_systems, &
                                    n_relaxations, &
                                    n_slip_systems_grain, &
                                    n_active_simplex, &
                                    ind_system, &
                                    ind_overstressed_slip_systems(8)  ! Theoretical maximum of overstressed systems is 8
        real(DP), dimension(cluster_ptr%n_systems):: slip_rates, &
                                                     overstress, &
                                                     rss
        real(DP), dimension(5*size(cluster_ptr%grains)):: stress_cluster

        type(Grain), pointer::      grain_ptr
        character(*), parameter::   PROC_NAME = 'apply_deformation_step'


        n_slip_systems_grain = size(cluster_ptr%grains(1)%slip_systems)
        n_relaxations = merge(2, 0, size(cluster_ptr%grains) > 1)
        cluster_size = size(cluster_ptr%grains)

        call update_cluster_state(cluster_ptr, deformation_gradient, index_cluster)

        call cluster_ptr%set_imposed_strain_rate(velocity_gradient)

        call simplex_solve(cluster_ptr%get_taylor_coeffs(), cluster_ptr%get_imposed_strain(), cluster_ptr%get_crss(), cluster_ptr%inverse_basis, &
        cluster_ptr%ind_basis_systems, slip_rates, stress_cluster, rss, overstress)

        call cluster_ptr%set_slip_rates(slip_rates)
        call cluster_ptr%set_rss(rss)

        do i = 1, cluster_size
            cluster_ptr%grains(i)%stress = convert_stress_strain_space(stress_cluster(5*(i-1)+1:5*i)) .fromframe. cluster_ptr%grains(i)%orientation
        end do

        do j = 1, size(cluster_ptr%grains)
            grain_ptr => cluster_ptr%grains(j)
            ind_system = (j-1)*n_slip_systems_grain

            !Determine if taylor ambiguity may be occuring. While we are iterating over the slip systems, might as well prepare for
            !resolving it
            n_overstressed_slip_systems = 0
            n_active_simplex = 0
            do i = 1, n_slip_systems_grain
                ind_system = ind_system+1
                if (abs(overstress(ind_system)) < TOLERANCE) then
                    n_overstressed_slip_systems = n_overstressed_slip_systems+1
                    ind_overstressed_slip_systems(n_overstressed_slip_systems) =i
                    if (grain_ptr%slip_systems(i)%slip_rate > TOLERANCE) &
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
                strain_relaxations = 0._DP
                if (n_relaxations > 0) strain_relaxations = matmul(cluster_ptr%get_taylor_coeffs_relaxations(j), cluster_ptr%relaxations%slip_rate)
                strain_grain = grain_ptr%imposed_strain-strain_relaxations

                grain_ptr%slip_systems%slip_rate = resolve_taylor_ambiguity(ind_overstressed_slip_systems(1:n_overstressed_slip_systems), &
                    grain_ptr%slip_systems(ind_overstressed_slip_systems(1:n_overstressed_slip_systems))%rss, &
                    strain_grain, &
                    grain_ptr%get_taylor_coeffs(), &
                    n_active_simplex)
            end if

            sum_slip = sum(abs(grain_ptr%slip_systems%slip_rate))
            grain_ptr%sum_slip = grain_ptr%sum_slip+sum_slip
            grain_ptr%sum_slip_current = sum_slip

            !Update hardening model state
            call hardening_update_state((index_cluster-1)*cluster_size+j, 1._DP, cluster_ptr%grains(j)%slip_systems%slip_rate)

            orientation_increment = UNIT_MATRIX_3X3 &
                                    +(imposed_spin .toframe. grain_ptr%orientation) &                !>Change of reference frame
                                    -convert_spin(matmul(grain_ptr%get_spin_coeffs(), grain_ptr%slip_systems%slip_rate))    !>Spin induced by activation of slip systems
            if (n_relaxations > 0) orientation_increment = orientation_increment-convert_spin(matmul(cluster_ptr%get_spin_coeffs_relaxations(j), cluster_ptr%relaxations%slip_rate))
            grain_ptr%orientation = matmul(orientation_increment, grain_ptr%orientation)
        end do
    end subroutine

    subroutine update_cluster_state(cluster_ptr, deformation_gradient, index_cluster)
        type(Cluster), pointer, intent(in):: cluster_ptr
        real(DP), dimension(3, 3), intent(in):: deformation_gradient
        integer, intent(in):: index_cluster
        integer:: i, &
                  j, &
                  cluster_size

        cluster_size = size(cluster_ptr%grains)

        !Update microstructure
        do i = 1, cluster_size
            call cluster_ptr%grains(i)%set_crss(hardening_get_crss((index_cluster-1)*cluster_size+i, cluster_ptr%grains(i)%sum_slip))
        enddo

        if (cluster_size == 2 ) then
            call cluster_ptr%update_relaxations(deformation_gradient)
            cluster_ptr%weight = cluster_weight(cluster_ptr, deformation_gradient)
        end if
    end subroutine

    real(DP) function cluster_weight(cluster_ptr, deformation_gradient) result(weight)
        type(Cluster), pointer, intent(in):: cluster_ptr
        real(DP), intent(in):: deformation_gradient(3, 3)
        real(DP):: grain_axes(3, 3), &
                   axis_lengths(3), &
                   alignment_factor

        !Applying deformation gradient to initial grain boundary orientation yields deformed grain axes
        grain_axes = matmul(deformation_gradient, cluster_ptr%boundary_reference_frame)
        axis_lengths = norm2(grain_axes, 1)
        !Alignment factor equals sin(axes 2 and 3) * cos(axis 1 and normal to plane defined by axes 2 and 3)
        !The more the axes are orthogonal, the more alignment factor tends to 1.
        alignment_factor = abs(grain_axes(:,1) .dot. (grain_axes(:,2) .cross. grain_axes(:,3))) / product(axis_lengths)

        !See Van Houtte et. al., 2004: Appendix A
        select case(minloc(axis_lengths, 1))
            case(1)
                weight = alignment_factor * (2._DP*(axis_lengths(2)-axis_lengths(1))*axis_lengths(1)**2+4.D0*axis_lengths(1)**3/3._DP)
            case(2)
                weight = alignment_factor * (2._DP*(axis_lengths(1)-axis_lengths(2))*axis_lengths(2)**2+4.D0*axis_lengths(2)**3/3._DP)
            case (3)
                weight = alignment_factor * (4._DP*(axis_lengths(1)-axis_lengths(3))*(axis_lengths(2)-axis_lengths(3))*axis_lengths(3) + &
                2._DP*(axis_lengths(1)+axis_lengths(2) - 2._DP*axis_lengths(3))*axis_lengths(3)**2+4._DP*axis_lengths(3)**3/3._DP)
        end select
    end function
end module
