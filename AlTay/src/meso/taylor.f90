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
                update_cluster_state

    !Initial values for the inverse basis and basis systems.

    character(*), parameter:: MOD_NAME = 'taylor'
    integer, parameter::   INITIAL_BASIS_SYSTEMS_FCC(5) = [2, 5, 6, 7, 8], &
                           INITIAL_BASIS_SYSTEMS_BCC(5) = [1, 2, 4, 5, 7], &
                           RELAXATIONS(3, 3, 2) = reshape([0, 0, 0, &
                                                           0, 0, 0, &
                                                           1, 0, 0, &
                                                           0, 0, 0, &
                                                           0, 0, 0, &
                                                           0, 1, 0], shape(RELAXATIONS))

contains


    function taylor_init(deformation_mechanism, cluster_size, initial_deformation_gradient, orientations, boundaries) result(clusters)
        integer, intent(in):: cluster_size
        type(Cluster), dimension(:), pointer, contiguous:: clusters
        integer, dimension(:,:,:), intent(in):: deformation_mechanism
        real(DP), dimension(3, 3), intent(in):: initial_deformation_gradient
        real(DP), dimension(:,:), allocatable, intent(in):: orientations, &
                                                            boundaries
        integer:: i, j, &
                  n_slip_systems_grain, &
                  ind_basis_systems_grain(5)

        n_slip_systems_grain = size(deformation_mechanism, 3)

        if (cluster_size == 1) then
            allocate(clusters(size(orientations, 2)))
            do i = 1, size(clusters)
                call clusters(i)%init(orientations(:,i:i), deformation_mechanism)
            end do
        else
            allocate(clusters(size(orientations, 2)/2))
            j = 1
            do i = 1, size(clusters)
                call clusters(i)%init(orientations(:,2*i-1:2*i), deformation_mechanism, boundaries(:,j), initial_deformation_gradient)
                j = merge(1, j+1, j == size(boundaries, 2))
            end do
        end if

        ind_basis_systems_grain = merge(INITIAL_BASIS_SYSTEMS_FCC, INITIAL_BASIS_SYSTEMS_BCC, n_slip_systems_grain == 12)

        !Temporary hack. Should move to FCTaylor and ALAMEL modules, respectively once they are available.
        if (cluster_size == 1) then
            do i = 1, size(clusters)
                clusters(i)%ind_basis_systems = ind_basis_systems_grain
                clusters(i)%inverse_basis = invert(clusters(i)%taylor_coeffs(:,clusters(i)%ind_basis_systems))
            end do
        else
            do i = 1, size(clusters)
                clusters(i)%ind_basis_systems(1:5) = ind_basis_systems_grain
                clusters(i)%ind_basis_systems(6:10) = ind_basis_systems_grain+n_slip_systems_grain
                clusters(i)%inverse_basis = invert(clusters(i)%taylor_coeffs(:,clusters(i)%ind_basis_systems))
            end do
        end if
    end function

    subroutine get_stress_state(cluster_ptr, stress_state)
        type(Cluster), pointer, intent(in):: cluster_ptr
        real(DP), intent(out)::     stress_state(3, 3)
        real(DP)::                  stress_grain(5)
        integer::                   cluster_size, &
                                    start_index_grain, &
                                    i
        real(DP), dimension(5*size(cluster_ptr%grains)):: stress_cluster

        cluster_size = size(cluster_ptr%grains)

        call simplex_solve(cluster_ptr%taylor_coeffs, cluster_ptr%get_imposed_strain(), cluster_ptr%crss, cluster_ptr%inverse_basis, &
        cluster_ptr%ind_basis_systems, cluster_ptr%slip_rates, stress_cluster, cluster_ptr%rss, cluster_ptr%overstress)

        stress_state = 0._DP
        do i = 1, cluster_size
            start_index_grain = 5*(i-1)
            stress_grain = stress_cluster(start_index_grain+1:start_index_grain+5)
            stress_state = stress_state + ((convert_stress_strain_space(stress_grain)) .fromframe. cluster_ptr%grains(i)%orientation)
        end do

        if (cluster_size == 2) then
            do i = 1, 2
                cluster_ptr%relaxations(i)%slip_rate = cluster_ptr%slip_rates(size(cluster_ptr%slip_rates)-2+i)
            end do
        end if

        !Homogenize quantity over cluster
        stress_state = stress_state/cluster_size
    end subroutine

    subroutine apply_deformation_step(cluster_ptr, sum_slip, work_rate, imposed_spin, index_cluster, deformation_gradient, velocity_gradient)
        type(Cluster), pointer, intent(in):: cluster_ptr
        integer, intent(in)::       index_cluster
        real(DP), dimension(3, 3), intent(in)::  imposed_spin, &
                                                deformation_gradient, &
                                                velocity_gradient
        real(DP), intent(out)::     sum_slip, &
                                    work_rate
        real(DP)::                  orientation_increment(3, 3), &
                                    strain_grain(5), &
                                    strain_relaxations(5), &
                                    sum_slip_current, &
                                    overstress_grain(size(cluster_ptr%grains(1)%slip_systems)), &
                                    rss_grain(size(cluster_ptr%grains(1)%slip_systems))
        integer::                   i, j, cluster_size, &
                                    n_overstressed_slip_systems, &
                                    n_relaxations, &
                                    n_slip_systems_grain, &
                                    n_active_simplex, &
                                    ind_overstressed_slip_systems(8)  ! Theoretical maximum of overstressed systems is 8
        character(*), parameter::   PROC_NAME = 'apply_deformation_step'


        n_slip_systems_grain = size(cluster_ptr%grains(1)%slip_systems)
        n_relaxations = merge(2, 0, size(cluster_ptr%grains) > 1)
        cluster_size = size(cluster_ptr%grains)
        work_rate = 0._DP
        sum_slip = 0._DP

        do j = 1, size(cluster_ptr%grains)
            rss_grain = cluster_ptr%rss((j-1)*n_slip_systems_grain+1:j*n_slip_systems_grain)
            overstress_grain = cluster_ptr%overstress((j-1)*n_slip_systems_grain+1:j*n_slip_systems_grain)

            !Determine if taylor ambiguity may be occuring. While we are iterating over the slip systems, might as well prepare for
            !resolving it
            n_overstressed_slip_systems = 0
            n_active_simplex = 0
            do i = 1, n_slip_systems_grain
                if (abs(overstress_grain(i)) < TOLERANCE) then
                    n_overstressed_slip_systems = n_overstressed_slip_systems+1
                    ind_overstressed_slip_systems(n_overstressed_slip_systems) =i
                    if (cluster_ptr%grains(j)%slip_rates(i) > TOLERANCE) &
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
                if (n_relaxations > 0) strain_relaxations = matmul(cluster_ptr%taylor_coeffs(5*(j-1)+1:5*j, size(cluster_ptr%taylor_coeffs, 2)-1:), cluster_ptr%relaxations%slip_rate)
                strain_grain = cluster_ptr%grains(j)%imposed_strain-strain_relaxations

                cluster_ptr%grains(j)%slip_rates = resolve_taylor_ambiguity(ind_overstressed_slip_systems(1:n_overstressed_slip_systems), &
                    cluster_ptr%grains(j)%rss(ind_overstressed_slip_systems(1:n_overstressed_slip_systems)), &
                    strain_grain, &
                    cluster_ptr%grains(j)%taylor_coeffs, &
                    n_active_simplex)
            end if

            !Update hardening model state
            call hardening_update_state((index_cluster-1)*cluster_size+j, 1._DP, cluster_ptr%grains(j)%slip_rates)

            !Increment grain strain
            sum_slip_current = sum(abs(cluster_ptr%grains(j)%slip_rates))
            cluster_ptr%grains(j)%sum_slip = cluster_ptr%grains(j)%sum_slip+sum_slip_current
            sum_slip = sum_slip+sum_slip_current

            !Calculate work rate
            do i = 1, n_slip_systems_grain
                work_rate = work_rate+merge(cluster_ptr%grains(j)%crss(1, i), cluster_ptr%grains(j)%crss(2, i), cluster_ptr%grains(j)%slip_rates(i)>0._DP) * cluster_ptr%grains(j)%slip_rates(i)
            end do

            orientation_increment = UNIT_MATRIX_3X3 &
                                    +(imposed_spin .toframe. cluster_ptr%grains(j)%orientation) &                !>Change of reference frame
                                    -convert_spin(matmul(cluster_ptr%grains(j)%spin_coeffs, cluster_ptr%grains(j)%slip_rates))    !>Spin induced by activation of slip systems
            if (n_relaxations > 0) orientation_increment = orientation_increment-convert_spin(matmul(cluster_ptr%get_spin_coeffs_relaxations(j), cluster_ptr%relaxations%slip_rate))
            cluster_ptr%grains(j)%orientation = matmul(orientation_increment, cluster_ptr%grains(j)%orientation)
        end do

        call update_cluster_state(cluster_ptr, deformation_gradient, velocity_gradient, index_cluster)

        !Homogenize quantity over cluster
        sum_slip = sum_slip/cluster_size
    end subroutine

    subroutine update_cluster_state(cluster_ptr, deformation_gradient, velocity_gradient, index_cluster)
        type(Cluster), pointer, intent(in):: cluster_ptr
        real(DP), dimension(3, 3), intent(in):: deformation_gradient, &
                                                velocity_gradient
        integer, intent(in):: index_cluster
        integer:: i, &
                  j, &
                  start_index_grain, &
                  start_index_slip_systems, &
                  start_index_relaxations, &
                  n_slip_systems_grain, &
                  cluster_size
        real(DP):: boundary_to_crystal(3, 3, 2), &
                   relaxations_crystal_frame(3, 3), &
                   dummy(10), new_vec(10)

        cluster_size = size(cluster_ptr%grains)
        n_slip_systems_grain = size(cluster_ptr%grains(1)%slip_systems)
        start_index_relaxations = 2*n_slip_systems_grain+1

        if (cluster_size == 2) cluster_ptr%weight = cluster_weight(cluster_ptr, deformation_gradient)
        !Update microstructure
        do i = 1, cluster_size
            start_index_grain = 5*(i-1)+1
            start_index_slip_systems = n_slip_systems_grain*(i-1)+1
            cluster_ptr%grains(i)%imposed_strain = convert_stress_strain_space(velocity_gradient .toframe. cluster_ptr%grains(i)%orientation)
            cluster_ptr%crss(:,start_index_slip_systems:start_index_slip_systems+n_slip_systems_grain-1) = hardening_get_crss((index_cluster-1)*cluster_size+i, cluster_ptr%grains(i)%sum_slip)
            if (cluster_size == 2) then
                    !Transform relaxation from boundary frame to crystal frame
                    !Composed of rotation from boundary to global frame and then from global to crystal frame.
                    boundary_to_crystal(:,:,i) = matmul(cluster_ptr%grains(i)%orientation, cluster_frame(cluster_ptr%boundary_reference_frame, deformation_gradient))
            end if
        enddo

        !For ALAMEL we may assume that the relaxations are part of the basis and they change with every time step. Therefore we
        !must always recalculate the inverse basis.
        if (cluster_size == 2) then
            do i = 1, 2
                call cluster_ptr%relaxations(i)%update(boundary_to_crystal)
                cluster_ptr%taylor_coeffs(:,size(cluster_ptr%taylor_coeffs, 2)-2+i) = cluster_ptr%relaxations(i)%taylor_coeffs
            end do

            do i = 1, 10
                if (cluster_ptr%ind_basis_systems(i)> 2*n_slip_systems_grain) then
                    new_vec = matmul(cluster_ptr%inverse_basis, cluster_ptr%taylor_coeffs(:,cluster_ptr%ind_basis_systems(i)))
                    call update_inverse_basis(cluster_ptr%inverse_basis, new_vec, i, dummy)
                end if
            end do
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

    pure function cluster_frame(boundary_frame, deformation_gradient) result(frame)
        real(DP), dimension(3, 3), intent(in):: boundary_frame, &
                                               deformation_gradient
        real(DP):: frame(3, 3)

        frame = matmul(deformation_gradient, boundary_frame)
        frame(:,3) = frame(:,1) .cross. frame(:,2)
        frame(:,2) = frame(:,3) .cross. frame(:,1)
        frame = normalize(frame)
    end function
end module
