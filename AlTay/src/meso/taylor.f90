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
                meso_update_model


    real(DP), dimension(3, 3):: deformation_gradient, &
                               deformation_gradient_during_time_step, &
                               next_deformation_gradient, &
                               deformation_gradient_increment

    character(*), parameter:: MOD_NAME = 'taylor'

contains

    function taylor_init(deformation_mechanism, cluster_size, initial_deformation_gradient, orientations, boundaries) result(clusters)
        integer, intent(in):: cluster_size
        real(DP), dimension(3, 3), intent(in):: initial_deformation_gradient
        type(Cluster), dimension(:), pointer, contiguous:: clusters
        integer, dimension(:,:,:), intent(in):: deformation_mechanism
        real(DP), dimension(:,:), allocatable, intent(in):: orientations, &
                                                            boundaries
        integer:: i, j, k, &
                  n_slip_systems_grain

        n_slip_systems_grain = size(deformation_mechanism, 3)
        deformation_gradient = UNIT_MATRIX_3X3

        if (cluster_size == 1) then
            allocate(clusters(size(orientations, 2)))
            do i = 1, size(clusters)
                call clusters(i)%init(orientations(:,i:i), deformation_mechanism)
                call clusters(i)%grains(1)%set_crss(hardening_get_crss(i, 0._DP))
            end do
        else
            allocate(clusters(size(orientations, 2)/2))
            j = 1
            do i = 1, size(clusters)
                call clusters(i)%init(orientations(:,2*i-1:2*i), deformation_mechanism, boundaries(:,j), initial_deformation_gradient)
                j = merge(1, j+1, j == size(boundaries, 2))

                do k = 1, cluster_size
                    call clusters(i)%grains(k)%set_crss(hardening_get_crss((i-1)*cluster_size+k, 0._DP))
                enddo

                call clusters(i)%update_relaxations(initial_deformation_gradient)
                clusters(i)%weight = cluster_weight(clusters(i), deformation_gradient)
            end do
        end if
    end function

    !Update the model after a time step has elapsed.
    !Note in the future the scope of an AlTay call should be limited to 1 velocity gradient. As such, the optional argument
    !velocity_gradient should move from this function to initialization.
    subroutine meso_update_model(velocity_gradient)
        real(DP), dimension(3, 3), intent(in), optional:: velocity_gradient

        if (present(velocity_gradient)) then
            deformation_gradient_increment = matrix_exponential_small_norm(velocity_gradient/2._DP)
        else
            deformation_gradient = next_deformation_gradient
        end if

        deformation_gradient_during_time_step = matmul(deformation_gradient_increment, deformation_gradient)
        next_deformation_gradient = matmul(deformation_gradient_increment, deformation_gradient_during_time_step)
    end subroutine

    !>@Brief Calculate the imposed strain rate vector corresponding to a certain velocity gradient for a cluster
    !>@Details Projects the velocity gradient onto the crystal frames of the grains comprising the cluster.
    pure function calc_imposed_strain_rate(cluster_ptr, velocity_gradient) result(imposed_strain_rate)
        type(Cluster), pointer, intent(in):: cluster_ptr
            real(DP), dimension(3, 3), intent(in):: velocity_gradient
        real(DP), dimension(size(cluster_ptr%grains)*5):: imposed_strain_rate

        integer:: i

        do i = 1, size(cluster_ptr%grains)
            imposed_strain_rate((i-1)*5+1:i*5) = convert_stress_strain_space(velocity_gradient .toframe. cluster_ptr%grains(i)%orientation)
        end do
    end function

    pure function homogenize_stress_state(cluster_ptr, stress_cluster) result(homogenized_stress)
        type(Cluster), pointer, intent(in):: cluster_ptr
        real(DP), dimension(5*size(cluster_ptr%grains)), intent(in):: stress_cluster
        real(DP), dimension(3, 3):: homogenized_stress

        integer:: i, &
                  cluster_size

        cluster_size = size(cluster_ptr%grains)

        homogenized_stress = 0._DP
        do i = 1, cluster_size
            homogenized_stress = homogenized_stress + (convert_stress_strain_space(stress_cluster(5*(i-1)+1:5*i)) .fromframe. cluster_ptr%grains(i)%orientation)
        end do
        homogenized_stress = homogenized_stress/cluster_size
    end function

    !>@Brief Get stress state for a cluster
    !>@details Calculate the homogenized stress over the cluster in the global frame
    function get_stress_state(cluster_ptr, velocity_gradient) result(stress)
        type(Cluster), pointer, intent(in):: cluster_ptr
        real(DP), dimension(3, 3), intent(in):: velocity_gradient
        real(DP), dimension(3, 3):: stress

        real(DP), dimension(cluster_ptr%n_systems):: slip_rates, &
                                                     overstress, &
                                                     rss
        integer::                   cluster_size, &
                                    i
        real(DP), dimension(5*size(cluster_ptr%grains)):: stress_cluster

        cluster_size = size(cluster_ptr%grains)

        call simplex_solve(cluster_ptr%get_taylor_coeffs(), &
                           calc_imposed_strain_rate(cluster_ptr, velocity_gradient), &
                           cluster_ptr%get_crss(), &
                           cluster_ptr%inverse_basis, &
                           cluster_ptr%ind_basis_systems, &
                           slip_rates, &
                           stress_cluster, &
                           rss, &
                           overstress)

        stress = homogenize_stress_state(cluster_ptr, stress_cluster)
    end function

    subroutine apply_deformation_step(cluster_ptr, imposed_spin, index_cluster, velocity_gradient, stress, slip)
        type(Cluster), pointer, intent(in):: cluster_ptr
        integer, intent(in)::       index_cluster
        real(DP), dimension(3, 3), intent(in)::  imposed_spin
        real(DP), dimension(3, 3), intent(in)::  velocity_gradient      !> Imposed velocity gradient for this time step
        real(DP), dimension(3, 3), intent(out):: stress                 !> Homogenized stress over the cluster
        real(DP), intent(out):: slip                                    !> Total slip in the cluster for this time step
        real(DP)::                  orientation_increment(3, 3), &
                                    strain_grain(5), &
                                    strain_relaxations(5), &
                                    slip_grain
        integer::                   i, j, cluster_size, &
                                    n_overstressed_slip_systems, &
                                    n_relaxations, &
                                    n_systems_grain, &
                                    n_active_simplex, &
                                    offset_systems, &
                                    offset_relaxations, &
                                    ind_overstressed_slip_systems(8)  ! Theoretical maximum of overstressed systems is 8
        real(DP), dimension(cluster_ptr%n_systems):: slip_rates, &
                                                     overstress, &
                                                     rss
        real(DP), dimension(5*size(cluster_ptr%grains)):: stress_cluster, &
                                                          imposed_strain_rate

        type(Grain), pointer::      grain_ptr
        character(*), parameter::   PROC_NAME = 'apply_deformation_step'

        n_systems_grain = size(cluster_ptr%grains(1)%slip_systems)
        n_relaxations = merge(2, 0, size(cluster_ptr%grains) > 1)
        cluster_size = size(cluster_ptr%grains)

        if (cluster_size == 2 ) then
            call cluster_ptr%update_relaxations(deformation_gradient_during_time_step)
            offset_relaxations = 2*n_systems_grain
        end if

        imposed_strain_rate = calc_imposed_strain_rate(cluster_ptr, velocity_gradient)

        call simplex_solve(cluster_ptr%get_taylor_coeffs(), imposed_strain_rate, cluster_ptr%get_crss(), cluster_ptr%inverse_basis, &
        cluster_ptr%ind_basis_systems, slip_rates, stress_cluster, rss, overstress)

        stress = homogenize_stress_state(cluster_ptr, stress_cluster)

        slip = 0._DP
        do j = 1, size(cluster_ptr%grains)
            grain_ptr => cluster_ptr%grains(j)
            offset_systems = (j-1)*n_systems_grain

            associate (slip_rates_grain=>slip_rates(offset_systems+1:offset_systems+n_systems_grain), &
                       slip_rates_relaxations=>slip_rates(offset_relaxations+1:))

                !Determine if taylor ambiguity may be occuring. While we are iterating over the slip systems, might as well prepare for
                !resolving it
                n_overstressed_slip_systems = 0
                n_active_simplex = 0
                do i = 1, n_systems_grain
                    if (abs(overstress(offset_systems+i)) < TOLERANCE) then
                        n_overstressed_slip_systems = n_overstressed_slip_systems+1
                        ind_overstressed_slip_systems(n_overstressed_slip_systems) =i
                        if (slip_rates_grain(i) > TOLERANCE) &
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
                    if (n_relaxations > 0) &
                        strain_relaxations = matmul(cluster_ptr%get_taylor_coeffs_relaxations(j), slip_rates_relaxations)
                    strain_grain = imposed_strain_rate((j-1)*5+1:j*5)-strain_relaxations

                    slip_rates_grain = resolve_taylor_ambiguity(ind_overstressed_slip_systems(1:n_overstressed_slip_systems), &
                        rss(ind_overstressed_slip_systems(1:n_overstressed_slip_systems)+(j-1)*n_systems_grain), &
                        strain_grain, &
                        grain_ptr%get_taylor_coeffs(), &
                        n_active_simplex)
                end if

                slip_grain = sum(abs(slip_rates_grain))
                grain_ptr%sum_slip = grain_ptr%sum_slip+slip_grain
                slip = slip+slip_grain

                !Update hardening model state
                call hardening_update_state((index_cluster-1)*cluster_size+j, 1._DP, slip_rates_grain)
                call grain_ptr%set_crss(hardening_get_crss((index_cluster-1)*cluster_size+j, grain_ptr%sum_slip))

                orientation_increment = UNIT_MATRIX_3X3 &
                                        +(imposed_spin .toframe. grain_ptr%orientation) &                !>Change of reference frame
                                        -convert_spin(matmul(grain_ptr%get_spin_coeffs(), slip_rates_grain))    !>Spin induced by activation of slip systems
                if (n_relaxations > 0) &
                    orientation_increment = orientation_increment-convert_spin(matmul(cluster_ptr%get_spin_coeffs_relaxations(j), slip_rates_relaxations))
                grain_ptr%orientation = matmul(orientation_increment, grain_ptr%orientation)
            end associate
        end do

        if (cluster_size == 2) then
            call cluster_ptr%update_relaxations(next_deformation_gradient)
            cluster_ptr%weight = cluster_weight(cluster_ptr, next_deformation_gradient)
        end if
    end subroutine

    real(DP) function cluster_weight(cluster_ptr, def_grad) result(weight)
        type(Cluster), pointer, intent(in):: cluster_ptr
        real(DP), intent(in):: def_grad(3, 3)
        real(DP):: grain_axes(3, 3), &
                   axis_lengths(3), &
                   alignment_factor

        !Applying deformation gradient to initial grain boundary orientation yields deformed grain axes
        grain_axes = matmul(def_grad, cluster_ptr%boundary_reference_frame)

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
