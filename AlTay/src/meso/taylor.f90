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
                apply_deformation_step

    real(DP):: inverse_basis_grain(5, 5), &
               imposed_strain_grain(5, 2), &
               slip_rates_relaxations(2)
    real(DP), allocatable ::  spin_slip_systems(:,:), &
                              spin_coeffs_relaxations(:,:,:), &
                              imposed_strain_cluster(:), &
                              taylor_coeffs_cluster(:,:)

    integer:: ind_basis_systems_grain(5)

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
    function taylor_init(deformation_mechanism, n_slip_systems_grain, taylor_coeffs_grain, cluster_size, file_name, initial_deformation_gradient) result(clusters)
        integer, intent(out):: n_slip_systems_grain  ! < total number of systems in slip system file (glide+twin)
        real(DP), intent(out), allocatable:: taylor_coeffs_grain(:,:)
        integer, intent(in):: cluster_size
        type(Cluster), dimension(:), allocatable, target:: clusters
        integer, dimension(:,:,:), intent(in):: deformation_mechanism
        character(len=*), intent(in):: file_name
        real(DP), dimension(3, 3), intent(in):: initial_deformation_gradient
        integer:: i, j, n_slip_systems_cluster, system_size, n_relaxations
        real(DP):: basis(5, 5), &
                    normalized(3, 2), &
                    tensor(3, 3)
        integer           :: file_handle, &
                             n_boundaries
        real(DP):: transformation_matrix(3, 3), &
                   angles(3)
        character(len = 40)  :: TitMic !<Microstructure title


        n_relaxations = merge(2, 0, cluster_size == 2)
        n_slip_systems_grain = size(deformation_mechanism, 3)
        n_slip_systems_cluster = cluster_size*n_slip_systems_grain+n_relaxations
        system_size = cluster_size*5

        allocate(taylor_coeffs_grain(5, n_slip_systems_grain))
        if (.not. allocated(spin_slip_systems)) then
            allocate(spin_slip_systems(3, n_slip_systems_grain))
            allocate(imposed_strain_cluster(system_size))
            allocate(taylor_coeffs_cluster(system_size, n_slip_systems_cluster))
            if (cluster_size == 2) &
                allocate(spin_coeffs_relaxations(3, 2, 2), source = 0._DP)
        end if

        ind_basis_systems_grain = merge(INITIAL_BASIS_SYSTEMS_FCC, INITIAL_BASIS_SYSTEMS_BCC, n_slip_systems_grain == 12)
        do i = 1, n_slip_systems_grain
            normalized = normalize(deformation_mechanism(:,:,i))
            tensor = outer_product(normalized(:,1), normalized(:,2))
            taylor_coeffs_grain(:,i) = convert_stress_strain_space(tensor)
            spin_slip_systems(:,i) = convert_spin(tensor)
        end do
        forall (i = 1:5) basis(:,i) = taylor_coeffs_grain(:,ind_basis_systems_grain(i))

        inverse_basis_grain = invert(basis)

        taylor_coeffs_cluster = 0.0_DP
        taylor_coeffs_cluster(1:5, 1:n_slip_systems_grain)=taylor_coeffs_grain
        if (cluster_size == 2) taylor_coeffs_cluster(6:10, n_slip_systems_grain+1:n_slip_systems_grain*2)=taylor_coeffs_grain

        !Temporary hack. Should move to FCTaylor and ALAMEL modules, respectively once they are available.
        if (cluster_size == 1) then
            allocate(clusters(size(grains)))
            do i = 1, size(clusters)
                clusters(i)%grains => grains(i:i)
                clusters(i)%weight = 1._DP
            end do
        else
            allocate(clusters(size(grains)/2))
            do i = 1, size(clusters)
                clusters(i)%grains => grains(2*i-1:2*i)
            end do

            !Read boundary orientations from file
            open (newunit = file_handle, file = file_name, status='old')
            read (file_handle, '(I5, 5x, A)') n_boundaries, TitMic  ! read number of grain boundaries and file title

            do i = 1, n_boundaries
                read (file_handle, '(3f10.0)') angles(3), angles(2), angles(1)  !read Euler angles from microstructure file in order: phi2, PHI, phi1
                !Calculate the transformation matrix
                !Cols 1 and 2 hold two non-parallel vectors within the initial GB (grain boundary) plane.
                !Col 3 holds a vector out of the initial GB plane (not necessarily perpendicular to the GB plane).
                transformation_matrix = matmul(initial_deformation_gradient, transpose(from_euler_angles(angles/RAD_TO_DEG)))

                !Assign boundaries to clusters
                !Because the number of boundaries is not necessary equal to the number of clusters, multiple clusters may have the
                !same boundary orientation.
                do j = i, size(clusters), 2*n_boundaries
                    clusters(j)%boundary_reference_frame = transformation_matrix
                end do
            enddo
            close(unit = file_handle)
        end if

    end function

    subroutine get_stress_state(cluster_ptr, index_cluster, n_slip_systems_grain, velocity_gradient, deformation_gradient, stress_state)
        type(Cluster), pointer, intent(in):: cluster_ptr
        integer, intent(in)::       n_slip_systems_grain, index_cluster
        real(DP), intent(in)::      velocity_gradient(3, 3), &
                                    deformation_gradient(3, 3)
        real(DP), intent(out)::     stress_state(3, 3)
        real(DP)::                  strain_relaxations(5), &
                                    boundary_to_crystal(3, 3), &
                                    relaxations_crystal_frame(3, 3), &
                                    stress_grain(5), &
                                    inverse_basis_cluster(5*size(cluster_ptr%grains), 5*size(cluster_ptr%grains))
        integer::                   n_slip_systems_cluster, &
                                    cluster_size, &
                                    size_system, &
                                    start_index_grain, &
                                    start_index_slip_systems, &
                                    start_index_relaxations, &
                                    i, &
                                    j, &
                                    ind_start, &
                                    ind_end, &
                                    ind_basis_systems_cluster(10), &
                                    n_relaxations
        character(*), parameter::   PROC_NAME = 'get_stress_state'
        real(DP), allocatable:: overstress(:), &
                                stress_cluster(:), &
                                rss_cluster(:), &
                                crss_cluster(:,:), &
                                slip_rates(:)


        cluster_size = size(cluster_ptr%grains)
        n_relaxations=(cluster_size-1)*2
        size_system = 5*cluster_size
        n_slip_systems_cluster = cluster_size*n_slip_systems_grain+n_relaxations
        start_index_relaxations = n_slip_systems_cluster-n_relaxations+1

        allocate(overstress(n_slip_systems_cluster))
        allocate(stress_cluster(size_system))
        allocate(rss_cluster(n_slip_systems_cluster))
        allocate(crss_cluster(2, n_slip_systems_cluster))
        crss_cluster = 0._DP
        allocate(slip_rates(n_slip_systems_cluster))

        if (cluster_size == 2) cluster_ptr%weight = cluster_weight(cluster_ptr, deformation_gradient)
        !Update microstructure
        inverse_basis_cluster = 0._DP
        ind_basis_systems_cluster(1:5) = ind_basis_systems_grain
        ind_basis_systems_cluster(6:10) = ind_basis_systems_grain+n_slip_systems_grain

        do i = 1, cluster_size
            start_index_grain = 5*(i-1)+1
            do j = 1, n_relaxations
                !Transform relaxation from boundary frame to crystal frame
                !Composed of rotation from boundary to global frame and then from global to crystal frame.
                boundary_to_crystal = matmul(cluster_ptr%grains(i)%orientation, transpose(cluster_frame(cluster_ptr%boundary_reference_frame, deformation_gradient)))
                relaxations_crystal_frame = rotate_to(real(RELAXATIONS(:,:,j), DP), boundary_to_crystal)
                !Invert direction of relaxations for second grain
                if (i == 2) relaxations_crystal_frame = -relaxations_crystal_frame
                !Rotational component of relaxations
                spin_coeffs_relaxations(:,j, i) = convert_spin(relaxations_crystal_frame) / SQR2
                !Insert the relaxations as columns in taylor_coeffs_cluster-matrix
                taylor_coeffs_cluster(start_index_grain:start_index_grain+4, start_index_relaxations-1+j)=convert_stress_strain_space(symmetric_part(relaxations_crystal_frame))
            end do

            imposed_strain_grain(1:5, i)=convert_stress_strain_space(velocity_gradient .toframe. cluster_ptr%grains(i)%orientation)
            imposed_strain_cluster(start_index_grain:start_index_grain+4)=imposed_strain_grain(1:5, i)
            ! Retrieve the CRSSmatrix
            start_index_slip_systems = n_slip_systems_grain*(i-1)+1
            crss_cluster(:,start_index_slip_systems:start_index_slip_systems+n_slip_systems_grain-1) = hardening_get_crss((index_cluster-1)*cluster_size+i, cluster_ptr%grains(i)%sum_slip)
            inverse_basis_cluster(start_index_grain:start_index_grain+4, start_index_grain:start_index_grain+4)=inverse_basis_grain
        enddo

        call simplex_solve(taylor_coeffs_cluster, imposed_strain_cluster, crss_cluster, inverse_basis_cluster, ind_basis_systems_cluster, slip_rates, stress_cluster, rss_cluster, overstress)

        stress_state = 0._DP
        do i = 1, cluster_size
            ind_start = 1 + (i-1) * n_slip_systems_grain
            ind_end = i*n_slip_systems_grain

            cluster_ptr%grains(i)%slip_systems%overstress = overstress(ind_start:ind_end)
            cluster_ptr%grains(i)%slip_systems%rss = rss_cluster(ind_start:ind_end)
            cluster_ptr%grains(i)%slip_systems%slip_rate = slip_rates(ind_start:ind_end)
            cluster_ptr%grains(i)%slip_systems%crss(1) = crss_cluster(1, ind_start:ind_end)
            cluster_ptr%grains(i)%slip_systems%crss(2) = crss_cluster(2, ind_start:ind_end)

            if (cluster_size == 2) slip_rates_relaxations(i) = slip_rates(2*n_slip_systems_grain+i)

            start_index_grain = 5*(i-1)
            stress_grain = stress_cluster(start_index_grain+1:start_index_grain+5)
            stress_state = stress_state + ((convert_stress_strain_space(stress_grain)) .fromframe. cluster_ptr%grains(i)%orientation)
        end do
        !Homogenize quantity over cluster
        stress_state = stress_state/cluster_size
    end subroutine

    subroutine apply_deformation_step(cluster_ptr, sum_slip, work_rate, imposed_spin, taylor_coeffs, n_slip_systems_grain, index_cluster)
        type(Cluster), pointer, intent(in):: cluster_ptr
        integer, intent(in)::       n_slip_systems_grain, index_cluster
        real(DP), intent(in)::      imposed_spin(3, 3), &
                                    taylor_coeffs(:,:)
        real(DP), intent(out)::     sum_slip, &
                                    work_rate
        real(DP)::                  orientation_increment(3, 3), &
                                    slip_rates_final(size(cluster_ptr%grains(1)%slip_systems)), &
                                    spin_relaxations(3), &
                                    strain_grain(5), &
                                    strain_relaxations(5), &
                                    spin_relaxations_matrix(3, 3), &
                                    sum_slip_current
        integer::                   i, j, cluster_size, &
                                    n_overstressed_slip_systems, &
                                    n_relaxations, &
                                    n_active_simplex, &
                                    start_index_grain, &
                                    start_index_slip_systems, &
                                    start_index_relaxations, &
                                    ind_overstressed_slip_systems(8)  ! Theoretical maximum of overstressed systems is 8
        type(SlipSystem), dimension(:), allocatable:: active_slip_systems
        type(Grain), pointer:: grain_
        character(*), parameter::   PROC_NAME = 'apply_deformation_step'

        cluster_size = size(cluster_ptr%grains)
        work_rate = 0._DP
        sum_slip = 0._DP

        do j = 1, size(cluster_ptr%grains)
            grain_ => cluster_ptr%grains(j)
            start_index_grain = 5*(j-1)
            start_index_slip_systems = n_slip_systems_grain*(j-1)
            n_relaxations = merge(2, 0, size(taylor_coeffs_cluster, 1) > 5)
            if (n_relaxations > 0) start_index_relaxations = 2*n_slip_systems_grain+1

            !Determine if taylor ambiguity may be occuring. While we are iterating over the slip systems, might as well prepare for
            !resolving it
            n_overstressed_slip_systems = 0
            n_active_simplex = 0
            do i = 1, n_slip_systems_grain
                if (abs(grain_%slip_systems(i)%overstress) < TOLERANCE) then
                    n_overstressed_slip_systems = n_overstressed_slip_systems+1
                    ind_overstressed_slip_systems(n_overstressed_slip_systems) =i
                    if (grain_%slip_systems(i)%slip_rate > TOLERANCE) &
                        n_active_simplex = n_active_simplex+1
                end if
            enddo

            !Check if the number of overstressed slip systems is within theoretical bounds.
            if (n_overstressed_slip_systems > 8) then
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Too many active slip systems.')
            elseif (n_overstressed_slip_systems == 0) then
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'No active slip systems found.')
            endif

            allocate(active_slip_systems(n_overstressed_slip_systems))
            active_slip_systems = grain_%slip_systems(ind_overstressed_slip_systems(1:n_overstressed_slip_systems))

            !If any of the overstressed slip systems has 0 slip, taylor ambiguity may be occuring.
            if (n_overstressed_slip_systems > n_active_simplex) then
                !Determine strain absorbed by slip systems (imposed strain-relaxations)
                strain_relaxations = 0._DP
                do i = 1, n_relaxations
                    strain_relaxations = strain_relaxations+taylor_coeffs_cluster(start_index_grain+1:start_index_grain+5, start_index_relaxations-1+i) * slip_rates_relaxations(i)
                end do
                strain_grain = imposed_strain_grain(:, j) - strain_relaxations

                slip_rates_final = resolve_taylor_ambiguity(ind_overstressed_slip_systems(1:n_overstressed_slip_systems), active_slip_systems%rss, strain_grain, taylor_coeffs, n_active_simplex)
            else
                slip_rates_final = 0._DP
                slip_rates_final(ind_overstressed_slip_systems(1:n_overstressed_slip_systems)) = active_slip_systems%slip_rate
            end if

            !Update hardening model state
            call hardening_update_state((index_cluster-1)*cluster_size+j, 1._DP, slip_rates_final)

            !Increment grain strain
            sum_slip_current = sum(abs(slip_rates_final))
            grain_%sum_slip = grain_%sum_slip+sum_slip_current
            sum_slip = sum_slip+sum_slip_current

            !Calculate work rate
            work_rate = work_rate+sum(merge(grain_%slip_systems%crss(1), -grain_%slip_systems%crss(2), slip_rates_final > 0._DP)*slip_rates_final)

            !Update grain orientation
            spin_relaxations = 0._DP
            do i = 1, n_relaxations
                spin_relaxations = spin_relaxations+spin_coeffs_relaxations(:,i, j) * slip_rates_relaxations(i)
            end do
            spin_relaxations_matrix = (convert_spin(spin_relaxations) * SQR2) .fromframe. grain_%orientation

            orientation_increment = UNIT_MATRIX_3X3 &
                                    -convert_spin(matmul(spin_slip_systems, slip_rates_final)) &    !Spin induced by activation of slip systems
                                    +(imposed_spin .toframe. grain_%orientation) &                         !Change of reference frame
                                    +(spin_relaxations_matrix .toframe. grain_%orientation)                !Spin absorbed by relaxations
            grain_%orientation = matmul(orientation_increment, grain_%orientation)

            deallocate(active_slip_systems)
        end do
        !Homogenize quantity over cluster
        sum_slip = sum_slip/cluster_size
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
        frame = transpose(normalize(frame))
    end function
end module
