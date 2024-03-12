module taylor
    use utils
    use criMathUtils
    use hardening
    use taylor_ambiguity
    use altayConfig, only: astate
    use logging
    use simplex
    use slip_systems
    use altayDynfil

    implicit none

    private
    public ::   taylor_init, &
                get_stress_state, &
                apply_deformation_step

    integer  :: n_active_slip_systems
    real(DP):: inverse_basis_grain(5, 5), &
                spin_relaxations(3, 3), &
                SLIPLP(8), &
                TAURLP(8), &
                strain_grain(5), &
                imposed_strain_grain(5, 2), &
                slip_rates_relaxations(2)
    real(DP), allocatable ::  spin_slip_systems(:,:), &
                              spin_coeffs_relaxations(:,:,:), &
                              slip_rates(:), &
                              imposed_strain_cluster(:), &
                              crss_cluster(:,:), &
                              overstress(:), &
                              rss_cluster(:), &
                              stress_cluster(:), &
                              taylor_coeffs_cluster(:,:)

    integer:: ind_basis_systems_grain(5), ind_active_slip_systems(8)
    
    character(*), parameter:: MOD_NAME = 'taylor'
    integer, parameter::   INITIAL_BASIS_SYSTEMS_Fcrss_grain(5) = [2, 5, 6, 7, 8], &
                           INITIAL_BASIS_SYSTEMS_Bcrss_grain(5) = [1, 2, 4, 5, 7], &
                           RELAXATIONS(3, 3, 2) = reshape([0, 0, 0, &
                                                           0, 0, 0, &
                                                           1, 0, 0, &
                                                           0, 0, 0, &
                                                           0, 0, 0, &
                                                           0, 1, 0], shape(RELAXATIONS))

contains
    subroutine taylor_init(deformation_mechanism, n_slip_systems_grain, taylor_coeffs_grain, cluster_size)
        integer, intent(out):: n_slip_systems_grain  ! < total number of systems in slip system file (glide+twin)
        real(DP), intent(out), allocatable:: taylor_coeffs_grain(:,:)
        integer, intent(in):: cluster_size
        integer, dimension(:,:,:), intent(in):: deformation_mechanism
        integer:: i, n_slip_systems_cluster, system_size, n_relaxations
        real(DP):: basis(5, 5), &
                    normalized(3, 2), &
                    tensor(3, 3)
        
        n_relaxations = merge(2, 0, cluster_size == 2)    
        n_slip_systems_grain = size(deformation_mechanism, 3)
        n_slip_systems_cluster = cluster_size*n_slip_systems_grain+n_relaxations
        system_size = cluster_size*5

        allocate(taylor_coeffs_grain(5, n_slip_systems_grain))
        if (.not. allocated(spin_slip_systems)) then
            allocate(spin_slip_systems(3, n_slip_systems_grain))
            allocate(slip_rates(n_slip_systems_cluster))
            allocate(imposed_strain_cluster(system_size))
            allocate(crss_cluster(2, n_slip_systems_cluster))
            allocate(overstress(n_slip_systems_cluster))
            allocate(rss_cluster(n_slip_systems_cluster))
            allocate(stress_cluster(system_size))
            allocate(taylor_coeffs_cluster(system_size, n_slip_systems_cluster))
            if (cluster_size == 2) &
                allocate(spin_coeffs_relaxations(3, 2, 2), source = 0._DP)
        end if
        
        ind_basis_systems_grain = merge(INITIAL_BASIS_SYSTEMS_Fcrss_grain, INITIAL_BASIS_SYSTEMS_Bcrss_grain, n_slip_systems_grain == 12)
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
    end subroutine   
    
    subroutine get_stress_state(stress_matrix, orientation_grain, index_grain, strain_ab, cluster_size, index_in_cluster, crss_grain, n_slip_systems_grain, velocity_gradient, deformation_gradient, weight)
        integer, intent(in)::       index_in_cluster, &
                                    index_grain, &
                                    cluster_size, &
                                    n_slip_systems_grain
        real(DP), intent(out)::     stress_matrix(3, 3), &
                                    weight
        real(DP), intent(in)::      orientation_grain(3, 3, 2), &
                                    strain_ab(2), &
                                    velocity_gradient(3, 3), &
                                    deformation_gradient(3, 3)
        real(DP), intent(inout)::   crss_grain(2, n_slip_systems_grain)
        real(DP)::                  strain_relaxations(5), &
                                    spin(3), &
                                    boundary_to_crystal(3, 3), &
                                    relaxations_crystal_frame(3, 3), &
                                    spanv(5), &
                                    inverse_basis_cluster(5*cluster_size, 5*cluster_size)
        integer::                   n_slip_systems_cluster, &
                                    size_system, &
                                    start_index_grain, &
                                    start_index_relaxations, &
                                    i, &
                                    j, &
                                    start_index_slip_systems, &
                                    JJ, &
                                    II, &
                                    ind_basis_systems_cluster(10), &
                                    n_relaxations
        logical::                   full_constraints
        character(*), parameter::   PROC_NAME = 'get_stress_state'

        n_relaxations=(cluster_size-1)*2
        size_system = 5*cluster_size
        n_slip_systems_cluster = cluster_size*n_slip_systems_grain+n_relaxations
        start_index_relaxations = n_slip_systems_cluster-n_relaxations+1
        full_constraints = (n_relaxations == 0)
        if (index_in_cluster == 1) then
            select case (cluster_size)
                case (1)
                   weight = 1._DP 
                case (2)
                    weight = cluster_weight(grains(index_grain), deformation_gradient)  
            end select
            !Update microstructure
            inverse_basis_cluster = 0._DP
            ind_basis_systems_cluster(1:5) = ind_basis_systems_grain
            ind_basis_systems_cluster(6:10) = ind_basis_systems_grain+n_slip_systems_grain

            do i = 1, cluster_size
                start_index_grain = 5*(i-1)+1
                if (.not. full_constraints) then
                    do j = 1, 2
                        !Transform relaxation from boundary frame to crystal frame
                        !Composed of rotation from boundary to global frame and then from global to crystal frame.
                        boundary_to_crystal = matmul(orientation_grain(:,:,i), transpose(cluster_frame(grains(index_grain), deformation_gradient)))
                        relaxations_crystal_frame = rotate_to(real(RELAXATIONS(:,:,j), DP), boundary_to_crystal)
                        !Invert direction of relaxations for second grain
                        if (i == 2) relaxations_crystal_frame = -relaxations_crystal_frame
                        !Rotational component of relaxations
                        spin_coeffs_relaxations(:,j, i) = convert_spin(relaxations_crystal_frame) / SQR2
                        !Insert the relaxations as columns in taylor_coeffs_cluster-matrix
                        taylor_coeffs_cluster(start_index_grain:start_index_grain+4, start_index_relaxations-1+j)=convert_stress_strain_space(symmetric_part(relaxations_crystal_frame))
                    end do
                endif

                imposed_strain_grain(1:5, i)=convert_stress_strain_space(velocity_gradient .toframe. orientation_grain(:,:,i))
                imposed_strain_cluster(start_index_grain:start_index_grain+4)=imposed_strain_grain(1:5, i)
                ! Retrieve the CRSSmatrix
                start_index_slip_systems = n_slip_systems_grain*(i-1)+1
                crss_cluster(:,start_index_slip_systems:start_index_slip_systems+n_slip_systems_grain-1) = hardening_get_crss(index_grain+i-1, strain_ab(i))
                inverse_basis_cluster(start_index_grain:start_index_grain+4, start_index_grain:start_index_grain+4)=inverse_basis_grain
            enddo

            !Relaxed constriants calculation. Called for ALAMEL.
            if (.not. full_constraints) then
                call simplex_solve(taylor_coeffs_cluster, imposed_strain_cluster, crss_cluster, inverse_basis_cluster, ind_basis_systems_cluster, slip_rates, stress_cluster, rss_cluster, overstress)
                slip_rates_relaxations(1:n_relaxations) = slip_rates(start_index_relaxations:n_slip_systems_cluster)
                !If not all grains deform, we take the full constraints solution
                full_constraints = (.not. all([(sum(abs(slip_rates(1+i*n_slip_systems_grain:(i+1)*n_slip_systems_grain))), i = 0, 1)]>=TOLERANCE))
            end if
            !Full constraints calculation. Called for FCTaylor and if only 1 grain deforms for ALAMEL
            if (full_constraints) then
                call simplex_solve(taylor_coeffs_cluster(1:size_system, 1:start_index_relaxations-1), imposed_strain_cluster, crss_cluster, inverse_basis_cluster, ind_basis_systems_cluster, slip_rates, stress_cluster, rss_cluster, overstress)
                slip_rates_relaxations = 0._DP
            endif
        endif
        ! From here on, output is produced for grain number "index_in_cluster"
        jj = n_slip_systems_grain*(index_in_cluster-1)
        crss_grain = crss_cluster(1:2, jj+1:jj+n_slip_systems_grain)
        ii = 5*(index_in_cluster-1)
        spin = 0._DP
        do i = 1, 5
            ! If one grain does not deform, the stress stress_cluster came from the fullconstraints solution.
            spanv(i)=stress_cluster(i+ii)
            strain_relaxations(i)=sum(taylor_coeffs_cluster(i+ii, start_index_relaxations:n_slip_systems_cluster)*slip_rates_relaxations(1:n_relaxations))
            strain_grain(i)=imposed_strain_grain(i, index_in_cluster)-strain_relaxations(i)
            if (i < 4) spin(i)=sum(spin_coeffs_relaxations(i, :, index_in_cluster)*slip_rates_relaxations(1:n_relaxations))
        enddo
        stress_matrix = convert_stress_strain_space(spanv) ! (5) -> sym.(3, 3)
        spin_relaxations = 0._DP
        
        spin_relaxations = convert_spin(spin) * SQR2
        ! note that if one of the grains does not deform at all, the stress and the active slip systems
        ! of the full constraint solution are used.
        n_active_slip_systems = 0
        do i = 1, n_slip_systems_grain
            if (abs(overstress(i+jj)) < TOLERANCE) then
                n_active_slip_systems = n_active_slip_systems+1
                ind_active_slip_systems(n_active_slip_systems) =i
            end if
        enddo
        if (n_active_slip_systems > 8) then
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Too many active slip systems.')
        elseif (n_active_slip_systems == 0) then
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'No active slip systems found.')
        endif
        SLIPLP(1:n_active_slip_systems)=slip_rates(ind_active_slip_systems(1:n_active_slip_systems)+jj)
        TAURLP(1:n_active_slip_systems)=rss_cluster(ind_active_slip_systems(1:n_active_slip_systems)+jj)

        !Transform stress from local frame (Scrys) to sample frame (Ssam)
        stress_matrix = stress_matrix .fromframe. orientation_grain(:,:,index_in_cluster)
        !Transform relaxation spin tensor from local frame to sample frame
        spin_relaxations = spin_relaxations .fromframe. orientation_grain(:,:,index_in_cluster)
    end subroutine

    subroutine apply_deformation_step(index_grain, total_slip_rate, work_rate, imposed_spin, crss, orientation, taylor_coeffs)
        integer, intent(in)::       index_grain
        real(DP), intent(in)::      imposed_spin(3, 3), &
                                    taylor_coeffs(:,:), &
                                    crss(:,:)
        real(DP), intent(out)::     total_slip_rate, &
                                    work_rate
        real(DP), intent(inout)::   orientation(3, 3)
        real(DP)::                  orientation_increment(3, 3), &
                                    slip_rates(size(crss, 2))

        call resolve_taylor_ambiguity(slip_rates, n_active_slip_systems, SLIPLP, TAURLP, ind_active_slip_systems, strain_grain, taylor_coeffs)
        call hardening_update_state(index_grain, 1._DP, slip_rates)

        total_slip_rate = sum(abs(slip_rates))
        work_rate = sum(merge(crss(1, :), -crss(2, :), slip_rates > 0._DP)*slip_rates)

        orientation_increment = UNIT_MATRIX_3X3 &
                                -convert_spin(matmul(spin_slip_systems, slip_rates)) & !Spin induced by activation of slip systems
                                +(imposed_spin .toframe. orientation) &                !Change of reference frame
                                +(spin_relaxations .toframe. orientation)              !Spin absorbed by relaxations
        orientation = matmul(orientation_increment, orientation)
    end subroutine

    pure real(DP) function cluster_weight(grain_, deformation_gradient) result(weight)
        type(Grain), intent(in):: grain_
        real(DP), intent(in):: deformation_gradient(3, 3)
        real(DP):: grain_axes(3, 3), &
                   axis_lengths(3), &
                   alignment_factor

        !Applying deformation gradient to initial grain boundary orientation yields deformed grain axes
        grain_axes = matmul(deformation_gradient, grain_%boundary_reference_frame)
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

    pure function cluster_frame(grain_, deformation_gradient) result(frame)
        type(Grain), intent(in):: grain_ 
        real(DP), intent(in):: deformation_gradient(3, 3)
        real(DP):: frame(3, 3)

        frame = matmul(deformation_gradient, grain_%boundary_reference_frame)
        frame(:,3) = frame(:,1) .cross. frame(:,2)
        frame(:,2) = frame(:,3) .cross. frame(:,1)
        frame = transpose(normalize(frame))
    end function
end module
