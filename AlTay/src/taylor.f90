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
                BB8(5), &
                B8(5, 2), &
                GAMR(2)
    real(DP), allocatable ::  spin_slip_systems(:,:), &
                              B3(:,:,:), &
                              slip_rates(:), &
                              BB(:), &
                              crss_cluster(:,:), &
                              overstress(:), &
                              TAUR1(:), &
                              UBUF(:), &
                              taylor_coeffs_cluster(:,:)

    integer:: DI1(5), ind_active_slip_systems(8)
    
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
            allocate(BB(system_size))
            allocate(crss_cluster(2, n_slip_systems_cluster))
            allocate(overstress(n_slip_systems_cluster))
            allocate(TAUR1(n_slip_systems_cluster))
            allocate(UBUF(system_size))
            allocate(taylor_coeffs_cluster(system_size, n_slip_systems_cluster))
            if (cluster_size == 2) &
                allocate(B3(3, 2, 2), source = 0._DP)
        end if
        
        DI1 = merge(INITIAL_BASIS_SYSTEMS_Fcrss_grain, INITIAL_BASIS_SYSTEMS_Bcrss_grain, n_slip_systems_grain == 12)
        do i = 1, n_slip_systems_grain
            normalized = normalize(deformation_mechanism(:,:,i))
            tensor = outer_product(normalized(:,1), normalized(:,2))
            taylor_coeffs_grain(:,i) = convert_stress_strain_space(tensor) 
            spin_slip_systems(:,i) = convert_spin(tensor)  
        end do
        forall (i = 1:5) basis(:,i) = taylor_coeffs_grain(:,DI1(i))

        inverse_basis_grain = invert(basis)
       
        taylor_coeffs_cluster = 0.0_DP
        taylor_coeffs_cluster(1:5, 1:n_slip_systems_grain)=taylor_coeffs_grain
        if (cluster_size == 2) taylor_coeffs_cluster(6:10, n_slip_systems_grain+1:n_slip_systems_grain*2)=taylor_coeffs_grain
    end subroutine   
    
    subroutine get_stress_state(stress_matrix, strain_matrix, orientation_grain, index_grain, strain_ab, cluster_size, index_in_cluster, crss_grain, n_slip_systems_grain, velocity_gradient, deformation_gradient, weight)
        integer, intent(in)::       index_in_cluster, &
                                    index_grain, &
                                    cluster_size, &
                                    n_slip_systems_grain
        real(DP), intent(out)::     stress_matrix(3, 3), &
                                    strain_matrix(3, 3), &
                                    weight
        real(DP), intent(in)::      orientation_grain(3, 3, 2), &
                                    strain_ab(2), &
                                    velocity_gradient(3, 3), &
                                    deformation_gradient(3, 3)
        real(DP), intent(inout)::   crss_grain(2, n_slip_systems_grain)
        real(DP)::                  strain(5), &
                                    spin(3), &
                                    boundary_to_crystal(3, 3), &
                                    C2(3, 3), &
                                    C3(3, 3), &
                                    spanv(5), &
                                    UU(5*cluster_size, 5*cluster_size)
        integer::                   n_slip_systems_cluster, &
                                    size_system, &
                                    IL, &
                                    L1, &
                                    IRL, &
                                    I, &
                                    K1, &
                                    IG, &
                                    JJ, &
                                    II, &
                                    DI(10), &
                                    n_relaxations
        logical::                   full_constraints
        character(*), parameter::   PROC_NAME = 'get_stress_state'

        n_relaxations=(cluster_size-1)*2
        size_system = 5*cluster_size
        n_slip_systems_cluster = cluster_size*n_slip_systems_grain+n_relaxations
        full_constraints = (n_relaxations == 0)
        if (index_in_cluster == 1) then
            select case (cluster_size)
                case (1)
                   weight = 1._DP 
                case (2)
                    weight = cluster_weight(grains(index_grain), deformation_gradient)  
            end select
            !Update microstructure
            crss_cluster(1:2, n_slip_systems_cluster-n_relaxations+1:n_slip_systems_cluster)=0._DP
            UU = 0.0_dp
            DI(1:5) = DI1
            DI(6:10) = DI1+n_slip_systems_grain

            do IL = 1, cluster_size
                L1 = 5*(IL-1)
                C2 = rotate_to(velocity_gradient, orientation_grain(:,:,IL))
                if (.not. full_constraints) then
                    do IRL = 1, 2
                        !Transform relaxation from boundary frame to crystal frame
                        !Composed of rotation from boundary to global frame and then from global to crystal frame.
                        boundary_to_crystal = matmul(orientation_grain(:,:,IL), transpose(cluster_frame(grains(index_grain), deformation_gradient)))
                        C3 = rotate_to(real(RELAXATIONS(:,:,IRL), DP), boundary_to_crystal)
                        !Invert direction of relaxations for second grain
                        if (IL == 2) C3 = -C3
                        !Rotational component of relaxations
                        B3(:,IRL, IL) = convert_spin(C3) / SQR2
                        !Insert the relaxations as columns in taylor_coeffs_cluster-matrix
                        taylor_coeffs_cluster(L1+1:L1+5, n_slip_systems_cluster-2+IRL)=convert_stress_strain_space(symmetric_part(C3))
                    end do
                endif

                B8(1:5, IL)=convert_stress_strain_space(C2)
                BB(L1+1:L1+5)=B8(1:5, IL)
                K1 = n_slip_systems_grain*(IL-1)

                ! Retrieve the CRSSmatrix
                crss_cluster(:,1+K1:n_slip_systems_grain+K1) = hardening_get_crss(index_grain+IL-1, strain_ab(IL))
                UU(L1+1:L1+5, L1+1:L1+5)=inverse_basis_grain
            enddo

            if (.not. full_constraints) then
                call simplex_solve(taylor_coeffs = taylor_coeffs_cluster, &
                         strain = BB, &
                         crss = crss_cluster, &
                         inverse_basis = UU, &
                         basis_systems = DI, &
                         slip = slip_rates, &
                         stress = UBUF, &
                         rss = Taur1, &
                         overstress = overstress)

                !GAMR will contain the relaxed shears:
                gamr(1:n_relaxations)=slip_rates(n_slip_systems_cluster-n_relaxations+1:n_slip_systems_cluster)

                !If not all grains deform, we take the full constraints solution
                full_constraints = (.not. all([(sum(abs(slip_rates(1+n_slip_systems_grain*IG:n_slip_systems_grain+n_slip_systems_grain*IG))), IG = 0, 1)]>=TOLERANCE))
            end if
            if (full_constraints) then
                ! Full constraints calculation
                call simplex_solve(taylor_coeffs = taylor_coeffs_cluster(1:size_system, 1:n_slip_systems_cluster-n_relaxations), &
                     strain = BB, &
                     crss = crss_cluster, &
                     inverse_basis = UU, &
                     basis_systems = DI, &
                     slip = slip_rates, &
                     stress = UBUF, &
                     rss = Taur1, &
                     overstress = overstress)
            endif
        endif
        ! From here on, output is produced for grain number "index_in_cluster"
        jj = n_slip_systems_grain*(index_in_cluster-1)
        crss_grain = crss_cluster(1:2, jj+1:jj+n_slip_systems_grain)
        ii = 5*(index_in_cluster-1)
        spin = 0._DP
        do i = 1, 5
            ! If one grain does not deform, the stress UBUF came from the fullconstraints solution.
            spanv(i)=UBUF(i+ii)
            strain(i)=-sum(taylor_coeffs_cluster(i+ii, n_slip_systems_cluster-n_relaxations+1:n_slip_systems_cluster)*gamr(1:n_relaxations))
            BB8(i)=B8(i, index_in_cluster)+strain(i)
            if (i < 4) spin(i)=sum(B3(i, :, index_in_cluster)*gamr(1:n_relaxations))
        enddo
        stress_matrix = convert_stress_strain_space(spanv) ! (5) -> sym.(3, 3)
        strain_matrix = convert_stress_strain_space(strain)  ! (5) -> sym.(3, 3)
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
        TAURLP(1:n_active_slip_systems)=TAUR1(ind_active_slip_systems(1:n_active_slip_systems)+jj)

        !Transform stress from local frame (Scrys) to sample frame (Ssam)
        stress_matrix = rotate_from(stress_matrix, orientation_grain(:,:,index_in_cluster))
        !Transform relaxation strain rate tensor from local frame to sample frame
        strain_matrix = rotate_from(strain_matrix, orientation_grain(:,:,index_in_cluster))
        !Transform relaxation spin tensor from local frame to sample frame
        spin_relaxations = rotate_from(spin_relaxations, orientation_grain(:,:,index_in_cluster))
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

        call resolve_taylor_ambiguity(slip_rates, n_active_slip_systems, SLIPLP, TAURLP, ind_active_slip_systems, BB8, taylor_coeffs)
        call hardening_update_state(index_grain, 1._DP, slip_rates)

        total_slip_rate = sum(abs(slip_rates))
        work_rate = sum(merge(crss(1, :), -crss(2, :), slip_rates > 0._DP)*slip_rates)

        orientation_increment = UNIT_MATRIX_3X3 &
                                -convert_spin(matmul(spin_slip_systems, slip_rates)) & !Spin induced by activation of slip systems
                                +rotate_to(imposed_spin, orientation) &                !Change of reference frame
                                +rotate_to(spin_relaxations, orientation)              !Spin absorbed by relaxations
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
