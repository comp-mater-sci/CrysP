module taylor
    use utils
    use altayMacroKinematic
    use criMathUtils
    use hardening
    use taylor_ambiguity
    use altayConfig, only: astate
    use logging
    use simplex
    use altayAlgorithms
    use slip_systems
    use altayDynfil

    implicit none

    private
    public ::   taylor_init, &
                taylor_solve, &
                taylor_update_state

    integer  :: n_active_slip_systems
    real(DP):: inverse_basis_grain(5, 5), &
                spin_matrix(3, 3), &
                SLIPLP(8), &
                TAURLP(8), &
                BB8(5), &
                B8(5, 2), &
                GAMR(2)
    real(DP), allocatable ::  rotation_slip_systems(:,:), &
                              B3(:,:), &
                              slip_rates(:), &
                              BB(:), &
                              CCC(:,:), &
                              overstress(:), &
                              TAUR1(:), &
                              UBUF(:), &
                              A2(:,:), &
                              SGNN(:)

    integer:: DI1(5), ind_active_slip_systems(8)
    
    character(*), parameter ::  MOD_NAME = 'taylor'
    real(DP), parameter     ::  PLUMIN(2, 2) = reshape([1._DP, -1._DP, 1._DP, -1._DP], shape(PLUMIN))
    integer, dimension(5), parameter ::    INITIAL_BASIS_SYSTEMS_FCC = [2, 5, 6, 7, 8], &
                                           INITIAL_BASIS_SYSTEMS_BCC = [1, 2, 4, 5, 7]
contains
    subroutine taylor_init(deformation_mechanism, n_slip_systems_grain, A1, cluster_size)
        integer, intent(out):: n_slip_systems_grain  ! < total number of systems in slip system file (glide+twin)
        real(DP), intent(out), allocatable:: A1(:,:)
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

        allocate(A1(5, n_slip_systems_grain))
        if (.not. allocated(B3)) then
            allocate(rotation_slip_systems(3, n_slip_systems_grain))
            allocate(B3(system_size, 3), source = 0._DP)
            allocate(slip_rates(n_slip_systems_cluster))
            allocate(BB(system_size))
            allocate(CCC(2, n_slip_systems_cluster))
            allocate(overstress(n_slip_systems_cluster))
            allocate(TAUR1(n_slip_systems_cluster))
            allocate(UBUF(system_size))
            allocate(A2(system_size, n_slip_systems_cluster))
            allocate(SGNN(n_slip_systems_grain))
        end if
        
        DI1 = merge(INITIAL_BASIS_SYSTEMS_FCC, INITIAL_BASIS_SYSTEMS_BCC, n_slip_systems_grain == 12)
        do i = 1, n_slip_systems_grain
            normalized = normalize(deformation_mechanism(:,:,i))
            tensor = outer_product(normalized(:,1), normalized(:,2))
            A1(:,i) = convert_stress_strain_space(tensor) 
            rotation_slip_systems(:,i) = get_rotation(tensor)  
        end do
        forall (i = 1:5) basis(:,i) = A1(:,DI1(i))

        inverse_basis_grain = invert(basis)
       
        A2 = 0.0_DP
        A2(1:5, 1:n_slip_systems_grain)=A1
        if (cluster_size == 2) A2(6:10, n_slip_systems_grain+1:n_slip_systems_grain*2)=A1
    end subroutine   
    
    !Note that IOR will be replaced by a reference to a grain object in the near future.
    subroutine taylor_solve(stress_matrix, strain_matrix, TRF, IOR, GMMAb, cluster_size, laml, CC, n_slip_systems_grain, macrodefrate)
        type(DeformationRate), intent(in):: macrodefrate
        integer, intent(in):: laml, IOR, cluster_size, n_slip_systems_grain
        real(dp), intent(out):: stress_matrix(3, 3), strain_matrix(3, 3)
        real(dp), intent(in):: TRF(3, 3, 2), GMMab(2)
        real(dp), intent(inout):: CC(2, n_slip_systems_grain)
        real(dp), dimension(5):: strain, spin
        real(dp):: C2(3, 3), rls(3, 3), rla(3, 3), C3(3, 3), spanv(5)
        real(DP):: mat_buffer(3, 3), UU(5*cluster_size, 5*cluster_size)
        integer:: n_slip_systems_cluster, size_system, IL, L1, IRL, I, K1, IG, JJ, II, DI(10), n_relaxations
        logical:: full_constraints

        character(*), parameter:: PROC_NAME = 'taylor_solve'

        n_relaxations=(cluster_size-1)*2
        size_system = 5*cluster_size
        n_slip_systems_cluster = cluster_size*n_slip_systems_grain+n_relaxations
        full_constraints = (n_relaxations == 0)
        if (laml == 1) then
            !Update microstructure
            CCC(1:2, n_slip_systems_cluster-n_relaxations+1:n_slip_systems_cluster)=0.0_DP
            UU = 0.0_dp
            DI(1:5) = DI1
            DI(6:10) = DI1+n_slip_systems_grain

            do IL = 1, cluster_size
                L1 = 5*(IL-1)
                C2 = rotateSRTensorFrom(MacroDefRate%VelGrad, TRF(:,:,IL))
                if (.not. full_constraints) then
                    do IRL = 1, 2
                        ! Transform relaxation from grain reference frame to macroscopic frame
                        mat_buffer = rotateSRTensorTo(real(RELAXATIONS(:,:,IRL), DP), DFIL(IOR)%boundary_reference_frame)
                        !   ... and now to crystal frame:
                        C3 = rotateSRTensorFrom(mat_buffer, TRF(:,:,IL))
                        RLS=(C3+transpose(C3))*0.5_dp
                        RLA=(C3-transpose(C3))*0.5_dp
                        B3(L1+1, IRL)=PLUMIN(IL, IRL)*RLA(2, 3)/sqr2
                        B3(L1+2, IRL)=PLUMIN(IL, IRL)*RLA(3, 1)/sqr2
                        B3(L1+3, IRL)=PLUMIN(IL, IRL)*RLA(1, 2)/sqr2
                        !  Insert the relaxations as columns in A1-matrix
                        A2(L1+1:L1+5, n_slip_systems_cluster-n_relaxations+IRL)=convert_stress_strain_space(RLS)*PLUMIN(IL, IRL)
                    end do
                endif

                ! Calculation of time increment by dividing von Mises equivalent
                ! strain by von Mises equivalent strain rate
                B8(1:5, IL)=convert_stress_strain_space(C2)/MacroDefRate%vMeqStrainRate  ! sym.(3, 3) -> (5)
                BB(L1+1:L1+5)=B8(1:5, IL)
                K1 = n_slip_systems_grain*(IL-1)

                ! Retrieve the CRSSmatrix
                CCC(:,1+K1:n_slip_systems_grain+K1) = hardening_get_crss(IOR+IL-1, GMMab(IL))
                UU(L1+1:L1+5, L1+1:L1+5)=inverse_basis_grain
            enddo

            if (.not. full_constraints) then
                call simplex_solve(taylor_coeffs = A2, &
                         strain = BB, &
                         crss = CCC, &
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
                call simplex_solve(taylor_coeffs = A2(1:size_system, 1:n_slip_systems_cluster-n_relaxations), &
                     strain = BB, &
                     crss = CCC, &
                     inverse_basis = UU, &
                     basis_systems = DI, &
                     slip = slip_rates, &
                     stress = UBUF, &
                     rss = Taur1, &
                     overstress = overstress)
            endif
        endif
        ! From here on, output is produced for grain number "laml"
        jj = n_slip_systems_grain*(laml-1)
        CC = CCC(1:2, jj+1:jj+n_slip_systems_grain)
        ii = 5*(laml-1)
        do i = 1, 5
            ! If one grain does not deform, the stress UBUF came from the fullconstraints solution.
            spanv(i)=UBUF(i+ii)
            strain(i)=-sum(A2(i+ii, n_slip_systems_cluster-n_relaxations+1:n_slip_systems_cluster)*gamr(1:n_relaxations))
            BB8(i)=B8(i, laml)+strain(i)
            spin(i)=-sum(B3(i+ii, 1:n_relaxations)*gamr(1:n_relaxations))
        enddo
        stress_matrix = convert_stress_strain_space(spanv) ! (5) -> sym.(3, 3)
        strain_matrix = convert_stress_strain_space(strain)  ! (5) -> sym.(3, 3)
        spin_matrix = 0._DP
        spin_matrix(2, 3)= spin(1)*sqr2*MacroDefRate%vMeqStrainRate
        spin_matrix(3, 1)= spin(2)*sqr2*MacroDefRate%vMeqStrainRate
        spin_matrix(1, 2)= spin(3)*sqr2*MacroDefRate%vMeqStrainRate
        spin_matrix(3, 2)= -spin_matrix(2, 3)
        spin_matrix(1, 3)= -spin_matrix(3, 1)
        spin_matrix(2, 1)= -spin_matrix(1, 2)

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
        stress_matrix = rotateSRTensorTo(stress_matrix, TRF(:,:,laml))
        !Transform relaxation strain rate tensor from local frame to sample frame
        strain_matrix = rotateSRTensorTo(strain_matrix, TRF(:,:,laml))
        !Transform relaxation spin tensor from local frame to sample frame
        spin_matrix = rotateSRTensorTo(spin_matrix, TRF(:,:,laml))
    end subroutine

    subroutine taylor_update_state(IOR, TOTGAMdot, WorkRate, spin, von_mises_strain_rate, CC, TRF, C2, XM)
        real(DP), intent(in):: spin(3, 3), &
                               von_mises_strain_rate
        integer, intent(in):: IOR
        real(DP), intent(in):: XM(:,:), TRF(3, 3), CC(:,:)
        real(DP), intent(out):: TOTGAMdot, C2(3, 3)
        !> Rate of plastic work per unit volume in the crystal
        real(DP), intent(out):: WorkRate

        real(DP), dimension(3):: ROT
        real(DP), dimension(3, 3):: RCcryst, RHOAcrys
        real(DP), dimension(size(CC, 2)):: GAMdot
        integer:: j
        real(DP), parameter:: ddt = 1.0_DP

        call resolve_taylor_ambiguity(GAMdot, SGNN, von_mises_strain_rate, n_active_slip_systems, SLIPLP, TAURLP, ind_active_slip_systems, BB8, XM)
        if (.not. astate%simulCalls(astate%this)%input%keep_state) call hardening_update_state(IOR, ddt, GAMdot)
        TOTGAMdot = sum(abs(GAMdot))
        ! Calculate RCcryst: the rigid body spin in the crystal frame
        RCcryst = rotateSRTensorFrom(spin, TRF)
        RHOAcrys = rotateSRTensorFrom(spin_matrix, TRF)
        WorkRate = sum(merge(CC(1, :), -CC(2, :), GAMdot > 0.0_DP)*GAMdot)

        ROT = matmul(rotation_slip_systems, GAMdot)

        forall (j = 1:3) C2(j, j) = 1._DP
 
        C2(3, 2)=ROT(1)-(RCcryst(3, 2)+RHOAcrys(3, 2))
        C2(1, 3)=ROT(2)-(RCcryst(1, 3)+RHOAcrys(1, 3))
        C2(2, 1)=ROT(3)-(RCcryst(2, 1)+RHOAcrys(2, 1))
        C2(2, 3)=-C2(3, 2)
        C2(3, 1)=-C2(1, 3)
        C2(1, 2)=-C2(2, 1)
        ! KORRIGEREN VAN DE NIEUWE ROTATIEMATRIX
        C2 = matmul(C2, TRF)

    end subroutine

end module
