module taylor
    use utils
    use altayMacroKinematic
    use criMathUtils
    use hardening
    use altaySliprate
    use altayConfig, only: astate
    use logging
    use simplex
    use altayMesostructure
    use altayAlgorithms
    use slip_systems

    implicit none

    private
    public ::   taylor_init, &
                taylor_solve, &
                taylor_update_state

    real(dp), parameter :: TLXX=5.0e-6_dp
    integer, dimension(5), parameter ::    INITIAL_BASIS_SYSTEMS_FCC = [2,5,6,7,8], &
                                           INITIAL_BASIS_SYSTEMS_BCC = [1,2,4,5,7]
    character(*), parameter ::  MOD_NAME = 'taylor'
    real(DP), parameter     ::  PLUMIN(2,2) = reshape([1._DP,-1._DP, &
                                                       1._DP,-1._DP], shape(PLUMIN)), &
                                GETAL = 1.0e6_dp,    &
                                TOL = 1.0e-6_dp

    integer  :: M, &         ! number of deformation mechanisms
                NACTIV
    integer :: DI1(5), INDACT(8),INDLP(8)
    real(dp) :: B8(5,2),GAMR(2),B(5,5),spin_matrix(3,3),SLIPLP(8),TAURLP(8),BB8(5)
    real(dp), dimension(:), allocatable :: SGNN,DTAU1,TAUR1,UBUF,BB,XX
    real(dp), dimension(:,:), allocatable :: B1,CCC,A2,B3

contains
    subroutine taylor_init(deformation_mechanism, M111,A1,NGR)
        integer, intent(in) :: NGR
        integer, intent(out) :: M111  ! < total number of systems in slip system file (glide+twin)
        real(dp), intent(out), allocatable :: A1(:,:)
        integer, dimension(:,:,:), intent(in) :: deformation_mechanism
        integer :: i
        real(DP) :: basis(5,5), &
                    normalized(3,2), &
                    tensor(3,3)

        M = size(deformation_mechanism,3)
        DI1 = merge(INITIAL_BASIS_SYSTEMS_FCC, INITIAL_BASIS_SYSTEMS_BCC, M == 12)
        M111=M

        allocate(A1(5,M))

        ! module variables
        if(.not. allocated(B3)) allocate(B3(NGR*5,3),source=0.0_DP)
        if(.not. allocated(SGNN)) allocate(SGNN(M))
        if(.not. allocated(B1)) allocate(B1(3,M))
        if(.not. allocated(DTAU1)) allocate(DTAU1(NGR*M+2))
        if(.not. allocated(TAUR1)) allocate(TAUR1(NGR*M+2))
        if(.not. allocated(CCC)) allocate(CCC(2,NGR*M+2))
        if(.not. allocated(XX)) allocate(XX(NGR*M+2))
        if(.not. allocated(UBUF)) allocate(UBUF(NGR*5))
        if(.not. allocated(BB)) allocate(BB(NGR*5))
        if(.not. allocated(A2)) allocate(A2(NGR*5,NGR*M+2))

        do i=1,M
            normalized = normalize(deformation_mechanism(:,:,i))
            tensor = outer_product(normalized(:,1), normalized(:,2))
            A1(:,i) = convert_stress_strain_space(tensor)
            B1(:,i) = get_rotation(tensor)
        end do
        forall (i=1:5) basis(:,i) = A1(:,DI1(i))

        B = invert(basis)

        A2=0.0_DP
        A2(1:5,1:M)=A1(1:5,1:M)
        if(NGR==2) A2(6:10,M+1:M*2)=A1(1:5,1:M) ! ALAMEL

    end subroutine

    subroutine taylor_solve(stress_matrix,strain_matrix, GEWF, IOR, TRF, GMMAb, NGR, laml, CC, M11, MacroDefRate,MacroDefState)
        type(DeformationRate),intent(in) :: MacroDefRate
        type(DeformationState),intent(in) :: MacroDefState
        integer, intent(in) :: laml,IOR, NGR, M11
        real(dp), intent(out) :: stress_matrix(3,3),strain_matrix(3,3)
        real(dp), intent(in) :: TRF(3,3,2), GMMab(2)
        real(dp), intent(inout) :: CC(2,M11),GEWF
        real(dp),dimension(5):: strain, spin
        real(dp) :: TPrinc(3,3)
        real(dp) :: C2(3,3), rls(3,3), rla(3,3), C3(3,3), spanv(5), DTAU(NGR*M+5), STRSS(5*NGR), TAUR(NGR*M+5)
        real(DP) :: mat_buffer(3,3), UU(5*NGR, 5*NGR)
        integer :: M12,N,M2,IL,L1,IRL,I,K1,IG,JJ,II, DI(NGR*5), IGrElm,NRL

        character(*), parameter :: PROC_NAME = 'taylor_solve'

        NRL=(NGR-1)*2
        N=5*NGR
        M2=NGR*M
        M12=NGR*M+2
        if (laml == 1) then

            ! Updating of microstructure
            if (NGR /= 1) then ! ALAMEL
                IGrElm = mod((IOR-1)/NGR,NGrElm) + 1
                call cluster1(IGrElm,MacroDefRate,MacroDefState,GEWF,Tprinc)
            endif
            CCC(1:2,M2+1:M12)=0.0_DP
            UU = 0.0_dp
            DI(1:5) = DI1
            if(NGR==2) DI(6:10) = DI1+M11 !ALAMEL

            do IL=1,NGR
                L1=5*(IL-1)

                C2 = rotateSRTensorFrom(MacroDefRate%VelGrad,TRF(:,:,IL))
                do IRL=1,NRL ! ALAMEL
                     ! Transform relaxation from grain reference frame to macroscopic frame
                     !   ... and now to crystal frame:
                     mat_buffer = rotateSRTensorTo(real(RELAXATIONS(:,:,IRL),DP),Tprinc)
                     C3 = rotateSRTensorFrom(mat_buffer,TRF(:,:,IL))
                     RLS=(C3+transpose(C3))*0.5_dp
                     RLA=(C3-transpose(C3))*0.5_dp
                     B3(L1+1,IRL)=PLUMIN(IL,IRL)*RLA(2,3)/sqr2
                     B3(L1+2,IRL)=PLUMIN(IL,IRL)*RLA(3,1)/sqr2
                     B3(L1+3,IRL)=PLUMIN(IL,IRL)*RLA(1,2)/sqr2
                     !  Insert the relaxations as columns in A1-matrix
                     A2(L1+1:L1+5,M2+IRL)=convert_stress_strain_space(RLS)*PLUMIN(IL,IRL)
                end do

                ! Calculation of time increment by dividing von Mises equivalent
                ! strain by von Mises equivalent strain rate
                B8(1:5,IL)=convert_stress_strain_space(C2)/MacroDefRate%vMeqStrainRate ! sym.(3,3) -> (5)
                BB(L1+1:L1+5)=B8(1:5,IL)
                K1=M*(IL-1)

                ! Retrieve the CRSSmatrix
                CCC(:,1+K1:M+K1) = hardening_get_crss(IOR+IL-1, GMMab(IL))
                UU(L1+1:L1+5,L1+1:L1+5)=B
            enddo
            !Suppress the relaxations in a first call of the TBH program
            if (NRL /= 0) CCC(1:2,M2+1:M12)=GETAL
            ! Full constraints calculation
            call simplex_solve(taylor_coeffs = A2, &
                     strain = BB, &
                     crss = CCC, &
                     inverse_basis = UU, &
                     basis_systems = DI, &
                     slip = XX, &
                     stress = UBUF, &
                     rss = TAUR, &
                     overstress = DTAU)

            DTAU1=DTAU
            TAUR1=TAUR

            if (NRL == 0) then
                STRSS=UBUF
            else
                do IRL=1,NRL
                    CCC(1:2,M2+IRL)=0.0_DP
                end do
                call simplex_solve(taylor_coeffs = A2, &
                         strain = BB, &
                         crss = CCC, &
                         inverse_basis = UU, &
                         basis_systems = DI, &
                         slip = XX, &
                         stress = STRSS, &
                         rss = TAUR, &
                         overstress = DTAU)

                ! GAMR will contain the relaxed shears:
                gamr(1:NRL)=XX(M2+1:M2+NRL)
            endif
            ! Check whether all grains deform
            if (all([(sum(abs(xx(1+M*IG:M+M*IG))),IG=0,NGR-1)]>=TOLERANCE)) then
                DTAU1=DTAU
                TAUR1=TAUR
                UBUF=STRSS
            endif
        endif
        ! From here on, output is produced for grain number "laml"
        jj=M*(laml-1)
        CC=CCC(1:2,jj+1:jj+M)
        ii=5*(laml-1)
        do i=1,5
            ! If one grain does not deform, the stress UBUF came from the fullconstraints solution.
            spanv(i)=UBUF(i+ii)
            strain(i)=-sum(A2(i+ii,M2+1:M2+NRL)*gamr(1:NRL))
            BB8(i)=B8(i,laml)+strain(i)
            spin(i)=-sum(B3(i+ii,1:NRL)*gamr(1:NRL))
        enddo
        stress_matrix = convert_stress_strain_space(spanv) ! (5) -> sym.(3,3)
        strain_matrix = convert_stress_strain_space(strain)  ! (5) -> sym.(3,3)
        spin_matrix=0._DP
        spin_matrix(2,3)= spin(1)*sqr2*MacroDefRate%vMeqStrainRate
        spin_matrix(3,1)= spin(2)*sqr2*MacroDefRate%vMeqStrainRate
        spin_matrix(1,2)= spin(3)*sqr2*MacroDefRate%vMeqStrainRate
        spin_matrix(3,2)= -spin_matrix(2,3)
        spin_matrix(1,3)= -spin_matrix(3,1)
        spin_matrix(2,1)= -spin_matrix(1,2)

        ! note that if one of the grains does not deform at all, the stress and the active slip systems
        ! of the full constraint solution are used.
        NACTIV = 0
        do i=1,M
            if (abs(DTAU1(i+jj)) > TOL) cycle
            NACTIV=NACTIV+1
            INDACT(NACTIV)=i
        enddo
        if (NACTIV > 8) then
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Too many active slip systems.')
        elseif (NACTIV == 0) then
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'No active slip systems found.')
        endif
        INDLP(1:NACTIV)=INDACT(1:NACTIV)
        SLIPLP(1:NACTIV)=XX(INDACT(1:NACTIV)+jj)
        TAURLP(1:NACTIV)=TAUR1(INDACT(1:NACTIV)+jj)

        !Transform stress from local frame (Scrys) to sample frame (Ssam)
        stress_matrix = rotateSRTensorTo(stress_matrix,TRF(:,:,laml))
        !Transform relaxation strain rate tensor from local frame to sample frame
        strain_matrix = rotateSRTensorTo(strain_matrix,TRF(:,:,laml))
        !Transform relaxation spin tensor from local frame to sample frame
        spin_matrix = rotateSRTensorTo(spin_matrix,TRF(:,:,laml))
    end subroutine

    subroutine taylor_update_state(IOR,TOTGAMdot,WorkRate,MacroDefRate,CC,TRF,C2,XM)
        type(DeformationRate),intent(in) :: MacroDefRate
        integer, intent(in) :: IOR
        real(dp), intent(in) :: XM(:,:),TRF(3,3),CC(:,:)
        real(dp), intent(out) :: TOTGAMdot, C2(3,3)
        !> Rate of plastic work per unit volume in the crystal
        real(dp), intent(out) :: WorkRate

        real(DP), dimension(size(CC,2)) :: GAMdot
        real(dp), dimension(3) :: ROT
        real(dp), dimension(3,3) :: RCcryst,RHOAcrys
        real(dp), parameter :: ddt=1.0_DP

        call SLIPRAT(GAMdot,SGNN,MacroDefRate,NACTIV,SLIPLP,TLXX,TAURLP,INDACT,INDLP,BB8,XM)
        if (.not. astate%simulCalls(astate%this)%input%keep_state) call hardening_update_state(IOR, ddt, GAMdot)
        TOTGAMdot=sum(abs(GAMdot))
        ! Calculate RCcryst: the rigid body spin in the crystal frame
        RCcryst = rotateSRTensorFrom(MacroDefRate%Spin,TRF)
        RHOAcrys = rotateSRTensorFrom(spin_matrix,TRF)
        WorkRate = sum(merge(CC(1,:),-CC(2,:),GAMdot>0.0_DP)*GAMdot)

        ROT = matmul(B1,GAMdot)

        C2 = 1._DP
        C2(3,2)=ROT(1)-(RCcryst(3,2)+RHOAcrys(3,2))
        C2(1,3)=ROT(2)-(RCcryst(1,3)+RHOAcrys(1,3))
        C2(2,1)=ROT(3)-(RCcryst(2,1)+RHOAcrys(2,1))
        C2(2,3)=-C2(3,2)
        C2(3,1)=-C2(1,3)
        C2(1,2)=-C2(2,1)
        ! KORRIGEREN VAN DE NIEUWE ROTATIEMATRIX
        C2 = matmul(C2,TRF)
    end subroutine
end module
