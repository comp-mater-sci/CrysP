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

    integer  :: M, &         ! number of deformation mechanisms
                NRL,   &
                NGL, &
                NACTIV, &
                NGR
    real(dp) :: B1(3,96),B(5,5),B2(6,96),G(96),spin_matrix(3,3),SLIPLP(8),TAURLP(8),BB8(5), A2(10,194)
    real(dp), parameter :: TLXX=5.0e-6_dp
    integer :: DI1(5), INDACT(8),INDLP(8)
    
    character(*), parameter :: MOD_NAME = 'taylor'

    integer, dimension(5), parameter ::    INITIAL_BASIS_SYSTEMS_FCC = [2,5,6,7,8], &
                                           INITIAL_BASIS_SYSTEMS_BCC = [1,2,4,5,7]

    real(DP), parameter :: RELAX(3,3,2) = reshape([0._DP, 0._DP, 0._DP, &
                                                   0._DP, 0._DP, 0._DP, &
                                                   1._DP, 0._DP, 0._DP, &
                                                   0._DP, 0._DP, 0._DP, &
                                                   0._DP, 0._DP, 0._DP, &
                                                   0._DP, 1._DP, 0._DP], shape(RELAX)), &
                           PLUMIN(2,2) = reshape([1._DP,-1._DP, &
                                                  1._DP,-1._DP], shape(PLUMIN)), & 
                           GETAL = 1.0e6_dp,    &
                           TOL = 1.0e-6_dp

    public  ::  taylor_init, &
                taylor_solve, &
                taylor_update_state

contains

    subroutine taylor_init(deformation_mechanism, M111,A1)
        integer, intent(out) :: M111  ! < total number of systems in slip system file (glide+twin)
        real(dp), intent(out), allocatable :: A1(:,:)
        integer, dimension(:,:,:), intent(in) :: deformation_mechanism
        integer :: i,j,k,l,I1
        real(DP), dimension(:,:,:), allocatable :: deformation_mechanism_tensor, deformation_mechanism_normalized
        real(DP) :: basis(5,5), &
                    antisym(3,3)
        
        M = size(deformation_mechanism,3)
        DI1 = merge(INITIAL_BASIS_SYSTEMS_FCC, INITIAL_BASIS_SYSTEMS_BCC, M == 12)
        M111=M
            
        allocate(deformation_mechanism_normalized(3,2,M))
        allocate(deformation_mechanism_tensor(3,3,M))
        allocate(A1(5,M))
        do i=1,M
            forall (j=1:2) deformation_mechanism_normalized(:,j,i) = real(deformation_mechanism(:,j,i),DP) / norm2(real(deformation_mechanism(:,j,i), DP))
            forall (j=1:3,k=1:3) deformation_mechanism_tensor(j,k,i) = deformation_mechanism_normalized(j,1,i) * deformation_mechanism_normalized(k,2,i)
            A1(:,i) = convert_stress_strain_space(deformation_mechanism_tensor(:,:,i)) 
            antisym = (deformation_mechanism_tensor(:,:,i) - transpose(deformation_mechanism_tensor(:,:,i)))/2._DP
            B1(:,i) = [-antisym(3,2), -antisym(1,3), -antisym(2,1)] !This conversion can likely be replaced by a more intuitive one
            print *, A1(:,i)
            print *, B1(:,i)
        end do
        forall (i=1:5) basis(:,i) = A1(:,DI1(i))

        B = inv(basis)
        
        A2=0.0_DP
        A2(1:5,1:M)=A1(1:5,1:M)
        A2(6:10,M+1:M*2)=A1(1:5,1:M)
    end subroutine   

    ! Returns the inverse of a matrix calculated by finding the LU
    ! decomposition.  Depends on LAPACK.
    function inv(A) result(Ainv)
      real(dp), dimension(:,:), intent(in) :: A
      real(dp), dimension(size(A,1),size(A,2)) :: Ainv
                                                                          
      real(dp), dimension(size(A,1)) :: work  ! work array for LAPACK
      integer, dimension(size(A,1)) :: ipiv   ! pivot indices
      integer :: n, info
    
      ! External procedures defined in LAPACK
      external DGETRF
      external DGETRI
                                                                         
      ! Store A in Ainv to prevent it from being overwritten by LAPACK
      Ainv = A 
      n = size(A,1)
                                                                         
      ! DGETRF computes an LU factorization of a general M-by-N matrix A
      ! using partial pivoting with row interchanges.
      call DGETRF(n, n, Ainv, n, ipiv, info)
                                                                         
      if (info /= 0) then
         stop 'Matrix is numerically singular!'
      end if
                                                                         
      ! DGETRI computes the inverse of a matrix using the LU factorization
      ! computed by DGETRF.
      call DGETRI(n, Ainv, n, ipiv, work, n, info)
                                                                         
      if (info /= 0) then
         stop 'Matrix inversion failed!'
      end if
    end function inv 

    subroutine taylor_solve(stress_matrix,strain_matrix, TRF, GEWF, IOR, TRFb, GMMAb, NGR, NRL, laml, CC, M11, MacroDefRate,MacroDefState)
        type(DeformationRate),intent(in) :: MacroDefRate
        type(DeformationState),intent(in) :: MacroDefState
        integer, intent(in) :: laml,IOR, NGR, NRL, M11
        real(dp), intent(out) :: stress_matrix(3,3),strain_matrix(3,3)
        real(dp), intent(in) :: TRFb(3,3,2),TRF(3,3), GMMab(2)
        real(dp), intent(inout) :: CC(2,M11),GEWF
        real(dp),dimension(5):: strain, spin
        real(dp) :: TPrinc(3,3)
        real(dp) :: C2(3,3), rls(3,3), rla(3,3), C3(3,3), spanv(5), DTAU(194), STRSS(10), TAUR(194)
        real(dp), save :: B3(10,3)=0.0_dp, XX(194),BB(10),CCC(2,194),DTAU1(194),TAUR1(194),B8(5,2),UBUF(10),GAMR(2)
        real(DP) :: mat_buffer(3,3), UU(5*NGR, 5*NGR)
        integer :: M12,N,M2,IL,L1,IRL,I,K1,IG,JJ,II, DI(10)
        integer, save :: IGrElm

        character(*), parameter :: PROC_NAME = 'pancak2'

        if (IOR == 1) IGrElm=0
        N=5*NGR
        M2=NGR*M
        M12=NGR*M+2
        if (laml == 1) then

            ! Updating of microstructure
            IGrElm=IGrElm+1
            if (IGrElm > NGrElm) IGrElm=1
            if (NGR == 1) then
                Tprinc = unit_sr_Matrix
            else
                call cluster1(IGrElm,MacroDefRate,MacroDefState,GEWF,Tprinc)
            endif
            CCC(1:2,M2+1:M12)=0.0_DP
            UU = 0.0_dp
            DI(1:5) = DI1
            DI(6:10) = DI1+M11

            do IL=1,NGR
                L1=5*(IL-1)

                C2 = rotateSRTensorFrom(MacroDefRate%VelGrad,TRFb(:,:,IL))
                if (NRL /= 0) then
                    do IRL=1,NRL
                        ! Transform relaxation from grain reference frame to macroscopic frame
                        !   ... and now to crystal frame:
                        mat_buffer = rotateSRTensorTo(RELAX(:,:,IRL),Tprinc)
                        C3 = rotateSRTensorFrom(mat_buffer,TRFb(:,:,IL))
                        RLS=(C3+transpose(C3))*0.5_dp
                        RLA=(C3-transpose(C3))*0.5_dp
                        B3(L1+1,IRL)=PLUMIN(IL,IRL)*RLA(2,3)/sqr2
                        B3(L1+2,IRL)=PLUMIN(IL,IRL)*RLA(3,1)/sqr2
                        B3(L1+3,IRL)=PLUMIN(IL,IRL)*RLA(1,2)/sqr2
                        !  Insert the relaxations as columns in A1-matrix
                        A2(L1+1:L1+5,M2+IRL)=convert_stress_strain_space(RLS)*PLUMIN(IL,IRL)
                    end do
                endif

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
            call simplex_solve(taylor_coeffs = A2(1:N, 1:M12), &
                     strain = BB, &
                     crss = CCC, &
                     inverse_basis = UU(1:N,1:N), &
                     basis_systems = DI(1:N), &
                     slip = XX, &
                     stress = UBUF, &
                     rss = Taur, &
                     overstress = DTAU)

            DTAU1=DTAU
            TAUR1=TAUR

            if (NRL == 0) then
                STRSS=UBUF
            else
                do IRL=1,NRL
                    CCC(1:2,M2+IRL)=0.0_DP
                end do
                call simplex_solve(taylor_coeffs = A2(1:N,1:M12), &
                         strain = BB(1:N), &
                         crss = CCC(1:2,1:M12), &
                         inverse_basis = UU(1:N,1:N), &
                         basis_systems = DI(1:N), &
                         slip = XX(1:M12), &
                         stress = STRSS(1:N), &
                         rss = Taur(1:M12), &
                         overstress = DTAU(1:M12))

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
        stress_matrix = rotateSRTensorTo(stress_matrix,TRF)
        !Transform relaxation strain rate tensor from local frame to sample frame
        strain_matrix = rotateSRTensorTo(strain_matrix,TRF)
        !Transform relaxation spin tensor from local frame to sample frame
        spin_matrix = rotateSRTensorTo(spin_matrix,TRF)
    end subroutine

    subroutine taylor_update_state(IOR,TOTGAMdot,WorkRate,MacroDefRate,CC,M111,TRF,C2,XM)
        type(DeformationRate),intent(in) :: MacroDefRate
        integer, intent(in) :: IOR, &
              M111     !< total number of systems in slip system file (glide+twin),
        real(dp), intent(in) :: XM(:,:),TRF(3,3),CC(2,M111)
        real(dp), intent(out) :: TOTGAMdot, C2(3,3)
        !> Rate of plastic work per unit volume in the crystal
        real(dp), intent(out) :: WorkRate

        real(dp), dimension(3) :: ROT
        real(dp), dimension(3,3) :: RCcryst,TDC,RHOAcrys
        real(dp), dimension(96), save :: SGNN,GAMdot
        integer :: i,j
        real(dp) :: rndm,x
        real(dp), parameter :: ddt=1.0_DP

        call SLIPRAT(M111,GAMdot(1:M111),SGNN(1:M111),MacroDefRate,NACTIV,SLIPLP,TLXX,TAURLP,INDACT,INDLP,BB8,XM)
        if (.not. astate%simulCalls(astate%this)%input%keep_state) call hardening_update_state(IOR, ddt, GAMdot)
        TOTGAMdot=sum(abs(GAMdot(1:M111)))
        ! Calculate RCcryst: the rigid body spin in the crystal frame
        RCcryst = rotateSRTensorFrom(MacroDefRate%Spin,TRF)
        RHOAcrys = rotateSRTensorFrom(spin_matrix,TRF)
        WorkRate = sum(merge(CC(1,1:M111),-CC(2,1:M111),GAMdot(1:M111)>0.0_DP)*GAMdot(1:M111))

        ROT = matmul(B1(:,1:M111),GAMdot(1:M111))

        forall (j=1:3) C2(j,j) = 1._DP
        
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
