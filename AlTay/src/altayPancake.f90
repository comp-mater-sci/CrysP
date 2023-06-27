#include "altayRCM.fpp"
module altayPancake
    use definitions
    use altayMiscutils
    use criMathUtils
    use altayRCM
    use altayMesostructure
    use altayIOConfig
    use altayTBH
    use altayAlgorithms
    use altayMacroKinematic
    use hardening

    implicit none
    private

    public :: pancak2

    contains

! THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
! WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY
!
    subroutine pancak2(NGL,B,DI1,S33,RHOS33,RHOA33,IPR,GEWF,A1,MacroDefRate,MacroDefState,NACTIV,&
                       SLIPLP,TLXX,TAURLP,INDACT,INDLP,IOR,ISTP,NBLOC,TRFb,GMMab,NGR,NRL,laml,BB8,CC,M11)
        type(DeformationRate),intent(in) :: MacroDefRate
        type(DeformationState),intent(in):: MacroDefState
        integer, intent(in) :: NGL,IPR,DI1(5),ISTP,NBLOC,NGR,NRL,laml,IOR,M11
        real(dp),dimension(3,3),intent(out):: S33, RHOS33, RHOA33,BB8(5)
        integer, intent(out) :: NACTIV
        integer, intent(inout) :: INDACT(8),INDLP(8)
        real(dp), intent(in) :: B(5,5),TRFb(3,3,2),TLXX,GMMab(2)
        real(dp), intent(inout) :: CC(2,96),GEWF,A1(10,194),SLIPLP(8),TAURLP(8)

        real(dp),dimension(5):: RHOS, RHOA
        integer ::  DI(10)
        real(dp) :: C2(3,3),rls(3,3),rla(3,3),C3(3,3),B3(10,3)=0.0_dp,UU(5*NGR,5*NGR), &
                    spanv(5),XX(194),STRSS(10),BB(10),CCC(2,194),DTAU(194),DTAU1(194),TAUR(194),TAUR1(194), &
                    B8(5,2),UBUF(10),GAMR(2),Tprinc(3,3),TAURL(2)=0.0_dp,XXTOT,COFCOS,COFSIN
        ! rls and rla are unit relaxation tensors in crystal frame (symmetric and anti-sym. part)
        real(dp), parameter :: SQR2=sqrt(0.5_dp)
        !     Definition of the two relaxations, representing a
        !     13-simple shear and a 23-simple shear, respectively:
        real(dp), dimension(3,3,3) :: relax = reshape([ &
                    0.0D0, 0.0D0, 0.0D0,                                   &
                    0.0D0, 0.0D0, 0.0D0,                                   &
                    1.0D0, 0.0D0, 0.0D0,                                   &
                    0.0D0, 0.0D0, 0.0D0,                                   &
                    0.0D0, 0.0D0, 0.0D0,                                   &
                    0.0D0, 1.0D0, 0.0D0,                                   &
                    0.0D0, 0.0D0, 0.0D0,                                   &
                    0.0D0, 0.0D0, 0.0D0,                                   &
                    0.0D0, 0.0D0, 0.0D0],shape(relax))
        real(dp), dimension(2,3) ::  PLUMIN = reshape([&
                    1.0D0,-1.0D0,                                          &
                    1.0D0,-1.0D0,                                          &
                    1.0D0, 1.0D0], shape(PLUMIN)) !first index: # of grain, second index: #of relaxation
        real(dp), parameter :: GETAL=1.0e6_dp, TOL=1.0e-6_dp
        integer :: info,M12,IGrElm,N,M2,IL,L1,IRL,J,I,K1,IG,JJ,II
        SAVE


        if (IOR == 1) IGrElm=0
        ! N is number of rows of A1
        N=5*NGR
        M2=NGR*M11
        M12=NGR*M11+NRL
        if (laml == 1) then

            ! Updating of microstructure
            IGrElm=IGrElm+1
            if (IGrElm > NGrElm) IGrElm=1
            call cluster1(NGR,IGrElm,MacroDefRate,MacroDefState,GEWF,Tprinc,Cofcos,Cofsin)
            CCC(1:2,M2+1:M12)=0.0
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
                        C3 = rotateSRTensorFrom(rotateSRTensorTo(RELAX(:,:,IRL),Tprinc),TRFb(:,:,IL))
                        RLS=(C3+transpose(C3))*0.5_dp
                        RLA=(C3-transpose(C3))*0.5_dp
                        B3(L1+1,IRL)=PLUMIN(IL,IRL)*RLA(2,3)/sqr2
                        B3(L1+2,IRL)=PLUMIN(IL,IRL)*RLA(3,1)/sqr2
                        B3(L1+3,IRL)=PLUMIN(IL,IRL)*RLA(1,2)/sqr2
                        !  Insert the relaxations as columns in A1-matrix
                        A1(L1+1:L1+5,M2+IRL)=Vector5D(RLS)*PLUMIN(IL,IRL)
                    end do
                endif

                ! Calculation of time increment by dividing von Mises equivalent
                ! strain by von Mises equivalent strain rate
                B8(1:5,IL)=Vector5D(C2)/MacroDefRate%vMeqStrainRate ! sym.(3,3) -> (5)
                BB(L1+1:L1+5)=B8(1:5,IL)
                K1=M11*(IL-1)

                ! Retrieve the CRSSmatrix
                !    IOR+IL-1  = sequence number of current grain
                !    GMMAb(IL) = the GAMMA of current grain

                ! Assign CRSSmatrix to proper section of CCC
                CCC(:,1+K1:M11+K1) = hardening_get_crss(IOR+IL-1)

                ! Set Tau_crit for antitwinning direction equal to
                ! GETAL times Tau_crit for twinning direction
                do I=NGL+1,M11 ! this do-loop will only be executed for NTW>0
                    CCC(2,I+K1)=CCC(1,I+K1)*GETAL
                end do
                UU(L1+1:L1+5,L1+1:L1+5)=B
            enddo
            if (NRL /= 0) CCC(1:2,M2+1:M12)=GETAL
            ! The coefficient of the relaxations is set to a very large number
            ! in order to suppress the relaxations in a first call of the TBH program
            ! Full constraints calculation
            ! UITVOEREN VAN DE SIMPLEX-SUBROUTINE
            if (IPR == 2 .and. NLIST == 1) then
                write (IMP,218) ((CCC(J,I),I=1,M12),J=1,2)
                write (IMP,219) (BB(I),I=1,N)
                write (IMP,400) IOR,ISTP,NBLOC
            endif
 218        format(/' COST FUNCTION',/,(2x,12F10.4))
 219        format (' right hand side',/,(2x,10F10.4),/)
 400        format (' First call of TBH   IOR,ISTP,NBLOC',3I5)
            call TBH(A = A1(1:N,1:M2), &
                     D = BB(1:N), &
                     TauC = CCC(1:2,1:M2), &
                     U = UU, &
                     Irp = DI(1:N), &
                     GDOT = XX(1:M2), &
                     SIG = UBUF(1:N), &
                     TauR = Taur(1:M2), &
                     DTAU = DTAU(1:M2))
            RCM_GUARD

            if (.not.(IPR < 4)) then
                RCM_RAISE(1,'Pancak2','IPR must be < 4',RCM_RTN)
            endif
            DTAU1=DTAU
            TAUR1=TAUR

            if (NRL == 0) then
                STRSS=UBUF
            else
                do IRL=1,NRL
                    CCC(1:2,M2+IRL)=TAURL(IRL)
                end do
                if (IPR == 2 .and. NLIST == 1) write(IMP,218) ((CCC(J,I),I=1,M12),J=1,2)
                ! Second call of Simplex (relaxed constraints)
                if (IPR == 2 .and. NLIST == 1) write(IMP,401)
 401            format (' Second call of TBH')
                call TBH(A = A1(1:N,1:M12), &
                         D = BB(1:N), &
                         TauC = CCC(1:2,1:M2), &
                         U = UU, &
                         Irp = DI(1:N), &
                         GDOT = XX(1:M12), &
                         SIG = STRSS(1:N), &
                         TauR = Taur(1:M12), &
                         DTAU = DTAU(1:M12))
                RCM_GUARD

                if (IPR >= 4) then
                    if(NLIST == 1) write (IMP,222) IPR,IOR,ISTP,NBLOC
                    write (*,222) IPR,IOR,ISTP,NBLOC
 222                format (' Pancak2 222 - Problem with TBH',/,' IPR IOR, ISTP, NBLOC=',4I5)
                    RCM_RAISE(1,'Pancak2','Problem with TBH',RCM_RTN)
                endif
                ! GAMR will contain the relaxed shears:
                gamr(1:NRL)=XX(M2+1:M2+NRL)
            endif
            ! Check whether 1 grain does not deform at all.
            do IG=0,NGR-1
                XXTOT=sum(abs(xx(1+M11*IG:M11+M11*IG)))
                if (XXTOT < TLXX) goto 213
            end do
            ! If all grains have a non-zero slip, do the following:
            DTAU1=DTAU
            TAUR1=TAUR
            UBUF=STRSS
 213        if(NLIST == 1) write (IMP,780) gamr
 780        format (' RELAXATIONS:                   ',2d12.4)
        endif
        !     From here on, output is produced for grain number "laml"
        jj=M11*(laml-1)
        CC(1:2,1:M11)=CCC(1:2,jj+1:jj+M11)
        ii=5*(laml-1)
        do i=1,5
            ! If one grain does not deform, the stress UBUF came from the fullconstraints solution.
            spanv(i)=UBUF(i+ii)
            RHOS(i)=-sum(A1(i+ii,M2+1:M2+NRL)*gamr(1:NRL))
            BB8(i)=B8(i,laml)+RHOS(i)
            RHOA(i)=-sum(B3(i+ii,1:NRL)*gamr(1:NRL))
        enddo
        S33=    SymMatrix(spanv) ! (5) -> sym.(3,3)
        RHOS33= SymMatrix(RHOS)  ! (5) -> sym.(3,3)
        RHOA33=0.0d0
        RHOA33(2,3)= RHOA(1)*sqr2*MacroDefRate%vMeqStrainRate
        RHOA33(3,1)= RHOA(2)*sqr2*MacroDefRate%vMeqStrainRate
        RHOA33(1,2)= RHOA(3)*sqr2*MacroDefRate%vMeqStrainRate
        RHOA33(3,2)= -RHOA33(2,3)
        RHOA33(1,3)= -RHOA33(3,1)
        RHOA33(2,1)= -RHOA33(1,2)

        if (IPR == 2 .AND. NLIST == 1) write (IMP,777) sum(spanv(1:5)*BB(ii+1:ii+5))
  777   format (' spanv . BB          :',d11.4)
        ! note that if one of the grains does not deform at all, the stress and the active slip systems
        ! of the full constraint solution are used.
        NACTIV=count(abs(DTAU1(jj+1:jj+M11)) <= TOL)
        if (NACTIV > 8) then
            RCM_RAISE(1,'Pancak2','Too many active slip systems',RCM_RTN)
        elseif (NACTIV == 0) then
            RCM_RAISE(1,'Pancak2','No active slip systems found',RCM_RTN)
        else
            j = 0
            do i=1,M11
                if (abs(DTAU1(i+jj)) > TOL) cycle
                j=j+1
                INDACT(j)=i
            enddo
        endif
        do i=1,NACTIV
            INDLP(i)=INDACT(i)
            SLIPLP(i)=XX(INDACT(i)+jj)
            TAURLP(i)=TAUR1(INDACT(i)+jj)
        enddo
    end subroutine

end module
