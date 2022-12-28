#include "altayRCM.fpp"
module altayPancake
    use altay_definitions
    use altayMiscutils
    use criMathUtils
    use altayRCM
    use altayMesostructure
    use altayIOConfig
    use altayHard
    use altayHardTypes
    use altayTBH
    use altayAlgorithms
    use altayMacroKinematic
    use altayHardLaw_DSH

    implicit none
    contains

! THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
! WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY
!
    subroutine pancak2(KOST,NGL,B,DI1,S33,RHOS33,RHOA33,SWRLX,XX,IPR,GEWF,MacroDefRate,MacroDefState)
        type(DeformationRate),intent(in) :: MacroDefRate
        type(DeformationState),intent(in):: MacroDefState
        integer, intent(in) :: KOST,NGL,IPR,DI1(5)
        logical, intent(in) :: SWRLX(3)

        type(CRSS) :: CRSSmatrix
        real(dp),dimension(3,3),intent(out):: S33, RHOS33, RHOA33
        real(dp),dimension(5):: RHOS, RHOA
        logical :: bas(194),VALID(194)
        integer ::  DI(10),DI2(10)

        ! common blocks
        real(dp) :: TRFb,GMMAb
        integer :: NGR,NRL,laml
        common /LAMEL/ TRFb(3,3,2),GMMAb(2),NGR,NRL,laml                                 ! NRL= number of relaxations, NGR= number of grains
        real(dp) :: CC
        integer :: M11
        common /IGLIJS/ CC(2,96), M11
        real(dp) :: A8,BB8
        common /DOUBLE/ A8(5,96),BB8(5)
        real(dp) :: A1,UU
        common /extra/ A1(10,194),UU(10,10)
        integer :: IOR,ISTP,NBLOC
        common /CEIGEN/ IOR,ISTP,NBLOC
        real(dp) :: SLIPLP,TLXX,TAURLP
        integer:: NACTIV,INDACT,NLP,INDLP
        common /ACTIVE/ NACTIV,INDACT(8),NLP,INDLP(8),SLIPLP(8),TLXX,TAURLP(8)


        real(dp) :: C2(3,3),B(5,5),DACC(10), &
                    rls(3,3),rla(3,3),C3(3,3),TRP(10),APRIME(10),B3(10,3),CUst(10)
        !     first index op PLUMIN = nr. of grain
        !     second index = nr. of relaxation
        !     rlm is unit relaxation tensor in macroscopic frame
        !     rls and rla in crystal frame (symmetric and anti-sym. part)
        real(dp) :: spanv(5),XX(194),STRSS(10),BB(10), &
                    CCC(2,194),DTAU(194),DTAU1(194),TAUR(194),TAUR1(194), &
                    B8(5,2),UBUF(10),UU2(10,10),UU3(10,10),DD(10), &
                    GAMR(2),Tprinc(3,3),TAURL(2),XXTOT,COFCOS,COFSIN,GEWF,fakm
        ! CCC (input): critical resolved shear stresses (Tauc)
        ! UU2 (input): initial inverse of "basis" = columns of A1
        !      corresponding to thoses slip systems which are active
        !      according to first guess
        ! UU (output): inverse of final "basis" (active slip systems)
        ! DI2 (input): indices of basis corresponding to UU2
        ! DI (output): indices of basis corresponding to UU
        ! Dacc (output): slip rates in basis DI
        ! XX (output): slip rates (numbered from 1 to M12)
        ! STRSS (output): stresses, in crystal frames
        !                  (2 sets of stresses, one for each crystal)
        ! Fakm: rate of plastic work of the 2 crsytals together
        ! Taur (output) resolved shear stress (can be + or -)
        ! DTAU (output)=abs(Taur)-Tauc
        data B3/30*0.0D0/
        real(dp), parameter :: SQR2=sqrt(0.5_dp),TOLXX=5.0e-6_dp
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
                    1.0D0, 1.0D0], shape(PLUMIN))
        integer, parameter :: NDIM=10 !     NDIM=dimension A
        data TAURL/2*0.0d0/
        real(dp), parameter :: GETAL=1.0e6_dp, TOL=1.0e-6_dp
        integer :: info,M12,IGrElm,N,M2,IL,L1,IRL,J,I,K1,IG,JJ,II,NU,jsgn
        SAVE

        if (laml.ne.1.and.laml.ne.2) then
            RCM_RAISE(1,'Pancak2','Wrong selection of lamels',RCM_RTN)
        endif
        if (IOR.eq.1) IGrElm=0
        !     N is number of rows of A1;   NU number of rows of UU2
        TLXX=TOLXX
        N=5*NGR
        NU=N
        M2=NGR*M11
        M12=NGR*M11+NRL
        if (laml /= 2) then

            ! Updating of microstructure
            IGrElm=IGrElm+1
            if (IGrElm.gt.NGrElm) IGrElm=1
            call cluster1(NGR,IGrElm,MacroDefRate,MacroDefState,GEWF,Tprinc,Cofcos,Cofsin)
            CCC(1:2,M2+1:M12)=0.0
            UU(1:NU,1:NU) = 0.0_dp
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
                call getCRSS(IOR+IL-1,GMMAb(IL),CRSSmatrix,info)

                ! Assign CRSSmatrix to proper section of CCC
                CCC(:,1+K1:M11+K1)=CRSSmatrix%crss(:,1:M11)

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
            if (IPR.EQ.2 .and. NLIST.eq.1) then
                write (IMP,218) ((CCC(J,I),I=1,M12),J=1,2)
                write (IMP,219) (BB(I),I=1,N)
                write (IMP,400) IOR,ISTP,NBLOC
            endif
 218        format(/' COST FUNCTION',/,(2x,12F10.4))
 219        format (' right hand side',/,(2x,10F10.4),/)
 400        format (' First call of TBH   IOR,ISTP,NBLOC',3I5)
            call TBH(IPR,NDIM,N,M2,A1,BB,CCC,UU,UU2,DI,DI2,Dacc,XX,UBUF,FakM,Taur,bas,Trp,Aprime,CUst,UU3,DD,DTAU,VALID)
            RCM_GUARD

            if (.not.(IPR.lt.4)) then
                RCM_RAISE(1,'Pancak2','IPR must be < 4',RCM_RTN)
            endif
            DTAU1=DTAU
            TAUR1=TAUR

            if (NRL.eq.0) then
                  UU=UU2
                  DI=DI2
                  STRSS=UBUF
            else
                do IRL=1,NRL
                    if (.not.swrlx(IRL)) exit
                    CCC(1:2,M2+IRL)=TAURL(IRL)
                end do
                if (IPR.EQ.2 .and. NLIST.eq.1) write(IMP,218) ((CCC(J,I),I=1,M12),J=1,2)
                ! Second call of Simplex (relaxed constraints)
                if (IPR.eq.2 .and. NLIST.eq.1) write(IMP,401)
 401            format (' Second call of TBH')
                call TBH(IPR,N,N,M12,A1,BB,CCC,UU2,UU,DI2,DI,Dacc,XX,STRSS,FakM,Taur,bas,Trp,Aprime,CUst,UU3,DD,DTAU,VALID)
                RCM_GUARD

                if (IPR.ge.4) then
                   if(NLIST.eq.1) write (IMP,222) IPR,IOR,ISTP,NBLOC
                   write (*,222) IPR,IOR,ISTP,NBLOC
 222               format (' Pancak2 222 - Problem with TBH',/,' IPR IOR, ISTP, NBLOC=',4I5)
                    RCM_RAISE(1,'Pancak2','Problem with TBH',RCM_RTN)
                endif
                ! GAMR will contain the relaxed shears:
                gamr(1:NRL)=XX(M2+1:M2+NRL)
            endif
            ! Check whether 1 grain does not deform at all.
            j=0
            do IG=1,NGR
                XXTOT=0.0
                do i=1,M11
                   j=j+1
                   XXTOT=XXTOT+abs(xx(j))
                enddo
                if (XXTOT.lt.TOLXX) goto 213
            end do
            ! If all grains have a non-zero slip, do the following:
            DTAU1=DTAU
            TAUR1=TAUR
            UBUF=STRSS
 213        if(NLIST.eq.1) write (IMP,780) gamr
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
        !Conversion of RHOA to dim(3,3)
        RHOA33=0.0d0
        RHOA33(2,3)= RHOA(1)*sqr2*MacroDefRate%vMeqStrainRate
        RHOA33(3,1)= RHOA(2)*sqr2*MacroDefRate%vMeqStrainRate
        RHOA33(1,2)= RHOA(3)*sqr2*MacroDefRate%vMeqStrainRate
        RHOA33(3,2)= -RHOA33(2,3)
        RHOA33(1,3)= -RHOA33(3,1)
        RHOA33(2,1)= -RHOA33(1,2)

        if (IPR.EQ.2 .AND. NLIST.eq.1) write (IMP,777) sum(spanv(1:5)*BB(ii+1:ii+5))
  777 format (' spanv . BB          :',d11.4)
       ! note that if one of the grains does
       ! not deform at all, the stress and the active slip systems
       ! of the full constraintssolution are used.)
        NACTIV=0
        do i=1,M11
            j=i+jj
            ! If one grain does not deform, then DTAU1 comes from the full constraints solution.
            if (abs(DTAU1(j)).gt.TOL) cycle
            NACTIV=NACTIV+1
            if (NACTIV.le.8) THEN
                INDACT(NACTIV)=i
            else
                RCM_RAISE(1,'Pancak2','Too many active slip systems',RCM_RTN)
            endif
        enddo
        if (NACTIV.eq.0) then
            RCM_RAISE(1,'Pancak2','No active slip systems found',RCM_RTN)
        endif
        do NLP=1,NACTIV
            INDLP(NLP)=INDACT(NLP)
            SLIPLP(NLP)=XX(INDACT(NLP)+jj)
            TAURLP(NLP)=TAUR1(INDACT(NLP)+jj)
        enddo
    end subroutine


    subroutine Fakeccc(ccc,ccc2,Cofcos,Cofsin,BB,UBUF,M11,ca1,ca2)
        real(dp), intent(in) :: ccc(2,194)
        integer, intent(in) :: M11
        real(dp), intent(out) :: ccc2(2,194)
        real(dp), intent(out) :: ca1,ca2
        real(dp), intent(in) :: Cofcos,Cofsin
        real(dp), intent(in) :: BB(10), & !< direction of the relaxation-1
          UBUF(10) !< BISHOP-HILL stress from TBH routine, in crystal frame

        real(dp) :: zeta

        if((abs(Cofsin) < epsilon(0.D0)) .and. (abs(Cofcos) < epsilon(0.D0))) then
            ccc2=ccc
        elseif(abs(Cofcos).lt.0.000000001) then
            call terminate(stopcode_runtimeerror)
        else
            zeta=sum(UBUF(1:5)*BB(1:5)/norm2(BB(1:5)))/sum(UBUF(6:10)*BB(6:10)/norm2(BB(6:10)))
            if(zeta<0.0_dp) call terminate(stopcode_runtimeerror)
            ca1=Cofcos*Cofcos*sqrt(1.0_dp/zeta)+Cofsin*Cofsin
            ca2=Cofcos*Cofcos*sqrt(zeta)+Cofsin*Cofsin
            CCC2(1:2,1:M11)      =ca1*CCC(1:2,1:M11)
            CCC2(1:2,1+M11:2*M11)=ca2*CCC(1:2,1+M11:2*M11)
        endif
    end subroutine

end module
