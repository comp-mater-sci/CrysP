module altayTaylor
    use definitions
    use altayAlgorithms
    use altayMiscutils
    use altayMacroKinematic
    use criMathUtils
    use hardening
    use altayIOConfig
    use altayPancake
    use altaySliprate
    use altayConfig, only: astate
    use hardening_model_dsh
    use logging

    implicit none
    private

    integer,private           :: M,   &       ! number of deformation mechanisms
                                 NGL, &       ! number of glide systems
                                 NTW, &       ! number of twin systems
                                 NACTIV
    real(dp) :: B1(3,96),B(5,5),B2(6,96),G(96),RHOAsa(3,3),SLIPLP(8),TAURLP(8),BB8(5),A2(10,194)
    real(dp), parameter :: TLXX=5.0e-6_dp
    integer :: DI1(5), INDACT(8),INDLP(8)

    public :: &
        TAYLOR1, &
        TAYLOR3, &
        TAYLOR4

    contains

    ! Read slip system file
    subroutine TAYLOR1(M111,A1)

        integer, intent(out) :: M111  ! < total number of systems in slip system file (glide+twin)
        real(dp), intent(inout) :: A1(5,96)

        character(len=72) :: TITglij  !< Name of slip system set
        integer, parameter :: MMAX=96 !< dimension of A1 and other arrays
        real(dp) :: x,y
        integer :: i,j,l,I1


        ! Read name of slip system set
        read (LEC,217) TITglij
  217   format(A)

        read (LEC,210) I,NGL,NTW,DI1,X,Y
 210    format (8I4,4X,2F10.0)
        M=NGL+NTW
        M111=M
        ! read glide + twin systems
        do I1=1,M111
            read (LEC,212) I,(A1(J,I1),J=1,5),(B1(L,I1),L=1,3)
        end do
 212    format (I4,8F20.16)

        do I=1,5
            read (LEC,214) J,(B(I,L),L=1,5)
        end do
 214    format (I4,5D23.16)

        if (NTW /= 0) then
            do I=1,NTW
                read (LEC,212) J,(B2(L,I),L=1,6),G(I)
            end do
        endif
        A2=0.0
        A2(1:5,1:M111)=A1(1:5,1:M111)
        A2(6:10,M111+1:M111*2)=A1(1:5,1:M111)

    end subroutine

    ! OMREKENING/TRANSFORMATION OF DISPLACEMENT GRADIENT.
    subroutine TAYLOR3(SSam,RHOSsa,TRF,GEWF,IOR,ISTP,NBLOC,TRFb,GMMAb,NGR,NRL,laml,CC,M11,MacroDefRate,MacroDefState)
        integer, intent(in) :: NGR,NRL,laml,IOR,ISTP,NBLOC
        real(dp), intent(out) :: Ssam(3,3),RHOSsa(3,3),GEWF
        real(dp), intent(inout) :: CC(2,96)
        type(DeformationRate), intent(in) :: MacroDefRate
        type(DeformationState),intent(in) :: MacroDefState
        integer, intent(in) :: M11
        real(dp), intent(in) :: TRFb(3,3,2),TRF(3,3),GMMAb(2)

        real(dp), dimension(3,3):: RHOScrys, RHOAcrys, Scrys


        call pancak2(NGL,B,DI1,Scrys,RHOScrys,RHOAcrys,IPR,GEWF,A2,MacroDefRate,MacroDefState,NACTIV, &
                     SLIPLP,TLXX,TAURLP,INDACT,INDLP,IOR,ISTP,NBLOC,TRFb,GMMAb,NGR,NRL,laml,BB8,CC,M11)
        !Transform stress from local frame (Scrys) to sample frame (Ssam)
        Ssam = rotateSRTensorTo(Scrys,TRF)
        !Transform relaxation strain rate tensor from local frame (RHOScrys)
        !                                         to sample frame (RHOSsa)
        RHOSsa = rotateSRTensorTo(RHOScrys,TRF)
        !Transform relaxation spin tensor from local frame (RHOAcrys) to sample frame (RHOAsa)
        RHOAsa = rotateSRTensorTo(RHOAcrys,TRF)
        !Report RHOSsa and RHOAsa to LST-file
    end subroutine


    subroutine TAYLOR4(ISTP,IOR,NFILE,TAU,TOTGAMdot,Seq,WorkRate,MacroDefRate,CC,M111,Ssam,RHOSsa,fi1,PHI,fi2, &
                      TRF,C1,C2,ITW,XM)

        type(DeformationRate),intent(in) :: MacroDefRate
        integer, intent(in) :: ISTP,IOR,NFILE, &
              M111     !< total number of systems in slip system file (glide+twin),
        real(dp), intent(in) :: TAU, XM(5,96),TRF(3,3),CC(2,96),RHOSsa(3,3), &
            Ssam(3,3) !< local stress in sample reference system
        real(dp), intent(inout) :: fi1,PHI,fi2
        real(dp), intent(out) :: TOTGAMdot, C2(3,3), C1(3,3), &
                                  Seq ! Equivalent stress in crystal, defined as..
                                      !  plastic work rate in crystal normalized by..
                                      !  (macro) von Mises equivalent strain rate
        !> Rate of plastic work per unit volume in the crystal
        real(dp), intent(out) :: WorkRate
        integer, intent(out) :: ITW

        real(dp), dimension(3) :: TRC,ROT
        real(dp), dimension(3,3) :: RCC,RCcryst,rhossaTot,TDC,RHOAcrys
        real(dp), dimension(96), save :: VOLFR,SGNN,GAMdot
        integer :: info, i,j
        real(dp), save :: ddt,rndm,Mgrain,ratlon,x
        type(EulerAngles) :: Euler


        call SLIPRAT(M111,96,GAMdot,ior,IPR,SGNN,MacroDefRate,NACTIV,SLIPLP,TLXX,TAURLP,INDACT,INDLP,BB8,XM)
        ddt = 1.D0
        if (.not. astate%simulCalls(astate%this)%input%keep_state) call KS_updateState(IOR,GAMdot,ddt,info)
        TOTGAMdot=sum(abs(GAMdot(1:M111)))
        if(NLIST == 1) write (IMP,103) ISTP,IOR,fi1,PHI,fi2

 103    format (' ISTP,IOR',2I5,' phi1, PHI, phi2:',3F15.6)
        ! Calculate RCcryst: the rigid body spin in the crystal frame
        RCcryst = rotateSRTensorFrom(MacroDefRate%Spin,TRF)
        RHOAcrys = rotateSRTensorFrom(RHOAsa,TRF)
        TRC(1)=RCcryst(3,2)+RHOAcrys(3,2)
        TRC(2)=RCcryst(1,3)+RHOAcrys(1,3)
        TRC(3)=RCcryst(2,1)+RHOAcrys(2,1)
        WorkRate = sum(merge(CC(1,1:M111)*GAMdot(1:M111),-CC(2,1:M111)*GAMdot(1:M111),GAMdot(1:M111)>0.0))
        Seq=WorkRate / MacroDefRate%vMeqStrainRate


        ROT = matmul(B1,GAMdot)

        do J=1,3
            C1(J,J)=1.0_dp
        end do
        C1(3,2)=ROT(1)-TRC(1)
        C1(1,3)=ROT(2)-TRC(2)
        C1(2,1)=ROT(3)-TRC(3)
        C1(2,3)=-C1(3,2)
        C1(3,1)=-C1(1,3)
        C1(1,2)=-C1(2,1)
        ! KORRIGEREN VAN DE NIEUWE ROTATIEMATRIX
        Euler= EuleranglesType(matmul(C1,TRF)) ! NIEUWE STAND UITWENDIG ASSENSTELSEL.
        fi1=Euler%fi1
        PHI=Euler%PHI
        fi2=Euler%fi2
        C2 = rotmat(Euler)
        ITW=0
        !Choose at random if the crystal orientation should be considered that of the original or twinned part.
        !See Van Houtte et. al, 1977
        if (NTW /= 0) then
            X=0.
            do I=1,NTW
                X=X+GAMdot(I+NGL)/G(I)
                VOLFR(I)=X
            end do
            if (X > 1.) &
                call log_error('Taylor', 'taylor4', ERR_VAL, 'Total volume fraction of twins exceeds unity')
            call RANDOM_NUMBER(RNDM)
            do I=1,NTW
                if (RNDM < VOLFR(I)) goto 87
            end do
            goto 31
  87        RCC = C2
            TDC(1,1)=B2(1,I)
            TDC(2,1)=B2(2,I)
            TDC(1,2)=B2(2,I)
            TDC(3,1)=B2(3,I)
            TDC(1,3)=B2(3,I)
            TDC(2,2)=B2(4,I)
            TDC(3,2)=B2(5,I)
            TDC(2,3)=B2(5,I)
            TDC(3,3)=B2(6,I)
            C2 = matmul(TDC,RCC)
            ITW=I
            Euler= EuleranglesType(C2)
            fi1=Euler%fi1
            PHI=Euler%PHI
            fi2=Euler%fi2
        endif
  31    if (.not. (nfile == 0.or.istp > 1)) then

            ! the ratio of the parallel strain rates
            ! MacroDefRate%StrainMode & rhossa: expressed in same (sample) reference frame
            ratlon= sum( (MacroDefRate%StrainMode+sqrt(2.0D0/3.0D0)*rhossa) * MacroDefRate%StrainMode )

            ! TAU: Reference-CRSS.
            ! Taylor Factor of the grain:
            Mgrain = TOTGAMdot / MacroDefRate%vMeqStrainRate
            ! Total, i.e. non-normalized, rhossa:
            rhossaTot = rhossa * MacroDefRate%vMeqStrainRate

            write (IMP2,150) ior,Seq,WorkRate,TAU,Mgrain,ratlon, &
             rhossaTot(1,1),rhossaTot(2,2),rhossaTot(3,3),rhossaTot(2,3),rhossaTot(3,1),rhossaTot(1,2), &
             rhoasa(2,3),rhoasa(3,1),rhoasa(1,2),ssam(1,1),ssam(2,2),ssam(3,3),ssam(2,3),ssam(3,1),ssam(1,2)
  150       format(i5,5(E12.5,1X),5x,6(E12.5,1X),5x,3(E12.5,1X),5x,6(E12.5,1X))
        endif
    end subroutine

end module
