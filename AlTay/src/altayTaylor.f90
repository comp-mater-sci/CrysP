module altayTaylor
    use definitions
    use altayMacroKinematic
    use criMathUtils
    use hardening
    use altayPancake
    use altaySliprate
    use altayConfig, only: astate
    use logging

    implicit none
    private

    integer,private           :: M,   &       ! number of deformation mechanisms
                                 NACTIV
    real(dp) :: B1(3,96),B(5,5),B2(6,96),G(96),RHOAsa(3,3),SLIPLP(8),TAURLP(8),BB8(5),A2(10,194)
    real(dp), parameter :: TLXX=5.0e-6_dp
    integer :: DI1(5), INDACT(8),INDLP(8)

    public :: &
        read_deformationsystems, &
        TAYLOR3, &
        TAYLOR4

    contains

    subroutine read_deformationsystems(M111,A1)

        integer, intent(out) :: M111  ! < total number of systems in slip system file
        real(dp), intent(out), allocatable :: A1(:,:)

        character(len=72) :: TITglij  !< Name of slip system set
        real(dp) :: x,y
        integer :: i,j,l,I1, dummy


        ! Read name of slip system set
        read (LEC,217) TITglij
  217   format(A)

        read (LEC,210) I,M,dummy,DI1,X,Y
 210    format (8I4,4X,2F10.0)
        M111=M
        allocate(A1(5,M111))
        do I1=1,M111
            read (LEC,212) I,(A1(J,I1),J=1,5),(B1(L,I1),L=1,3)
        end do
 212    format (I4,8F20.16)

        do I=1,5
            read (LEC,214) J,(B(I,L),L=1,5)
        end do
 214    format (I4,5D23.16)

        A2=0.0_DP
        A2(1:5,1:M111)=A1(1:5,1:M111)
        A2(6:10,M111+1:M111*2)=A1(1:5,1:M111)

    end subroutine read_deformationsystems

    ! OMREKENING/TRANSFORMATION OF DISPLACEMENT GRADIENT.
    subroutine TAYLOR3(SSam,RHOSsa,TRF,GEWF,IOR,TRFb,GMMAb,NGR,NRL,laml,CC,M11,MacroDefRate,MacroDefState)
        integer, intent(in) :: NGR,NRL,laml,IOR, M11
        real(dp), intent(out) :: Ssam(3,3),RHOSsa(3,3)
        real(dp), intent(inout) :: CC(2,M11),GEWF
        type(DeformationRate), intent(in) :: MacroDefRate
        type(DeformationState),intent(in) :: MacroDefState
        real(dp), intent(in) :: TRFb(3,3,2),TRF(3,3),GMMAb(2)

        real(dp), dimension(3,3):: RHOScrys, RHOAcrys, Scrys


        call pancak2(M,B,DI1,Scrys,RHOScrys,RHOAcrys,GEWF,A2,MacroDefRate,MacroDefState,NACTIV, &
                     SLIPLP,TLXX,TAURLP,INDACT,INDLP,IOR,TRFb,GMMAb,NGR,NRL,laml,BB8,CC,M11)
        !Transform stress from local frame to sample frame
        Ssam = rotateSRTensorTo(Scrys,TRF)
        !Transform relaxation strain rate tensor from local frame to sample frame
        RHOSsa = rotateSRTensorTo(RHOScrys,TRF)
        !Transform relaxation spin tensor from local frame to sample frame
        RHOAsa = rotateSRTensorTo(RHOAcrys,TRF)
    end subroutine


    subroutine TAYLOR4(IOR,TOTGAMdot,WorkRate,MacroDefRate,CC,M111,TRF,C2,XM)

        type(DeformationRate),intent(in) :: MacroDefRate
        integer, intent(in) :: IOR, &
              M111     !< total number of systems in slip system file
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
        RHOAcrys = rotateSRTensorFrom(RHOAsa,TRF)
        WorkRate = sum(merge(CC(1,1:M111),-CC(2,1:M111),GAMdot(1:M111)>0.0_DP)*GAMdot(1:M111))


        ROT = matmul(B1(:,1:M111),GAMdot(1:M111))

        do J=1,3
            C2(J,J)=1.0_dp
        end do
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
