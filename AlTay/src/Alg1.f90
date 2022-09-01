#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
      module altayTaylor
      use altayAlgorithms
      use altayMiscutils, only: terminate, stopcode_runtimeerror
      use altayMacroKinematic
      use criMathUtils
      integer,parameter,private :: N = 5, N1 = N + 1

      integer,private           :: M,   &       ! number of glide systems + number of twin systems
                                   NGL, &       ! number of glide systems
                                   NTW          ! number of twin systems
      double precision,private  :: B1(3,96),B(5,5),B2(6,96),G(96)
      integer,private           :: DI1(5)

      contains

! MODIFICATIONS AUG 2010
! THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
! WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY
!
      subroutine TAYLOR(IRICHT, KOST, MacroDefRate, MacroDefState)
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif
      use altayIOConfig
      use altayPancake
      implicit double precision (a-h,o-z)
      ! optional argument - required for IRICHT=2 or 3:
      type(DeformationRate), intent(in),optional :: MacroDefRate
      ! optional argument - required for IRICHT=3:
      type(DeformationState),intent(in),optional :: MacroDefState
      common /TEXTUR/ TRF(3,3),C1(3,3),C2(3,3),NO,                       &
      ITW,GEWF
      common /IGLIJS/ M11,CC(2,96)      ! M11...total number of systems in slip system file (glide+twin),
      common /TLR2/ RHOAsa
      common /DOUBLE/ A1(5,96),BB8(5),RHO(5),B5(5)
      common /GENRLX/ YY(5,5),SHsam(3,3),Ssam(3,3),RHOSsa(3,3),          &
       SWRLX(3)
      double precision, dimension(3,3):: RHOScrys(3,3)
      double precision, dimension(3,3):: RHOAcrys(3,3), RHOAsa(3,3)
      character(len=72) :: TITglij                                          ! Name of slip system set
!
!     Extra arrays necessary for linear programming of 2 grains simultaneously
!
      common /extra/ A2(10,194),UU(10,10)
      dimension XXLP(194)
      logical SWRLX
      integer R
      data MMAX/96/ ! dimension of A1 and other arrays
!
!     DVM = von Mises equivalent strain rate
!
      ! Local stress in crystal reference system:
      double precision, dimension(3,3):: Scrys=0.0d0
      save
      !
      goto (1000,2000,3000),IRICHT
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! IRICHT=1
      ! Read slip system file
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !
 1000 if(NLIST.eq.1) then
            write (IMP,216)
      end if
 216  format (/,' SUBROUTINE TAYLOR - READS ITS CRYSTAL DATA',//)
!
      R=LEC         ! slip system file
!
      ! Read name of slip system set
  507 read (R,217) TITglij
  217 format(A)
      if(NLIST.eq.1) write (IMP,221) TITglij
  221 format (/,' Slip system set:',A,/)
      !
      read (R,210) I,NGL,NTW,DI1,X,Y        ! I is implicitly typed integer
 210  format (8I4,4X,2F10.0)
      if(NLIST.eq.1) write (IMP,211) I,NGL,NTW,DI1
 211  format (1X,I4,10X,2I5,10X,5I5)
#ifndef ALTAY_SUBROUTINE
      if (I.NE.0) call terminate(stopcode_runtimeerror)
#else
      if (I.NE.0) then
            RCM_RAISE(1,'TAYLOR','Improper slip system set',RCM_RTN)
      endif
#endif
      M=NGL+NTW
      M11=M
      if (M11.gt.MMAX)then
#ifndef ALTAY_SUBROUTINE
            write (*,5001) M11,MMAX
            if(NLIST.eq.1) write (IMP,5001) M11,MMAX
            call terminate(stopcode_runtimeerror)
#else
            RCM_RAISE(1,'TAYLOR','Too large slip system set',RCM_RTN)
#endif
      endif
 5001 format(' TAYLOR - NGL+NTW=',I5,' LARGER THAN  MMAX=',I5)
      ! read glide + twin systems
      do I1=1,M11
          read (R,212) I,(A1(J,I1),J=1,5),(B1(L,I1),L=1,3)
          if(NLIST.eq.1) write (IMP,213) I,(A1(J,I1),J=1,5),(B1(L,I1),L=1,3)
      end do
 212  format (I4,8F20.16)
 213  format (I3,' A ',5F10.7,' B ',3F10.7)
      !
      do I=1,5
          read (R,214) J,(B(I,L),L=1,5) ! J implicitly typed as integer
          if(NLIST.eq.1) write (IMP,215) J,(B(I,L),L=1,5)
      end do
 214  format (I4,5D23.16)
 215  format (1X,I4,10X,5D15.8)
      !
      if (NTW /= 0) then
          do I=1,NTW
              read (R,212) J,(B2(L,I),L=1,6),G(I)
              if(NLIST.eq.1) write (IMP,218) J,(B2(L,I),L=1,6),G(I)
          end do
 218      format (i4,' B2',6f10.7,' G',f10.7)
      endif
      ! set all components of A2=0
 502  A2=0.0
      do j=1,M11
          do i=1,5
              x8=A1(i,j)
              A2(i,j)=x8
              A2(i+5,j+M11)=x8
      end do; end do
      return
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
! 2000 if (IGLIJ.EQ.0) goto 70
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! IRICHT=2
      ! Write velocity gradient, strain rate and spin tensor; then check norm of strain rate
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !
 2000 continue
      if(NLIST.eq.1) write (IMP,203)
      do I=1,3
          if(NLIST.eq.1) &
              write (IMP,204) (MacroDefRate%VelGrad(I,J),J=1,3),         &
                              (MacroDefRate%StrainRate(I,J),J=1,3),      &
                              (MacroDefRate%Spin(I,J),J=1,3)
      end do
 203  format (' TAYLOR - VELOCITY GRADIENT WHICH WILL BE USED FOR THE SIMULATION', &
              //T9,'GLOBAL TENSOR',T47,'SYMMETRICAL PART',T85,     &
              'ANTISYMMETRICAL PART',/)
 204  format (1X,3(3F10.5,10X))
  70  continue
      if (MacroDefRate%NormStrainRate.lt.1.0D-10) then
#ifndef ALTAY_SUBROUTINE
         write (*,205) MacroDefRate%NormStrainRate
         if(NLIST.eq.1) write (IMP,205) MacroDefRate%NormStrainRate
         call terminate(stopcode_runtimeerror)
#else
         RCM_RAISE(1,'TAYLOR','Symmetric part of the strain step is too small',RCM_RTN)
#endif
      endif
 205  format (' Taylor - symmetric part of strain step is too small'     &
       ,d20.8)
!
      return
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! IRICHT=3
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !
      ! OMREKENING/TRANSFORMATION OF DISPLACEMENT GRADIENT.
 3000 continue
!      write (*,1234)
! 1234 format (' Just before Pancak2')
      !
      call Pancak2(KOST,NGL,B,DI1,Scrys,RHOScrys,RHOAcrys,              &
                    SWRLX,XXLP,IPR,GEWF,MacroDefRate,MacroDefState)
      ! OUT: Scrys,RHOScrys,RHOAcrys
      !
      !Report Scrys to LST-file
 100  format(' Bishop-Hill stress (crystal system):')
 101  format(3d20.7)
      if(NLIST.eq.1) then
          write (IMP,100)
          do i=1,3
              write (IMP,101) (Scrys(i,j),j=1,3)
          end do
      end if
      !Transform stress from local frame (Scrys) to sample frame (Ssam)
      Ssam = rotateSRTensorTo(Scrys,TRF)
      !Transform relaxation strain rate tensor from local frame (RHOScrys)
      !                                         to sample frame (RHOSsa)
      RHOSsa = rotateSRTensorTo(RHOScrys,TRF)
      !Transform relaxation spin tensor from local frame (RHOAcrys) to sample frame (RHOAsa)
      RHOAsa = rotateSRTensorTo(RHOAcrys,TRF)
      !Report RHOSsa and RHOAsa to LST-file
 1701 format(/,' RHOSsa')
 1706 format(/,' RHOAsa')
      if(NLIST.eq.1) then
          write (IMP,1701)
          do i=1,3
              write (IMP,101) (RHOSsa(i,j),j=1,3)
          end do
          write (IMP,1706)
          do i=1,3
              write (IMP,101) (RHOAsa(i,j),j=1,3)
          end do
      end if

#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif
!      write (*,1235)
! 1235 format (' Just after Pancak2')
      return
      end subroutine
      !
      !
      subroutine TAYLR1(ISTP,IOR,NFILE,TAU,TOTGAMdot,Seq,WorkRate,MacroDefRate)
#ifdef ALTAY_SUBROUTINE
      use altayRCM
      use altayConfig, only: astate
#endif
#ifdef PEBP_ENABLED
      use AltayDSHstate, KOST => iKOST
#endif
      use altayIOConfig
      use altaySliprate
      use altayHard, only: hard_BP, hard_PEBPscrew, hard_PEBPloop
      implicit double precision (a-h,o-z)
      type(DeformationRate),intent(in) :: MacroDefRate
      common /TEXTUR/ TRF(3,3),C1(3,3),C2(3,3),NO,ITW,GEWF
      common /IGLIJS/ M11,CC(2,96)
      common/TLR2/ RHOAsa
      common /DOUBLE/ A1(5,96),BB8(5),RHO(5),B5(5)
      common /EULERA/ fi1,PHI,fi2
      logical SWRLX
      double precision, intent(out):: Seq ! Equivalent stress in crystal, defined as..
                                    !  plastic work rate in crystal normalized by..
                                    !  (macro) von Mises equivalent strain rate
      !> Rate of plastic work per unit volume in the crystal
      double precision, intent(out) :: WorkRate
      double precision :: Mgrain
      type(EulerAngles):: Euler
!
!     SHsam:    macroscopic stress in sample reference system
!     SH:   macroscopic stress in crystal reference system
!     SPANH: macroscopic stress in crystal reference system
!     Ssam:        local stress in sample reference system
!
      common /GENRLX/ YY(5,5),SHsam(3,3),Ssam(3,3),RHOSsa(3,3),          &
       SWRLX(3)
      DIMENSION RCC(3,3),RCcryst(3,3),rhossaTot(3,3)
      DIMENSION TRC(3),VOLFR(96),ROT(3),TDC(3,3),SGNN(96)
      dimension RHOAsa(3,3),RHOAcrys(3,3),GAMdot(96)
      real, dimension(3,3) :: test !!single precision!!
!      data SQR2/0.7071067811865476D+00/
#ifdef PEBP_ENABLED
      integer :: info
      double precision :: ddt
#endif
      double precision, intent(OUT) :: TOTGAMdot
      SAVE
      WACC1=0.0
      WACC2=0.0
!      pause
!EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
!      if (IGLIJ.eq.0) goto 11
!EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
!  11  write (*,1771) IOR
! 1771 format (I5)
  11  call SLIPRAT(M11,96,GAMdot,ior,IPR,SGNN,MacroDefRate)
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif
#ifdef PEBP_ENABLED
      select case(KOST)
      case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
            ! Here we explicitly set time increment to the value
            ! that is implicitly assumed in Pancak2.
            ddt = 1.D0
#ifdef ALTAY_SUBROUTINE
            if (.not. astate%simulCalls(astate%this)%input%keep_state)   &
            call KS_updateState(IOR,GAMdot,ddt,info)
#else
            call KS_updateState(IOR,GAMdot,ddt,info)
#endif
      endselect
#endif
      TOTGAMdot=sum(abs(GAMdot(1:M11)))
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
!  13  if (IGLIJ.eq.1) then
!EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if(NLIST.eq.1) write (IMP,103) ISTP,IOR,fi1,PHI,fi2

!  13  write (IMP,103) ISTP,IOR,fi1,PHI,fi2
 103  format (' ISTP,IOR',2I5,' phi1, PHI, phi2:',3F15.6)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
!      if (iend.ne.1) goto 34
!     if(NLIST.eq.1) then
!      write (IMP,102) ISTP,IOR,fi1,PHI,fi2
!     end if
! 102  format (' Taylr1 - Problem with SLIPRAT - ISTP,IOR',2I5,/,
!     1' Euler angles phi1, PHI, phi2:',3F15.6)
!      return
!  51  continue
!EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
!
      !Calculate RCcryst: the rigid body spin in the crystal frame
      RCcryst = rotateSRTensorFrom(MacroDefRate%Spin,TRF)
      RHOAcrys = rotateSRTensorFrom(RHOAsa,TRF)
   71   TRC(1)=RCcryst(3,2)+RHOAcrys(3,2)
        TRC(2)=RCcryst(1,3)+RHOAcrys(1,3)
        TRC(3)=RCcryst(2,1)+RHOAcrys(2,1)
      WorkRate=0.0
      do i=1,M11
          if (GAMdot(i).GT.0.0) then
              !positive slip rate
              WorkRate= WorkRate + CC(1,i)*GAMdot(i)
          else
              !negative or 0 slip rate
              WorkRate= WorkRate - CC(2,i)*GAMdot(i)
          endif
      end do
      Seq=WorkRate / MacroDefRate%vMeqStrainRate
  43  J=M
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  QGX 4/11/2011
!      if (IGLIJ.EQ.0) goto 90
!EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if(NLIST.eq.1) write (IMP,301) WorkRate
 301  format (//,1X,'SYSTEM - SLIPS    VIRTUAL WORK=',D17.8,//)
!      if(NLIST.eq.1) then
!      do 302 I=1,M
! 302  write (IMP,303) I,GAMdot(I)
!      end if
!
 303  format (1X,I5,(12F10.6))
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
!      if (IGLIJ.NE.0) then
!EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if(NLIST.eq.1) &
          write (IMP,109) MacroDefRate%vMeqStrainRate,Seq,                   &
                          (GAMdot(I)/MacroDefRate%vMeqStrainRate,I=1,M)

 109  format ('vMeqStrainRate=',D17.8,' RATE OF VIRTUAL WORK=',D17.8,/,  &
       '  SLIP RATES',/,(T2,10F10.5))
  90  continue
  202 ROT = matmul(B1,GAMdot)

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
!      if (IGLIJ.NE.0) then
!EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if(NLIST.eq.1) write (IMP,305) ROT

  305 format (' ROTATIONS',3F12.6)
!      do 58 K=1,M
!      X=ABS(GAMdot(K))
!  58  continue
      do J=1,3
            C1(J,J)=1.D0
      end do
      C1(3,2)=ROT(1)-TRC(1)
      C1(1,3)=ROT(2)-TRC(2)
      C1(2,1)=ROT(3)-TRC(3)
      C1(2,3)=-C1(3,2)
      C1(3,1)=-C1(1,3)
      C1(1,2)=-C1(2,1)
!     NIEUWE STAND UITWENDIG ASSENSTELSEL.
      C2 = matmul(C1,TRF)
!     KORRIGEREN VAN DE NIEUWE ROTATIEMATRIX
      ROTM= SQRT(C1(3,2)**2+C1(1,3)**2+C1(2,1)**2)
      Euler= EuleranglesType(C2)
      fi1=Euler%fi1 !
      PHI=Euler%PHI !use of EulerAngles2Arr impeded
      fi2=Euler%fi2 !   by common block /EULERA/
      C2 = rotmat(Euler)
      ITW=0
      if (NTW.EQ.0) goto 31
      X=0.
      do I=1,NTW
          J=I+NGL
          X=X+GAMdot(J)/G(I)
          VOLFR(I)=X
      end do
      if (X.LE.1.) goto 85
#ifndef ALTAY_SUBROUTINE
      if(NLIST.eq.1) write (IMP,107) X
 107  format (' SUM OF VOLUME FRACTIONS OF TWINS IS',D15.8,              &
      '   SHOULD BE LESS THAN 1')
       call terminate(stopcode_runtimeerror)
#else
      RCM_RAISE(1,'TAYLR1','Total volume fraction of twins exceeds unity',RCM_RTN)
#endif
  85  call RANDOM_NUMBER(RNDM)
      do I=1,NTW
        if (RNDM.LT.VOLFR(I)) goto 87
      end do
      goto 31
  87  do 88 K=1,3
      do 89 J=1,3
  89  RCC(K,J)=C2(K,J)
  88  continue
      TDC(1,1)=B2(1,I)
      X=B2(2,I)
      TDC(2,1)=X
      TDC(1,2)=X
      X=B2(3,I)
      TDC(3,1)=X
      TDC(1,3)=X
      TDC(2,2)=B2(4,I)
      X=B2(5,I)
      TDC(3,2)=X
      TDC(2,3)=X
      TDC(3,3)=B2(6,I)
      C2 = matmul(TDC,RCC)
      ITW=I
      Euler= EuleranglesType(C2)
      fi1=Euler%fi1 !
      PHI=Euler%PHI !use of EulerAngles2Arr impeded
      fi2=Euler%fi2 !   by common block /EULERA/
  31  if (nfile.eq.0.or.istp.gt.1) goto 61
!
      !“the ratio of the parallel strain rates”
      ! MacroDefRate%StrainMode & rhossa: expressed in same (sample) reference frame
      ratlon= sum( (MacroDefRate%StrainMode+sqrt(2.0D0/3.0D0)*rhossa) *  &
                    MacroDefRate%StrainMode                            )
!
      ! TAU: Reference-CRSS.
      ! Taylor Factor of the grain:
      Mgrain = TOTGAMdot / MacroDefRate%vMeqStrainRate
      ! Total, i.e. non-normalized, rhossa:
      rhossaTot = rhossa * MacroDefRate%vMeqStrainRate
      !
      write (IMP2,150) ior,Seq,WorkRate,TAU,Mgrain,ratlon,               &
       rhossaTot(1,1),rhossaTot(2,2),rhossaTot(3,3),                     &
       rhossaTot(2,3),rhossaTot(3,1),rhossaTot(1,2),                     &
       rhoasa(2,3),rhoasa(3,1),rhoasa(1,2),                              &
       ssam(1,1),ssam(2,2),ssam(3,3),ssam(2,3),ssam(3,1),ssam(1,2)
  150 format(i5,5(E12.5,1X),5x,6(E12.5,1X),5x,3(E12.5,1X),               &
             5x,6(E12.5,1X))
 101  format(3d20.7)
   61 return
  26  write (IMP,106)
 106  format (1X,'TAYLOR - NO UPPER LIMIT FOR LINEAR PROGRAMMING PROBLEM')
#ifndef ALTAY_SUBROUTINE
  52  call terminate(stopcode_runtimeerror)
#else
  52  RCM_RAISE(1,'TAYLR1','No upper limit for linear programming problem',RCM_RTN)
#endif
      end subroutine

      end module
