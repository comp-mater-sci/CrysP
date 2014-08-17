#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
      module altayTaylor
      use altayAlgorithms
      use altayMiscutils, only: terminate, stopcode_runtimeerror
      use altayMacroKinematic
      use criMathUtils
      integer,parameter,private :: N = 5, N1 = N + 1 
      
      integer,private           :: M,NGL,NTW
      double precision,private  :: B1(3,96),B(5,5),B2(6,96),G(96)
      integer,private           :: DI1(5)
     
      contains
      
C MODIFICATIONS AUG 2010
C THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
C WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY 
C
      SUBROUTINE TAYLOR (IRICHT, KOST, MacroDefRate, MacroDefState)
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
      COMMON /TEXTUR/ TRF(3,3),C1(3,3),C2(3,3),NO,
     1ITW,GEWF
      COMMON /IGLIJS/ M11,CC(2,96)
      COMMON/TLR2/ TRC(3,3),RHOAsa
      COMMON /DOUBLE/ A1(5,96),BB8(5),RHO(5),B5(5)
      COMMON /GENRLX/ YY(5,5),SHsam(3,3),Ssam(3,3),RHOSsa(3,3),
     1 SWRLX(3)
      double precision, dimension(3,3):: RHOScrys(3,3)
      double precision, dimension(3,3):: RHOAcrys(3,3), RHOAsa(3,3) 
      character*72 TITGLIJ
C
C     Extra arrays nodig voor lineare programmatie op 2 korrels tegelijk
C
      common /extra/ A2(10,194),UU(10,10)
      dimension XXLP(194)
      logical SWRLX
      INTEGER R 
      DATA MMAX/96/ ! dimension of A1 and other arrays 
C
C     DVM = von Mises equivalent strain rate
C
      !Local stress in crystal reference system
      double precision, dimension(3,3):: Scrys=0.0d0 
      SAVE
      GOTO (1000,2000,3000),IRICHT
 1000 if(NLIST.eq.1) then
      WRITE (IMP,216)
      end if
 216  FORMAT (/,' SUBROUTINE TAYLOR - READS ITS CRYSTAL DATA',//)
C
      R=LEC
C
  507 read (R,217) TITglij
  217 format(A)
      if(NLIST.eq.1) then
      write (IMP,221) titglij
      end if
  221 format (/,' Slip system set:',A,/)
      READ (R,210) I,NGL,NTW,DI1,X,Y
 210  FORMAT (8I4,4X,2F10.0)
      if(NLIST.eq.1) then
      WRITE (IMP,211) I,NGL,NTW,DI1
      end if
 211  FORMAT (1H ,I4,10X,2I5,10X,5I5)
#ifndef ALTAY_SUBROUTINE
      IF (I.NE.0) call terminate(stopcode_runtimeerror)
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
            if(NLIST.eq.1) then
                  write (IMP,5001) M11,MMAX
            end if
            call terminate(stopcode_runtimeerror)
#else
            RCM_RAISE(1,'TAYLOR','Too large slip system set',RCM_RTN)
#endif
      endif
 5001 format(' TAYLOR - NGL+NTW=',I5,' LARGER THAN  MMAX=',I5) 
      DO 500 I1=1,M11                                                     
      READ (R,212) I,(A1(J,I1),J=1,5),(B1(L,I1),L=1,3)
 212  FORMAT (I4,8F20.16)
      if(NLIST.eq.1) then
      WRITE (IMP,213) I,(A1(J,I1),J=1,5),(B1(L,I1),L=1,3)
      end if
 213   FORMAT (I3,' A ',5F10.7,' B ',3F10.7)
 500  CONTINUE
      DO 501 I=1,5                                                      
      READ (R,214) J,(B(I,L),L=1,5)
 214  FORMAT (I4,5D23.16)
      if(NLIST.eq.1) then
      WRITE (IMP,215) J,(B(I,L),L=1,5)
      end if
 215  FORMAT (1H ,I4,10X,5D15.8)
 501  CONTINUE
      IF (NTW.EQ.0) GOTO 504                                            
      DO 505 I=1,NTW                                                    
      READ (R,212) J,(B2(L,I),L=1,6),G(I)
      if(NLIST.eq.1) then
      WRITE (IMP,218) J,(B2(L,I),L=1,6),G(I)
      end if
 218  format (i4,' B2',6f10.7,' G',f10.7)
 505  CONTINUE
 504  CONTINUE
 502  do 30 j=1,194
      do 30 i=1,10
      A2(i,j)=0.0
  30  continue
      do 31 j=1,M11
      do 31 i=1,5
      x8=A1(i,j)
      A2(i,j)=x8
      A2(i+5,j+M11)=x8
  31  continue
      RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011 
C 2000 IF (IGLIJ.EQ.0) GOTO 70  
 2000 continue
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if(NLIST.eq.1) then                                         
      WRITE (IMP,203)
      end if     
      TRC=MacroDefRate%Spin
      do I=1,3                                                       
          if(NLIST.eq.1) then                                   
              WRITE (IMP,204) (MacroDefRate%VelGrad(I,J),J=1,3),
     &                        (MacroDefRate%StrainRate(I,J),J=1,3),
     &                        (MacroDefRate%Spin(I,J),J=1,3) 
          end if
      end do
 203  FORMAT (' TAYLOR - DISPLACEMENT GRADIENT WHICH WILL BE USED FOR TH
     1E SIMULATION',//T9,'GLOBAL TENSOR',T47,'SYMMETRICAL PART',T85,    
     2'ANTISYMMETRICAL PART',/)                                         
 204  FORMAT (1H ,3(3F10.5,10X))
   70  continue
      if (MacroDefRate%NormStrainRate.lt.1.0D-10) then
#ifndef ALTAY_SUBROUTINE
         write (*,205) MacroDefRate%NormStrainRate
         if(NLIST.eq.1) then
              write (IMP,205) MacroDefRate%NormStrainRate
         end if
         call terminate(stopcode_runtimeerror)
#else
         RCM_RAISE(1,'TAYLOR',
     &  'Symmetric part of the strain step is too small',RCM_RTN)
#endif
      endif
 205  format (' Taylor - symmetric part of strain step is too small'
     1 ,d20.8)
C      
      RETURN
CC     OMREKENING DISPLACEMENT GRADIENT.
 3000 continue
C      write (*,1234)
C 1234 format (' Just before Pancak2')
       CALL Pancak2(KOST,NGL,B,DI1,Scrys,RHOScrys,RHOAcrys,
     1 SWRLX,XXLP,IPR,GEWF,MacroDefRate,MacroDefState)
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
C      write (*,1235)
C 1235 format (' Just after Pancak2')
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE TAYLR1(ISTP,IOR,NFILE,TAU,TOTGAMdot,Seq,WorkRate,
     &                  MacroDefRate)
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
      COMMON /TEXTUR/ TRF(3,3),C1(3,3),C2(3,3),NO,
     1ITW,GEWF
      COMMON /IGLIJS/ M11,CC(2,96)
      COMMON/TLR2/ RC(3,3),RHOAsa
      COMMON /DOUBLE/ A1(5,96),BB8(5),RHO(5),B5(5)
      COMMON /EULERA/ fi1,PHI,fi2
      logical SWRLX
      double precision, intent(out):: Seq ! Equivalent stress in crystal, defined as..
                                    !  plastic work rate in crystal normalized by..
                                    !  (macro) von Mises equivalent strain rate
      !> Rate of plastic work per unit volume in the crystal
      double precision, intent(out) :: WorkRate
      double precision :: Mgrain
      type(EulerAngles):: Euler
C
C     SHsam:    macroscopic stress in sample reference system
C     SH:   macroscopic stress in crystal reference system
C     SPANH: macroscopic stress in crystal reference system
C     Ssam:        local stress in sample reference system
C
      COMMON /GENRLX/ YY(5,5),SHsam(3,3),Ssam(3,3),RHOSsa(3,3),
     1 SWRLX(3)
      DIMENSION RCcryst(3,3),rhossaTot(3,3)
      DIMENSION TRC(3),VOLFR(96),ROT(3),TDC(3,3),SGNN(96)
      dimension RHOAsa(3,3),GAMdot(96)
      real, dimension(3,3) :: test !!single precision!!
C      data SQR2/0.7071067811865476D+00/
#ifdef PEBP_ENABLED      
      integer :: info
      double precision :: ddt
#endif
      double precision, intent(OUT) :: TOTGAMdot
      SAVE
      WACC1=0.0
      WACC2=0.0
c      pause
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C      if (IGLIJ.eq.0) goto 11
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
C  11  write (*,1771) IOR
C 1771 format (I5)
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
            if (.not. astate%simulCalls(astate%this)%input%keep_state)
     &      call KS_updateState(IOR,GAMdot,ddt,info)
#else
            call KS_updateState(IOR,GAMdot,ddt,info)
#endif
      endselect
#endif
      TOTGAMdot=sum(abs(GAMdot(1:M11)))
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C  13  if (IGLIJ.eq.1) then
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if(NLIST.eq.1) then
      write (IMP,103) ISTP,IOR,fi1,PHI,fi2
      end if

C  13  write (IMP,103) ISTP,IOR,fi1,PHI,fi2
 103  format (' ISTP,IOR',2I5,' phi1, PHI, phi2:',3F15.6)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C      if (iend.ne.1) goto 34
C     if(NLIST.eq.1) then
C      write (IMP,102) ISTP,IOR,fi1,PHI,fi2
C     end if
C 102  format (' Taylr1 - Problem with SLIPRAT - ISTP,IOR',2I5,/,
C     1' Euler angles phi1, PHI, phi2:',3F15.6)
C      return
c  51  continue
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
C
      !Calculate RCcryst: the rigid body spin in the crystal frame 
      RCcryst = rotateSRTensorFrom(RC,TRF)
      
   71   TRC(1)=RCcryst(3,2)+RHOAsa(3,2)
        TRC(2)=RCcryst(1,3)+RHOAsa(1,3)
        TRC(3)=RCcryst(2,1)+RHOAsa(2,1)
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
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  QGX 4/11/2011
C      IF (IGLIJ.EQ.0) GOTO 90
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if(NLIST.eq.1) then
      WRITE (IMP,301) WorkRate
      end if
 301  FORMAT (//,1H ,'SYSTEM - SLIPS    VIRTUAL WORK=',D17.8,//)
!      if(NLIST.eq.1) then
!      DO 302 I=1,M
! 302  WRITE (IMP,303) I,GAMdot(I)
!      end if
C
 303  FORMAT (1H ,I5,(12F10.6))  
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011                                        
C      IF (IGLIJ.NE.0) then
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if(NLIST.eq.1) then
      WRITE (IMP,109) MacroDefRate%vMeqStrainRate,Seq,
     &                (GAMdot(I)/MacroDefRate%vMeqStrainRate,I=1,M)
      end if

 109  FORMAT ('vMeqStrainRate=',D17.8,' RATE OF VIRTUAL WORK=',D17.8,/,
     1 '  SLIP RATES',/,(T2,10F10.5))
  90  CONTINUE                                                          
  202 ROT = matmul(B1,GAMdot)

C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C      IF (IGLIJ.NE.0) then
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if(NLIST.eq.1) then
      WRITE (IMP,305) ROT
      end if
                               
  305 FORMAT (' ROTATIONS',3F12.6)
C      DO 58 K=1,M                                                       
C      X=ABS(GAMdot(K))
C  58  CONTINUE                                                          
      DO 75 J=1,3
  75  C1(J,J)=1.D0                                                      
      C1(3,2)=ROT(1)-TRC(1)                                             
      C1(1,3)=ROT(2)-TRC(2)                                             
      C1(2,1)=ROT(3)-TRC(3)                                         
      C1(2,3)=-C1(3,2)                                                  
      C1(3,1)=-C1(1,3)                                                  
      C1(1,2)=-C1(2,1)                                                  
C     NIEUWE STAND UITWENDIG ASSENSTELSEL.                          
      C2 = matmul(C1,TRF)      
C     KORRIGEREN VAN DE NIEUWE ROTATIEMATRIX                            
      ROTM= SQRT(C1(3,2)**2+C1(1,3)**2+C1(2,1)**2)
      Euler= EuleranglesType(C2)
      fi1=Euler%fi1 !
      PHI=Euler%PHI !use of EulerAngles2Arr impeded
      fi2=Euler%fi2 !   by common block /EULERA/
      C2 = rotmat(Euler)
      ITW=0
      IF (NTW.EQ.0) GOTO 31                                             
      X=0.                                                              
      DO 84 I=1,NTW                                                     
      J=I+NGL                                                           
      X=X+GAMdot(J)/G(I)                                                
      VOLFR(I)=X                                                        
  84  CONTINUE                                                          
       IF (X.LE.1.) GOTO 85  
#ifndef ALTAY_SUBROUTINE
       if(NLIST.eq.1) then                                           
      WRITE (IMP,107) X   
      end if                                              
 107  FORMAT (' SUM OF VOLUME FRACTIONS OF TWINS IS',D15.8,
     1'   SHOULD BE LESS THAN 1')                                       
       call terminate(stopcode_runtimeerror)
#else       
      RCM_RAISE(1,'TAYLR1',
     x'Total volume fraction of twins exceeds unity',RCM_RTN)
#endif
  85  CALL RANDOM_NUMBER(RNDM)
      DO 86 I=1,NTW                                                     
      IF (RNDM.LT.VOLFR(I)) GOTO 87                                     
  86  CONTINUE                                                          
      GOTO 31                                                           
  87  DO 88 K=1,3                                                       
      DO 89 J=1,3                                                       
  89   RC(K,J)=C2(K,J)                                                  
  88  CONTINUE                                                          
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
      C2 = matmul(TDC,RC) 
      ITW=I
      Euler= EuleranglesType(C2)
      fi1=Euler%fi1 !
      PHI=Euler%PHI !use of EulerAngles2Arr impeded
      fi2=Euler%fi2 !   by common block /EULERA/      
  31  if (nfile.eq.0.or.istp.gt.1) goto 61
C
      !“the ratio of the parallel strain rates”
      ! MacroDefRate%StrainMode & rhossa: expressed in same (sample) reference frame
      ratlon= sum( (MacroDefRate%StrainMode+sqrt(2.0D0/3.0D0)*rhossa) *
     &              MacroDefRate%StrainMode                            ) 
C
      ! TAU: Reference-CRSS.
      ! Taylor Factor of the grain:
      Mgrain = TOTGAMdot / MacroDefRate%vMeqStrainRate
      ! Total, i.e. non-normalized, rhossa:
      rhossaTot = rhossa * MacroDefRate%vMeqStrainRate
      !
      write (IMP2,150) ior,Seq,WorkRate,TAU,Mgrain,ratlon,
     1 rhossaTot(1,1),rhossaTot(2,2),rhossaTot(3,3),
     2 rhossaTot(2,3),rhossaTot(3,1),rhossaTot(1,2),
     3 rhoasa(2,3),rhoasa(3,1),rhoasa(1,2),
     4 ssam(1,1),ssam(2,2),ssam(3,3),ssam(2,3),ssam(3,1),ssam(1,2)
  150 format(i5,5(E12.5,1X),5x,6(E12.5,1X),5x,3(E12.5,1X),
     1       5x,6(E12.5,1X))
 101  format(3d20.7)      
   61 RETURN
  26  WRITE (IMP,106)
 106  FORMAT (1H ,'TAYLOR - NO UPPER LIMIT FOR LINEAR PROGRAMMING PROBL
     1EM')
#ifndef ALTAY_SUBROUTINE
  52  call terminate(stopcode_runtimeerror)
#else
  52  RCM_RAISE(1,'TAYLR1',
     x'No upper limit for linear programming problem',RCM_RTN)
#endif
      END SUBROUTINE
      
      end module
