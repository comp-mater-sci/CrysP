C     Versie nov 1997  "LAMEL"
C     modified October 2000
C     modified june 2001
C                                                                       
C
C     Modifications december 2000
C     - NGLS is set equal to M11
C
      SUBROUTINE TAYLOR (IRICHT,IGLIJ,KOST,BBVM,IRELX,IROT,Ftot)
      implicit double precision (a-h,o-z)
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1,NDAT2
      COMMON /TEXTUR/ TRF(3,3),C1(3,3),C2(3,3),WDOT,ROTM,NO,DG(3,3),
     1ITW,IPR,DELTAW,GEWF,NLIST
      COMMON /IGLIJS/    FK1(96),NUNGL,NGLS,CC(96)
      COMMON/TLR1/ N,M,N1,NGL,NTW,M11,NC,LC,B1(3,48),B(5,5),
     1B2(6,48),G(48),DI1(5)
      COMMON/TLR2/ TRC(3,3),XX(96),buftrf(3,3)
C      COMMON/TLR3/ BB(5),V(6),MASK(101)
C      real * 8 BB,V
      COMMON /DOUBLE/ A1(5,96),BB8(5),DELTAT,RHO(5),B5(5)
      COMMON /GENRLX/ YY(5,5),SHsam(3,3),Ssam(3,3),SPANV(5),RHOSsa(3,3),
     1 WR,WRTOT,SWRLX(3)
      DIMENSION TDC(3,3),Ftot(3,3)
      character*72 TITGLIJ
C
C     Extra arrays nodig voor lineare programmatie op 2 korrels tegelijk
C
      common /extra/ A2(10,198),U2(11,11),FK(198),W(11)
      dimension XXLP(198)
      logical SWRLX
      INTEGER DI1,R
C
C     DVM = von Mises equivalent strain rate
C
C     Instructie die allen maar dient om een compilerbericht over
C     IRELX te onderdrukken
      SAVE
      i=IRELX
      GOTO (1000,2000,3000),IRICHT
 1000 continue
#ifndef NOLSTFILE      
      WRITE (IMP,216)
 216  FORMAT (/,' SUBROUTINE TAYLOR - READS ITS CRYSTAL DATA',//)
#endif 
C
C
C
      R=LEC
C
C
C
  507 read (R,217) TITglij
  217 format(A)
#ifndef NOLSTFILE
      write (IMP,221) titglij
  221 format (/,' Slip system set:',A,/)
#endif  
      READ (R,210) I,NGL,NTW,DI1,X,Y
 210  FORMAT (8I4,4X,2F10.0)
#ifndef NOLSTFILE
  508 WRITE (IMP,211) I,NGL,NTW,DI1
 211  FORMAT (1H ,I4,10X,2I5,10X,5I5)
#endif 
      IF (I.NE.0) STOP 5                                                
      M=NGL+NTW                                                         
      DO 500 I1=1,M                                                     
      READ (R,212) I,(A1(J,I1),J=1,5),(B1(L,I1),L=1,3)
 212  FORMAT (I4,8F20.16)
#ifndef NOLSTFILE
      WRITE (IMP,213) I,(A1(J,I1),J=1,5),(B1(L,I1),L=1,3)
 213   FORMAT (I3,' A ',5F10.7,' B ',3F10.7)
#endif 
 500  CONTINUE
      DO 501 I=1,5                                                      
      J1=I+M                                                            
      READ (R,214) J,(B(I,L),L=1,5)
 214  FORMAT (I4,5D23.16)
#ifndef NOLSTFILE         
      WRITE (IMP,215) J,(B(I,L),L=1,5)
 215  FORMAT (1H ,I4,10X,5D15.8)
#endif
 501  CONTINUE
      IF (NTW.EQ.0) GOTO 504                                            
      DO 505 I=1,NTW                                                    
      J1=I+M+5                                                          
      READ (R,212) J,(B2(L,I),L=1,6),G(I)
#ifndef NOLSTFILE
      WRITE (IMP,218) J,(B2(L,I),L=1,6),G(I)
 218  format (i4,' B2',6f10.7,' G',f10.7)
#endif
 505  CONTINUE
 504  N1=N+1                                                            
      M11=2*NGL+NTW
      NGLS=M11
      IF (M11.LE.96.OR.M.LE.48) GOTO 515
#ifndef NOLSTFILE
      WRITE (IMP,222) M11,M
#endif
! Exception: print message to the console before STOP
      WRITE (*,222) M11,M
  222 FORMAT (' TAYLOR - PROBLEMS WITH DIMENSIONS',2I10)                
      STOP 5                                                            
  515 IF (NGL.EQ.0) GOTO 42                                             
      DO 1 I=1,NGL                                                      
      K=I+M                                                             
      DO 2 J=1,N                                                        
      A1(J,K)=-A1(J,I)                                                  
   2  CONTINUE                                                          
   1  CONTINUE                                                          
  42  IF (KOST.EQ.1) GOTO 502                                           
      DO 503 I=1,M11                                                    
 503  FK1(I)=1.                                                         
 502  do 30 j=1,196
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
 2000 IF (IGLIJ.EQ.0) GOTO 70
#ifndef NOLSTFILE                                            
      WRITE (IMP,203)                                                   
#endif      
      DO 71 I=1,3                                                       
      DO 72 J=1,3                                                       
      TDC(I,J)=(DG(I,J)+DG(J,I))*0.5                                    
  72  TRC(I,J)=(DG(I,J)-DG(J,I))*0.5                                    
#ifndef NOLSTFILE
      WRITE (IMP,204) (DG(I,J),J=1,3),(TDC(I,J),J=1,3),(TRC(I,J),J=1,3) 
#endif      
  71  CONTINUE                                                          
 203  FORMAT (' TAYLOR - DISPLACEMENT GRADIENT WHICH WILL BE USED FOR TH
     1E SIMULATION',//T9,'GLOBAL TENSOR',T47,'SYMMETRICAL PART',T85,    
     2'ANTISYMMETRICAL PART',/)                                         
 204  FORMAT (1H ,3(3F10.5,10X))
   70  continue
C     Normalisation of TDC (which is used in CLUSTER1 in PANCAK2)
      X=0.0d0
      do i=1,3
         do j=1,3
            X=X+TDC(i,j)**2
         enddo
      enddo
      if (X.lt.1.0D-6) then
         write (*,205) X
#ifndef NOLSTFILE         
         write (IMP,205) X
#endif
         stop
      endif
 205  format (' Taylor - symmetric part of strain step is too small'
     1 ,d20.8)
      X=sqrt(2.0*X/3.0)
      do i=1,3
         do j=1,3
            TDC(i,j)=TDC(i,j)/X
         enddo
      enddo
C      
      X=ABS(DG(1,1)+DG(2,2)+DG(3,3))
      IF (X.LE.1.0D-6) RETURN                                           
      WRITE (*,202)
#ifndef NOLSTFILE         
      WRITE (IMP,202)                                                   
#endif
 202  FORMAT (' TAYLOR - SUM OF DIAGONAL ELEMENTS OF DISPLACEMENT GRADIE
     1NT MUST BE ZERO')                                                 
      STOP 5                                                            
CC     OMREKENING DISPLACEMENT GRADIENT.
 3000 do 45 i=1,3
      do 45 j=1,3
      buftrf(i,j)=TRF(j,i)
  45  continue
C      write (*,1234)
C 1234 format (' Just before Pancak2')
      CALL Pancak2(KOST,NGL,NTW,M11,B,DI1,DG,TDC,SPANV,WR,SWRLX,
     1 BBVM,CC,XXLP,IPR,IROT,Ftot,GEWF)
C      write (*,1235)
C 1235 format (' Just after Pancak2')
      RETURN
      END                                                               
      SUBROUTINE TAYLR1(IGLIJ,ISTP,IOR,JRELX,IROT,IEND,
     1 NFILE,TAU)
      implicit double precision (a-h,o-z)
C
C     If JRELX=0: No relaxation- stress coming from TAYLOR and LA01P
C                 is taken as first guess of the stress
C        JRELX=1: Relaxation - macroscopic stress is first guess
C        JRELX=2: Relaxation - stress in SPANV is first guess
C        JRELX=3: Relaxation is taken from input variable
C                 (in fact, a non-relaxed calculation is carried out).
C
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1,NDAT2
      COMMON/TLR1/ N,M,N1,NGL,NTW,M11,NC,LC,B1(3,48),B(5,5),
     1B2(6,48),G(48),DI1(5)
      COMMON /TEXTUR/ TRF(3,3),C1(3,3),C2(3,3),WDOT,ROTM,NO,DG(3,3),
     1ITW,IPR,DELTAW,GEWF,NLIST
      COMMON/TLR2/ RC(3,3),XX(96),buftrf(3,3)
      COMMON /IGLIJS/    FK1(96),NUNGL,NGLS,CC(96)
      COMMON /EULERA/ fi1,PHI,fi2
      COMMON /DOUBLE/ A1(5,96),BB8(5),DELTAT,RHO(5),B5(5)
      logical SWRLX
C
C     SHsam:    macroscopic stress in sample reference system
C     SH:   macroscopic stress in crystal reference system
C     SPANH: macroscopic stress in crystal reference system
C     SPANT,SPANV: local stress in crystal reference system
C     Ssam:        local stress in sample reference system
C
      COMMON /GENRLX/ YY(5,5),SHsam(3,3),Ssam(3,3),SPANV(5),RHOSsa(3,3),
     1 WR,WRTOT,SWRLX(3)
C     volgend instr. alleen om geheugen (MASK) te sparen.
      COMMON/TLR3/ BB(10),V(11),MASK(197)
      DIMENSION TRC(3),GAMMA(50),ROT(3),TDC(3,3),SPANT(3,3)
      dimension bufsp(3,3),RHOAsa(3,3),SPNV(5)
C      dimension SPANH(5),SH(5)
      COMMON /RHO/ RHOS(5),RHOA(5)
      INTEGER DI1
C      SQR2=sqrt(0.5)
      data SQR2/0.7071067811865476D+00/
      SAVE
      WACC1=0.0
      WACC2=0.0
      IRELX=JRELX
C      do 48 i=1,N
C      RHOS(i)=0.0
C  48  continue
C      if (IRELX.eq.0) goto 46
C      call MATPROD(bufsp,SHsam,buftrf,3,3,3)
C      call MATPROD(SH,TRF,bufsp,3,3,3)
C      call STR5(SPANH,SH)
C      If (IGLIJ.eq.1) write (IMP,122) SPANH
 122  format (' SH-stress (crystal system):',
     1 /,5F12.6)
C     if (IRELX.ne.3) goto 12
C      IRELX=0
C      call MATPROD(bufsp,RHOSsa,buftrf,3,3,3)
C      call MATPROD(SPANT,TRF,bufsp,3,3,3)
C      call STR5(RHOS,SPANT)
C      goto 46
C  12  if (IRELX.GT.1) goto 46
C      do 47 i=1,N
C      SPANV(i)=SPANH(i)
C  47  continue
  46  do 49 i=1,N
      SPNV(i)=SPANV(i)
  49  continue
      call STR33(SPANT,SPNV)
C      do 30 i1=1,M
C      i=i1+1
C      WRITE (IMP,213) i,(A1(J,I1),J=1,5)
C 213   FORMAT (i3,5F10.7,10X,3F10.7)
C  30  continue
      if (IGLIJ.eq.0) goto 11
      write (IMP,100)
 100  format (' Bishop-Hill stress (crystal system):')
      do 10 i=1,3
      write (IMP,101) (SPANT(i,j),j=1,3)
 101  format(3d20.7)
  10  continue
C      write (IMP,1703) IRELX,IROT
C 1703 format (' IRELX=',i5,'  IROT=',I5,///)
C  11  write (*,1771) IOR
C 1771 format (I5)
  11  call SLIPRAT(M11,96,XX,ior,iend,IPR)
  13  if (IGLIJ.eq.1) write (IMP,103) ISTP,IOR,fi1,PHI,fi2
C  13  write (IMP,103) ISTP,IOR,fi1,PHI,fi2
 103  format (' ISTP,IOR',2I5,' phi1, PHI, phi2:',3F15.6)
      if (iend.ne.1) goto 34
      write (IMP,102) ISTP,IOR,fi1,PHI,fi2
 102  format (' Taylr1 - Problem with SLIPRAT - ISTP,IOR',2I5,/,
     1' Euler angles phi1, PHI, phi2:',3F15.6)
      return
C      SPNV(i)=SPANV(i)
  51  continue
C      call STR33(SPANT,SPNV)
C      write (IMP,100)
C      do i=1,3
C      write (IMP,101) (SPANT(i,j),j=1,3)
C      enddo
  34   call MATPROD(bufsp,SPANT,TRF,3,3,3)
      call MATPROD(Ssam,buftrf,bufsp,3,3,3)
C
C     Transformation of relaxation
C     From here on, SPANT is corrupted
C
      call STR33(SPANT,RHOS)
      call MATPROD(bufsp,SPANT,TRF,3,3,3)
      call MATPROD(RHOSsa,buftrf,bufsp,3,3,3)
      if (IGLIJ.eq.0) goto 77
      write (IMP,1701)
 1701 format(/,' RHOSsa')
      do 1700 i=1,3
      write (IMP,101) (RHOSsa(i,j),j=1,3)
 1700 continue
   77 if (IROT.eq.0) return
      do 74 i=1,3
      do 74 j=1,3
      SPANT(i,j)=0.0
  74  continue
C     Sqrt(0.5) comes from the definition of rho
      SPANT(2,3)=RHOA(1)*SQR2
      SPANT(3,2)=-SPANT(2,3)
      SPANT(3,1)=RHOA(2)*SQR2
      SPANT(1,3)=-SPANT(3,1)
      SPANT(1,2)=RHOA(3)*SQR2
      SPANT(2,1)=-SPANT(1,2)
      call MATPROD(bufsp,SPANT,TRF,3,3,3)
      call MATPROD(RHOAsa,buftrf,bufsp,3,3,3)
      if (IGLIJ.eq.0) goto 71
      write (IMP,1706)
 1706 format (/,' RHOAsa')
      do 1705 i=1,3
      write (IMP,101) (RHOAsa(i,j),j=1,3)
 1705 continue
  71  TRC(1)=RC(3,2)-RHOA(1)*DELTAT*SQR2
      TRC(2)=RC(1,3)-RHOA(2)*DELTAT*SQR2
      TRC(3)=RC(2,1)-RHOA(3)*DELTAT*SQR2
      DELTAW=0.0
      do 44 i=1,M11
      DELTAW=DELTAW+ABS(CC(i)*XX(i))
  44  continue
      WDOT=DELTAW/DELTAT
      DO 41 J=1,M
      gamma(J)=XX(J)
      if (J.gt.NGL)  goto 41
      gamma(J)=gamma(J)-XX(J+M)
  41  continue
  43  J=M
      IF (IGLIJ.EQ.0) GOTO 90
      WRITE (IMP,301) WDOT
 301  FORMAT (//,1H ,'SYSTEM - SLIPS    VIRTUAL WORK=',D17.8,//)
      DO 302 I=1,M
 302  WRITE (IMP,303) I,gamma(I)
 303  FORMAT (1H ,I5,(12F10.6))                                         
 106  FORMAT (1H ,'TAYLOR - NO UPPER LIMIT FOR LINEAR PROGRAMMING PROBL
     1EM')                                                              
  90  CONTINUE                                                          
      IF (IGLIJ.NE.0) WRITE (IMP,109) (GAMMA(I),I=1,M)
 109  FORMAT (' SLIP RATES',/,(T2,10F10.5))
 202  DO 56 J=1,M
  56  MASK(J)=0                                                         
      CALL MATPROD(ROT,B1,GAMMA,3,M,1)
      IF (IGLIJ.NE.0) WRITE (IMP,305) ROT                               
  305 FORMAT (' ROTATIONS',3F12.6)
      DO 58 K=1,M                                                       
      X=ABS(GAMMA(K))                                                   
      IF (X.GT.0.2D-4) MASK(K)=1                                        
  58  CONTINUE                                                          
      DO 75 J=1,3
  75  C1(J,J)=1.                                                        
      C1(3,2)=ROT(1)-TRC(1)                                             
      C1(1,3)=ROT(2)-TRC(2)                                             
      C1(2,1)=ROT(3)-TRC(3)                                             
      C1(2,3)=-C1(3,2)                                                  
      C1(3,1)=-C1(1,3)                                                  
      C1(1,2)=-C1(2,1)                                                  
C     NIEUWE STAND UITWENDIG ASSENSTELSEL.                              
      CALL MATPROD(C2,C1,TRF,3,3,3)                                        
C     KORRIGEREN VAN DE NIEUWE ROTATIEMATRIX                            
      ROTM= SQRT(C1(3,2)**2+C1(1,3)**2+C1(2,1)**2)
      call EULER1(C2,fi1,PHI,fi2)
      call Tmatrix(C2,fi1,PHI,fi2)
      ITW=0
      IF (NTW.EQ.0) GOTO 31                                             
      X=0.                                                              
      DO 84 I=1,NTW                                                     
      J=I+NGL                                                           
      X=X+GAMMA(J)/G(I)                                                 
      GAMMA(I)=X                                                        
  84  CONTINUE                                                          
       IF (X.LE.1.) GOTO 85                                             
      WRITE (IMP,107) X                                                 
 107  FORMAT (' SUM OF VOLUME FRACTIONS OF TWINS IS',D15.8,
     1'   SHOULD BE LESS THAN 1')                                       
       STOP 5                                                           
  85  CALL RANDOM_NUMBER(RNDM)
      DO 86 I=1,NTW                                                     
      IF (RNDM.LT.GAMMA(I)) GOTO 87                                     
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
      CALL MATPROD(C2,TDC, RC,3,3,3)                                       
      ITW=I
      call EULER1(C2,fi1,PHI,fi2)
  31  if (nfile.eq.0.or.istp.gt.1) goto 61
      x=0.0
      do 60 i=1,3
      do 60 j=1,3
C     Picking up of D in sample system:
      y=0.5*(DG(i,j)+DG(j,i))/DELTAT
C     Scalar product between D and D+RHOS
      x=x+(y+rhossa(i,j))*y
  60  continue
C     calculation of ratio of projection of D+RHOS on D, and D itself.
C     Note that length of D = sqrt(3/2)
      x=x*2.0/3.0
      ratlon=x
C      do i=1,5
C           write (IMP,997) B5(i),SPANV(i)
C 997  format (' B5  ',d15.8, '  SPANV  ',d15.8)
C      enddo
C      write (imp,988) x
C 988  format (' ratlon',d15.8)
      if (x.lt.5.0D-6) then
         WDOT1=0.0
         do i=1,5
           WDOT1=WDOT1+B5(i)*SPANV(i)
         enddo
      else
         WDOT1=WDOT/x
      endif
c <jg>
#ifndef NORESFILE
      write (IMP2,150) ior,WDOT,WDOT1,TAU,WDOT1/TAU,ratlon,
     1 rhossa(1,1),rhossa(2,2),rhossa(3,3),
     2 rhossa(2,3),rhossa(3,1),rhossa(1,2),
     3 rhoasa(2,3),rhoasa(3,1),rhoasa(1,2),
     4 ssam(1,1),ssam(2,2),ssam(3,3),ssam(2,3),ssam(3,1),ssam(1,2)
#endif
c <jg>      
  150 format (i5,5f10.6,5x,6f10.6,5x,3f10.6,5x,6f10.6)
   61 RETURN
C      DO 27 I=1,N
C      IF (T(I).GT.0.) GOTO 27
C      DO 28 J=1,M11
C  28  A1(I,J)=-A1(I,J)
C  27  CONTINUE
C      RETURN
  26  WRITE (IMP,106)                                                   
  52  STOP                                                             
      END                                                               
