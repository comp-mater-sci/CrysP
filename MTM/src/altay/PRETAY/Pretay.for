C     PRETAY  MAY 1995 - All crystal lattices
      IMPLICIT double precision (A-H,O-Z)
      DIMENSION A1(5,48),DI(5),B(5,5),BVEC(25)
      dimension L1(5),L2(5),B1(5,48),B2(6,48)
     1,G(48),UV(3,3),UVINV(3,3),UVEC(9)
      dimension IV(3),IR(3),V(3),R(3),DG(3,3)
      character*80 titel
      character * 12 cods1
      character * 8 codsim

      equivalence (B(1,1),BVEC(1)),(UVEC(1),UVINV(1,1))
      INTEGER DI,PNCH
      DATA KLEC,LEC,IMP,PNCH/4,8,7,9/
      DATA N /5/
      NXX=3*3
      NXXX=N*N
C     UNIT KLEC = CONTROL FILE
      open (unit=KLEC,file='PRETAY.CTL',status='old')
C     read code of set of slip systems
  90  format (a)
      read (KLEC,90) codsim
      write (*,92) codsim
  92  format (' Code for this set of slip systems: ',a)
      L=LEN_TRIM(codsim)
      cods1=codsim
      cods1(L+1:L+4)='.LST'
C     UNIT IMP = PRINTER
      open (unit=IMP,file=cods1,status='replace')
      write (IMP,92) codsim
      cods1(L+1:L+4)='.DAT'
C     UNIT LEC = INPUT CARDS
      open (unit=LEC,file=cods1,status='old')
      cods1(L+1:L+4)='.PRE'
C     UNIT PNCH = CARD PUNCHER
      open (unit=PNCH,file=cods1,status='replace')

      Read (LEC,99) TITEL
  99  format(A)
      print 93,TITEL
      write (IMP,93) TITEL
  93  format(' Title= ',A)



C     Read unit vectors of crystal lattice
      write (IMP,94)
  94  format (' Unit vectors of unit cell of crystal lattice:')
      do 801 i=1,3
      read (LEC,97) (UV(i,j),j=1,3)
      do 802 j=1,3
      UVINV(i,j)=UV(i,j)
 802  continue
  97  format (3f20.0)
      write (IMP,96) i,(UV(i,j),j=1,3)
  96  format (1x,I3,5X,3f20.16)
 801  continue
      call MINV(UVEC,3,D,L1,L2,NXX)
      if (D.gt.1.0d-6) goto 30
      write (*,95)
      write (IMP,95)
  95  format (' There must be an error in the unit vectors')
      stop
  30  READ (LEC,100,END=31) NGL,NTW
 100  FORMAT (6I3)
      M=NGL+NTW                                                         
C     INLEZEN DER GLIJSYSTEMEN.                                         
      WRITE (IMP,202) NGL,NTW                                           
 202  FORMAT(/,1H ,I5,' SLIP SYSTEMS  AND',5X,I5,'  TWINNING SYSTEMS')
      DO 1 IS=1,M
      READ (LEC,100) (iv(j),j=1,3),(ir(k),k=1,3)
      WRITE (IMP,201) IS,(iv(j),j=1,3),(ir(k),k=1,3)
 201  FORMAT (1H ,i3,5x,3I3,5X,3I3)
      J=IV(1)*IR(1)+IV(2)*IR(2)+IV(3)*IR(3)
      IF (J.EQ.0) GOTO 70                                               
      WRITE (IMP,200) IS
 200  FORMAT (1H0,' SYSTEM',I5,'   NOT ORTHOGONAL')                     
      STOP                                                              
  70  do 803 i=1,3
      x=0.0
      do 804 k=1,3
      x=x+UVINV(i,k)*IV(k)
 804  continue
      v(i)=x
 803  continue
      do 805 k=1,3
      x=0.0
      do 806 i=1,3
      x=x+IR(i)*UV(i,k)
 806  continue
      r(k)=x
 805  continue
      x=0.0
      y=0.0
      do 807 i=1,3
      x=x+v(i)**2
      y=y+r(i)**2
 807  continue
      x=sqrt(x)
      y=sqrt(y)
      do 808 i=1,3
      v(i)=v(i)/x
      r(i)=r(i)/y
 808  continue
      do 809 k=1,3
      do 809 l=1,3
      DG(k,l)=V(l)*R(k)
 809  continue
      call STR5(A1(1,IS),DG)
      B1(1,IS)=(R(3)*V(2)-R(2)*V(3))/2.
      B1(2,IS)=(R(1)*V(3)-R(3)*V(1))/2.
      B1(3,IS)=(R(2)*V(1)-R(1)*V(2))/2.
      IF (IS.LE.NGL) GOTO 1
      L=IS-NGL
      B2(1,L)=2.*V(1)*V(1)-1.
      B2(2,L)=2.*V(2)*V(1)
      B2(3,L)=2.*V(3)*V(1)
      B2(4,L)=2.*V(2)*V(2)-1.
      B2(5,L)=2.*V(3)*V(2)
      B2(6,L)=2.*V(3)*V(3)-1.
   1  CONTINUE                                                          
      IF (NTW.EQ.0) GOTO 3
      write (IMP,91)
  91  format (' List of twinning shears')
      DO 2 I=1,NTW                                                      
      READ (LEC,198) GG                                                 
 198  FORMAT (F20.0)
      j=ngl+i
      WRITE (IMP,197) j,GG
 197  FORMAT (1X,I3,3X,F20.16)
      IF (GG.GT.0.) GOTO 2                                              
      WRITE (IMP,196)                                                   
 196  FORMAT (' ARRANGE THE TWINNING SYSTEMS IN SUCH A WAY THAT THE TWIN
     1NING SHEARS ARE POSITIVE')                                        
      STOP                                                              
  2   G(I)=GG
  3   write (IMP,101)
101   format (/,' List of basises which have been tried out:')
C       INITIALISEREN DIAGONAL FUNKTIE                                  
      DO 4 I=1,N
  4   DI(I)=I                                                           
C     INVERSIE VAN EEN DEELMATRIX VAN A1                                
  5   DO 6 I=1,N                                                        
      K=DI(I)                                                           
      DO 7 J=1,N                                                        
  7   B(J,I)=A1(J,K)                                                    
  6   CONTINUE                                                          
  13  CALL MINV (BVEC,N,D,L1,L2,NXXX)
      IF (DABS(D)-1.D-6) 8,8,9
C     KIEZEN VAN EEN ANDERE DEELMATRIX                                  
  8   M2=M                                                              
      N1=N                                                              
  11  IF (DI(N1).NE.M2) GO TO 10                                        
      N1=N1-1                                                           
      M2=M2-1                                                           
      IF (N1.NE.0) GO TO 11                                             
      WRITE (IMP,299)                                                   
 299   FORMAT ('0THERE IS NO NON-SINGULAR 5X5 PARTIAL MATRIX,WHICH MEANS
     1 THAT ',/,                                                        
     1    ' THE PROPOSED SET OF SLIP / TWINNING SYSTEMS CANNOT ACCOMODAT
     2E ARBITRARY STRAINS')                                             
      STOP                                                              
  10  IX=DI(N1)                                                         
  12  IX=IX+1                                                           
      DI(N1)=IX                                                         
      N1=N1+1                                                           
      IF (N1.LE.N) GO TO 12                                             
      WRITE (IMP,98) DI                                                 
  98  FORMAT (1H ,20I5)                                                 
      GO TO 5                                                           
   9  I=0                                                               
      write (PNCH,209) TITEL
 209  format (a)
      WRITE (PNCH,210) I,NGL,NTW,DI
 210  FORMAT ( 8I4)
      DO 500 I=1,M
      WRITE (PNCH,212) I,(A1(J,I),J=1,5),(B1(L,I),L=1,3)
 212  FORMAT ( I4,8F20.16)
 500  CONTINUE
      DO 501 I=1,5                                                      
      J=I+M                                                             
      WRITE (PNCH,214) J,(B(I,L),L=1,5)
 214  FORMAT ( I4,5D23.16)
 501  CONTINUE
      IF (NTW.EQ.0) GOTO 30                                             
      DO 502 I=1,NTW                                                    
      J=I+M+5                                                           
      WRITE (PNCH,212)J,(B2(L,I),L=1,6),G(I)
 502  CONTINUE
  31  STOP
      END                                                               
      SUBROUTINE MINV(A,N,D,L,M,NXXX)                                   
      DIMENSION A(NXXX),L(N),M(N)                                       
C                                                                       
C        ...............................................................
C                                                                       
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE  
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION      
C        STATEMENT WHICH FOLLOWS.                                       
C                                                                       
      DOUBLE PRECISION A,D,BIGA,HOLD
C                                                                       
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS    
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS      
C        ROUTINE.                                                       
C                                                                       
C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO      
C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  ABS IN STATEMENT  
C        10 MUST BE CHANGED TO DABS.                                    
C                                                                       
C        ...............................................................
C                                                                       
C        SEARCH FOR LARGEST ELEMENT                                     
C                                                                       
      D=1.0                                                             
      NK=-N                                                             
      DO 80 K=1,N                                                       
      NK=NK+N                                                           
      M(K)=K                                                            
      L(K)=K                                                            
      KK=NK+K                                                           
      BIGA=A(KK)                                                        
      DO 20 J=K,N                                                       
      IZ=N*(J-1)                                                        
      DO 20 I=K,N                                                       
      IJ=IZ+I                                                           
   10 IF( DABS(BIGA)- DABS(A(IJ))) 15,20,20
   15 BIGA=A(IJ)                                                        
      L(K)=I                                                            
      M(K)=J                                                            
   20 CONTINUE                                                          
C                                                                       
C        INTERCHANGE ROWS                                               
C                                                                       
      J=L(K)                                                            
      IF(J-K) 35,35,25                                                  
   25 KI=K-N                                                            
      DO 30 I=1,N                                                       
      KI=KI+N                                                           
      HOLD=-A(KI)                                                       
      JI=KI-K+J                                                         
      A(KI)=A(JI)                                                       
   30 A(JI) =HOLD                                                       
C                                                                       
C        INTERCHANGE COLUMNS                                            
C                                                                       
   35 I=M(K)                                                            
      IF(I-K) 45,45,38                                                  
   38 JP=N*(I-1)                                                        
      DO 40 J=1,N                                                       
      JK=NK+J                                                           
      JI=JP+J                                                           
      HOLD=-A(JK)                                                       
      A(JK)=A(JI)                                                       
   40 A(JI) =HOLD                                                       
C                                                                       
C        DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS        
C        CONTAINED IN BIGA)                                             
C                                                                       
   45 IF(BIGA) 48,46,48                                                 
   46 D=0.0                                                             
      RETURN                                                            
   48 DO 55 I=1,N                                                       
      IF(I-K) 50,55,50                                                  
   50 IK=NK+I                                                           
      A(IK)=A(IK)/(-BIGA)                                               
   55 CONTINUE                                                          
C                                                                       
C        REDUCE MATRIX                                                  
C                                                                       
      DO 65 I=1,N                                                       
      IK=NK+I                                                           
      HOLD=A(IK)                                                        
      IJ=I-N                                                            
      DO 65 J=1,N                                                       
      IJ=IJ+N                                                           
      IF(I-K) 60,65,60                                                  
   60 IF(J-K) 62,65,62                                                  
   62 KJ=IJ-I+K                                                         
      A(IJ)=HOLD*A(KJ)+A(IJ)                                            
   65 CONTINUE                                                          
C                                                                       
C        DIVIDE ROW BY PIVOT                                            
C                                                                       
      KJ=K-N                                                            
      DO 75 J=1,N                                                       
      KJ=KJ+N                                                           
      IF(J-K) 70,75,70                                                  
   70 A(KJ)=A(KJ)/BIGA                                                  
   75 CONTINUE                                                          
C                                                                       
C        PRODUCT OF PIVOTS                                              
C                                                                       
      D=D*BIGA                                                          
C                                                                       
C        REPLACE PIVOT BY RECIPROCAL                                    
C                                                                       
      A(KK)=1.0/BIGA                                                    
   80 CONTINUE                                                          
C                                                                       
C        FINAL ROW AND COLUMN INTERCHANGE                               
C                                                                       
      K=N                                                               
  100 K=(K-1)                                                           
      IF(K) 150,150,105                                                 
  105 I=L(K)                                                            
      IF(I-K) 120,120,108                                               
  108 JQ=N*(K-1)                                                        
      JR=N*(I-1)                                                        
      DO 110 J=1,N                                                      
      JK=JQ+J                                                           
      HOLD=A(JK)                                                        
      JI=JR+J                                                           
      A(JK)=-A(JI)                                                      
  110 A(JI) =HOLD                                                       
  120 J=M(K)                                                            
      IF(J-K) 100,100,125                                               
  125 KI=K-N                                                            
      DO 130 I=1,N                                                      
      KI=KI+N                                                           
      HOLD=A(KI)                                                        
      JI=KI-K+J                                                         
      A(KI)=-A(JI)                                                      
  130 A(JI) =HOLD                                                       
      GO TO 100                                                         
  150 RETURN                                                            
      END
      Subroutine STR5(D5,VGRAD)
      IMPLICIT double precision (A-H,O-Z)
      dimension D5(5),VGRAD(3,3)
      data c1/1.3660254037844390D0/,
     1 c2/0.3660254037844387D0/,
     2 c3/0.7071067811865476D0/
      D5(1)=c1*VGRAD(2,2)+c2*VGRAD(3,3)
      D5(2)=c2*VGRAD(2,2)+c1*VGRAD(3,3)
      D5(3)=c3*(VGRAD(2,3)+VGRAD(3,2))
      D5(4)=c3*(VGRAD(3,1)+VGRAD(1,3))
      D5(5)=c3*(VGRAD(1,2)+VGRAD(2,1))
      return
      end
