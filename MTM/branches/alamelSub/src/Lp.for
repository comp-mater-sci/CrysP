      SUBROUTINE MC03AS(A,I1,J01,B,X,Y,M)
      IMPLICIT REAL * 8 (A-H,O-Z)
      DIMENSION A(I1,1),B(1)
      SAVE
      Y=X
      DO 1 I=1,M
      Y=Y+A(J01,I)*B(I)
   1  CONTINUE
      RETURN                                                            
      END                                                               
      SUBROUTINE MC03AA(A,J,B,I2,J02,X,Y,M,IA)
      IMPLICIT REAL * 8 (A-H,O-Z)
      DIMENSION A(IA,1),B(I2,1)
      SAVE
      Y=X
      DO 1 I=1,M
      Y=Y+A(I,J)*B(J02,I)
   1  CONTINUE
      RETURN
      END
      SUBROUTINE LA01P(N,M,A,B,C,X,F,IA,IPR,U,V,W,IND,ICOL,FK,IU,N2,
     1 TLCOST)
      IMPLICIT REAL * 8 (A-H,O-Z)
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1,NDAT2
C      VERSIE  OM  IN TAYLOR PROGRAMMA TE GEBRUIKEN.
C     N2 is 2 x the number of relaxed constraints
      DIMENSION A(IA,N),B(M),C(N),X(N),FK(N)
      dimension U(IU,1),IND(1),ICOL(1),W(1),V(1)
      DATA L,KL /0,1/
      data TL,TLM/5.0d-10,-5.0d-10/
      SAVE
C      IA=M
      M1=M+1
      MM1=M-1
C      M11=N
      LQ=L+1                                                            
      LP=LQ                                                             
      NR=N+L                                                            
      NL=N+L+1                                                          
      DO 152 K=1,N                                                      
      ICOL(K)=1                                                         
  152 CONTINUE                                                          
      DO 140 K=1,M                                                      
      J=IND(K)                                                          
 140  ICOL(J)=2                                                         
C     INVERSE COMPLETE TEST IF FEASIBLE                                 
   46 DO 75 I=1,M                                                       
      CALL MC03AS(U,IU,I,B,0.0D00,SUM,M)
      IF(SUM+TL)77,78,78                                                   
   78 if (abs(SUM).le.TL) SUM=0.0
      W(I)=SUM                                                          
   75 CONTINUE                                                          
C      if (IPR.eq.2) write (*,701)
C 701  format ('701')
      ICNT=0                                                            
      GO TO 23                                                          
C     NOT FEASIBLE INTRODUCE ARTIFITIAL VARIABLE(ONE ONLY)              
   77 IF(M-L)300,300,302                                                
  300 DO 301 I=1,M                                                      
      V(I)=B(I)-X(I)                                                    
  301 CONTINUE                                                          
      GO TO 170                                                         
  302 DO 80 I=1,M                                                       
      SUM=0.0                                                           
      DO 81 J=LQ,M                                                      
      K=IND(J)
      SUM=SUM+A(I,K)
   81 CONTINUE
      V(I)=B(I)-SUM                                                     
   80 CONTINUE                                                          
      GO TO(170,171),KL                                                 
  171 DO 172 I=1,L                                                      
      V(I)=V(I)-X(I)                                                    
  172 CONTINUE                                                          
  170 AMAX=0.0
c      if (IPR.eq.2) write (*,702)
c 702  format ('702')                                                          
      DO 82 I=1,M                                                       
      CALL MC03AS(U,IU,I,V,1.0D00,SUM,M)
      IF(abs(SUM)-AMAX)82,82,84
   84 ID=I                                                              
      AMAX=abs(SUM)
      DIV=SUM                                                           
   82 CONTINUE  
C      if (IPR.eq.2) write (*,803)
C 803  format ('803')                                                        
      DX=1.0/DIV                                                        
      DIV=DIV-1.0                                                       
      KK=IND(ID)                                                        
      ICOL(KK)=1                                                        
      IND(ID)=N+L+1                                                     
      II1=ID-1                                                          
      I1=ID+1                                                           
      IF(II1)91,91,88                                                   
   88 DO 86 J=1,II1                                                     
      CALL MC03AS(U,IU,J,V,0.0D00,SUM,M)
      SUM=SUM*DX                                                        
      DO 90 K=1,M                                                       
      U(J,K)=U(J,K)-SUM*U(ID,K)                                         
   90 CONTINUE                                                          
   86 CONTINUE                                                          
      IF(I1-M)91,91,92                                                  
   91 DO 93 J=I1,M                                                      
      CALL MC03AS(U,IU,J,V,0.0D00,SUM,M)
      SUM=SUM*DX                                                        
      DO 95 K=1,M                                                       
      U(J,K)=U(J,K)-SUM*U(ID,K)                                         
   95 CONTINUE                                                          
   93 CONTINUE                                                          
   92 DO 96 K=1,M                                                       
      U(ID,K)=U(ID,K)*DX                                                
   96 CONTINUE                                                          
      DO 85 I=1,M                                                       
      W(I)=1.0                                                          
   85 CONTINUE                                                          
C     PHASE ONE OF THE SIMPLEX METHOD                                   
      ICNT=0                                                            
   22 ICNT=ICNT+1                                                       
      AMAX=0.0                                                          
      DO 1 I=1,N                                                        
      KK=ICOL(I)                                                        
      GO TO(2,1),KK                                                     
    2 CALL MC03AA(A,I,U,IU,ID,0.0D00,SUM,M,IA)
      IF(AMAX-SUM)6,1,1                                                 
    6 AMAX=SUM                                                          
      JK=I                                                              
    1 CONTINUE                                                          
      GO TO(24,25),KL                                                   
   25 DO 26 I=1,L                                                       
      IK=I+N                                                            
      SUM=0.0                                                           
      KK=ICOL(IK)                                                       
      GO TO(27,26),KK                                                   
   27 SUM=U(ID,I)*X(I)                                                  
      IF(AMAX-SUM)28,26,26                                              
   28 AMAX=SUM                                                          
      JK=IK                                                             
   26 CONTINUE                                                          
   24 IF(AMAX)8,8,7                                                     
    8 write (IMP,39)
   39 FORMAT(1H ,35HTHERE IS NO SOLUTION TO THE PROBLEM)
      IPR=4                                                            
      RETURN                                                            
    7 THETA=-1.0                                                        
      ICOL(JK)=2                                                        
      IF(JK-N)30,30,31                                                  
   31 JJ=JK-N                                                           
      ASSIGN 35 TO J8                                                   
      DO 32 I=1,M                                                       
      SUM=U(I,JJ)*X(JJ)                                                 
      IF(SUM)33,33,34                                                   
   34 Y=W(I)/SUM                                                        
      GO TO J8,(35,36)                                                  
   35 ASSIGN 36 TO J8                                                   
      GO TO 37                                                          
   36 IF(THETA-Y)33,33,37                                               
   37 THETA=Y                                                           
      IL=I                                                              
   33 V(I)=SUM                                                          
   32 CONTINUE                                                          
      GO TO 38                                                          
   30 ASSIGN 9 TO J8                                                    
      DO 10 I=1,M                                                       
      CALL MC03AA(A,JK,U,IU,I,0.0D00,SUM,M,IA)
      IF(SUM)12,12,13                                                   
   13 Y=W(I)/SUM                                                        
      GO TO J8,(9,14)                                                   
    9 ASSIGN 14 TO J8                                                   
      GO TO 15                                                          
   14 IF(THETA-Y)12,12,15                                               
   15 THETA=Y                                                           
      IL=I                                                              
   12 V(I)=SUM                                                          
   10 CONTINUE                                                          
   38 JJ=IND(IL)                                                        
      ICOL(JJ)=1                                                        
      IND(IL)=JK                                                        
      IF(THETA)16,17,17                                                 
   16 write (IMP, 40) THETA
   40 FORMAT(1H ,21HSOLUTION IS UNBOUNDED,2d15.5)
      IPR=5
C      STOP                                                            
      RETURN                                                            
   17 DO 18 I=1,M                                                       
      W(I)=W(I)-THETA*V(I)                                              
   18 CONTINUE                                                          
      W(IL)=THETA                                                       
      DO 19 J=1,M                                                       
      DELTA=U(IL,J)/V(IL)                                               
      DO 21 I=1,M                                                       
      U(I,J)=U(I,J)-DELTA*V(I)                                          
   21 CONTINUE                                                          
      U(IL,J)=DELTA                                                     
   19 CONTINUE                                                          
      IF(IL-ID)22,23,22                                                 
C     INTRODUCE OBJECTIVE FUNCTION FOR PHASE TWO                        
   23 DO 98 I=1,M                                                       
      SUM=0.0                                                           
      DO 97 J=1,M                                                       
      K=IND(J)                                                          
      IF(K-N)160,160,97                                                 
  160 SUM=SUM+U(J,I)*C(K)                                               
   97 CONTINUE                                                          
      U(M1,I)=SUM                                                       
      U(I,M1)=0.0                                                       
   98 CONTINUE                                                          
      CALL MC03AS(U,IU,M1,B,0.0D00,W(M1),M)
C      if (IPR.eq.2) write (*,804)
C 804  format ('804')
      IND(M1)=NL                                                        
      U(M1,M1)=1.0                                                      
C      GO TO(3,103),IPR 
      if (IPR.ne.2) goto 3                                                  
  103 write (IMP, 703) ICNT
  703 FORMAT(1H ,14HPHASE ONE TOOK,I4,11H ITERATIONS)
C     PHASE TWO OF THE REVISED SIMPLEX METHOD                           
    3 ICNT=0                                                            
  100 ICNT=ICNT+1                                                       
      AMAX=0.0
      JK=0                                                          
      DO 101 I=1,N                                                      
      SUM=0.0                                                           
      KK=ICOL(I)                                                        
C      if (IPR.eq.2) write (*,807) KK
C 807  format ('807',I10) 
      GO TO(102,101),KK                                                 
  102 CALL MC03AA(A,I,U,IU,M1,-C(I),SUM,M,IA)
C      if (IPR.eq.2) write (*,700) I,AMAX,SUM
C  700 format('700 I=',i5,2F15.10)
C      if ((AMAX-SUM-TL).lt.0.0d0.and.ipr.eq.2) write (*,808) I,SUM
C  808 format ('808',i5,d15.5)  
      IF(AMAX-SUM)106,101,101
C      IF(AMAX-SUM)180,101,101
C 180  IF (I.LE.M11.OR.I.GT.N) GOTO 106
C      J1=I-M11
C      J2=J1/2
C      IF (J1-2*J2) 181,181,182
C 181  J2=I-1
C      GOTO 183
C 182  J2=I+1
C 183  IF (ICOL(J2).EQ.2) GOTO 101
  106 AMAX=SUM                                                          
      JK=I                                                              
  101 FK(I)=SUM 
C      if (ipr.eq.2) write (*,805)
C 805  format ('805')                                                        
C     WRITE (IMP,1116) IND                                              
C1116 FORMAT (/' BASIS',(T12,6I10),/,' INVERSE OF BASIS',/)
C     DO 1120 I=1,M1                                                    
C1120 WRITE (IMP,1121) I,(U(I,J),J=1,M1)                                
C1121 FORMAT (' ',I3,(T5,6D20.8))
C
C     If you want to see the actual simplex table, change the 3
C     in next instruction into a 2 (and use IPR=2)
C
      if (IPR.lt.3) goto 1119
      WRITE (IMP,1118)
 1118 FORMAT (' ACTUAL COST FUNCTION AND SIMPLEX TABLE',/,
     1' THE LAST LINE GIVES THE CONSTANT TERMS',/)
      DO 1117 I=1,N
      CALL KOLA(A,IA,U,IU,V,I,M)
 1117 WRITE (IMP,1115) I,FK(I),(V(J),J=1,M)
      WRITE (IMP,1115) I,W(M1),(W(J),J=1,M)
 1115 FORMAT (1X,I3,D12.4,(T18,5D12.4))
 1119 continue
      GO TO(124,125),KL
  125 DO 126 I=1,L                                                      
      IK=I+N                                                            
      SUM=0.0                                                           
      KK=ICOL(IK)                                                       
      GO TO(127,126),KK                                                 
  127 SUM=U(M1,I)*X(I)
      IF(AMAX-SUM)128,126,126                                           
  128 AMAX=SUM                                                          
      JK=IK                                                             
  126 CONTINUE
C     Special test to check if new variable for basis corresponds
C     to a relaxed constraints (N2/2 in number)
  124 continue
C      if (IPR.eq.2) write (*,806) JK,N,N2
C 806  format ('806',3I10)          
      if (JK.LE.N-N2) goto 190
      JK1=JK+2
      if (JK1.gt.N) JK1=JK-2
      if (ICOL(JK1).eq.2) goto 108
  190 IF (AMAX-TLCOST) 108,107,107
C     SOLUTION IS REACHED                                               
  108 GO TO(147,148),KL
  148 write (*,1000)
      write (IMP,1000)
 1000 format (' Format 1000 in LA01P - Unexpected branch of program')
      stop
c      DO 149 I=1,L
c      IF(X(I))150,149,149
c  150 DO 151 J=1,N
c      A(I,J)=-A(I,J)
c  151 CONTINUE
c      B(I)=-B(I)
c  149 CONTINUE
  147 DO 99 K=1,N                                                       
      X(K)=0.0                                                          
   99 CONTINUE                                                          
      DO 79 J=1,M                                                       
      K=IND(J)                                                          
      IF(K-N)179,179,79                                                 
  179 X(K)=W(J)                                                         
   79 CONTINUE                                                          
      F=W(M1)                                                           
C      GO TO(303,304),IPR
      if (IPR.ne.2) goto 303                                                
  304 write (IMP,365) F
  365 FORMAT(1H ,5X,24HOPTIMUM FUNCTION VALUE =,D25.14)
      DO 413 I=1,M
      write (IMP,314) IND(I),W(I)
  413 CONTINUE
C      do 417 I=1,M+1
C      write (IMP,416) (U(I,J),j=1,M)
C  417 continue
  305 FORMAT(6X,16HFUNCTION VALUE =,D25.14)
      write (IMP,308)
  308 FORMAT(1H ,10X,8HSOLUTION)
      DO 306 I=1,N                                                      
      write (IMP,307) X(I)
  306 CONTINUE                                                          
  307 FORMAT(1X,D25.14)
  303 RETURN                                                            
C  107 GO TO(309,310),IPR
  107 if (IPR.ne.2) goto 309                                                
  310 write (IMP,312) ICNT
  312 FORMAT(1H ,5X,14HITERATION NO. ,I3)
      write (IMP,305) W(M1)
      write (IMP,311)
  311 FORMAT(11X,9HVARIABLES)                                           
      DO 313 I=1,M                                                      
      write (IMP,314) IND(I),W(I)
  314 FORMAT(5X,2HX(,I3,2H)=,D21.14)
  313 CONTINUE
C      do 415 I=1,M+1
C      write (IMP,416) (U(I,J),j=1,M)
C  415 continue
C  416 format (1x,10F7.4)
  309 THETA=-1.0                                                        
      ICOL(JK)=2                                                        
      IF(JK-N)130,130,131                                               
  131 JJ=JK-N                                                           
      ASSIGN 135 TO J8                                                  
      DO 132 I=1,M                                                      
      SUM=U(I,JJ)*X(JJ)                                                 
      IF(SUM)133,133,134                                                
  134 Y=W(I)/SUM                                                        
      GO TO J8,(135,136)                                                
  135 ASSIGN 136 TO J8                                                  
      GO TO 137                                                         
  136 IF(THETA-Y)133,133,137                                            
  137 THETA=Y                                                           
      IL=I                                                              
  133 V(I)=SUM                                                          
  132 CONTINUE                                                          
      V(M1)=AMAX                                                        
      GO TO 138                                                         
  130 ASSIGN 109 TO J8                                                  
      DO 110 I=1,M                                                      
      CALL MC03AA(A,JK,U,IU,I,0.0D00,SUM,M,IA)
      IF (SUM.LT.TL.AND.SUM.GT.TLM) SUM=0.                              
      IF(SUM-TL)112,112,113                                             
  113 Y=W(I)/SUM                                                        
      GO TO J8,(109,114)                                                
  109 ASSIGN 114 TO J8                                                  
      GO TO 115                                                         
  114 IF(THETA-Y)112,112,115                                            
  115 THETA=Y                                                           
      IL=I                                                              
  112 V(I)=SUM                                                          
  110 CONTINUE                                                          
      V(M1)=AMAX                                                        
  138 JJ=IND(IL)                                                        
      ICOL(JJ)=1                                                        
      IND(IL)=JK                                                        
      IF (THETA+TL) 116,117,117                                         
  116 write (IMP,40) THETA,TL
      IPR=6
C      stop                                                            
      RETURN                                                            
  117 DO 118 I=1,M1                                                     
      W(I)=W(I)-THETA*V(I)                                              
      IF (W(I).LT.TLM)GOTO 510                                          
      IF (W(I).LT.0.) W(I)=0.                                           
  118 CONTINUE                                                          
      I=IL                                                              
      W(IL)=THETA                                                       
      IF (THETA.LT.TLM) GOTO 510                                        
      IF (THETA.LT.0.) W(IL)=0.                                         
      DO 119 J=1,M1                                                     
      DELTA=U(IL,J)/V(IL)                                               
      DO 121 I=1,M1                                                     
      U(I,J)=U(I,J)-DELTA*V(I)                                          
  121 CONTINUE                                                          
      U(IL,J)=DELTA                                                     
  119 CONTINUE                                                          
      GO TO 100                                                         
  510 WRITE (IMP,511) I,W(I)                                            
  511 FORMAT (' LA01P - RIGHT HAND SIDES TOO MUCH NEGATIVE',I5,D15.8)
      IPR=7                                                            
      RETURN                                                            
      END
      Subroutine KOLA(A,IA,U,IU,V,I,M)
      IMPLICIT REAL * 8 (A-H,O-Z)
      dimension A(IA,1),U(IU,1),V(1)
      do 1 j=1,M
      x=0.0
      do 2 k=1,M
      x=x+U(j,k)*A(k,I)
   2  continue
      V(j)=x
   1  continue
      return
      end
