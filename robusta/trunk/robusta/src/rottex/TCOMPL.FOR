      SUBROUTINE TCOMPL(FI1,PHI,FI2,T,IDN,IDM,IC)
C     BEREKENT EEN STEL COMPLEXE TLMN-FUNKTIES, IN PRINCIPE             
C     ZONDER SYMMETRIE, L ONEVEN EN EVEN GEINTEGREERD.                  
C     DE HOEKEN IN RADIALEN .                                           
      COMMON /BIB1/LMAX,idumm(21)                                                 
      COMPLEX PLMN,TAC,TACC,TLMN(33,65),T(23985)
      dimension Q(32,33,33) 
      DIMENSION ML(32),SC(33,2),C1(33),C2(33),S1(33),S2(33),AACC(33)    
      DATA ML /3*0,1,0,1,0,3*1,0,2,3*1,2,1,2,1,3*2,
     1 1,3,3*2,3,2,3,2,3/
      DATA IBIB /0/                                                     
      IF (LMAX.LE.32) GOTO 1                                            
C     WRITE (6,100) LMAX                                                
 100  FORMAT ('0TCOMPL IS ONLY SUITED FOR LMAX.LE.32-=',I10)            
      STOP                                                              
   1  IF (IBIB.EQ.1) GOTO 3                                             
      DO 2 L=1,32                                                       
      L1=L+1                                                            
      DO 2 M1=1,L1                                                      
      READ (24) (Q(L,M1,N1),N1=1,M1)                                    
      DO 12 N1=1,M1                                                     
      Q(L,N1,M1)=Q(L,M1,N1)                                             
  12  CONTINUE                                                          
   2  CONTINUE                                                          
   3  IBIB=1                                                            
      LMX1=LMAX+1                                                       
      DO 4 I=1,LMX1                                                     
      F=I-1                                                             
      SC(I,1)=COS(F*PHI)                                                
      SC(I,2)=SIN(F*PHI)                                                
   4  CONTINUE                                                          
      DO 13 I=1,LMX1,IDN                                                
      F=I-1                                                             
      C1(I)=COS(F*FI1)                                                  
      S1(I)=SIN(F*FI1)                                                  
  13  CONTINUE                                                          
      DO 14 I=1,LMX1,IDM
      F=I-1                                                             
      C2(I)=COS(F*FI2)                                                  
      S2(I)=SIN(F*FI2)                                                  
  14  CONTINUE                                                          
      IC=0                                                              
      DO 5 L=1,LMAX                                                     
      LEV=L-((L/2)*2)                                                   
      L1=L+1                                                            
      L2=L/IDM+1
      L3=2*(L/IDN)+1                                                    
      MUM=ML(L)                                                         
      IF (MUM.EQ.0) GOTO 5                                              
C     BOVENSTAANDE INSTRUKTIE ENKEL VOOR KUBISCH ROOSTER.               
      DO 6 M1=1,L1,IDM
      M=M1-1                                                            
      MPL=1+M/IDM
      DO 7 N1=1,L1,IDN                                                  
      N=N1-1                                                            
      NPL=L/IDN+1+N/IDN                                                 
      NMI=L/IDN+1-N/IDN                                                 
C     A-ACCENT-LMNS COEFICIENTEN.                                       
      NEV=1+M+N-((M+N)/2)*2                                             
C     NEV=1 EVEN   N=2 ONEVEN                                           
      DO 8 IS=1,L1                                                      
      QQ=Q(L,M1,IS)*Q(L,IS,N1)                                          
      IF (IS.EQ.1) QQ=0.5*QQ                                            
      AACC(IS)=2.*QQ                                                    
   8  CONTINUE                                                          
C     PLMN                                                              
      P=0.                                                              
      DO 9 IS=1,L1                                                      
      P=P+AACC(IS)*SC(IS,NEV)                                           
   9  CONTINUE                                                          
      PLMN=P                                                            
      IF (NEV.EQ.2) PLMN=CMPLX(0.,P)                                    
      TAC=PLMN*(C2(M1)*C1(N1)-S2(M1)*S1(N1))                            
      TACC=PLMN*(C2(M1)*S1(N1)+S2(M1)*C1(N1))                           
      TLMN(MPL,NPL)=TAC+(0.,1.)*TACC                                    
C     WRITE (6,102) L,M,N,TLMN(MPL,NPL)                                 
C102  FORMAT (' L=',I5,' M=',I5,' N=',I5,2E15.6)                        
C     OMVORMING DER AACC VOOR M --> -M                                  
      EPS=-1.                                                           
      IF (LEV.EQ.1) EPS=1.                                              
      DO 10 IS=1,L1                                                     
      EPS=-EPS                                                          
      AACC(IS)=EPS*AACC(IS)                                             
  10  CONTINUE                                                          
C     PLMN                                                              
      P=0.                                                              
      DO 11 IS=1,L1                                                     
      P=P+AACC(IS)*SC(IS,NEV)                                           
  11  CONTINUE                                                          
      PLMN=P                                                            
      IF (NEV.EQ.2) PLMN=CMPLX(0.,P)                                    
      TAC=PLMN*(C2(M1)*C1(N1)+S2(M1)*S1(N1))                            
      TACC=PLMN*(C2(M1)*S1(N1)-S2(M1)*C1(N1))                           
      TLMN(MPL,NMI)=TAC-(0.,1.)*TACC                                    
      NMIN=-N                                                           
C     WRITE (6,102) L,M,NMIN,TLMN(MPL,NMI)                              
   7  CONTINUE                                                          
   6  CONTINUE                                                          
      DO 15 N1=1,L3                                                     
      DO 15 M1=1,L2                                                     
      IC=IC+1                                                           
      T(IC)=TLMN(M1,N1)                                                 
  15  CONTINUE                                                          
   5  CONTINUE                                                          
C      WRITE (6,101) IC
C 101  FORMAT (' MAX.INDEX=',I10)
      RETURN                                                            
      END                                                               
