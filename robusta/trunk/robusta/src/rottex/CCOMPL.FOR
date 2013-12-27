      SUBROUTINE CCOMPL(ISW,CCOF,CX1,IDN,IC2)                           
C     CX1=C-COMPLEX VOOR N=0,1,...L                                     
C     ISW=1 OMZETTING C-COEFFICIENTEN -->  COMPLEXE C-COEFFICIENTEN.    
C     ISW=-1 OF ISW= -2   TERUG OMZETTEN VAN COMPLEXE COEFFICIENTEN.    
C     ISW=-2 DE C-COEFFICIENTEN WORDEN AFGEDRUKT.                       
C     ALS ISW=1 ZAL IC2 HET TOTAAL AANTAL COMPLEXE COEFFICIENTEN GEVEN. 
      COMMON /BIB1/LMAX,idumm(21)                                                 
      DIMENSION CCOF(2985),ML(32)                                       
      COMPLEX CX1(1020),CX
      DATA W05/0.707107/                                                
      DATA ML /3*0,1,0,1,0,3*1,0,2,3*1,2,1,2,1,3*2,
     1 1,3,3*2,3,2,3,2,3/
      IF (LMAX.LE.32) GOTO 1                                            
C     WRITE (6,100) LMAX                                                
 100  FORMAT ('0CCOMPL IS ONLY SUITED FOR LMAX.LE.32- =',I10)           
      STOP                                                              
   1  IF (ISW.LT.0) GOTO 2                                              
      CX1(1)=CCOF(1493)                                                 
      IC1=0                                                             
      DO 3 LEV=1,2                                                      
      IC2=1                                                             
      LP=1                                                              
      DO 4 L=1,LMAX                                                     
      LP=LP+1                                                           
      IF (LP.GT.2) LP=1                                                 
C     LP=1 L=PAAR  LP=2  L=ONPAAR                                       
      MUM=ML(L)                                                         
      IF (MUM.EQ.0) GOTO 4                                              
      L1=L+1                                                            
      DO 5 NN=1,L1,IDN                                                  
      N=NN-1                                                            
      DO 60 MU=1,MUM                                                    
      IC2=IC2+1                                                         
      IF (LEV.NE.LP) GOTO 60                                            
      IC1=IC1+1                                                         
      IF (NN.EQ.1) GOTO 7                                               
      CX=W05*CMPLX(CCOF(IC1),CCOF(IC1+746))                             
      CX1(IC2)=CX                                                       
      GOTO 6                                                            
   7  CX1(IC2)=CMPLX(CCOF(IC1),CCOF(IC1+746))
C  6  WRITE (6,102) IC2,L,MU,N,CX1(IC2)                                 
   6  CONTINUE                                                          
  60  CONTINUE                                                          
 102  FORMAT (' IC=',I5,' L=',I5,' MU=',I5,' N=',I5,2E15.6)             
   5  CONTINUE                                                          
   4  CONTINUE                                                          
      IC1=1493                                                          
   3  CONTINUE                                                          
C     WRITE (6,101) IC2                                                 
 101  FORMAT('0MAX. INDEX=',I10)                                        
      RETURN                                                            
   2  DO 12 I=1,2985                                                    
  12  CCOF(I)=0.                                                        
      CCOF(1493)=REAL(CX1(1))                                           
      IC1=0                                                             
C     IF (ISW.EQ.-2) WRITE (6,102) IC1,IC1,IC1,IC1,CCOF(1493),CCOF(1)   
      DO 13 LEV=1,2                                                     
      IC2=1                                                             
      LP=1                                                              
      DO 14 L=1,LMAX                                                    
      LP=LP+1                                                           
      IF (LP.GT.2) LP=1                                                 
      MUM=ML(L)                                                         
      IF (MUM.EQ.0) GOTO 14                                             
      L1=L+1                                                            
      DO 15 NN=1,L1,IDN                                                 
      N=NN-1                                                            
      DO 160 MU=1,MUM                                                   
      IC2=IC2+1                                                         
      IF (LEV.NE.LP) GOTO 160                                           
      IC1=IC1+1                                                         
      IF (NN.EQ.1) GOTO 17                                              
      CCOF(IC1)=2.*W05*REAL(CX1(IC2))                                   
      CCOF(IC1+746)=2.*W05*AIMAG(CX1(IC2))                              
      GOTO 16                                                           
  17  CCOF(IC1)=REAL(CX1(IC2))                                          
      CCOF(IC1+746)=AIMAG(CX1(IC2))
  16  CONTINUE
C     IF(ISW.EQ.-2) WRITE (6,102) IC1,L,MU,N,CCOF(IC1),CCOF(IC1+746)    
 160  CONTINUE                                                          
  15  CONTINUE                                                          
  14  CONTINUE                                                          
      IC1=1493                                                          
  13  CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE ROTC(CY1,CX1,T,IDNC,IDNT,IDMT,ICY)                          
C     ROTATIE VAN C-COEFFICIENTEN.                                      
C     IDNC HEEFT BETREKKING OP CX                                       
C     IDNT HEEFT BETREKKING OP T                                        
C     VOOR CY1 IS IDN=IDMT (RESULTAAT V.D. ROTATIE)
      COMMON /BIB1/ LMAX,idumm(21) 
      COMPLEX CY1(1020),CX1(1020),T(23985),TLNS(33,65),C,CLMS(3,65),
     1 CLMN(3,33)
      DIMENSION ML(32)                                                  
      DATA ML /3*0,1,0,1,0,3*1,0,2,3*1,2,1,2,1,3*2, 
     1 1,3,3*2,3,2,3,2,3/
      IF (IDNT.LE.IDNC) GOTO 1                                          
C     WRITE (6,100) IDNT,IDNC                                           
 100  FORMAT ('0ROTC-IDNT=',I5,'  MUST NOT BE LARGER THAN IDNC=',I5)    
      STOP                                                              
   1  ICT=0                                                             
      ICX=1                                                             
      ICY=1                                                             
      CY1(1)=CX1(1)                                                     
C     WRITE (6,101) CY1(1)                                              
 101  FORMAT ('1CY1(1)=',2E15.6)                                        
      TTT=1.                                                            
      DO 2 L=1,LMAX                                                     
      TTT=-TTT                                                          
      MUM=ML(L)                                                         
      IF (MUM.EQ.0) GOTO 2                                              
      L1=L+1                                                            
      L2=L/IDMT+1 
      L3=2*(L/IDNT)+1                                                   
      L4=2*(L/IDNC)+1                                                   
      L5=L/IDNT+1                                                       
      DO 3 IS=1,L3                                                      
      DO 3 IN=1,L2                                                      
      ICT=ICT+1                                                         
      TLNS(IN,IS)=T(ICT)                                                
   3  CONTINUE                                                          
      TT=TTT                                                            
      DO 4 IS1=1,L1,IDNC                                                
      IS=IS1-1                                                          
      ISPL=L5+IS/IDNT                                                   
      ISMI=L5-IS/IDNT                                                   
      DO 5 MU=1,MUM                                                     
      ICX=ICX+1                                                         
      CLMS(MU,ISPL)=CX1(ICX)                                            
      IF (IS.EQ.0) GOTO 5                                               
      CLMS(MU,ISMI)=TT*CONJG(CX1(ICX))                                  
   5  CONTINUE                                                          
      IF (IDNC.EQ.1) TT=-TT                                             
   4  CONTINUE                                                          
C     EIGENLIJKE ROTATIE.                                               
      DO 6 MU=1,MUM                                                     
      DO 7 N1=1,L1,IDMT
      N=N1-1                                                            
      NPL=1+N/IDMT
      C=(0.,0.)                                                         
      DO 8 IS1=1,L1,IDNC                                                
      IS=IS1-1                                                          
      ISPL=L5+IS/IDNT                                                   
      ISMI=L5-IS/IDNT                                                   
      C=C+CLMS(MU,ISPL)*TLNS(NPL,ISPL)                                  
      IF (IS.EQ.0) GOTO 8                                               
      C=C+CLMS(MU,ISMI)*TLNS(NPL,ISMI)                                  
   8  CONTINUE                                                          
      CLMN(MU,NPL)=C                                                    
   7  CONTINUE                                                          
   6  CONTINUE                                                          
C     INSCHRIJVEN RESULTAAT.                                            
      DO 9 N1=1,L1,IDMT
      N=N1-1                                                            
      NPL=1+N/IDMT
      DO 10 MU=1,MUM                                                    
      ICY=ICY+1                                                         
      CY1(ICY)=CLMN(MU,NPL)                                             
C      WRITE (6,103) L,N,MU,CY1(ICY)                                    
 103  FORMAT(' L=',I5,' N=',I5,' MU=',I5,2E20.6)                        
  10  CONTINUE                                                          
   9  CONTINUE                                                          
   2  CONTINUE                                                          
C      write (6,104) ICY
C 104  format (' ICY=',i5)
      RETURN                                                            
      END                                                               
      SUBROUTINE CSCAL(SOM1,SOM2,CCF1,CCF2,IDN1,IDN2,IMG1,IMG2)         
C     SCALAIR PRODUKT VAN TWEE REEKSEN C-COEFFICIENTEN.                 
      DIMENSION CCF1(2985),CCF2(2985),ML(16,2)                          
      COMMON /BIB1/ LMAX,idumm(21)                                                
      DATA ML/4*1,2,1,4*2,3,2,4*3,1,0,4*1,2,1,4*2,3,3*0/                
      X=CCF1(1493)*CCF2(1493)                                           
      X1=0.                                                             
      LSTART=4                                                          
      IC1=0                                                             
      IC2=0                                                             
      JMAG=(IMG1-1)*(IMG2-1)                                            
      DO 1 LEV=1,2                                                      
      DO 2 L=LSTART,LMAX,2                                              
      LL=(L-LSTART)/2+1                                                 
      L1=L+1                                                            
      AL=1./FLOAT(2*L+1)                                                
      Y=0.                                                              
      Y1=0.                                                             
      MUM=ML(LL,LEV)                                                    
      IF (MUM.EQ.0) GOTO 2                                              
      DO 4 NN=1,L1                                                      
      N=NN-1                                                            
      NEV=N-(N/2)*2                                                     
      ISW1=NEV*(IDN1-1)                                                 
      ISW2=NEV*(IDN2-1)                                                 
      ISW3=ISW1+ISW2                                                    
      DO 5 MU=1,MUM                                                     
      IC1=IC1+1-ISW1                                                    
      IC2=IC2+1-ISW2                                                    
      IF (ISW3.NE.0) GOTO 5                                             
      Y=Y+CCF1(IC1)*CCF2(IC2)                                           
      IF (JMAG.EQ.0) GOTO 5                                             
      Y1=Y1+CCF1(IC1+746)*CCF2(IC2+746)                                 
   5  CONTINUE                                                          
   4  CONTINUE                                                          
      X=X+Y*AL                                                          
      IF (JMAG.EQ.0) GOTO 2                                             
      X1=X1+Y1*AL                                                       
   2  CONTINUE                                                          
      LSTART=9                                                          
      IC1=1493                                                          
      IC2=1493                                                          
   1  CONTINUE                                                          
      SOM1=X+X1                                                         
      SOM2=X-X1                                                         
      RETURN                                                            
      END                                                               
