C     To express the C-coeff. of a texture in a different reference
C     system. LMAX=32
      COMMON /BIB1/LMAX,IMAG,IDM,IDN,idumm(18)                                    
      COMMON /TEKEN/TITEL(10)                                           
      COMMON /CCOEF/CCF1(2985)                                          
      COMPLEX CX1(1020),CX2(1020),T(23985)
      common /hugeT/ T
      common /hugeCX/CX1,CX2
      DATA FPI/1.745329E-2/                                             
C     UNIT 5 = CARD READER
      open (unit=5,file='ROTTEX.I01',mode='read',status='old')
C     UNIT 6 = PRINTER
C     open (unit=6,file=' ',mode='write')
C     UNIT 24 = LIBRARY FILE
      open (unit=24,file='WAGNER.B04',status='old',form='unformatted')
C     1 OPEN COMMAND WILL BE ISSUED BY INUIT FOR AN INPUT C-COEFFICIENT
C     FILE ON UNIT 11
C     1 OPEN COMMAND WILL BE ISSUED BY INUIT FOR OUTPUT C-COEFFICIENT
C     FILES ON UNIT 12
      CALL INUIT(2)                                                     
      IMAG0=IMAG                                                        
      IF (LMAX.LE.32) GOTO 4                                            
C     WRITE (6,102)                                                     
  102 FORMAT (' LMAX IS REDUCED TO 32')                                 
      LMAX=32                                                           
   4  READ (5,99,END=5) FI1,PH,FI2                                      
  99  FORMAT (3F10.0)                                                   
      READ (5,97) IMG,IDMT
  97  FORMAT (2I5)
C     WRITE (6,100) FI1,PH,FI2,IMG,IDMT
 100  FORMAT ('0ROTATION ANGLES',/,' PHI1=',F12.4,'  PHI=',F12.4,       
     1'   PHI2=',F12.4,/,' IMPOSED IMAG PARAMETER=',I2,
     2/,'  IMPOSED IDN Parameter=',i5)
      IF (IMG.GE.1.OR.IMG.LE.2) GOTO 6                                  
C     WRITE (6,103)                                                     
 103  FORMAT ('0WARNING  - IMG IS INVALID AND IS REPLACED BY 1',//)     
      IMG=1                                                             
   6  READ (5,98) TITEL                                                 
  98  FORMAT (10A4)                                                     
C     WRITE (6,101) TITEL                                               
 101  FORMAT ('0NEW TITLE=',10A4)                                       
      IMAG=IMAG0                                                        
      PHI1=FPI*FI1                                                      
      PHI=FPI*PH                                                        
      PHI2=FPI*FI2                                                      
      CALL CCOMPL(1,CCF1,CX1,IDN,IC1)                                   
      CALL TCOMPL(PHI1,PHI,PHI2,T,IDN,IDMT,IC)                               
      CALL ROTC(CX2,CX1,T,IDN,IDN,IDMT,IC2)                                  
      IMAG=IMG                                                          
      IDN=IDMT
      CALL CCOMPL(-1,CCF1,CX2,IDN,IC3)                                  
      IF (IMAG.NE.1) GOTO 1                                             
      DO 2 I=747,1492                                                   
   2  CCF1(I)=0.                                                        
      DO 3 I=2240,2985                                                  
   3  CCF1(I)=0.                                                        
   1  CALL INUIT(1)                                                     
   5  STOP                                                              
      END                                                               
