      SUBROUTINE INUIT(ISW)                                             
C     ISW=1 OUTPUT  ISW=2 INPUT  C-COEFF. PAAR EN ONPAAR                
      COMMON /TEKEN/ TITEL(10)                                          
      COMMON /BIB1/ LMAX,IMAG,IDM,IDN,ncoif,lstart,ml(16)
      COMMON /CCOEF/ C1(746),C2(746),C010,C3(746),C4(746)               
      GOTO (10,20,10),ISW
  10  OPEN (UNIT=12,FILE=' ',FORM='UNFORMATTED')
      WRITE (12) TITEL,LMAX,IMAG,IDM,IDN,ISNIJ,IDIM1,IDIM3 
      WRITE (12) C1 
      WRITE (12) C2 
      WRITE (12) C010 
      IF (ISW.EQ.3) GOTO 1
      WRITE (12) C3   
      WRITE (12) C4
   1  END FILE 12
      CLOSE (UNIT=12)
      RETURN
  20  OPEN (UNIT=11,FILE=' ',STATUS='OLD',FORM='UNFORMATTED')
      READ (11) TITEL,LMAX,IMAG,IDM,IDN,JSNIJ,IDIM1,IDIM3               
      WRITE (6,100) TITEL,LMAX,IMAG,IDM,IDN                             
 100  FORMAT('1INUIT-INPUT TEXTUUR=',10A4,/,'0LMAX=',I5,            
     1'  IMAG=',I5,'  IDM=',I5,'  IDN=',I5)                             
      READ (11) C1                                                      
      READ (11) C2                                                      
      C010=1.                                                           
      READ (11,END=14) C010                                             
      READ (11,END=14) C3                                               
      READ (11) C4                                                      
      READ (11,END=12) X                                                
  12  GOTO 15
  14  WRITE (6,101)                                                     
 101  FORMAT ('0NO ODD COEFFICIENTS ON INPUT FILE')                     
      DO 11 I=1,746                                                     
      C3(I)=0.                                                          
      C4(I)=0.                                                          
  11  CONTINUE                                                          
  15  CLOSE (UNIT=11)
      RETURN                                                            
      END                                                               
      SUBROUTINE KNIV(NIV,VMIN,HNIV)                                    
C     KEUZE HOOGTELIJNNIVEAUS.                                          
C     NIV=1 AANPASSING AAN MINIMUM- EN MAXIMUMWAARDE.                   
C     NIV=2 AANPASSING AAN MINIMUMWAARDE ALLEEN.                        
C     NIV=3 INLEZEN VAN DE NIVEAUS VANOP KAART.                         
      DIMENSION DNIV(32,2),HNIV(10)                                     
      DATA DNIV/ 0.8, 1.0, 1.3, 1.6, 2.0, 2.5, 3.2, 4.0,                
     1           5.0, 6.4, 8.0, 10., 13., 16., 20., 25.,                
     2           32., 40., 50., 64., 80.,100.,130.,160.,                
     3          200.,250.,320.,400.,500.,640.,800.,999.,                
     4           0.7, 1.0, 1.4, 2.0, 2.8, 4.0, 5.6, 8.0,                
     5           11., 16., 22., 32., 44., 64., 88.,128.,                
     6          176.,256.,352.,512.,12*999./                            
      ENIV=ABS(VMIN)                                                    
      GOTO (1,2,3),NIV                                                  
   1  IF (ENIV.LT.0.7) ENIV=0.7                                         
      ZNIV=HNIV(1)                                                      
      REL=ZNIV/ENIV                                                     
      IS=1                                                              
      J=1                                                               
      IF (REL.LT.14.) GOTO 4                                            
      J=2                                                               
      IF (REL.LT.45.) GOTO 4                                            
      IS=2                                                              
      J=1                                                               
      IF (REL.LT.228.) GOTO 4                                           
      J=2                                                               
      GOTO 4                                                            
   2  IS=1                                                              
      J=2                                                               
   4  DO 5 I1=1,31                                                      
      IF (ENIV.LE.DNIV(I1,J)) GOTO 6                                    
   5  CONTINUE                                                          
      I1=32                                                             
   6  GOTO (7,8),NIV                                                    
   7  I2=I1+9*IS+1                                                      
      IF (I2.GT.32) GOTO 8                                              
      IF (ZNIV.LE.DNIV(I2,J)) GOTO 8                                    
      I1=I1+1                                                           
      GOTO 7                                                            
   8  DO 9 I=1,10                                                       
      HNIV(I)=1.0E30                                                    
      IF (I1.GT.32) GOTO 9                                              
      HNIV(I)=DNIV(I1,J)                                                
   9  I1=I1+IS                                                          
  10  WRITE (6,100) HNIV                                                
 100  FORMAT (' CHOICE OF CONTOUR LEVELS',/,1X,10F6.1,/)                
      RETURN                                                            
   3  READ (5,99) HNIV                                                  
  99  FORMAT (5F10.0)                                                   
      GOTO 10                                                           
      END                                                               
