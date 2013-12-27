C     TO compare an ODF with one or several other ones.
      COMMON /TEKEN/TITEL(10)
      COMMON /CCOEF/CCF1(2985)
      dimension ccf(2985)
C     UNIT 6 = PRINTER
      open (unit=6,file='versch.l01',mode='write')
C     HEREAFTER, INUIT WILL ISSUE AN OPEN COMMAND FOR A C-COEFFICIENTS
C     INPUT FILE ON UNIT 11.
      CALL INUIT('first.c')
      versch=0.0
      write (6,100) versch,TITEL
 100  format(F16.8,4x,10A4)
   4  CONTINUE
C     write(*,200)
 200  format('0Give number of ODF to be compared with the first:',/
     1' ( 1 digit only - 0 terminates program)')
C      read(*,99) NODF
      NODF=1
  99  format(i1)
C      if (nodf.eq.0) stop
      do 2 i=1,2985
      ccf(i)=ccf1(i)
   2  continue
      do 1 iodf=1,nodf
  98  format(f10.0)
      CALL INUIT('second.c')
      CALL vergel(versch,ccf,CCF1)
      write (6,100) versch,TITEL
   1  continue
      stop
      END
      SUBROUTINE vergel(versch,ccf,CCF1)
      COMMON /BIB1/ LMAX,IMAG,IDM,IDN,IDUMM(18)                         
      DIMENSION CCF(2985),CCF1(2985),ML(16,2)                                      
      DATA ML /4*1,2,1,4*2,3,2,4*3,                                     
     1         1,0,4*1,2,1,4*2,3,3*0/                                   
      LSTART=4                                                          
      IREF=0
      versch=(ccf(1493)-ccf1(1493))**2
      DO 1 LP=1,2                                                       
      IC=0                                                              
      DO 7 L=LSTART,LMAX,2
      DENOM=2*L+1
      LL=(L-LSTART)/2+1                                                 
      L1=L+1                                                            
      MUM=ML(LL,LP)                                                     
      IF (MUM.EQ.0) GOTO 7                                              
      DO 13 NN=1,L1,IDN
      N=NN-1                                                            
      DO 13 MU=1,MUM
      IC=IC+1                                                           
      IC0=IC+IREF
C      ccf(ic0)=ccf(ic0)+gw*ccf1(ic0)
      versch=versch+(ccf(ic0)-ccf1(ic0))**2/DENOM
      IC0=IC0+746                                                       
C      ccf(ic0)=ccf(ic0)+gw*ccf1(ic0)
      versch=versch+(ccf(ic0)-ccf1(ic0))**2/DENOM
  13  CONTINUE
   7  CONTINUE                                                          
      IREF=1493                                                         
      LSTART=9                                                          
   1  CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE INUIT(flname)                                             
C     ISW=1 OUTPUT  ISW=2 INPUT  C-COEFF. PAAR EN ONPAAR                
      COMMON /TEKEN/ TITEL(10)                                          
      COMMON /BIB1/ LMAX,IMAG,IDM,IDN,ncoif,lstart,ml(16)
      COMMON /CCOEF/ C1(746),C2(746),C010,C3(746),C4(746)               
      character*12 flname
      GOTO 20
C      GOTO (10,20,10),ISW
  10  CONTINUE
C     write(*,200)
 200  format(' Give Filename:')
C      read(*,99) flname
  99  format(a)
      OPEN (UNIT=12,FILE=flname,FORM='UNFORMATTED')
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
  20  CONTINUE
C     write(*,200)
C     read(*,99) flname
      OPEN (UNIT=11,FILE=flname,STATUS='OLD',FORM='UNFORMATTED')
      READ (11) TITEL,LMAX,IMAG,IDM,IDN,JSNIJ,IDIM1,IDIM3               
C      WRITE (*,100) TITEL,LMAX,IMAG,IDM,IDN                             
 100  CONTINUE
C     FORMAT('0INUIT-INPUT TEXTUUR=',10A4,/,' LMAX=',I5,            
C     1'  IMAG=',I5,'  IDM=',I5,'  IDN=',I5)                             
      READ (11) C1                                                      
      READ (11) C2                                                      
      C010=1.                                                           
      READ (11,END=14) C010                                             
      READ (11,END=14) C3                                               
      READ (11) C4                                                      
      READ (11,END=12) X                                                
  12  GOTO 15
  14  WRITE (*,101)                                                     
 101  FORMAT ('0NO ODD COEFFICIENTS ON INPUT FILE')                     
      DO 11 I=1,746                                                     
      C3(I)=0.                                                          
      C4(I)=0.                                                          
  11  CONTINUE                                                          
  15  if (IMAG.eq.1) then
         do i=1,746
           C2(i)=0.0
           C4(i)=0.0
         enddo
      endif
  16  CLOSE (UNIT=11)
      RETURN                                                            
      END                                                               
