C      FCURODF- TO CONVERT THE RESULT OF A TAYLOR SIMULATION
C              STORED IN A 'CUR'-FILE (SIMTEX SYSTEM)
C              INTO A SET OF C-COEFFICIENTS.
C      FASTER BUT LESS PRECISE THAN CURODF
       COMMON /HUGE27/ TITL(10),V1(19,730)
       COMMON /HUGE28/ V2(19,730),V0(2,2,2)
       COMMON /PARAS/AMFI2,HMFI2
       COMMON /BIB1/LMAX,IMAG,IDM,IDN,NCOIF,LSTART,ML(16)
      integer fi2lim
       DATA A5/5.0/,ISTP/1/
C     UNIT 5 = INPUT CARDS
      open (unit=5,file=' ',mode='read',status='old')
C     UNIT 6 = PRINTER
      open (unit=6,file=' ',mode='write')
C     UNIT 7 = INPUT TEXTURE
      OPEN (UNIT=7,FILE=' ',mode='read',status='old')
C     UNIT 25 = LIBRARY FILE
      open (unit=25,file=' ',status='old',form='unformatted',
     1 blocksize=20480)
C     UNIT 26 = LIBRARY FILE
      open (unit=26,file=' ',status='old',form='unformatted',
     1 blocksize=20480)
C     AN OPEN COMMAND WILL HEREAFTER BE ISSUED BY INUIT FOR A
C     C-COEFFICIENT OUTPUT FILE ON UNIT 12.
       READ (5,99) IMAG,idn,idm,IPR
       write (6,100) imag,idn,idm,IPR
  100  format (' imag=',i5,'   idn=',i5,'  idm=',i5,'  IPR=',i5)
   99  FORMAT (4I5)
       ifim=19
       ifi1m=73
       AMFI2=360.0/float(IDM)
       HMFI2=0.5*AMFI2
       FI2LIM=36/idm
       IFI2M=FI2LIM+1
       IFI2L=IFI2M
       if (idn.eq.2) ifi1m=37
       if (imag.eq.2) IFI2M=2*FI2LIM+1
       nrun=1
       read (5,99) nrun
       READ(7,98) (TITL(i),i=1,10)
   98  FORMAT(10A4)
       write (*,101) titl
       write (6,101) titl
  101  format (' title on input file=',10a4)
       DO 1 IRUN=1,NRUN
       read (5,99) NSTP
       write (*,114) irun,nstp
  114  format(' SMTODF - RUN NR.',i5,'   INPUT BLOCK NR.',i5)
       if (NSTP.GE.ISTP) goto 4
       write (*,120) ISTP
  120  format (' Out of order - NSTP should not be smaller than',i5)
       stop
    4  do 9 iphi=1,ifim
       do 9 i=1,730
       v1(iphi,i)=0.0
       v2(iphi,i)=0.0
    9  continue
       do 5 JSTP=ISTP,NSTP
       read (7,98) DOMME
       read (7,98) DOMME
       READ(7,96) NRECO,NPNT
   96  format(I6,5x,I5)
       if (JSTP.NE.NSTP) goto 3
       write (*,102) NRECO,npnt
       write (6,102) NRECO,npnt
  102  FORMAT (' SEQUENCE NUMBER OF SIMULATION STEP',I6,
     1' NUMBER OF CRYSTALLITES=',I5)
    3  read (7,98) DOMME
       DO 2 IPNT=1,NPNT
       READ (7,97) GW,F1,P,F2
   97  FORMAT(6X,f10.5,2X,3f10.5)
       if (JSTP.ne.NSTP) goto 2
       fi2=f2
       phi=p
       fi1=f1
       call symmetr(fi1,phi,fi2)
       ifi1=fi1/a5
       ifi1=ifi1+1
       if (ifi1.eq.ifi1m) ifi1=ifi1-1
       if (ifi1.lt.0.or.ifi1.gt.ifi1m) goto 10
       ifi2=fi2/a5
       ifi2=ifi2+1
       if (ifi2.eq.ifi2m) ifi2=ifi2-1
       if (ifi2.lt.0.or.ifi2.gt.ifi2m) goto 10
       iphi=phi/a5
       iphi=iphi+1
       if (iphi.eq.ifim) iphi=iphi-1
       if (iphi.ge.0.and.iphi.le.ifim) goto 11
   10  write (6,110) f1,p,f2,fi1,phi,fi2
  110  format (3f12.3,/,3f12.3,/,' format 110 in fcurodf')
       stop
C      DISTRIBUTION OF THE WEIGHT OF A CRYSTALLITE OVER THE 8 NODAL
C      POINTS AROUND ITS ORIENTATION (EULER SPACE).
   11  f1=fi1-a5*float(ifi1-1)
       f2=fi2-a5*float(ifi2-1)
       p=phi-a5*float(iphi-1)
       do 12 i=1,2
       do 12 i2=1,2
       do 12 i1=1,2
       v0(i1,i2,i)=gw
   12  continue
       call red1(v0,(a5-f1)/a5,1)
       call red1(v0,f1/a5,2)
       call red2(v0,(a5-f2)/a5,1)
       call red2(v0,f2/a5,2)
       call red(v0,(a5-p)/a5,1)
       call red(v0,p/a5,2)
C      write (6,111) v0
C 111  format (8f10.3)
       do 13 i=1,2
       jphi=iphi+i-1
       do 13 i2=1,2
       jfi2=ifi2+i2-1
       do 13 i1=1,2
       jfi1=ifi1+i1-1
       if (jfi2.gt.IFI2L) goto 14
       iv1=IFI2L*(jfi1-1)+jfi2
       v1(jphi,iv1)=v1(jphi,iv1)+v0(i1,i2,i)
       goto 13
   14  iv1=IFI2L*(jfi1-1)+jfi2-fi2lim
       v2(jphi,iv1)=v2(jphi,iv1)+v0(i1,i2,i)
   13  continue
    2  CONTINUE
    5  continue
       ISTP=NSTP+1
C      do 19 iphi=1,ifim
C      do 19 ifi2=1,ifim
C      write(6,112) iphi,ifi2,(v1(ifi1,ifi2,iphi),ifi1=1,19)
C 112  format (//2I5/19f6.3)
C  19  continue
C       write(*,115)
C  115  format(' SUBPROGRAM GAUSS IS CALLED')
       call gauss(IPR)
C       write (*,121)
C  121  format (' tot hier')
       CALL INUIT(1)
C       write (*,122)
C  122  format (' Ook tot hier')
       rewind 25
       rewind 26
    1  CONTINUE 
       STOP
       END
      subroutine symmetr(fi1,phi,fi2)
C     IN THIS SUBROUTINE: ANGLES IN DEGREES
      COMMON /BIB1/LMAX,IMAG,IDM,IDN,NCOIF,LSTART,ML(16)
      COMMON /PARAS/AMFI2,HMFI2
      data a90/90.0/,a180/180.0/,a360/360.0/
  10  if (phi.le.a180) goto 11
      phi=phi-a360
      goto 10
  11  if (phi.ge.(-a180)) goto 12
      phi=phi+a360
      goto 11
  12  if (phi.ge.0.0) goto 2
C     PHI NEGATIVE
      phi=-phi
      fi1=fi1+a180
      fi2=fi2-a180
   2  if (phi.le.a90) goto 1
c     PHI LARGER THAN 90ø - rotation around x2B
      phi=a180-phi
      fi1=fi1+a180
      fi2=-fi2
   1  if (fi2.ge.0.0) goto 3
C     FI2 NEGATIVE
      fi2=fi2+AMFI2
      goto 1
   3  if (fi2.le.AMFI2) goto 4
C     FI2 GREATER THAN AMFI2
      fi2=fi2-AMFI2
      goto 3
   4  if (fi1.ge.0.0) goto 5
C     FI1 NEGATIVE 
      fi1=fi1+a360
      goto 4
   5  if (fi1.lt.a360) goto 6
C     FI1 LARGER THAN 360
      fi1=fi1-a360
      goto 5
   6  if (imag.eq.2) goto 7
      if (fi2.le.HMFI2) goto 7
      fi2=AMFI2-fi2
      fi1=a180-fi1
      if (fi1.lt.0.0) fi1=fi1+a360
   7  if (idn.eq.1) return
      if (fi1.ge.a180) fi1=fi1-a180
      return
      end
      subroutine red1(v0,x,i1)
      dimension v0(2,2,2)
      do 1 i=1,2
      do 1 i2=1,2
      v0(i1,i2,i)=v0(i1,i2,i)*x
   1  continue
      return
      end
      subroutine red2(v0,x,i2)
      dimension v0(2,2,2)
      do 1 i=1,2
      do 1 i1=1,2
      v0(i1,i2,i)=v0(i1,i2,i)*x
   1  continue
      return
      end
      subroutine red(v0,x,i)
      dimension v0(2,2,2)
      do 1 i1=1,2
      do 1 i2=1,2
      v0(i1,i2,i)=v0(i1,i2,i)*x
   1  continue
      return
      end
      SUBROUTINE Gauss(IPR)
C     Calculates C-coefficients of a series of Gauss-distributions
C     on a 5ø grid in Euler space
      COMMON /HUGE27/ TITL(10),V1(19,730)
      COMMON /HUGE28/ V2(19,730),V0(2,2,2)
      COMMON /BIB1/ LMAX,IMAG,IDM,IDN,NCOIF,LSTART,ML(16)                   
      COMMON /CCOEF/ CCOF(2985)                                         
      COMMON /TEKEN/ TITEL(10)                                          
      DATA KLMX,FPI /4HLMAX,1.745329E-2/                                
      READ (5,99) KODE,LMAX,FHI0
  99  FORMAT (A4,I6,2F10.0)                                             
      IF (KODE.EQ.KLMX) GOTO 1                                          
      WRITE (6,100) KODE                                                
 100  FORMAT ('0GAUSS - KODE SHOULD BE LMAX BUT WAS ',A4)               
      STOP                                                              
   1  READ (5,98) TITEL                                                 
  98  FORMAT (10A4)                                                     
      WRITE (6,103) TITEL                                               
 103  FORMAT ('1SUBROUTINE GAUSS',/,1H0,10A4)                           
  20  IF (FHI0.EQ.0.) GOTO 14                                           
      EFI0=-(FPI*FHI0)**2/4.                                            
      FS=1./(1.-EXP(EFI0))                                              
      IF (FHI0.NE.0.) WRITE (6,104) FHI0                                
 104  FORMAT (' UITSMERING - STANDAARDAFWIJKING =',F10.4,'  (GRADEN)',/)
  14  CALL SUMODF(1,V1,V2,25,CCOF,IPR,EFI0,FHI0,FS)
      CALL SUMODF(1,V1,V2,26,CCOF,IPR,EFI0,FHI0,FS)
      RETURN                                                            
      END                                                               
      SUBROUTINE SUMODF(NORM,V1,V2,NUNIT,CCF,IPR,EFI0,FHI0,FS)     
C     Sommeren VAN EEN ODF.
C
C     Let FI2MAX=360ø/IDM (i.e. 90ø for cubic metals)
C     Let FI2LIM=FI2MAX/2
C
C     Euler space is contained in 2 arrays:
C     v1(ph,fi2,phi1) ph:  0..90ø
C                     fi2: 0..FI2LIM
C                     fi1: 0..360ø/IDN
C     v2(ph,fi2,phi1) ph:  0..90ø
C                     fi2: FI2LIM..FI2MAX
C                     fi1: 0..360ø/IDN
C     v2 is in fact redundant if IMAG=1
C
C     for technical reasons, the indices fi2 and phi1 are contracted
C     into a single index "iv"
C
      COMMON /BIB1/ LMAX,IMAG,IDM,IDN,NCOIF,LSTART,ML(16)
      DIMENSION CCF(2985),FT1(746),FT2(746),vv1(0:36),vv2(0:36)
      dimension V1(19,730),V2(19,730)
      common /gonfu/ cosn(32,0:36),sinn(32,0:36),CX,TX,YN
      complex CX,TX,YN(0:32)
      logical gonio,LEVEN
      integer fi2lim
      data gonio/.false./,pi/ 3.14159/,W05/0.707107/
      if (gonio) goto 50
      do 51 ifi1=0,36
      do 51 n=1,32
      x=float(ifi1*n*5)*pi/180.0
      cosn(n,ifi1)=cos(x)
      sinn(n,ifi1)=sin(x)
  51  continue
      gonio=.true.
  50  fi2lim=36/idm
      IFI2L=fi2lim+1
      JMAX=19*IFI2L*(72/idn+1)
C
C     Calculation of constant factor
C     The 2.0 in front is due to the fact, that
C     the weighing factors that come from CREATE5 are only half
C     of what they should be.
C
      H=2.0*((5.0/180.0)**3)*pi*8.0/float(IMAG*IDM)
      IREF=0
      READ (NUNIT) IMAX,LMX,IMG,IDM,IDNN,NCOIF,LSTART,ML
      WRITE (6,100) LMX,IMG,IDM,IDNN,NCOIF,LSTART,IMAX
 100  FORMAT ('0INTODF-LMX=',I5,'   IMG=',I5,'  IDM=',I5,               
     1 '   IDN=',I5,'   NCOIF=',I5,'   LSTART=',I5,' IMAX=',I7)         
      if (LMAX.LE.LMX) goto 1
      write (6,101) LMX
 101  format (' Warning- LMAX is reduced to',i3)
      LMAX=LMX
   1  LEVEN=((LSTART/2)*2).eq.LSTART
      IF (.not.LEVEN) IREF=1493
      C010=0.0
      DO 2 IC=1,1492                                                    
      IC0=IC+IREF                                                       
   2  CCF(IC0)=0.                                                       
      DO 4 IPHI=1,19
      do 4 ifi2=0,2*fi2lim
      jfi2=ifi2+1
      kfi2=ifi2-fi2lim+1
C      write (6,200) iphi,ifi2
C 200  format (' iphi,ifi2',2i5)
      READ (NUNIT) GEW,(FT1(J),J=1,NCOIF) 
      READ (NUNIT) (FT2(J),J=1,NCOIF)
      if (ifi2.gt.fi2lim) goto 71
      do 72 ifi1=0,36
      iv1=IFI2L*ifi1+jfi2
      x1=v1(iphi,iv1)
      if (IDN.eq.1) goto 73
      vv1(ifi1)=2.0*x1
      goto 72
  73  iv2=IFI2L*(ifi1+36)+jfi2
      x2=v1(iphi,iv2)
      vv1(ifi1)=(x1+x2)
      vv2(ifi1)=(x1-x2)
  72  continue
      goto 74
  71  if (imag.eq.1) goto 4
      do 82 ifi1=0,36
      iv1=IFI2L*ifi1+kfi2
      x1=v2(iphi,iv1)
      if (IDN.eq.1) goto 83
      vv1(ifi1)=2.0*x1
      goto 82
  83  iv2=IFI2L*(ifi1+36)+kfi2
      x2=v2(iphi,iv2)
      vv1(ifi1)=(x1+x2)
      vv2(ifi1)=(x1-x2)
  82  continue
  74  f1=0.0
      do 75 ifi1=0,36
      f1=f1+vv1(ifi1)
  75  continue
      YN(0)=CMPLX(f1,0.0)
      do 76 n=2,lmax,2
      f1=0.0
      f2=0.0
      do 77 ifi1=0,36
      x=vv1(ifi1)
      if (x.eq.0.0) goto 77
      f1=f1+x*cosn(n,ifi1)
      f2=f2-x*sinn(n,ifi1)
  77  continue
      YN(n)=CMPLX(f1,f2)
  76  continue
      if (idn.eq.2) goto 78
      do 79 n=1,lmax,2
      f1=0.0
      f2=0.0
      do 80 ifi1=0,36
      x=vv2(ifi1)
      if (x.eq.0.0) goto 80
      f1=f1+x*cosn(n,ifi1)
      f2=f2-x*sinn(n,ifi1)
  80  continue
      YN(n)=CMPLX(f1,f2)
  79  continue
  78  IC=0    
      ic0=IREF
      C010=C010+REAL(YN(0))
      DO 5 L=LSTART,LMAX,2                                              
      LL=(L-LSTART)/2+1                                                 
      MUM=ML(LL)                                                        
      IF (MUM.EQ.0) GOTO 5                                              
      DO 6 N=0,L
      DO 6 MU=1,MUM                                                     
      IC=IC+1                                                           
      if ((n/idn)*idn.ne.n) goto 6
      IC0=IC0+1
      TX=CMPLX(FT1(IC),-FT2(IC))
C      if (n.eq.0) write (6,203) l,mu,ft1(ic),ft2(ic)
C 203  format (2i5,2e20.8)
      CX=YN(N)*TX
      x1=REAL(CX)
      CCF(IC0)=CCF(IC0)+x1
      if (imag.eq.1) goto 6
      x2=AIMAG(CX)
      CCF(IC0+746)=CCF(IC0+746)+x2
   6  CONTINUE
   5  CONTINUE
   4  CONTINUE
      fak1=H
      F2=0.0
      C010=C010*H
      IF (NORM.NE.1) GOTO 12                                            
      FAK1=FAK1/C010                                                    
      C010=1.                                                           
  12  IF (IPR.EQ.1) WRITE (6,102) C010                                  
 102  FORMAT ('0C010=',F12.5)                                           
      IC=0                                                              
      DO 7 L=LSTART,LMAX,2                                              
      LL=(L-LSTART)/2+1                                                 
      L1=L+1                                                            
      TL1=2*L+1                                                         
      IF (FHI0.EQ.0.0) GOTO 15                                          
      XL=L**2                                                           
      XL1=L1**2                                                         
      TL1=FS*(EXP(EFI0*XL)-EXP(EFI0*XL1))                               
  15  MUM=ML(LL)                                                        
      IF (MUM.EQ.0) GOTO 7                                              
      DO 13 NN=1,L1,IDN                                                 
      N=NN-1                                                            
      DO 13 MU=1,MUM                                                    
      IC=IC+1                                                           
      IC0=IC+IREF                                                       
      F1=CCF(IC0)*TL1*FAK1                                              
      CCF(IC0)=F1                                                       
      IF (IMAG.NE.2) GOTO 8                                             
      IC0=IC0+746                                                       
      F2=CCF(IC0)*TL1*FAK1                                              
      CCF(IC0)=F2                                                       
   8  IF (IPR.EQ.1) WRITE (6,103) IC,L,N,MU,F1,F2                       
 103  FORMAT (1H ,I5,'   L=',I5,'   N=',I5,'   MU=',I5,                 
     1  '   C1=',E15.8,'    C2=',E15.8)                                 
  13  CONTINUE                                                          
   7  CONTINUE                                                          
      if (LEVEN) CCF(1493)=C010
      RETURN                                                            
      END    
