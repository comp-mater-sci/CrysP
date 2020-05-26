c     ODFLAM - PROGRAMMA OM UIT EEN ODF EEN STEL IDEALE ORIENTATIES TE
C     HALEN, BRUIKBAAR VOOR DE TAYLOR-SOFTWARE.                         
C     OUTPUTBESTAND UNIT=9 = GESCHIKT VOOR FORTRANVERSIE
C     OUTPUTBESTAND UNIT=10 = GESCHIKT VOOR PASCALVERSIE
      common /groot2/ V(19,19,73),IRAND(12996)
      dimension arfi1(12996),arfi2(12996),arphi(12996),FF(-3:3)
      DIMENSION TITEL(10),S(0:37),RAND(12996),YINT(0:20)
C     Raam(SIGMA,PHI,part of FI2-range)
      common /groot1/ raam(-2:146,-2:38,4)
      integer * 2 ZAADJE
      logical strange,geval1,geval2
      EQUIVALENCE (RAND(1),V(1,1,1),arfi1(1))
      EQUIVALENCE (V(1,1,37),arfi2(1))
      EQUIVALENCE (Raam(-2,-2,1),arphi(1))
      DATA FPI /0.174533E-1/,ZAADJE/80/,Z/1.0/,MRAND/12996/
      do layer=1,4
         do KPHI=-2,38
            do KSIGMA=-2,146
               raam(KSIGMA,KPHI,layer)=0.0
            enddo
         enddo
      enddo
      call seed(ZAADJE)
C     UNIT 8 = Parameter file with NUMBER of Selectors
      OPEN (UNIT=8,FILE=' ',status='old')
C     UNIT 6 = PRINTER
      open (unit=6,file=' ',mode='write')
C     UNIT 7 = Temporary file
      open (unit=7,file='temp.odf',form='unformatted')
C     UNIT 9 = FORTRAN-TAYLOR OUTPUTTEXTURE
      OPEN (UNIT=9,FILE=' ')
C     UNIT 10 = PASCAL-TAYLOR OUTPUTTEXTURE
      OPEN (UNIT=10,FILE=' ')
C     UNIT 26 = O.D.F. IN DISCRETE FORM (ON GRID IN EULER SPACE)
      open (unit=26,file=' ',status='old')
      READ(8,98) N
  98  FORMAT(I5)
      write (6,106) n
  106 format (' Number of discrete orientations to be generated:',i5)
      if (n.gt.MRAND) then
         write (*,110) MRAND
         stop
      endif
 110  format (' Should not be larger than',I5)
      do i=1,N
         CALL RANDOM(RND)
C         rnd=i
         rand(i)=RND
         IRAND(i)=i
      enddo
      call sorteer(N,IRAND,rand)
C
C     Reading of ODF
C
      read (26,196) Titel
 196  format (10A4)
      write (*,109) titel
      write (6,109) titel
 109  format(' Title of ODF: ',10a4)
      read (26,198) step
 198  format(f10.0)
      read (26,194) nfi1
 194  format(i5)
      read (26,198) fi10
      read (26,194) nfi2
      read (26,198) fi20
      read (26,194) nphi
      read (26,198) phi0
      FMAX=0.0
      FMIN=0.0
      do 112 iphi=1,nphi
      do 111 ifi2=1,nfi2
      read (26,195) (v(iphi,ifi2,ifi1),ifi1=1,nfi1)
 195  format(8f9.3)
      do ifi1=1,nfi1
         x=v(iphi,ifi2,ifi1)
         if (x.gt.FMAX) FMAX=X
         if (x.lt.FMIN) FMIN=X
      enddo
 111  continue
 112  continue
      close (unit=26)
C     Find the cut-off level
      if (FMIN.eq.0.0) goto 5
      HSTEP=-0.1*FMIN
      S(0)=-1.0
      S(19)=0.0
      do iphi=1,18
         phi=(5*iphi-2.5)*FPI
         S(iphi)=-cos(phi)
      enddo
      Y=0.0
      do i=0,20
         YINT(i)=0.0
      enddo
      do iphi=1,19
         wtphi=S(iphi)-S(iphi-1)
         do ifi2=1,nfi2-1
            do ifi1=1,nfi1
               wtfi1=wtphi
               if (ifi1.eq.1.or.ifi1.eq.nfi1) wtfi1=0.5*wtfi1
               x=v(iphi,ifi2,ifi1)
               Y=Y+x*wtfi1
               do 6 i=0,20
                  dremp=HSTEP*i
                  if (x.gt.dremp) goto 6
                  YINT(i)=YINT(i)+x*wtfi1
  6            continue
            enddo
         enddo
      enddo
      do i=1,20
         if (YINT(i).gt.0.0) goto 7
      enddo
      i=21
C     Interpolation to find treshold
      dremp=HSTEP*20
      if (Y.eq.YINT(i-1)) then
            FMIN=dremp+(FMAX-dremp)*YINT(i-1)/(Y-YINT(i-1))
         else
            FMIN=dremp
      endif
      goto 8
   7  dremp=HSTEP*(i-1)
      if (YINT(i).eq.YINT(i-1)) then
            FMIN=dremp+HSTEP*YINT(i-1)/(YINT(i)-YINT(i-1))
         else
            FMIN=dremp
      endif
   8  write (6,357) i,FMIN,FMAX
      write (*,357) i,FMIN,FMAX
 357  format (i3,' Treshold=',e15.4,'    Max. of ODF=',e15.4)
   5  mfi2=(nfi2-1)*5
      write (6,350) mfi2
 350  format (' Mfi2',I5)
      ifi10=0
      fi1lim=nfi1-1
      fi2lim=(nfi2-1)/2
      IFI2L=fi2lim+1
      IDM=72/(nfi2-1)
      IMAG=2
      IDN=1
      if (nfi1.eq.73) goto 1
      if (fi10.lt.0.0) goto 2
      IDN=2
      if (nfi1.eq.37) goto 1
   2  IMAG=1
   1  strange=imag.eq.1.and.idn.eq.1
      if (strange) ifi10=-18
      IIDN=360/IDN
      mfi1=nfi1+ifi10
      write (6,351) IDN,IMAG,IIDN
 351  format (' IDN, IMAG=',2I5,/,' IIDN',I5)







      S(0)=-1.0
      S(37)=0.0
      do iphi=1,36
         phi=(2.5*iphi-1.25)*FPI
         S(iphi)=-cos(phi)
      enddo
      Y=0.0

      do LLFI2=-5,95,5
C        Let the window shift upwards over 1 position
         if (LLFI2.eq.-5) goto 3
         do layer=2,4
            i=layer-1
            do KPHI=-2,38
               do KSIGMA=-2,146
                  raam(KSIGMA,KPHI,i)=raam(KSIGMA,KPHI,layer)
               enddo
            enddo
         enddo
C        Generate a new layer FI2=constant from the ODF
  3      do LLPHI=-5,95,5
            KPHI=LLPHI*2/5
            do LSIGMA=-5,365,5
               KSIGMA=LSIGMA*2/5
               LFI2=LLFI2
               LPHI=LLPHI
               LFI1=LSIGMA-LLFI2
               if (LLPHI.eq.-5) then
                  LPHI=5
                  LFI1=LFI1+180
                  LFI2=LFI2+180
               endif
               if (LLPHI.eq.95) then
                  LPHI=180-LLPHI
                  LFI1=-LFI1
                  LFI2=LFI2+180
               endif
               DO WHILE (LFI1.lt.0)
                  LFI1=LFI1+IIDN
               enddo
               DO WHILE (LFI1.ge.IIDN)
                  LFI1=LFI1-IIDN
               enddo
               geval1=LFI1.gt.90.and.mfi1.eq.19
               geval2=strange.and.(LFI1.gt.90.and.LFI1.lt.270)
               if (geval1.or.geval2) then
                  LFI1=180-LFI1
                  LFI2=180-LFI2
               endif
               DO WHILE (LFI2.lt.0)
                  LFI2=LFI2+mfi2
               enddo
               DO WHILE (LFI2.ge.mfi2)
                  LFI2=LFI2-mfi2
               enddo
               if (strange.and.LFI1.gt.90) LFI1=LFI1-360
               JFI1=1+LFI1/5-IFI10
               JFI2=1+LFI2/5
               JPHI=1+LPHI/5
               Raam(KSIGMA,KPHI,4)=v(JPHI,JFI2,JFI1)
            enddo
         enddo
C        Adding values by cubic interpolation
C        first in the direction PHI
         do KSIGMA=-2,146,2
            do KPHI=1,35,2
               do i=-3,3,2
                 FF(i)=Raam(KSIGMA,(KPHI+i),4)
               enddo
               AFP1=(FF(3)-FF(-1))/4.0
               AFM1=(FF(1)-FF(-3))/4.0
               Raam(KSIGMA,KPHI,4)=(FF(1)+FF(-1)-AFP1+AFM1)*0.5
            enddo
         enddo
C        then in the direction SIGMA
         do KPHI=-2,38
            do KSIGMA=1,143,2
               do i=-3,3,2
                 FF(i)=Raam((KSIGMA+i),KPHI,4)
               enddo
               AFP1=(FF(3)-FF(-1))/4.0
               AFM1=(FF(1)-FF(-3))/4.0
               Raam(KSIGMA,KPHI,4)=(FF(1)+FF(-1)-AFP1+AFM1)*0.5
            enddo
         enddo
C        then again in the direction PHI, to improve the
C        interpolation in the centres of squares by basing them
C        on estimates obtained in 2 dimensions
         do KSIGMA=1,143,2
            do KPHI=1,35,2
               do i=-3,3,2
                 FF(i)=Raam(KSIGMA,(KPHI+i),4)
               enddo
               AFP1=(FF(3)-FF(-1))/4.0
               AFM1=(FF(1)-FF(-3))/4.0
               X=(FF(1)+FF(-1)-AFP1+AFM1)*0.5
               Raam(KSIGMA,KPHI,4)=(X+Raam(KSIGMA,KPHI,4))*0.5
            enddo
         enddo
         if (LLFI2.lt.10) goto 4
C        Writing out of the layer nr.2
         FI2=LLFI2-10
C 150     format ('FI2= ',F10.1,5x,' PHI=',F10.1,5x,e15.8)
         do KPHI=0,36
            wtphi=S(KPHI+1)-S(KPHI)
            PHI=KPHI*2.5
C            write (6,150) FI2,PHI,FMIN
C            write (6,302) (Raam(KSIGMA,KPHI,2),KSIGMA=0,143)
            write (7) (Raam(KSIGMA,KPHI,2),KSIGMA=0,143)
            do KSIGMA=0,143
               x=Raam(KSIGMA,KPHI,2)
               if (x.gt.FMIN) Y=Y+x*wtphi
            enddo
         enddo
C        Interpolation of a layer for intermediate value of FI2
         FI2=LLFI2-7.5
         do KPHI=0,36
            wtphi=S(KPHI+1)-S(KPHI)
            PHI=KPHI*2.5
C            write (6,150) FI2,PHI
            do KSIGMA=0,143
               FI1=KSIGMA-FI2
               j=0
               do i=-3,3,2
                 j=j+1
                 FF(i)=Raam(KSIGMA,KPHI,j)
               enddo
               AFP1=(FF(3)-FF(-1))/4.0
               AFM1=(FF(1)-FF(-3))/4.0
C              De tussenliggende waarde is X :
               X=(FF(1)+FF(-1)-AFP1+AFM1)*0.5
               Raam(KSIGMA,KPHI,1)=X
               if (X.gt.FMIN) Y=Y+x*wtphi
            enddo
C            write (6,302) (Raam(KSIGMA,KPHI,1),KSIGMA=0,143)
            write (7) (Raam(KSIGMA,KPHI,1),KSIGMA=0,143)
 302        format (10F10.5)
         enddo
  4      continue
      enddo
      WRITE (6,102) Y
  102 FORMAT(' TOTAL INTEGRAL=',E15.8)
      REWIND 7

C
C     berekenen VAN N GETALLEN tussen 0 en Y, IN VOLGORDE GESORTEERD.
C                                                                       
      denom=Y/float(n)
C      write (*,400) denom
C 400  format (' denom,',e15.8)
      RND=0.5*denom
C      do 35 i=1,n
C      rand(i)=denom*float(i)-denom2
C  35  continue
C  97  FORMAT(8F10.8)
C
C     UITKIEZEN VAN MAX. N DISKRETE ORIENTATIES OP BASIS VAN DE N       
C     GETALLEN "Selectors".                                                  
C                                                                       
      Z=1.0
      NK=0.
      Y=0.
      I=1


      do IFI2=0,35
C        Reading out of a layer
         FI2=IFI2*2.5
         do KPHI=0,36
            wtphi=S(KPHI+1)-S(KPHI)
            PHI=KPHI*2.5
            read (7) (Raam(KSIGMA,KPHI,4),KSIGMA=0,143)
            do KSIGMA=0,143
               FI1=KSIGMA*2.5-FI2
               x=Raam(KSIGMA,KPHI,4)
               if (X.gt.FMIN) Y=Y+x*wtphi
               IF (RND.GT.Y) GOTO 30
               NK=NK+1
               IF (NK.GT.N) GOTO 33
               J=IRAND(NK)
               arfi2(j)=FI2
               arfi1(j)=FI1
               arphi(j)=PHI
               RND=RND+denom
  30           continue
            enddo
         enddo
      enddo

 103  FORMAT(3F10.3,14X,'1',5X,F10.1)
 203  FORMAT(1H ,3F10.3,14X,'1',5X,F10.1)
 104  FORMAT(I5,5x,10A4)
  33  WRITE (6,204) NK,TITEL
 204  FORMAT(1H ,I5,' ORIENTATIONS - ',10A4)
      WRITE (10,300) NK
      write (9,104) NK,TITEL
 300  FORMAT(I5,4(' ',F12.5))                                           
      DO 34 I=1,NK                                                      
C      READ (7,105) FI2,PHI,FI1,Z
      FI2=arfi2(I)
      FI1=arfi1(I)
      PHI=arphi(I)
 105  FORMAT (3F10.3,20X,F10.1)                                         
      WRITE (10,300) I,Z,FI1,PHI,FI2
      write (9,103) FI2,PHI,FI1,Z
  34  CONTINUE                                                          
      STOP                                                              
      END                                                               
      subroutine sorteer(N,IG,G)
      dimension IG(N),G(N)
      do i=N,2,-1
         x=-1.0e20
         k=0
         do j=1,i
            if (G(j).gt.x) then
               x=G(j)
               k=j
            endif
         enddo
         j=IG(k)
         G(k)=G(i)
         IG(k)=IG(i)
         G(i)=x
         IG(i)=j
      enddo
      return
      end
