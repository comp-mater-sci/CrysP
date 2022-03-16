c     LAMELSET -To make set of randomly oriented lamellae.
C     OUTPUTBESTAND UNIT=9 = GESCHIKT VOOR FORTRANVERSIE
      integer * 2 ZAADJE
      DATA FPI /0.174533E-1/,ZAADJE/80/,Z/1.0/
      call seed(ZAADJE)
C     UNIT 8 = Parameter file with TITLE and NUMBER of Selectors
      OPEN (UNIT=8,FILE=' ',status='old')
C     UNIT 9 = FORTRAN-TAYLOR OUTPUTTEXTURE
      OPEN (UNIT=9,FILE=' ')
      read (8,196) Titel
 196  format (10A4)
      write (*,109) titel
 109  format(' Title of File: ',10a4)
      READ(8,98) NK
  98  FORMAT(I5)
      write (*,106) NK
  106 format (' Number of lamellae to be generated:',i5)
  97  FORMAT(8F10.8)
C                                                                       
C     UITKIEZEN VAN NK DISKRETE ORIENTATIES OP BASIS VAN DE N
C     GETALLEN "Selectors".                                                  
C                                                                       
 104  FORMAT(I5,5x,10A4)
      write (9,104) NK,TITEL
      do 34 I=1,NK
      CALL RANDOM(RND)
      FI2=RND*360.0
      CALL RANDOM(RND)
      FI1=RND*360.0
      CALL RANDOM(RND)
      X=1.0-2.0*RND
      if (X.gt.1.0) x=1.0
      if (X.lt.-1.0) X=-1.0
      PHI=ACOS(X)/FPI
      write (9,103) FI2,PHI,FI1,Z
 103  FORMAT(3F10.3,14X,'1',5X,F10.1)
  34  CONTINUE
      STOP                                                              
      END                                                               
