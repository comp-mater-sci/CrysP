C ALAMEL V3
C THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
C WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY
C  All comments about modifications of the source have been removed
C for clarity 
C See "annotated source codes" if you need these
C
C     When you set KOST=1, then 2 things will happen:
C     1) Not the value 1.0, but the values in the input data set will be used
C        for the critical resolved shear stresse
C     2) They will be multiplied with TAU, calculated from GAMMA
C        using the FTAU function.
C
      SUBROUTINE SIMUL(IW,EPS,NFILE0,NUNIT)
C     TO ORGANIZE SIMULATIONS OF DEFORMATION TEXTURES
C     USING THE ALAMEL MODEL
      implicit double precision (a-h,o-z)
C
C     IW=2 is meant for outputting the final texture.
C

      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1,NDAT2
      COMMON /ES1/ IMP3
      COMMON /IGLIJS/ FK1(2,96),NUNGL,M11,CC(2,96)
      COMMON /DOUBLE/ XM(5,96),XEPS(5),DELTAT,RHO(5),B5(5)
      COMMON /STAP/ SG,GMM                                              
      COMMON /TEXTUR/ TRF(3,3),C1(3,3),C2(3,3),WDOT,ROTM,NO,DG(3,3),
     1ITW,IPR,DELTAW,GEWF,NLIST
      COMMON /SYMP/ INV,ISP,LOM,KSYM,KTYP,NPOINT,TEN(3,3),TOTGEW        
      COMMON /EULERA/ fi1,PHI,fi2
      COMMON /GENRLX/ YY(5,5),SHsam(3,3),Ssam(3,3),SPANV(5),RHOSsa(3,3),
     1 WR,WRTOT,SWRLX(3)
      COMMON /NRSTEP/ nrstep
      COMMON /LAMEL/ laml,fi10b(2),phi0b(2),fi20b(2),TRFb(3,3,2),
     1 gewfb(2),GMMAb(2),Fb(3,3,2),GAXESb(3,2),GEULRb(3,2),
     2 CIJb(3,3,2),TGb(3,3,2),RHOSSb(3,3,2),
     3 fi1b(2),phib(2),fi2b(2),
     4 fk1b(2,96,2),NGR,NRL
      common /CEIGEN/ IOR,ISTP,NBLOC,CMICRO(3,3)
      DIMENSION F(3,3),F1(3,3),GAXES(3),GEULR(3),TG(3,3),
     1 CIJ(3,3),F2(3,3),GLR(3),STOT(3,3),BUFSPV(5),
     2 RHOST(3,3),RHOSm(3,3),FMicro(3,3),Ftot(3,3),
     3 L1MINV(3),L2MINV(3),CMic0(3,3),FTINV(3,3)
      dimension FS(3,3)
      character*40 TITEL
      logical SWRLX
      DATA JW /0/
      DATA Cmic0 /1.0D0,0.0D0,0.0D0,
     1            0.0D0,1.0D0,0.0D0,
     2            0.0D0,0.0D0,1.0D0/
      data convf/0.5729577951308232D+02/
      data FS/9*1.0D0/ 
      SAVE
      IF (IW) 32,33,30
  33  call  random_seed
C     Number of grains in ALAMEL cluster
      read (KLEC,99) NGR
      read (KLEC,99) NLIST
      read (KLEC,99) NFILE1
      read (KLEC,99) NFILTW
      read (KLEC,99) NTEN
      read (KLEC,99) KOST
      read (KLEC,99) IGLIJ
      read (KLEC,99) IPR
      WRITE (IMP,101) NGR,NLIST,NFILE1,NFILTW,NTEN,KOST,IGLIJ,
     1 IPR
      WRITE (*,101) NGR,NLIST,NFILE1,NFILTW,NTEN,KOST,IGLIJ,
     1 IPR
 101  FORMAT (' SIMUL - PARAMETERS:',/
     1'NGR=   ',I5,/,'NLIST= ',I5,/,'NFILE1=',I5,/,'NFILTW=',i5,/,
     1'NTEN=  ',I5,/,'KOST=  ',I5,/,'IGLIJ= ',I5,/,'IPR=   ',I5)  
      if (NGR.lt.1.or.NGR.gt.2) then 
                                   write (*,140) NGR
                                   write (IMP,140) NGR
                                   stop
                                endif
 140  format (' NGR can only take the values 1 or 2 but was',I5)   
C     Number of relaxations: 0 for Taylor and 2 for ALAMEL: 
      NRL=(NGR-1)*2
      do i=1,3
         read (KLEC,94)(FMicro(i,j),j=1,3)
         write (IMP,106)(FMicro(i,j),j=1,3)
      enddo
 106  format ('F_Microstructure=',3f12.6)
  99  FORMAT (2I5)
  94  format (3F10.0)
  16  read (KLEC,98) TITEL
  98  format (A)
      write (IMP,97) TITEL
  97  format (' Title of the new simulation: ',A)
      write (IMP1,98) TITEL
      write (IMP2,98) TITEL
C     read the parameters of the work hardening model
      X=FTAU(-1000.0D00)
      TAU=1.0
      CALL TAYLOR(1,0,KOST,EPS,IROT,Ftot)
      if  (KOST.eq.1) then
        do L=1,NGR
           do i=1,M11
              do j=1,2   
                 FK1b(j,i,L)=FK1(j,i)
              enddo
           enddo
        enddo
      else
        do L=1,NGR
          do i=1,M11
              do j=1,2   
                 FK1b(j,i,L)=1.0
              enddo
          enddo
        enddo
      endif
      if (NFILTW.eq.1) then
          write (IMP3,98) TITEL
          write (IMP3,99) NPOINT
      endif
      RETURN
  30  i=NPOINT/NGR
      if (NGR*i.eq.npoint) goto 36
      write (6,405) NPOINT
      write (*,405) NPOINT
 405  format (' Subroutine SIMUL',/' The LAMEL version works only if',
     1' the number of orientations NPOINT=',I5,/,
     2' is an even number')
      stop
  36  NFILE=NFILE0*NFILE1
      read (KLEC,99) NSTP
      write (IMP,115) NSTP
 115  format (//,' S I M U L         NR. STEPS=',I5,//)
      read (KLEC,99) ICRAT1,ICRAT2
      write (IMP,104) ICRAT1,ICRAT2
 104  format (' ICRAT:',2I5)
      swrlx(1)=(ICRAT1.eq.1)
      swrlx(2)=(ICRAT2.eq.1)
      swrlx(3)=.false.
      if (IPR.gt.0) write (IMP,*) 'Relaxations:',swrlx(1)
      CALL TAYLOR(2,NTEN,KOST,EPS,IROT,Ftot)
C
C     Main Loop over the Steps
C
      DO 8 ISTP=1,NSTP
       WRTOT=0.0
      IROT=1
      TOTGEW=0.0
      do 50 i=1,3
      do 50 j=i,3
      STOT(i,j)=0.0
      RHOST(i,j)=0.0
  50  continue
      SG=0.
      GMM=0.
       call dynfil2(nunit,nrstep,F,GAXES,GEULR,CIJ,TG)
      write (*,96) ISTP,GAXES
      write (IMP,96) ISTP,GAXES
  96  format(' Step nr.',i5,5X,3f12.5)
      if (IROT.eq.0.or.IW.gt.1) goto 70
      if (IGLIJ.eq.1) write (IMP,3456) WRTOT, DG
 3456 format (' WRTOT=',d12.3,/,' DG=',3(T10,3d12.3,/))
C
C     Get the 5x5 transformation matrix MACRO to morfol. GRAIN AXES
C
      if (IGLIJ.eq.1) write (IMP,3458) TG
 3458 format (' TG=',3(T10,3d12.3,/))
  70  if (nfile.eq.0.or.ISTP.gt.1) goto 44
C     INSTRUCTION ADDED IN LAMEL model:
      if (ISTP.gt.1) goto 44
      IF (NLIST.EQ.2) WRITE (IMP,112) ISTP
 112  FORMAT (//' DEFORMATION STEP ',I5,//)
      write (IMP1,402)
 402  format (/,' Def. Step    ','Number of orientations',27X,
     1 2X,'F(1,1)',4X,'F(2,1)',4X,'F(3,1)',4X,
     2 2X,'F(1,2)',4X,'F(2,2)',4X,'F(3,2)',4X,
     3 2X,'F(1,3)',4X,'F(2,3)',4X,'F(3,3)',
     4 6X,'a',9X,'b',9x,'c',9x,'G-phi1',4x,'G-PHI',4x,'G-phi2')
      write (IMP2,404) nrstep+1,NPOINT
 404  format (' Def. Step ',i5,'  Number of orientations',i5,/,8x,
     1 ' WDOT  ','WDOT/STR.RAT.','  TAU     ','   M      ','STR.RAT. '
     2 ,5x,24X,'RHO-SYMMETRIC',24x,8x,'RHO-ROTATIONAL',8x,
     3 24x,'STRESS',/,1x,219('*'))
      do 48 i=1,3
      GLR(i)=GEULR(i)*convf
  48  continue
      write (IMP1,403) nrstep,NPOINT,F,GAXES,GLR
 403  format(I6,5X,i5,44x,3(2X,3F10.6),2(2x,3f10.5))
      write (IMP1,401)
 401  format (' CRYSTAL WEIGHT ',5X,'phi1',6X,'PHI',7X,'phi2',6X,
     1'  GAMMA',5X,2X,'F(1,1)',4X,'F(2,1)',4X,'F(3,1)',4X,
     2             2X,'F(1,2)',4X,'F(2,2)',4X,'F(3,2)',4X,
     3             2X,'F(1,3)',4X,'F(2,3)',4X,'F(3,3)',
     4 6X,'a',9X,'b',9x,'c',9x,'G-phi1',4x,'G-PHI',4x,'G-phi2')
  44  call MATPROD(Ftot,F,FMicro,3,3,3)
      FTINV=Ftot
      CALL MINV(FTINV,3,DMINV,L1MINV,L2MINV,9)
      CMICRO=CMic0
      call UPDATC(CMICRO,FTINV)
      if (IGLIJ.eq.1) then 
          do i=1,3 
             write (IMP,407) (Ftot(j,i),j=1,3)
          enddo
      endif
 407      format (' Ftot ',3d15.7)
      if (IROT.ne.1) goto 10
      nrstep=nrstep+1
      call Ftensor(DG,F1,F2)
      call UPDATF(F,F1)
      call UPDATC(CIJ,F2)
      call GETANG(CIJ,GAXES,GEULR,TG)
      call DYNFIL3(IDISK1,nrstep,F,GAXES,GEULR,CIJ,TG)

C
C       Added for lamel model:
C     Organisation reading temporary texture file,
C     in such way that the program TAYLOR can process the crystals
C     by sets of 2.
C     Taylor must therefore have "advance knowledge" of the
C     orientation to come at the moment that it starts such
C     computation.
C     See also the comment before the calling of subroutine TAYLOR.
C
  10  laml=1
      laml1=NGR
      ifil4=0
      if (NFILTW.eq.1) write (IMP3,399)
 399  format(1x)
      DO 23 IOR=1,NPOINT
C      if (IOR.eq.789.and.ISTP.eq.1) IPR=2
C      if (IOR.eq.790.and.istp.eq.1) stop
C      if (IPR.ne.2) goto 2626
C      write (IMP,2627) istp,IOR
C      write (*,2627) istp,IOR
 2627 format ('ISTP=',I5,'   IOR=',i5)
C      if (IOR.eq.261.and.istp.eq.8) IPR=2
C      if (IOR.gt.261.and.istp.eq.8) stop
C      IPR=1
C      IGLIJ=0
 2626 do 80 L=laml,laml1
      if (ifil4.eq.NPOINT) goto 80
      ifil4=ifil4+1
      call DYNFIL4(nunit,ifil4,fi10b(L),PHI0b(L),fi20b(L),
     1 TRFb(1,1,L),GEWFb(L),GMMAb(L),Fb(1,1,L),GAXESb(1,L),
     2 GEULRb(1,L),CIJb(1,1,L),TGb(1,1,L),RHOSSb(1,1,L))
C
      fi1b(L)=fi10b(L)*convf
      PHIb(L)=PHI0b(L)*convf
      fi2b(L)=fi20b(L)*convf
C      IF (NUNGL.NE.0) READ(NUNGL) ((FK1b(K,J,L),J=1,M11),K=1,2)
  80  continue
      laml1=laml1+1
      if (laml1.gt.NGR) laml1=1
      laml=laml1
      GMM0=GMMAb(laml)
      if (KOST.eq.1) TAU=FTAU(GMM0)
      fi1=fi1b(laml)
      PHI=PHIb(laml)
      fi2=fi2b(laml)
      do 81 j=1,3
      do 81 i=1,3
      TRF(i,j)=TRFb(i,j,laml)
      TG(i,j)=TGb(i,j,laml)
      RHOSSa(i,j)=RHOSSb(i,j,laml)
  81  continue
C      if (NUNGL.ne.0) then
C         Next instruction will ultimately result in some effefct if KOST=1
C          do i=1,M11
C             do j=1,2
C                FK1(j,i)=FK1b(j,i,laml)*TAU
C             enddo
C          enddo
C      endif 
C
C 
C     In case of NGR=2:
C        LAML=1: TAYLOR
C                - has the present and the next orientation available
C                - must perform the computation of a set of 2 crystals
C                - has to output the result for the first crystal
C        LAML=2: TAYLOR
C                - should not perform any computation
C                - has to output the result of the second crystal found
C                  during the previous computation.
C
C      write (*,3210)
C 3210 format (' Just before Taylor')
      IF (IW.le.1) CALL  TAYLOR(3,IGLIJ,KOST,EPS,IROT,Ftot)
C      write (*,3211)
 3211 format (' Just after Taylor')
      TOTGEW=TOTGEW+GEWF
      IF (JW.EQ.0) GOTO 34
  35  IF (JW.NE.2) GMM0=0.
  34  IF (NFILE.eq.0.or.ISTP.gt.1) goto 41
C
C     Output file with current condition (as it was before call of Taylor!)
C
      if (IROT.ne.1) goto 41
      do 47 i=1,3
      GLR(i)=GEULRb(i,laml)*convf
  47  continue
      write (IMP1,400) IOR,GEWF,fi1,PHI,fi2,GMM0,
     1 ((Fb(i,j,laml),i=1,3),j=1,3),(GAXESb(j,laml),j=1,3),GLR
 400  format (I6,f10.5,2X,3f10.5,2X,f10.5,3(2X,3F10.6),2(2x,3f10.5))
  41  if (IW.gt.1) goto 23
      if (IROT.eq.1) goto 59
      WRTOT=WRTOT+WR*GEWF
      goto 23
  59  do 56 i=1,5
      BUFSPV(i)=SPANV(i)
  56  continue
C      if (IOR.eq.1.and.ISTP.eq.1) IPR=2
      CALL TAYLR1(IGLIJ,ISTP,IOR,IROT,IEND,NFILE,TAU)
C      if (IOR.eq.1.and.ISTP.eq.1) stop
      if (iend.ne.1) goto 49
C      if (iend.eq.0) goto 49
      if (IGLIJ.eq.1.and.IPR.eq.2) stop
      do 55 i=1,5
      SPANV(i)=BUFSPV(i)
  55  continue
      IGLIJ=1
      IPR=2
      call taylr1(IGLIJ,ISTP,IOR,0,IEND,NFILE,TAU)
      stop
   49 if (NFILTW.eq.1) write (IMP3,398) ITW
 398  format (I3)
      do 51 i=1,3
      do 51 j=i,3
      STOT(i,j)=STOT(i,j)+Ssam(i,j)*GEWF
      RHOST(i,j)=RHOST(i,j)+RHOSsa(i,j)*GEWF
  51  continue
  63  SG=SG+WDOT*GEWF
      GMM=GMM+WDOT*GEWF/TAU
      if (IROT.NE.1) goto 23
      GAMMA=GMM0+DELTAW/TAU
      call DYNFIL5(IDISK1,IOR,fi1,PHI,fi2,C2,GEWF,GAMMA,
     1 F,GAXES,GEULR,CIJ,TG,RHOSsa)
C      IF (NLIST.LT.2) GOTO 15
  15  CONTINUE
      IF (NFILE.eq.0.or.ISTP.gt.1) goto 20
C
C     Plaats hier output van "vervormingsstap"
C     (ALLEEN zo ISTP=1)
C
  20  CONTINUE
C  9  CONTINUE
C
C     End of loop over crystals
C
  23  CONTINUE
C      REWIND NUNIT  
      call DYNFIL6(NUNIT)
C     NEXT INSTRUCTION ADDED FOR LAMEL MODEL
      if (IROT.eq.0) goto 62
      IF (NUNGL.NE.0) REWIND NUNGL                                      
      if (IW.gt.1) goto 22
      do 52 i=1,3
      do 52 j=i,3
      SHsam(i,j)=STOT(i,j)/TOTGEW
      RHOSm(i,j)=RHOST(i,j)/TOTGEW
  52  continue
C      call STR5(vec1,SHsam)
  66  do 65 i=1,2
      do 65 j=i+1,3
      SHsam(i,j)=SHsam(i,j)*FS(i,j)
      SHsam(j,i)=SHsam(i,j)
      RHOSm(i,j)=RHOSm(i,j)*FS(i,j)
      RHOSm(j,i)=RHOSm(i,j)
  65  continue
      GMM=GMM/TOTGEW
      SG=SG/TOTGEW
      call DYNFIL6(IDISK1)
      CALL COPYT(IDISK1,NUNIT)
      JW=0
      WRITE (IMP,105) ISTP,SG,GMM,EPS
 105  FORMAT (' FOR STEP',I5,'  AVERAGE STRESS=',F15.5,'   AVERAGE M-VAL
     1UE=',F10.5,'  EFF. STRAIN EPS USED=',F10.5) 
  62  continue
C
C     End of loop over steps
  57  continue
   8  CONTINUE                                                          
  22  RETURN
  32  JW=-IW                                                            
      GOTO 22                                                           
      END                                                               
