C     When you set NSYM=1, then 2 things will happen:
C     1) Not the value 1.0, but the values in the input data set will be used
C        for the critical resolved shear stresse
C     2) They will be multiplied with TAU, calculated from GAMMA
C        using the FTAU function.
C
      FUNCTION FTAU(GAMMA)
C      Double Precision FTAU 
      implicit double precision (a-h,o-z)
      double precision GAMMA
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1,NDAT2
      SAVE
C     check wether model parameters must be read:
      if (GAMMA.gt.-1000.0) goto 1
C     Read the parameters of the work hardening model:
      read (KLEC,99) TIII1,TIIIS,TIVS
      read (KLEC,99) THIII1,THT
  99  format (3f10.0)
      write (IMP,100) TIII1,TIIIS,TIVS,THIII1,THT
 100  format(' Work hardening model = DOUBLE VOCE-model',/,
     2 ' TAU-III-1=  ',f20.8,/,
     1 ' TAU-III-S = ',F20.8,/,
     4 ' TAU-IV-S =  ',f20.8,/,
     3 ' THETA-III-1=',f20.8,/,
     3 ' THETA-T=    ',f20.8)
      if (TIIIS.gt.TIII1.and.THIII1.gt.THT) goto 3
      write (IMP,101)
 101  format (' ALG0 - FTAU - reading data - TAU-III-S must be'
     1 ,' larger than TAU-III-1',
     2 /, '     also, THETA-III-1 must be larger than THETA-T')
      stop
C     Calculation of transition-gamma
   3  THIII=THIII1/(1.0-TIII1/TIIIS)
      ETA=THT/THIII
      GAMMAT=-TIIIS*LOG(ETA*TIIIS/(TIIIS-TIII1))/THIII
C     Calculation of transition TAU
      TAUT=TIIIS-(TIIIS-TIII1)*exp(-THIII*GAMMAT/TIIIS)
C     Calculation of theta-IV-0
      THIV=THT/(1.0-TAUT/TIVS)
C     Calculation of TAU-IV-0
      TIV0=TIVS+(TAUT-TIVS)*exp(THIV*GAMMAT/TIVS)
      write (IMP,102) GAMMAT,TAUT,THIV,TIV0
 102  format (' GAMMA-T, TAU-T, THETA-IV-0, TAU-IV-0',/,4d15.5)
      FTAU=0.0
      goto 2
C     Implementation of the VOCE-model
   1  if (GAMMA.le.GAMMAT) then
        FTAU=TIIIS-(TIIIS-TIII1)*EXP(-THIII*GAMMA/TIIIS)
      else
        FTAU=TIVS-(TIVS-TIV0)*EXP(-THIV*GAMMA/TIVS)
      endif
   2  RETURN
      END                                                               
      BLOCK DATA
      implicit double precision (a-h,o-z)
      COMMON/TLR1/ N,M,N1,NGL,NTW,M11,NC,LC,B1(3,96),B(5,5),
     1B2(6,48),G(48),DI1(5)
      COMMON /IGLIJS/ FK1(96),NUNGL,NGLS,cc(96)
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1,NDAT2
      COMMON /ES1/ IMP3
C      COMMON /RCFILS/ NUNRC(2)
      COMMON /NRSTEP/ nrstep
C      double precision B,B2,G
C
C
C     LEC= data set with slip systems
C     KLEC= data set with parameters
C     IMP= printer
C     IMP1=output-file with successive "current situations"
C     IMP2=output-file with successive "responses to imposed strain"
C     IMP3=output-file with twinning information
C     IDISK1= work file
C     NDAT1= input texture file
C     NDAT2=
C
      DATA LEC,KLEC,IDISK1,IMP,NDAT1,NDAT2/4,5,12,3,9,10/
C      data NUNRC/13,14/
      data IMP1,IMP2 /7,8/
      data IMP3/11/
      DATA NUNGL /0/
      DATA N,NC,LC    /5,1,300/
      data nrstep/0/
      END
      SUBROUTINE SIMUL(IW,JPAR,EPS,NFILE0,NUNIT,INSG0)
      implicit double precision (a-h,o-z)
C
C     IW=2 is meant for outputting the final texture.
C
C     Special version for LAMELLAR STRUCTURE-Model
C     November 97
C
C     Modified October 2000
C
C
C     Modifications december 2000
C     - if NSYM eq 1, FK1b is in initialised to values FK1
C     - in instruction 40, "0" is replaced by NSYM
C

      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1,NDAT2
      COMMON /ES1/ IMP3
	COMMON /OUTMIC/nomic
      COMMON /IGLIJS/ FK1(96),NUNGL,NGLS,cc(96)
      COMMON /STAP/ SG,GMM                                              
      COMMON /TEXTUR/ TRF(3,3),C1(3,3),C2(3,3),WDOT,ROTM,NO,DG(3,3),
     1ITW,IPR,DELTAW,GEWF,NLIST
      COMMON /SYMP/ INV,ISP,LOM,KSYM,KTYP,NPOINT,TEN(3,3),TOTGEW        
      COMMON /EULERA/ fi1,PHI,fi2
      COMMON /GENRLX/ YY(5,5),SHsam(3,3),Ssam(3,3),SPANV(5),RHOSsa(3,3),
     1 WR,WRTOT,SWRLX(3)
C      COMMON /RCFILS/ NUNRC(2)
      COMMON /DOUBLE/ XM(5,96),XEPS(5),DELTAT,RHO(5)
      COMMON /NRSTEP/ nrstep
      COMMON /LAMEL/ laml,fi10b(2),phi0b(2),fi20b(2),TRFb(3,3,2),
     1 gewfb(2),GMMAb(2),Fb(3,3,2),GAXESb(3,2),GEULRb(3,2),
     2 CIJb(3,3,2),TGb(3,3,2),RHOSSb(3,3,2),
     3 fi1b(2),phib(2),fi2b(2),
     4 fk1b(96,2)
      common /CEIGEN/ IOR,ISTP,NBLOC,ALFAK,CMICRO(3,3)
      DIMENSION F(3,3),F1(3,3),GAXES(3),GEULR(3),TG(3,3),
     1 CIJ(3,3),F2(3,3),GLR(3),STOT(3,3),BUFSPV(5),
     2 RHOST(3,3),RHOSm(3,3),FMicro(3,3),Ftot(3,3),
     3 L1MINV(3),L2MINV(3),CMic0(3,3),FTINV(3,3)
      dimension FS(3,3)
C      dimension VAXES(3)
C      dimension vec1(5),vec2(5),vec3(5),vec4(5),YYY(5,5)
C      double precision SPANV,RHO,XM,XEPS,DELTAT
      character*40 TITEL
      logical SWRLX
      DATA JW /0/
      DATA Cmic0 /1.0D0,0.0D0,0.0D0,
     1            0.0D0,1.0D0,0.0D0,
     2            0.0D0,0.0D0,1.0D0/
      data convf/0.5729577951308232D+02/
C      data criter/0.00025/
      data FS/9*1.0D0/ 
      SAVE
C      write (*,406) IW
C      write (IMP,406) IW
C 406  format (' SIMUL - IW=',I5,' (2 is for final output only)')
C
C     NRCMOD is set to 1 (Self-Consistent algorithm is switched off)
C
c <jg>:
	if (IW .EQ.2) then
		write (nomic,9393) NPOINT,TITEL
 9393 format(I5,5x,A)
	endif
c </jg>

      NRCMOD=1
      i=NPOINT/2
      if (2*i.eq.npoint) goto 36
      write (6,405) NPOINT
      write (*,405) NPOINT
 405  format (' Subroutine SIMUL',/' The LAMEL version works only if',
     1' the number of orientations NPOINT=',I5,/,
     2' is an even number')
      stop
  36  IF (IW) 32,33,30
  33  call  random_seed
      WRITE (IMP,100)  JPAR
 100  FORMAT (//,' INITIALISATION OF SUBROUTINE SIMUL',/,
     1' PARAMETER J =',I5)
      read (KLEC,99) NLIST
      read (KLEC,99) NFILE1
      read (KLEC,99) NFILTW
      read (KLEC,99) NTEN
      read (KLEC,99) NSYM
      read (KLEC,99) IGLIJ
      read (KLEC,99) IPR
      read (KLEC,94) ETAFAK
      read (KLEC,94) ATTENF
  99  FORMAT (2I5)
  94  format (3F10.0)
      WRITE (IMP,101) NLIST,NFILE1,NFILTW,NTEN,NSYM,IGLIJ,
     1 IPR,ETAFAK,ATTENF
      WRITE (*,101) NLIST,NFILE1,NFILTW,NTEN,NSYM,IGLIJ,
     1 IPR,ETAFAK,ATTENF
 101  FORMAT (' SIMUL - PARAMETERS:',/
     1,' NLIST=',I5,5X,'NFILE=',I5,5X,'NFILTW=',i5,/,
     1' NTEN=',I5,5X ,'NSYM=',I5,/,' IGLIJ=',I5,5x,'IPR=',I5,
     1 '    ETAFAK=',F10.5,'   ATTENF=',F10.5,/)
      do i=1,3
         read (KLEC,94)(FMicro(i,j),j=1,3)
         write (IMP,106)(FMicro(i,j),j=1,3)
      enddo
 106  format ('F_Microstructure=',3f12.6)  
  16  read (KLEC,98) TITEL
  98  format (A)
      write (IMP,97) TITEL
  97  format (' Title of the new simulation: ',A)
      write (IMP1,98) TITEL
      write (IMP2,98) TITEL
C     read the parameters of the work hardening model
      X=FTAU(-1000.0D00)
      TAU=1.0
C      do i=0,20
C      GAMMA=i*0.02
C      X=FTAU(GAMMA)
C      write (IMP,7654) GAMMA,X
C 7654 format (' Gamma=',F10.3,'  x=',d15.5)
C      enddo
C      stop
      CALL TAYLOR(1,0,JPAR,EPS,0,IROT,Ftot)
      if  (NSYM.eq.1) then
        do laml=1,2
           do i=1,NGLS
              FK1b(i,laml)=FK1(i)
           enddo
        enddo
      endif
C      IF (IPR.NE.2) IPR=1
      if (NFILTW.eq.1) then
          write (IMP3,98) TITEL
          write (IMP3,99) NPOINT
      endif
      RETURN
  30  NFILE=NFILE0*NFILE1
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
C 456  format (' Relaxation Allowed')
      if (INSG0.eq.1) SG0=1.0d06
      alfak=ATTENF*(etafak-1.0)*SG0/3.0  
      if (IPR.gt.0) write (IMP,130) SG0,ALFAK
 130  format (' SG0=',d15.5,'  ALFA Factor:',d15.5)
      CALL TAYLOR(2,NTEN,NSYM,EPS,0,IROT,Ftot)
C
C     Main Loop over the Steps
C
      DO 8 ISTP=1,NSTP
C     Set IRELX to: NO RELAXATION
      IRELX=0
      IROT=0
C
C     IN THE LAMEL MODEL, A FULL
C     CONSTRAINTS CALCULATION IS ALSO CALCULATED FIRST FOR ISTP>1
C     BY SETTING IRCM0 TO 1.
C
      RHOVM=1.0
      IRCM0=1
      IRCMOD=IRCM0
   78 continue
      if (IRCMOD.GT.1) IRELX=2
      IDUB1=1
      if (IRCMOD.eq.1) IDUB1=2
C     MODIFICATION FOR LAMEL MODEL:
      IDUBLE=2
       WRTOT=0.0
       IROT0=0
C       if (IW.eq.2) IROT=1
      IROT=1
  86  JDUBLE=3-IDUBLE
C
C     IN THE LAMEL MODEL, IDUBLE IS FIXED TO 2
C     THE AVERAGE RELAXATION IS NOT SET TO ZERO (IS NOT NECESSARY)
C
      if (IDUBLE.eq.2.and.IRELX.GT.0) IRELX=3  
      TOTGEW=0.0
      do 50 i=1,3
      do 50 j=i,3
      STOT(i,j)=0.0
      RHOST(i,j)=0.0
  50  continue
      SG=0.
      GMM=0.
C      read (nunit) nrstep,F,GAXES,GEULR,CIJ,TG
       call dynfil2(nunit,nrstep,F,GAXES,GEULR,CIJ,TG)
      write (*,96) ISTP,GAXES
      write (IMP,96) ISTP,GAXES
  96  format(' Step nr.',i5,5X,3f12.5)
      if (IROT.eq.0.or.IW.gt.1) goto 70
C
C     Instruction added for the LAMEL model:
C
      if (IGLIJ.eq.1) write (IMP,3456) WRTOT, DG
 3456 format (' WRTOT=',d12.3,/,' DG=',3(T10,3d12.3,/))
C
C     Get the 5x5 transformation matrix MACRO to morfol. GRAIN AXES
C
      if (IGLIJ.eq.1) write (IMP,3458) TG
 3458 format (' TG=',3(T10,3d12.3,/))
  70  if (IDUBLE.eq.2.and.IRCMOD.gt.IRCM0) goto 44
      if (nfile.eq.0.or.ISTP.gt.1.or.IRCMOD.gt.IRCM0) goto 44
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
C  44  if (IRCMOD.lt.NRCMOD) goto 10
C      if (IDUBLE.lt.2) goto 10
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
C      write (IDISK1) nrstep,F,GAXES,GEULR,CIJ,TG
      call DYNFIL3(IDISK1,nrstep,F,GAXES,GEULR,CIJ,TG)

C
C       Added for lamel model:
C     Organisation reading temporary texture file,
C     in such way that the program TAYLOR can process the crystals
C     by sets of 2.
C     Taylor must therefore have "advance knowledge" of the
C     orientation to come at the moment that it starts a such
C     computation.
C     See also the comment before the calling of subroutine TAYLOR.
C
  10  laml=1
      laml1=2
      ifil4=0
      if (NFILTW.eq.1) write (IMP3,399)
 399  format(1x)
      DO 23 IOR=1,NPOINT
C      if (IOR.eq.1193.and.ISTP.eq.6) IPR=2
C      if (IOR.eq.1195.and.istp.eq.6) stop
      if (IPR.ne.2) goto 2626
      write (IMP,2627) IOR
      write (*,2627) IOR
 2627 format (' IOR=',i5)
C      if (IOR.eq.261.and.istp.eq.8) IPR=2
C      if (IOR.gt.261.and.istp.eq.8) stop
C      IPR=1
C      IGLIJ=0
 2626 do 80 L=laml,laml1
      if (IOR.eq.NPOINT) goto 80
C      READ (NUNIT) fi10b(L),PHI0b(L),fi20b(L),
C     1 ((TRFb(i,j,L),i=1,3),j=1,3),GEWFb(L),GMMAb(L),
C     2 ((Fb(i,j,L),i=1,3),j=1,3),(GAXESb(i,L),i=1,3),
C     3 (GEULRb(i,L),i=1,3),((CIJb(i,j,L),i=1,3),j=1,3),
C     4 ((TGb(i,j,L),i=1,3),j=1,3),((RHOSSb(i,j,L),i=1,3),j=1,3)
      ifil4=ifil4+1
      call DYNFIL4(nunit,ifil4,fi10b(L),PHI0b(L),fi20b(L),
     1 TRFb(1,1,L),GEWFb(L),GMMAb(L),Fb(1,1,L),GAXESb(1,L),
     2 GEULRb(1,L),CIJb(1,1,L),TGb(1,1,L),RHOSSb(1,1,L))
C
      fi1b(L)=fi10b(L)*convf
      PHIb(L)=PHI0b(L)*convf
      fi2b(L)=fi20b(L)*convf
      IF (NUNGL.NE.0) READ(NUNGL) (FK1b(J,L),J=1,NGLS)
  80  continue
      laml1=laml1+1
      if (laml1.gt.2) laml1=1
      laml=laml1
C
C     Hier moet nu code komen, waarbij fi10=fi10b(laml) etc.
C     voor alles wat diende ingelezen te worden.
C
      fi10=fi10b(laml)
      PHI0=PHI0b(laml)
      fi20=fi20b(laml)
C     OCT 13 2002  NEXT LINE WAS REMOVED, AS GEWF IS OBTAINED FROM PANCAK2
C      GEWF=GEWFb(laml)
      GMM0=GMMAb(laml)
      if (NSYM.eq.1) TAU=FTAU(GMM0)
      fi1=fi1b(laml)
      PHI=PHIb(laml)
      fi2=fi2b(laml)
      do 81 j=1,3
C      GAXES(j)=GAXESb(j,laml)
C      GEULR(j)=GEULRb(j,laml)
      do 81 i=1,3
      TRF(i,j)=TRFb(i,j,laml)
C      F(i,j)=Fb(i,j,laml)
C      CIJ(i,j)=CIJb(i,j,laml)
      TG(i,j)=TGb(i,j,laml)
      RHOSSa(i,j)=RHOSSb(i,j,laml)
  81  continue
      if (NUNGL.ne.0) then
C         Next instruction will ultimately result in some effefct if NSYM=1
          do i=1,NGLS
              FK1(i)=FK1b(i,laml)*TAU
          enddo
      endif  
C
C     If LAML=1, TAYLOR
C                - has the present and the next orientation available
C                - must perform the computation of a set of 2 crystals
C                - has to output the result for the first crystal
C     If LAML=2, TAYLOR
C                - should not perform any computation
C                - has to output the result of the second crystal found
C                  during the previous computation.
C
C      write (*,3210)
C 3210 format (' Just before Taylor')
      IF (IW.le.1) CALL  TAYLOR(3,IGLIJ,NSYM,EPS,IRELX,IROT,Ftot)
C      write (*,3211)
 3211 format (' Just after Taylor')
      TOTGEW=TOTGEW+GEWF
      IF (JW.EQ.0) GOTO 34
  35  IF (JW.NE.2) GMM0=0.
  34  IF (NFILE.eq.0.or.ISTP.gt.1.or.IRCMOD.gt.IRCM0) goto 41
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
c <jg> ! Write .SMT file.
	if (IW .EQ. 2) then
! include additional data
!		write(nomic,9394)  fi2,PHI,fi1, 1 ,GEWF, GMM0
! 9394 format (3F10.3,10X,I5,5X,2F10.3)
! follow 'bare' smt format
		write(nomic,9395)  fi2,PHI,fi1, 1 , 1.0
 9395 format (3F10.3,10X,I5,5X,F10.1)
c Compare the format with those used in leesor:   
c  20  READ (NDAT1,96) PHI2,PHI,PHI1,STAP,NSTAP,GEW,GAMMA
c  96  FORMAT (4F10.0,I5,5X,2F10.0)                                      
c
  	endif
c </jg>
  41  if (IW.gt.1) goto 23
C
C      5 NEXT INSTRUCTIONS ADDED FOR LAMEL MODEL:
      if (IROT.eq.1) goto 85
      WRTOT=WRTOT+WR*GEWF
      goto 23
  85  if (IRCMOD.eq.1) goto 59
C      read(NUNRC(IDUBLE)) SPANV,RHOSsa,RHO
  59  do 56 i=1,5
      BUFSPV(i)=SPANV(i)
  56  continue
C      write (*,456) IRELX
C 456  format (' IRELX=',I5)
C      if (IOR.eq.1.and.ISTP.eq.1) IPR=2
      CALL TAYLR1(IGLIJ,ISTP,IOR,IRELX,IROT,IEND,NFILE,TAU)
C      if (IOR.eq.1.and.ISTP.eq.1) stop
      if (iend.ne.1) goto 49
C      if (iend.eq.0) goto 49
      if (IGLIJ.eq.1.and.IPR.eq.2) stop
      do 55 i=1,5
      SPANV(i)=BUFSPV(i)
  55  continue
      IGLIJ=1
      IPR=2
      call taylr1(IGLIJ,ISTP,IOR,IRELX,0,IEND,NFILE,TAU)
      stop
C  49  write (nunrc(JDUBLE)) SPANV,RHOSsa,RHO
   49 if (NFILTW.eq.1) write (IMP3,398) ITW
 398  format (I3)
      do 51 i=1,3
      do 51 j=i,3
      STOT(i,j)=STOT(i,j)+Ssam(i,j)*GEWF
      RHOST(i,j)=RHOST(i,j)+RHOSsa(i,j)*GEWF
  51  continue
C      call STR5(vec2,Ssam)
C      call STR5(vec3,RHOSsa)
C      if (IDUBLE.eq.2.or.ircmod.ne.10) goto 73
C      do 72 i=1,5
C      X=0.0
C      do 74 j=1,5
C      X=X+YYY(i,j)*(vec1(j)-vec2(j))
C  74  continue
C      vec4(i)=vec3(i)-X
C  72  continue
C      write (IMP,75) IOR,(vec3(j),j=1,2),(vec4(j),j=1,2)
C  75  format (i5,4d15.5)
  63  SG=SG+WDOT*GEWF
      GMM=GMM+WDOT*GEWF/TAU
C      if (IRCMOD.lt.NRCMOD.or.IDUBLE.eq.1) goto 23
      if (IROT.NE.1) goto 23
      GAMMA=GMM0+DELTAW/TAU
C     Next instruction: if you want to update the shape of each
C     grain separately. In that case: adapt DG
C      call Ftensor(DG,F1,F2)
C      call UPDATF(F,F1)
C      call UPDATC(CIJ,F2)
C      call GETANG(CIJ,GAXES,GEULR,TG)
C      WRITE (IDISK1) fi1,PHI,fi2,C2,GEWF,GAMMA,F,GAXES,GEULR,CIJ,TG
C     1 ,RHOSsa
      call DYNFIL5(IDISK1,IOR,fi1,PHI,fi2,C2,GEWF,GAMMA,
     1 F,GAXES,GEULR,CIJ,TG,RHOSsa)
C      IF (NLIST.LT.2) GOTO 15
  15  CONTINUE
      IF (NFILE.eq.0.or.ISTP.gt.1.or.IRCMOD.gt.IRCM0) goto 20
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
C      if (IRCMOD.gt.1) rewind NUNRC(IDUBLE)
C      rewind NUNRC(JDUBLE)
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
      if (IPR.lt.2) goto 53
      if (IDUBLE.eq.1) goto 77
      write (IMP,121)
 121  format (//' Macroscopic stress:',/)
      do 54 i=1,3
      write (IMP,122) (SHsam(i,j),j=1,3)
 122  format (3f15.5)
  54  continue
  77  if (IDUBLE.eq.2) goto 53
      RHOVM=0.0
      write (IMP,124)
 124  format (/' Average Relaxation:',/)
      do 58 i=1,3
      write (IMP,122) (RHOSm(i,j),j=1,3)
      do 58 j=1,3
      RHOVM=RHOVM+RHOSm(i,j)**2
  58  continue
      RHOVM=SQRT(2.0*RHOVM/3.0)
      write (IMP,123)
 123  format (/)
  53  if (IRCMOD.lt.NRCMOD.or.IDUBLE.eq.1) goto 57
C      REWIND IDISK1
      call DYNFIL6(IDISK1)
      CALL COPYT(IDISK1,NUNIT)
      JW=0
      WRITE (IMP,105) ISTP,SG,GMM,EPS
 105  FORMAT (' FOR STEP',I5,'  AVERAGE STRESS=',F15.5,'   AVERAGE M-VAL
     1UE=',F10.5,'  EFF. STRAIN EPS USED=',F10.5) 
      SG0=SG
C     NEXT INSTRUCTION ADDED FOR LAMEL MODEL:
  62  continue
C
C     End of loop over steps
  57  continue
   4  IRCMOD=IRCMOD+1
      if (IRCMOD.le.nrcmod) GOTO 78
   8  CONTINUE                                                          
  22  RETURN
  32  JW=-IW                                                            
      GOTO 22                                                           
      END                                                               
