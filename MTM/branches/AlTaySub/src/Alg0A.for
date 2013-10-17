#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
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
C     2) They will be multiplied with TAU, i.e. the reference stress. 
C        Note that TAU is the stress that is work-equivalent to the total slip rate in the
C          grain ONLY if all active slip systems hold the same CRSS with value equal to TAU.
C
      SUBROUTINE SIMUL(IW,EPS,NFILE0)
C     TO ORGANIZE SIMULATIONS OF DEFORMATION TEXTURES
C     USING THE ALAMEL MODEL
      use curAccess
      use dynfil
      use altayHard
#ifdef PEBP_ENABLED
      use KOST1xState
#endif
#ifdef ALTAY_SUBROUTINE
      use altayConfig
      use altayRCM
#endif
      use IOConfig
      use miscutils
      implicit double precision (a-h,o-z)
C
C     IW=2 is meant for outputting the final texture.
C
      COMMON /IGLIJS/ FK1(2,96),M11,CC(2,96)
      COMMON /DOUBLE/ XM(5,96),XEPS(5),DELTAT,RHO(5),B5(5)
      COMMON /TEXTUR/ TRF(3,3),C1(3,3),C2(3,3),WDOT,ROTM,NO,DG(3,3),
     1ITW,DELTAW,GEWF
      COMMON /SYMP/ INV,ISP,LOM,KSYM,KTYP,NPOINT,TEN(3,3),TOTGEW        
      COMMON /EULERA/ fi1,PHI,fi2
      COMMON /GENRLX/ YY(5,5),SHsam(3,3),Ssam(3,3),SPANV(5),RHOSsa(3,3),
     1 WR,SWRLX(3)
      COMMON /LAMEL/ laml,fi10b(2),phi0b(2),fi20b(2),TRFb(3,3,2),
     1 gewfb(2),GMMAb(2),Fb(3,3,2),GAXESb(3,2),GEULRb(3,2),
     2 CIJb(3,3,2),TGb(3,3,2),RHOSSb(3,3,2),
     3 fi1b(2),phib(2),fi2b(2),
     4 fk1b(2,96,2),NGR,NRL,ENTA,ITFMAS
      common /CEIGEN/ IOR,ISTP,NBLOC
      DIMENSION F(3,3),F1(3,3),GAXES(3),GEULR(3),TG(3,3),
     1 CIJ(3,3),F2(3,3),GLR(3),STOT(3,3),BUFSPV(5),
     2 RHOST(3,3),RHOSm(3,3),FMicro(3,3),Ftot(3,3),
     3 L1MINV(3),L2MINV(3),FTINV(3,3)
      dimension FS(3,3)
      character*40 TITEL
      logical SWRLX
      
      integer :: info
      ! HGAM: homogenized slip per step
      ! HGAMCALL: homogenized slip per call
      ! HGAMTOT: homogenized slip accumulated over calls
      double precision :: HGAM=0.D0,HGAMCALL=0.D0,HGAMTOT=0.D0   
      ! HEPS: homogenized von Mises equivalent strain (per step) ![PE:] Identified as obsolete comment
      ! HEPSCALL: homogenized vM strain (per call)               ![PE:] Identified as obsolete comment
      ! HEPSTOT: homogenized vM strain (cumulative over calls)   ![PE:] Identified as obsolete comment
      double precision :: HEPS=0.D0,HEPSCALL=0.D0,HEPSTOT=0.D0   ![PE:] Identified as obsolete statement
      ! Macroscopically imposed vM equivalent strain per step and 
      ! accumulated over the calls.
      double precision :: MEPS=0.D0,MEPSTOT=0.D0 
#ifdef ALTAY_SUBROUTINE
      ! Variables for simple stress calculations: full_model=.false.
      ! This operation mode is inspired by QGX's way of calculating
      ! stresses without a call to TAYLR1
      double precision,dimension(3,3) :: spant,TRFT,bufsp
#endif
      double precision :: GMMdot !Total slip rate in current grain      
      double precision :: Mgrain !Taylor factor of the current grain
      double precision :: Mavg   !Volume-averaged Taylor factor
      double precision :: srh !Strain Rate Heterogeneity in polycrystal
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C      DATA JW /0/
C      DATA Cmic0 /1.0D0,0.0D0,0.0D0,
C     1            0.0D0,1.0D0,0.0D0,
C     2            0.0D0,0.0D0,1.0D0/
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      data convf/0.5729577951308232D+02/
      data FS/9*1.0D0/ 
      SAVE
      IF (IW) 32,33,30
  33  call  random_seed
#ifdef ALTAY_SUBROUTINE
      NGR    = acnf%simul_init%NGR
      ENTA   = acnf%simul_init%ENTA
      KOST   = acnf%slipsystem%KOST
      !
      NLIST  = acnf%output_config%NLIST   ! control "listing"
      NFILE1 = acnf%output_config%NFILE   ! control "CUR"
      NFILTW = acnf%output_config%NFILTW  ! control "TWN"
      IPR    = acnf%output_config%IPR     ! control printing level
      NRES   = acnf%output_config%NRES    ! control "RES"
      NPEBP  = acnf%output_config%NPEBP   ! control "BEP"
      NMSS   = acnf%output_config%NMSS    ! control "MSS"
#else
C     Number of grains in ALAMEL cluster
      read (KLEC,99) NGR
      read (KLEC,*)  ENTA
      read (KLEC,99) NLIST
      read (KLEC,99) NFILE1
      read (KLEC,99) NFILTW
      read (KLEC,99) KOST
      read (KLEC,99) IPR
      NRES = NFILE1  ! IMP2 and IMP3 are controlled only by NFILE1
      NPEBP = 0
      if (KOST == hard_PEBP) NPEBP  = NFILE1
      NMSS = NLIST
#endif
      HGAMTOT=0.D0
      HEPSTOT=0.D0 ![PE:] Identified as obsolete statement
      MEPSTOT=0.D0
      ! NGR == 3: enable MAS-AL
      if(NGR.eq.3) then
            ITFMAS=1
            NGR=2
      else
            ITFMAS=0
      endif
      !
#ifndef ALTAY_SUBROUTINE
      if(NLIST.eq.1) then
            WRITE (IMP,101) NGR,NLIST,NFILE1,NFILTW,KOST,IPR
      end if
#ifndef NO_STDOUT   
      WRITE (*,101) NGR,NLIST,NFILE1,NFILTW,KOST,IPR
#endif
 101  FORMAT (' SIMUL - PARAMETERS:',/
     1'NGR=   ',I5,/,'NLIST= ',I5,/,'NFILE1=',I5,/,'NFILTW=',i5,/,
     1'KOST=  ',I5,/,'IPR=   ',I5) 
#endif
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE 
      if (NGR.lt.1.or.NGR.gt.2) then
#ifndef ALTAY_SUBROUTINE
            write (*,140) NGR
            if(NLIST.eq.1) then
            write (IMP,140) NGR
            end if
            stop
#else
            RCM_RAISE(1,'SIMUL','Incorrect value of NGR',RCM_RTN)
#endif
      endif
 140  format (' NGR can only take the values 1 or 2 but was',I5)   

C     Number of relaxations: 0 for Taylor and 2 for ALAMEL: 
      NRL=(NGR-1)*2
#ifdef ALTAY_SUBROUTINE
      !
      FMicro = acnf%simul_init%FMicro
      !
      TITEL  = acnf%jobtitle
#else
      do i=1,3
         read (KLEC,94)(FMicro(i,j),j=1,3)
         if(NLIST.eq.1) then
         write (IMP,106)(FMicro(i,j),j=1,3)
         end if
      enddo
 106  format ('F_Microstructure=',3f12.6)
  99  FORMAT (2I5)
  94  format (3F10.0)
  16  read (KLEC,98) TITEL
#endif      
      if(NLIST.eq.1) then
      write (IMP,97) TITEL
      end if
      if (NRES.gt.0) write (IMP2,98) TITEL
  97  format (' Title of the new simulation: ',A)
      ! Only if CUR file is requested
      if (NFILE1.eq.1) call CURwriteTitle(IMP1,TITEL,info)
  98  format (A)      
C     read the parameters of the work hardening model
      call readHardParams(KLEC,KOST,info)
      TAU=1.0
      CALL TAYLOR(1,KOST,EPS,Ftot)
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif
      select case(KOST)
      case(hard_voce)
            do L=1,NGR
                  do i=1,M11
                        do j=1,2   
                              FK1b(j,i,L)=FK1(j,i)
                        enddo
                  enddo
            enddo
      case(hard_none)
            do L=1,NGR
                  do i=1,M11
                        do j=1,2   
                              FK1b(j,i,L)=1.0
                        enddo
                  enddo
            enddo
      end select      
#ifndef ALTAY_SUBROUTINE
      if (NFILTW.eq.1) then
          write (IMP3,98) TITEL
          write (IMP3,99) NPOINT
      endif
#endif
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  30  continue
#ifdef ALTAY_SUBROUTINE
      ! Per-call selection of the model: NGR & NRL must be set
      NGR = acnf%simul_init%NGR
      if(NGR.eq.3) then
            ITFMAS=1
            NGR=2
      else
            ITFMAS=0
      endif
      ! Number of relaxations: 0 for Taylor and 2 for ALAMEL: 
      NRL=(NGR-1)*2
#endif
      i=NPOINT/NGR
      if (NGR*i.ne.npoint) then
#ifndef ALTAY_SUBROUTINE      
            write (6,405) NPOINT
            write (*,405) NPOINT
 405  format (' Subroutine SIMUL',/' The LAMEL version works only if',
     1' the number of orientations NPOINT=',I5,/,
     2' is an even number')
            stop
#else
            RCM_RAISE(1,'SIMUL',
     1      'The number of grains must be an even number',RCM_RTN)
#endif
      endif
  36  NFILE=NFILE0*NFILE1
      NPEBPx=NFILE0*NPEBP   ! control "BEP" (effective value)
      NMSSx= NFILE0*NMSS    ! control "MSS" (effective value)

#ifdef ALTAY_SUBROUTINE
      NSTP     = astate%simulCalls(astate%this)%input%nsteps
      swrlx(1) = astate%simulCalls(astate%this)%input%rlx1
      swrlx(2) = astate%simulCalls(astate%this)%input%rlx2
      swrlx(3) =.false.
#else      
      read (KLEC,99) NSTP
      if(NLIST.eq.1) then
      write (IMP,115) NSTP
      end if
 115  format (//,' S I M U L         NR. STEPS=',I5,//)
      read (KLEC,99) ICRAT1,ICRAT2
      if(NLIST.eq.1) then
      write (IMP,104) ICRAT1,ICRAT2
      end if
 104  format (' ICRAT:',2I5)

      swrlx(1)=(ICRAT1.eq.1)
      swrlx(2)=(ICRAT2.eq.1)
      swrlx(3)=.false.
      if (IPR.gt.0.and.NLIST.eq.1) write (IMP,*)'Relaxations:',swrlx(1)
#endif
      !
      CALL TAYLOR(2,KOST,EPS,Ftot)
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif
      ! Output the current texture
      if (NFILE.eq.1) call CURwriteBlock(IMP1,info)
#ifdef PEBP_ENABLED
      if ((KOST == hard_PEBP).and.(NPEBPx.eq.1)) then
            info = KS_writeState(IMP4)
      endif
#endif
      HGAMCALL = 0.D0
      HEPSCALL = 0.D0 ![PE:] Identified as obsolete statement
C
C     Main Loop over the Steps
C
      DO 8 ISTP=1,NSTP

C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C       WRTOT=0.0
c      IROT=1
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      TOTGEW=0.0
      do 50 i=1,3
      do 50 j=i,3
      STOT(i,j)=0.0
      RHOST(i,j)=0.0
  50  continue
      SG=0.
      Mavg=0.
      srh=0.
      HGAM=0.D0
      HEPS=0.D0 ![PE:] Identified as obsolete statement
      MEPS=sqrt(2./3.)*0.5*sqrt(sum((DG+transpose(DG))**2))
      call dynfil2(nrstep,F,GAXES,GEULR,CIJ,TG)
#ifndef NO_STDOUT       
      write (*,96) ISTP,GAXES
#endif
      if(NLIST.eq.1) then
      write (IMP,96) ISTP,GAXES
      end if
  96  format(' Step nr.',i5,5X,3f12.5)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C      if (IROT.eq.0.or.IW.gt.1) goto 70
      if (IW.gt.1) goto 70
C      if (IGLIJ.eq.1) then
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if(NLIST.eq.1) then
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
      write (IMP,3456) DG
      end if
 3456 format ('DG=',3(T10,3d12.3,/))
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
C
C     Get the 5x5 transformation matrix MACRO to morfol. GRAIN AXES
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C      if (IGLIJ.eq.1) then
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if(NLIST.eq.1) then
      write (IMP,3458) TG
      end if
      
 3458 format (' TG=',3(T10,3d12.3,/))
  70  if (nfile.eq.0.or.ISTP.gt.1) goto 44
C     INSTRUCTION ADDED IN LAMEL model:
      if (ISTP.gt.1) goto 44
      IF (NLIST.EQ.1) WRITE (IMP,112) ISTP
 112  FORMAT (//' DEFORMATION STEP ',I5,//)
      if (NRES.gt.0) write (IMP2,404) nrstep+1,NPOINT
 404  format (' Def. Step ',i5,'  Number of orientations',i5,/,T3,'ior'
     1 ,T10,'Wdot/DvM',T27,'Wdot',T37,'tau_ref',T56,'M',T64,'ratlon',
     2 T109,'RHO-SYMMETRIC',T172,'RHO-ROTATIONAL',T239,'STRESS',/,
     3 1x,278('*'))
      do 48 i=1,3
      GLR(i)=GEULR(i)*convf
  48  continue
C
  44  call MATPROD(Ftot,F,FMicro,3,3,3)
      FTINV=Ftot
      CALL MINV(FTINV,3,DMINV,L1MINV,L2MINV,9)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C      CMICRO=CMic0
C      call UPDATC(CMICRO,FTINV)
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C      if (IGLIJ.eq.1) then 
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if (NLIST.eq.1) then
          do i=1,3 
             write (IMP,407) (Ftot(j,i),j=1,3)
          enddo
      end if
      
 407      format (' Ftot ',3d15.7)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C      if (IROT.ne.1) goto 10
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      nrstep=nrstep+1
      ! Here DG = [L]*dt, where [L] is the velocity gradient 
      ! and the time step dt = 1.0
      call Ftensor(DG,F1,F2) 
      call UPDATF(F,F1)
      call UPDATC(CIJ,F2)
      call GETANG(CIJ,GAXES,GEULR,TG)
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
      ! We can choose not to update the texture data
      if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
            call DYNFIL3(nrstep,F,GAXES,GEULR,CIJ,TG)
      endif
#else          
      call DYNFIL3(nrstep,F,GAXES,GEULR,CIJ,TG)
#endif
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
      Mgrain=0.0
      GAMdot=0.0
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
      call DYNFIL4(ifil4,fi10b(L),PHI0b(L),fi20b(L),
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
      TAU=FTAU(GMM0,KOST)
      fi1=fi1b(laml)
      PHI=PHIb(laml)
      fi2=fi2b(laml)
      do 81 j=1,3
      do 81 i=1,3
      TRF(i,j)=TRFb(i,j,laml)
      TG(i,j)=TGb(i,j,laml)
      RHOSSa(i,j)=RHOSSb(i,j,laml)
  81  continue
#ifdef ALTAY_SUBROUTINE
      ! Collect the TRF for calculations of stresses later on.
      if (.not. astate%simulCalls(astate%this)%input%full_model) then
            WDOT = 0.
            TRFT = transpose(TRF)
      endif
#endif
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 27/10/2011
      if(laml.eq.1) then
      qgx=GEWFb(laml)
      GEWF=qgx
      else
      GEWF=qgx
      end if
      IF (NFILE.eq.0.or.ISTP.gt.1) goto 999
C
C     Output file with current condition (as it was before call of Taylor!)
C
c      if (IROT.ne.1) goto 999
      
      do 47 i=1,3
      GLR(i)=GEULRb(i,laml)*convf
  47  continue
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
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
 999  if (IW.le.1) then
            CALL  TAYLOR(3,KOST,EPS,Ftot)
#ifdef ALTAY_SUBROUTINE
            RCM_GUARD
#endif            
      endif
C      write (*,3211)
c 3211 format (' Just after Taylor')
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 2/1/2011     
C this modification is to suit for the output of stress     
      if(laml.eq.1) then
      ssqgx=GEWF
      else
      GEWF=ssqgx
      end if
cEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      TOTGEW=TOTGEW+GEWF
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 27/10/2011
C      IF (JW.EQ.0) GOTO 41
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
C  35  IF (JW.NE.2) GMM0=0.



C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 27/10/2011
c  34  IF (NFILE.eq.0.or.ISTP.gt.1) goto 41
C
C     Output file with current condition (as it was before call of Taylor!)
C
c      if (IROT.ne.1) goto 41
      
c     do 47 i=1,3
c      GLR(i)=GEULRb(i,laml)*convf
c  47  continue
c      write (IMP1,400) IOR,GEWF,fi1,PHI,fi2,GMM0,
c     1 ((Fb(i,j,laml),i=1,3),j=1,3),(GAXESb(j,laml),j=1,3),GLR
c 400  format (I6,f10.5,2X,3f10.5,2X,f10.5,3(2X,3F10.6),2(2x,3f10.5))
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE

  41  if (IW.gt.1) goto 23
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
c      if (IROT.eq.1) goto 59
c      WRTOT=WRTOT+WR*GEWF
c      goto 23
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
  59  do 56 i=1,5
      BUFSPV(i)=SPANV(i)
  56  continue

#ifdef ALTAY_SUBROUTINE
      ! altay-subroutine allows a way of calculating stresses
      ! without a call to TAYLR1.
      if (.not. astate%simulCalls(astate%this)%input%full_model) then
            call STR33(SPANT,SPANV)
            call MATPROD(bufsp,SPANT,TRF,3,3,3)
            call MATPROD(Ssam,TRFT,bufsp,3,3,3)
      else
            CALL TAYLR1(ISTP,IOR,NRES,TAU,GMMdot)
            RCM_GUARD
      endif
#else
C      if (IOR.eq.1.and.ISTP.eq.1) IPR=2
      CALL TAYLR1(ISTP,IOR,NFILE,TAU,GMMdot)
#endif      
      
      

C      if (IOR.eq.1.and.ISTP.eq.1) stop
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGC 4/11/2011
C    IEND is always equal to 0
C      if (iend.ne.1) goto 49
C      if (iend.eq.0) goto 49
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C      if (IGLIJ.eq.1.and.IPR.eq.2) stop
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
C      do 55 i=1,5
C      SPANV(i)=BUFSPV(i)
C  55  continue
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C      IGLIJ=1
C      IPR=2
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
C      call taylr1(ISTP,IOR,IEND,NFILE,TAU)
C     write(*,*) 'IEND=', IEND
C      stop
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
   49 if (NFILTW.eq.1) write (IMP3,398) ITW
 398  format (I3)
      do 51 i=1,3
      do 51 j=i,3
      STOT(i,j)=STOT(i,j)+Ssam(i,j)*GEWF
      RHOST(i,j)=RHOST(i,j)+RHOSsa(i,j)*GEWF
  51  continue
  63  SG=SG+WDOT*GEWF
      Mgrain=GMMdot/DELTAT
      Mavg=Mavg+Mgrain*GEWF
      ! norm2(RHOSsa)=||RHOSsa||=(||d-D||)/DELTAT with DELTAT=D_vM=sqrt(2/3)*||D|| 
      srh=srh+norm2(RHOSsa)*GEWF
      HGAM = HGAM + GMMdot*GEWF !Step time here implicitly assumed to be 1.0s      
      HEPS = HEPS + EPS * GEWF ![PE:] Identified as obsolete statement
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX¡¡4/11/2011
C      if (IROT.NE.1) goto 23
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      GMM1=GMM0+GMMdot !Step time here implicitly assumed to be 1.0s
#ifdef ALTAY_SUBROUTINE
      ! We can choose not to update the texture state
      if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
            call DYNFIL5(IOR,fi1,PHI,fi2,C2,GEWF,GMM1,
     1                   F,GAXES,GEULR,CIJ,TG,RHOSsa)
      endif
#else
      call DYNFIL5(IOR,fi1,PHI,fi2,C2,GEWF,GMM1,
     1 F,GAXES,GEULR,CIJ,TG,RHOSsa)
#endif
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
C     NEXT INSTRUCTION ADDED FOR LAMEL MODEL
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C      if (IROT.eq.0) goto 62
c      IF (NUNGL.NE.0) REWIND NUNGL   
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE                                   
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
65    continue

      Mavg=Mavg/TOTGEW
      ! DEFINITION: srh = (||d-D||) / ||D||
      srh=sqrt(2./3.)*srh/TOTGEW 
      SG=SG/TOTGEW
      !
      if (NMSSx /= 0) then
            call writeMSSRecord(IMP5,MEPS*(ISTP-1),MEPSTOT,HGAMCALL,
     &                          HGAMTOT,SHsam,Mavg,srh,info)
      endif
#ifdef ALTAY_SUBROUTINE
      ! Get the homogenized quantities:
      associate (callout => astate%simulCalls(astate%this)%output)
            callout%stress_tensor= SHsam
            callout%taylor_factor= Mavg
            callout%strain_rate_heterogeneity = srh
            callout%equivalent_stress= SG
            callout%effective_stress = sqrt(3.D0/2.D0)*norm2(SHsam)
            !suggestion for replacement of next statement:: callout%homogenised_slip = HGAMCALL
            callout%effective_strain = HEPSCALL ![PE:] Identified as obsolete statement
            !suggestion for replacement of next statement:: callout%homogenised_slip_tot = HGAMTOT            
            callout%effective_strain_tot = HEPSTOT ![PE:] Identified as obsolete statement
            callout%effective_macro_strain = MEPS*(ISTP-1)
            callout%effective_macro_strain_tot = MEPSTOT
      end associate
#endif
      !
      HGAM = HGAM / TOTGEW
      HGAMCALL = HGAMCALL + HGAM
      HEPS = HEPS / TOTGEW       ![PE:] Identified as obsolete statement
      HEPSCALL = HEPSCALL + HEPS ![PE:] Identified as obsolete statement
#ifdef ALTAY_SUBROUTINE
      ! We can choose not to update the internal state
      if (.not.astate%simulCalls(astate%this)%input%keep_state) then  
            HGAMTOT = HGAMTOT + HGAM 
            HEPSTOT = HEPSTOT + HEPS ![PE:] Identified as obsolete statement
            MEPSTOT = MEPSTOT + MEPS
      endif
#else
      HGAMTOT = HGAMTOT + HGAM
      HEPSTOT = HEPSTOT + HEPS ![PE:] Identified as obsolete statement
      MEPSTOT = MEPSTOT + MEPS
#endif
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C      JW=0
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if(NLIST.eq.1) then
      WRITE (IMP,105) ISTP,SG,Mavg,EPS
      end if
 105  FORMAT (' FOR STEP',I5,'  AVERAGE STRESS=',F15.5,'   AVERAGE M-VAL
     1UE=',F10.5,'  EFF. STRAIN EPS USED=',F10.5) 
  62  continue
C
C     End of loop over steps
  57  continue
   8  CONTINUE                                                          
  22  RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C  32  JW=-IW                                                            
C      GOTO 22
  32  return
C  this never happen!
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE                                                           
      END                                                               
