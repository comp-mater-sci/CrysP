C     ------------------------------------------------------------------
C     SAI-YI LI
C     KATHOLIEKE UNIVERSITEIT LEUVEN
C     DEPARTEMENT METAALKUNDE EN TOEGEPASTE MATERIAALKUNDE (MTM)
C     DE CROYLAAN 2
C     B-30001 HEVERLEE (LEUVEN)
C     TEL:+16 (0) 32/32.17.81
C     SAI-YI.LI@MTM.KULEUVEN.AC.BE
C
C     ERIC HOFERLIN
C     KATHOLIEKE UNIVERSITEIT LEUVEN
C     DEPARTEMENT METAALKUNDE EN TOEGEPASTE MATERIAALKUNDE (MTM)
C     DE CROYLAAN 2
C     B-30001 HEVERLEE (LEUVEN)
C     TEL:+16 (0) 32/32.12.45
C     ERIC.HOFERLIN@MTM.KULEUVEN.AC.BE
C     ------------------------------------------------------------------
      SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     & RPL,DDSDDT,DRPLDE,DRPLDT,STRAN,DSTRAN,
     & TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,CMNAME,NDI,NSHR,NTENS,
     & NSTATV,PROPS,NPROPS,COORDS,DROT,PNEWDT,CELENT,
     & DFGRD0,DFGRD1,NOEL,NPT,KSLAY,KSPT,KSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
C     INCLUDE '/SOFT/COM/ABAQUS58/SITE/ABA_PARAM_DP.INC'

C     ------------------------------------------------------------------
C     ABAQUS VARIABLES
C     ------------------------------------------------------------------
      INTEGER NOEL,NPT,KSLAY,KSPT,KSTEP,KINC
      CHARACTER*80 CMNAME
      DIMENSION STRESS(NTENS),STATEV(NSTATV),
     & DDSDDE(NTENS,NTENS),DDSDDT(NTENS),DRPLDE(NTENS),
     & STRAN(NTENS),DSTRAN(NTENS),TIME(2),PREDEF(1),DPRED(1),
     & PROPS(NPROPS),COORDS(3),DROT(3,3),DFGRD0(3,3),DFGRD1(3,3)
C
      PARAMETER (ONE=1.0D0,TWO=2.0D0,THREE=3.0D0,SIX=6.0D0)
C     DATA NEWTON,TOLER/10,1.0D-6/

C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      PARAMETER (NCHOUT=16,ISCR=0,MPAME=508,MAXME=508)
      DIMENSION LPARA(6),ISTRA(20),PAMET(MPAME),STRAT(99)
      DIMENSION DSTN(6),DSTN_RATE(9),QA(47),SIGMA(6),QB(47),SIGMB(6)
      DIMENSION C99(9,9)
      INTEGER   NCHSDV,NCHTAY
      PARAMETER(NCHSDV=17,NCHTAY=18)
C
      CHARACTER*250 FILEDIR, FILEOUT1, FILEOUT2, FILEOUT3, FILEOUT4
      CHARACTER*200 MET_PATH    
      CHARACTER*280 MET_FILE
      CHARACTER*50  FILENAME
      CHARACTER*256 OUTDIR
C
      INTEGER LENOUTDIR
      INTEGER NTINP,NT6,NTPRI,NCHLST
      INTEGER I,NLAWM,IDENT,J,IPTIAL,Idx
      INTEGER IDER,NCFE

      REAL*8 PARAM(16)  
      COMMON /KPARAM/ PARAM

      LOGICAL PRTU

      LOGICAL OK_MET
      COMMON /K_OKMET/OK_MET

      LOGICAL OK_OUT
      COMMON /K_OKOUT/OK_OUT

      INTEGER MAXEL3
      PARAMETER(MAXEL3=20000)
      INTEGER NELEM,MAXEL2,INDXEL(MAXEL3),OLDINC(MAXEL3)
      COMMON /KABAQUS/ NELEM,MAXEL2,INDXEL,OLDINC

      LOGICAL NEWINC, NEWITE
      COMMON /KNEWSTP/ NEWINC,NEWITE

      INTEGER INDELE
      COMMON /KINDELE/ INDELE

      INTEGER NITE
      COMMON /K_NITE/ NITE

      CHARACTER*80 PATH, FNAME
      COMMON /K_PATH/ PATH, FNAME

      INTEGER ELETAY
      COMMON /K_HIST1/ ELETAY
      REAL*8  HISTORY(6)
      COMMON /K_HIST2/ HISTORY

      INTEGER OUT6,OUT9,OUTOPT
      COMMON /KNCHOUT/ OUT6,OUT9,OUTOPT

      DOUBLE PRECISION N_UREF
      COMMON /KN_UREF/ N_UREF

C
      CHARACTER*100 KFACETID1, KFACETID2
      INTEGER KTYPEXPR, KTYPFACET, KSTGFACET, KCTRi, KCTRj
      INTEGER KNUMFACET, KORDFACET
      DOUBLE PRECISION KLAMFACET(2500), KSNSFACET(2500,5)
      COMMON /KFACETPAR1/ FFACETID1, KFACETID2
      COMMON /KFACETPAR2/ KTYPFACET, KSTGFACET, KNUMFACET,
     &                    KORDFACET, KTYPEXPR
      COMMON /KFACETPAR3/ KLAMFACET
      COMMON /KFACETPAR4/ KSNSFACET
C
      DOUBLE PRECISION AVR(5),DFR(5),FVALR,DDFR(5,5),UVR(5)
      DOUBLE PRECISION tauval, nhaval, gamval, zsep, zfacet, prflag
      PARAMETER (prflag=1.D0)
C
      OUT6     = 122
      OUT9     = 122
      OUTOPT   = 122
C
C     First time increment
C
      IF(.NOT.OK_OUT) THEN
C
        FILEDIR='C:\Facet\FacetFEM\inpfil\aba_facet.dir'
        OPEN(NCHOUT,FILE=FILEDIR,ERR=10)
        READ(NCHOUT,'(A)') MET_PATH
        READ(NCHOUT,'(A)') OUTDIR
        READ(NCHOUT,'(A)') FILENAME
        CLOSE(NCHOUT)
        GOTO 20
C
10      WRITE(6,540)
        CALL XIT
C
20      CONTINUE 
C
        LENOUTDIR=INDEX(OUTDIR,' ')
        OUTDIR(LENOUTDIR:LENOUTDIR)='\'
        J=INDEX(FILENAME,' ')
C
        FILEOUT1=OUTDIR(1:LENOUTDIR)//FILENAME(1:J-1)//'.out'
        FILEOUT2=OUTDIR(1:LENOUTDIR)//FILENAME(1:J-1)//'.tay'
C
        OPEN(NCHOUT,FILE=FILEOUT1,STATUS='REPLACE')
        OPEN(NCHTAY,FILE=FILEOUT2,STATUS='REPLACE')
C
        ELETAY= 1
        IF (prflag==1.D0)
     &    WRITE(NCHOUT,'(3A)')'     KSTEP        PROPS(1)    PROPS(2)',
     &          '    PROPS(3)       PROPS(4)     PROPS(5)    PROPS(6)',
     &          '      NOEL       NPT      KINC            DTIME'
C
        OK_OUT=.TRUE.
C
      END IF 
C     ------------------------------------------------------------------
C     DETECT IF IT IS A NEW INCREMENT
C     ------------------------------------------------------------------
      NEWINC=.FALSE.
      I=1
      DO WHILE(INDXEL(I).NE.NOEL)
         I=I+1
      ENDDO
      INDELE=I
      IF(KINC.NE.OLDINC(INDELE))NEWINC=.TRUE.
      IF(NEWINC) OLDINC(INDELE)=KINC

C     ------------------------------------------------------------------
C     DETECT IF IT IS A NEW ITERATION
C     ------------------------------------------------------------------
      NEWITE=.FALSE.
      IF(NITE.EQ.NELEM) THEN
         NEWITE=.TRUE.
         NITE=0
      ENDIF
      NITE=NITE+1

C     ------------------------------------------------------------------
C     SET LOCAL VARIABLES
C     ------------------------------------------------------------------
      PRTU =.FALSE.   ! .TRUE. FOR PRINTS IN UMAT
      
      DO I=1,NPROPS
        PARAM(I) = PROPS(I)
      ENDDO

      DO I=1,NSTATV
        QA(I) = STATEV(I)                    
      ENDDO

      DO I=1,6
        SIGMA(I) = STRESS(I)
      ENDDO                 

      IF(PRTU) THEN
        WRITE(NCHOUT,970) (SIGMA(I),I=1,6)
        WRITE(NCHOUT,950) (QA(I),I=1,NSTATV)
      ENDIF
C
C    Open the texture file and  .nt6 and .oum
C
      IF(.NOT.OK_MET) THEN
C
        NTINP=121
        NT6  =122
        NTPRI=123
        NCHLST=NT6
C
        I=INDEX(MET_PATH,' ')
        MET_PATH(I:I)='\'
        MET_FILE=MET_PATH(1:I)//CMNAME
        OPEN (NTINP,FILE=MET_FILE)
C
        FILEOUT3=OUTDIR(1:LENOUTDIR)//FILENAME(1:J-1)//'.nt6'
        OPEN(NT6,FILE=FILEOUT3,STATUS='REPLACE')
C
        FILEOUT4=OUTDIR(1:LENOUTDIR)//FILENAME(1:J-1)//'.oum'
        OPEN(NTPRI,FILE=FILEOUT4,STATUS='REPLACE')
C
        IF( (PROPS(3) .EQ. 2.D0) .OR. (PROPS(3) .EQ. 3.D0) .OR.
     &      (PROPS(3) .EQ. 4.D0) ) THEN
          CALL KREAD2(PAMET,MPAME,NTINP,NT6,NTPRI)
          NLAWM=1       ! number of materials (Allow more than 1 material)
          CALL KINIF3(PAMET,MAXME,NLAWM,NCHLST)
        END IF
        IF (PROPS(3) .EQ. 30.0D0) THEN
          CALL KRDLINES(NTINP,3)
          READ(NTINP,'(A)') KFACETID1
          READ(NTINP,'(A)') KFACETID2 
          CALL KRDLINES(NTINP,1)
          READ(NTINP,'(7X,I3,17X,I3)') KTYPFACET, KSTGFACET
          CALL KRDLINES(NTINP,2)
          READ(NTINP,'(I5)') KNUMFACET
          CALL KRDLINES(NTINP,2)
          READ(NTINP,'(I5)') KORDFACET
          CALL KRDLINES(NTINP,5)
          DO KCTRi = 1, KNUMFACET
            READ(NTINP,'(F8.5)') KLAMFACET(KCTRi)
          END DO
          CALL KRDLINES(NTINP,5)
          DO KCTRi = 1, KNUMFACET
            READ(NTINP,'(5(F8.5,1X))') KSNSFACET(KCTRi,1),
     &             KSNSFACET(KCTRi,2), KSNSFACET(KCTRi,3),
     &             KSNSFACET(KCTRi,4), KSNSFACET(KCTRi,5)   
          END DO
        END IF
C
C       Convert Material properties sig0, eps0, n to tau0, gam0, n´ 
C       for SEP & FACET methods
C
        IF( (PROPS(3) .EQ. 2.D0) .OR. (PROPS(3) .EQ. 3.D0) .OR.
     &      (PROPS(3) .EQ. 4.D0) ) THEN
          CALL KGETZSEP(zsep, PAMET, MPAME)
          tauval = PROPS(4) / zsep
          nhaval = PROPS(5)
          gamval = PROPS(6)*zsep
          IPTIAL = PAMET(6)
          WRITE(ISCR,'(A,E13.5)')
     &      '   Conversion Factor........... z    = ', zsep
          WRITE(ISCR,'(A,I3)')
     &      '   Type of SEP Expression...iptial = ', IPTIAL
        ELSE IF(PROPS(3) .EQ. 30.D0) THEN
          CALL KGETZFACET(zfacet)
          tauval = PROPS(4) / zfacet
          nhaval = PROPS(5)
          gamval = PROPS(6)*zfacet
          WRITE(ISCR,'(A,E13.5)')
     &       '   Conversion Factor........... z    = ', zfacet
          WRITE(ISCR,'(A,I3)')
     &       '   Type of Facet Expression...iptial = ', KTYPFACET
        END IF 
C
C    Print out the input data 
C
        IF(PROPS(3).EQ.1.0D0) THEN
          WRITE(ISCR,925) (PROPS(I),I=1,2),(PROPS(I),I=4,6)
        ELSE IF(PROPS(3).EQ.2.D0) THEN
          WRITE(ISCR,924) (PROPS(I),I=1,2),tauval,nhaval,gamval
        ELSE IF(PROPS(3).EQ.3.D0) THEN
          WRITE(ISCR,923) (PROPS(I),I=1,2),tauval,nhaval,gamval
        ELSE IF(PROPS(3).EQ.4.D0) THEN
          WRITE(ISCR,922) (PROPS(I),I=1,2),tauval,nhaval,gamval
        ELSE IF(PROPS(3) .EQ. 30.D0) THEN
          WRITE(ISCR,3000)(PROPS(I),I=1,2),tauval,nhaval,gamval
        ELSE
          WRITE(ISCR,921)
          CALL XIT
        ENDIF
C
        WRITE(ISCR,930) MET_FILE
        OK_MET = .TRUE.
      END IF
C     ------------------------------------------------------------------
C     PREPARE FIXED INPUT AND THEN CALL
C     ------------------------------------------------------------------
      IF(PROPS(3) .EQ. 2.D0) THEN
C       CALL KGETZSEP(zsep, PAMET, MPAME)    ! Must be uncommented for texture update (i.e. for new PAMET)
        PARAM(4) = PROPS(4) / zsep
        PARAM(5) = PROPS(5)
        PARAM(6) = PROPS(6)*zsep
      ELSE IF(PROPS(3) .EQ. 30.D0) THEN
        PARAM(4) = PROPS(4) / zfacet
        PARAM(5) = PROPS(5)
        PARAM(6) = PROPS(6)*zfacet
      END IF
C     IF   (prflag==1.D0)
      IF ( (prflag==1.D0) .AND. (KINC==1). AND. (NOEL==1) )
     &  WRITE(NCHOUT,3002) KSTEP, PARAM(1),PARAM(2),PARAM(3),
     &        PARAM(4),PARAM(5),PARAM(6),NOEL,NPT,KINC,DTIME
C
      ISTRA(1) = 1              ! -> IJUMP=1: COMPLIANCE TENSOR REQUIRED
      ISTRA(2) = -99
      ISTRA(5) = 3              ! USE CLOC AS DDSDDE
      ISTRA(11)= KINC
      ISTRA(12)= NOEL
      ISTRA(13)= NPT
      LPARA(1) = 504
      LPARA(2) = NSTATV
      LPARA(3) = 6
      LPARA(5) = 1              ! NUMBER OF SUB-INTERVAL PER INCREMENT
      LPARA(6) = 6              ! USE THE SERIES EXPANSION OF BVB
      IDENT    = 1345           ! SET PRINT FLAG IW ON IF IDENT=1245
      STRAT(1) = 1.0D-6         ! FOR NUMERICAL EVALUTION OF COMPL. TENSOR      

C     IF(NOEL.EQ.3117) IDENT=1245
C     WRITE(ISCR,915) KSTEP,NOEL,KINC,NPT,DTIME

C     ------------------------------------------------------------------
C     DUMP THE COMMON BLOCK HISTORY TO TEXT FILE
C     CONVENTION: 
C                HISTORY(1)=D11
C                HISTORY(2)=D22
C                HISTORY(3)=D33
C                HISTORY(4)=D12
C                HISTORY(5)=D13
C                HISTORY(6)=D23
C     ------------------------------------------------------------------
      IF(ELETAY.GT.0.AND.NOEL.EQ.ELETAY) THEN
         WRITE(NCHTAY,'(I5,18E13.5)') KINC,
     .                                HISTORY(1),HISTORY(4),HISTORY(5),
     .                                HISTORY(4),HISTORY(2),HISTORY(6),
     .                                HISTORY(5),HISTORY(6),HISTORY(3),
     .                                DROT(1,1),DROT(1,2),DROT(1,3),
     .                                DROT(2,1),DROT(2,2),DROT(2,3),
     .                                DROT(3,1),DROT(3,2),DROT(3,3)

         DO I=1,6
            HISTORY(I)=0.0D0
         ENDDO
      ENDIF
C     ------------------------------------------------------------------
C     CALL CONSTITUTIVE LAW
C     ------------------------------------------------------------------
      DSTN(1)= DSTRAN(1)        ! 11
      DSTN(2)= DSTRAN(2)        ! 22
      DSTN(3)= DSTRAN(3)        ! 33
      DSTN(4)= DSTRAN(4)*0.5D0  ! 11 ABAQUS USES ENG. STRAINS
      DSTN(5)= DSTRAN(5)*0.5D0  ! 13 ABAQUS USES ENG. STRAINS
      DSTN(6)= DSTRAN(6)*0.5D0  ! 23 ABAQUS USES ENG. STRAINS
      IF (PRTU) THEN   
        WRITE(NCHOUT,890) (DSTN(I),I=1,6)
      ENDIF
      DSTN_RATE(1)=DSTN(1)/DTIME ! 11
      DSTN_RATE(2)=DSTN(4)/DTIME ! 21
      DSTN_RATE(3)=DSTN(5)/DTIME ! 31
      DSTN_RATE(4)=DSTN(4)/DTIME ! 12
      DSTN_RATE(5)=DSTN(2)/DTIME ! 22
      DSTN_RATE(6)=DSTN(6)/DTIME ! 32
      DSTN_RATE(7)=DSTN(5)/DTIME ! 13
      DSTN_RATE(8)=DSTN(6)/DTIME ! 23
      DSTN_RATE(9)=DSTN(3)/DTIME ! 33
      
      CALL KANI3VH(C99,SIGMB,QB,DSTN_RATE,SIGMA,QA,PARAM,
     &             LPARA,PAMET,DTIME,NT6,STRAT,ISTRA,IDENT)

      CALL KMAKE66 (C99,DDSDDE)
      DO I=1,6
         DO J=4,6
            DDSDDE(I,J)=DDSDDE(I,J)/2.0D0 !for abaqus strain convension
         ENDDO
      ENDDO

      IF (PRTU) THEN
         WRITE(NCHOUT,870) ((DDSDDE(I,J),J=1,6),I=1,6)
      ENDIF
C     ------------------------------------------------------------------
C     DIRECT TRANSFER FROM QB(47) TO STATEV(47)
C     ------------------------------------------------------------------
      DO I=1,NSTATV
        STATEV(I)=QB(I)
      ENDDO

      DO I=1,6
        STRESS(I)=SIGMB(I)
      ENDDO
C
      IF(PRTU) THEN
         WRITE(NCHOUT,850) (STRESS(I),I=1,6)
         WRITE(NCHOUT,830) (STATEV(I),I=1,NSTATV)
      ENDIF
C     ------------------------------------------------------------------
980   FORMAT(  1X,'MAXEL2 AND MAXEL3 SHOULD BE IDENTICAL',/,
     .         1X,'WHEREAS MAXEL2=',I5,' AND MAXEL3=',I5 ,/)
978   FORMAT(///,3X,'READ SDV FROM:', A) 
977   FORMAT(/,3X,'TAYLOR FILE NAME:', A) 
976   FORMAT(/,3X,'SDV FILE NOT FOUND: SDV SET TO 0 BY SDVINI')
974   FORMAT(/,3X,'ERROR WHEN READING SDV FOR ELEMENT:', I)
970   FORMAT (/,'SIGMA  = ',6E13.5)
950   FORMAT (/,'QA     = ',/,8(6E13.5,/))
930   FORMAT (/,3X,'READ TEXTURE DATA FROM: ',A)
925   FORMAT (/,3X,'CONSTITUTIVE MODEL ......... <vMises>',     /,
     .          3X,'YOUNG MODULUS .............. EMOD = ',E13.5,/,
     .          3X,'POISSON RATIO .............. ANU  = ',E13.5,/,
     .          3X,'ISOTROPIC HARDENING                 ',      /,
     .          3X,'   -->  SIG_VM=K*(EPS0+EPS_VM)**N   ',      /,
     .          3X,'SWIFT COEFFICIENT .......... K    = ',E13.5,/,
     .          3X,'SWIFT EXPONENTT ............ N    = ',E13.5,/,
     .          3X,'SWFIT OFFSET STRAIN ........ EPS0 = ',E13.5,/)
924   FORMAT (/,3X,'CONSTITUTIVE MODEL ......... <TexIso>' ,    /,
     .          3X,'YOUNG MODULUS............... EMOD = ',E13.5,/,
     .          3X,'POISSON RATIO............... ANU  = ',E13.5,/,
     .          3X,'ISOTROPIC HARDENING                 ',      /,
     .          3X,'   -->  TAU=K*(G0+GAMMA)**N         ',      /,
     .          3X,'SWIFT COEFFICIENT .......... K    = ',E13.5,/,
     .          3X,'SWIFT EXPONENT ............. N    = ',E13.5,/,
     .          3X,'SWFIT OFFSET STRAIN ........ G0   = ',E13.5,/)
923   FORMAT (/,3X,'CONSTITUTIVE MODEL ............ <TexMic>',      /,
     .          3X,'YOUNG MODULUS ................. EMOD = ' ,E13.5,/,
     .          3X,'POISSON RATIO ................. ANU  = ' ,E13.5,/,
     .          3X,'INITIAL CRSS .................. TAU0 = ' ,E13.5,/,
     .          3X,'INITIAL BACK-STRESS ........... X0   = ' ,E13.5,/,
     .          3X,'ACTIVE DISLOCATIONSATURATION .. SSAT = ' ,E13.5,/,
     .          3X,'SATURATION FOR STATE VARIABLE R RSAT = ' ,E13.5,/,
     .          3X,'POLARITY COEFFICIENT .......... CP   = ' ,E13.5,/,   
     .          3X,'LATENT DISLOCATION COEFFICIENT. CSL  = ' ,E13.5,/,
     .          3X,'ACTIVE DISLOCATION COEFFICIENT. CSD  = ' ,E13.5,/,
     .          3X,'BACK STRESS COEFFICIENT ....... CX   = ' ,E13.5,/,
     .          3X,'PACING RATE FOR STATE VAR R ... CR   = ' ,E13.5,/,
     .          3X,'EXPONENT FOR HP IF P:A>= 0 .... NP   = ' ,E13.5,/,
     .          3X,'LATENT DISLOCATION EXPONENT ... NL   = ' ,E13.5,/,
     .          3X,'ISOTROPIC FRACTION ............ M    = ' ,E13.5,/,
     .          3X,'LATENT CONTRIBUTION TO XSAT ... R    = ' ,E13.5,/)
922   FORMAT (/,3X,'CONSTITUTIVE MODEL ............ <TexMic> ALUM ',/,
     .          3X,'YOUNG MODULUS ................. EMOD = ' ,E13.5,/,
     .          3X,'POISSON RATIO ................. ANU  = ' ,E13.5,/,
     .          3X,'INITIAL CRSS .................. TAU0 = ' ,E13.5,/,
     .          3X,'INITIAL BACK-STRESS ........... X0   = ' ,E13.5,/,
     .          3X,'ACTIVE DISLOCATIONSATURATION .. SSAT = ' ,E13.5,/,
     .          3X,'SATURATION FOR STATE VARIABLE R RSAT = ' ,E13.5,/,
     .          3X,'POLARITY COEFFICIENT .......... CP   = ' ,E13.5,/,   
     .          3X,'LATENT DISLOCATION COEFFICIENT. CSL  = ' ,E13.5,/,
     .          3X,'ACTIVE DISLOCATION COEFFICIENT. CSD  = ' ,E13.5,/,
     .          3X,'BACK STRESS COEFFICIENT ....... CX   = ' ,E13.5,/,
     .          3X,'PACING RATE FOR STATE VAR R ... CR   = ' ,E13.5,/,
     .          3X,'EXPONENT FOR HP IF P:A>= 0 .... NP   = ' ,E13.5,/,
     .          3X,'LATENT DISLOCATION EXPONENT ... NL   = ' ,E13.5,/,
     .          3X,'ISOTROPIC FRACTION ............ M    = ' ,E13.5,/,
     .          3X,'LATENT CONTRIBUTION TO XSAT ... R    = ' ,E13.5,/)
3000  FORMAT (/,3X,'CONSTITUTIVE MODEL ......... <FaceTexIso>' ,    /,
     &          3X,'YOUNG MODULUS............... EMOD = ',E13.5,/,
     &          3X,'POISSON RATIO............... ANU  = ',E13.5,/,
     &          3X,'ISOTROPIC HARDENING                 ',      /,
     &          3X,'   -->  TAU=K*(G0+GAMMA)**N         ',      /,
     &          3X,'SWIFT COEFFICIENT .......... K    = ',E13.5,/,
     &          3X,'SWIFT EXPONENT ............. N    = ',E13.5,/,
     &          3X,'SWFIT OFFSET STRAIN ........ G0   = ',E13.5,/)
3002  FORMAT(I10,3X,E13.5,6X,F5.2,7X,F5.2,3X,E13.5,5X,F8.5,4X,F8.5,
     &       3I10,3X,E14.8)
921   FORMAT('KWARNING! OUT OF RANGE PARAM(3):',E13.5)
915   FORMAT (/,3X,' STEP = ',I5,' NOEL = ',I5,' KINC = ',I5,
     .             '  NPT = ',I5,' DTIME = ',E13.5,/)
890   FORMAT(/,'DSTN   = ',6E13.5)
870   FORMAT(/,'DDSDDE = ',/,6(6E13.5,/))
850   FORMAT(/,'SIGMB  = ',6E13.5)
830   FORMAT(/,'QB     = ',8(6E13.5,/))
810   FORMAT(/,3X,'NUMBER OF ELEMENTS: ',I5,/,
     .         3X,'10 FIRST ELEMENTS:  ',10I5)
540   FORMAT(/,3X,'INCORRECT READIN A FILE ABA.DIR'/)
C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
      SUBROUTINE SDVINI(STATEV,COORDS,NSTATV,NCRDS,NOEL,NPT,LAYER,KSPT)
C     ------------------------------------------------------------------
      DIMENSION STATEV(NSTATV),COORDS(NCRDS)
      INTEGER  I
      INTEGER  MAXEL1
      PARAMETER (MAXEL1=20000)

      REAL*8  N_UREF
      COMMON /KN_UREF/ N_UREF

      INTEGER NELEM,MAXEL2,INDXEL(MAXEL1),OLDINC(MAXEL1)
      COMMON /KABAQUS/ NELEM,MAXEL2,INDXEL,OLDINC

      INTEGER NITE
      COMMON /K_NITE/ NITE
C
      DO I=1,NSTATV
        STATEV(I)=0.0
      ENDDO

C     ------------------------------------------------------------------
C     COMPUTES HOW MANY ELEMENTS
C     ------------------------------------------------------------------
      MAXEL2=MAXEL1
      NELEM=NELEM+1
      IF(NELEM.GT.MAXEL1) THEN
         WRITE(6,100) NELEM,MAXEL1
         CALL XIT
      ENDIF
      INDXEL(NELEM)=NOEL
      OLDINC(NELEM)=0

C     ------------------------------------------------------------------
C     INITIALISATION FOR THE COUNT OF NEW ITERATIONS
C     ------------------------------------------------------------------
      NITE=NELEM
      WRITE(*,'(A,2I6)') 'SDVINI-nel,maxel',NELEM,MAXEL1

C     ------------------------------------------------------------------
C     INITIALISE N_UREF TO A NEGATIVE VALUE
C     ------------------------------------------------------------------
      N_UREF=-99.9D9

C     ------------------------------------------------------------------
100   FORMAT('NELEM SHOULD NOT BE LARGER THAN MAXEL',/,
     .       'NELEM IS: ',I5,' AND MAXEL IS: ',I5)
C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
C     VER10
C     ------------------------------------------------------------------
C     ERIC HOFERLIN
C     KATHOLIEKE UNIVERSITEIT LEUVEN
C     DEPARTEMENT METAALKUNDE EN TOEGEPASTE MATERIAALKUNDE (MTM)
C     DE CROYLAAN 2
C     B-30001 HEVERLEE (LEUVEN)
C     TEL:+16 (0) 32/32.12.45
C     ERIC.HOFERLIN@MTM.KULEUVEN.AC.BE
C
C     SAIYI LI
C     KATHOLIEKE UNIVERSITEIT LEUVEN
C     DEPARTEMENT METAALKUNDE EN TOEGEPASTE MATERIAALKUNDE (MTM)
C     DE CROYLAAN 2
C     B-30001 HEVERLEE (LEUVEN)
C     TEL:+16 (0) 32/32.17.81
C     SAI-YI.LI@MTM.KULEUVEN.AC.BE
C     ------------------------------------------------------------------
C     TO BE CHECKED:
C        01. THE ANALYTICAL INTEGRATIONS
C        02. IN KANI3VH, IF(TAU=0) ... QUID IF STG IS NOT 0 INITIALLY
C     ------------------------------------------------------------------
C     THIS SOURCE FILE CONTAINS THE FOLLOWING SUBROUTINES/FUNCTION
C
C        01. KANI3VH
C        02. KTEXHAR
C        03. KBEULER
C        04. KDETECT
C        05. KYLPMUL
C        06. KBE13VM
C        07. KANGCTR
C        08. KYLPBIS
C        09. KYLPERR
C        10. KMODELA
C        11. KMODPLA
C        12. KCALMPL
C        13. KCALMEP
C        14. KHARDEN
C        15. KASOLVE
C        16. KLENGTH
C        17  KLENGT6
C        18. KMODCON
C        19. KX5_2X6
C        20. KX52X33
C        21. KX6_2X5
C        22. KX62X33
C        23. KX332X5
C        24. KX332X6
C        25. KMAKE66
C        26. KMAKE99
C        27. KPRODUC
C        28. KPRTMAT
C        29. KCOFBCK
C        30. KINIDPL
C        31. KINIARG
C        32  KFDJAC
C        33. KAFDJAC
C        34. KFUNCV5
C        35. KVMISES
C        36. ZXIT
C        37. KINISTA
C        38. KPRTSTA
C        39. KDMATIN
C        40. KNEWT
C        41. KLNSRCH -> FROM NUMERICAL RECIPES + CORRECTION '.0D0'
C        42. KLUDCMP -> FROM NUMERICAL RECIPES + CORRECTION '.0D0'
C        43. KLUBKSB -> FROM NUMERICAL RECIPES + CORRECTION '.0D0'
C        44. KFMIN   (FUNCTION)
C
C     ------------------------------------------------------------------
C     OPTIONS TO BE CHOSEN BEFORE COMPILATION
C
C        KANI3VH: IW......CONTROLING ONLY THE OUTPUT OF KANI3VH
C        KTEXHAR: IW......CONTROLING ALL THE OUPUT BELOW KTEXHAR
C                 USEELA..IF TRUE, ELASTIC MOD WHEN THE PT REMAINS
C                         ON THE YL, ELSE CONTINU PLASTIC MOD
C        ITRY3  : 0 OR 1..YLP CUT THE FISHTAILS IF ITRY3=1
C        KYLPMUL: IW2.....CONTROLING OUTPUT OF YLP AND BE4INI
C
C     ------------------------------------------------------------------
C     EXTERNAL SOURCE FILE NEEDED
C        01. KSERIES.F
C
************************************************************************ 
      SUBROUTINE  KANI3VH(C,SIGMB,QB,VGRAD,SIGMA,QA,
     .                    P,LPARA,S,DELTAT,NT6,STRAT,ISTRA,IDENT)
C     ------------------------------------------------------------------
CU    SUBROUTINE KANI3VH USES SUBROUTINES
CU       KTEXHAR
CU       KVMISES
CU       XIT      (DEDICATED EXIT ROUTINE OF ABAQUS)
C     ------------------------------------------------------------------
CA    CALL KANI3VH(C,SIGMB,QB,VGRAD,SIGMA,QA,
CA   .             P,LPARA,S,DELTAT,NT6,STRAT,ISTRA,IDENT)
C     ------------------------------------------------------------------
CB    SUBROUTINE KANI3VH USES COMMON BLOCKS
CB       KNCHOUT
CB       KOLDTAU
CB       KNEWSTP
CB       KINDELE
CB       KXSAT
C     ------------------------------------------------------------------
CV    SOME VARIABLES FOR SUBROUTINE KANI3VH
CV    ARGUMENTS IN
CV       VGRAD(3,3) STRAIN RATE, NOT VELOCITY GRADIENT
CV       SIGMA(6)   SIGXX,SIGYY,SIGZZ,SIGXY,SIGXZ,SIGYZ
CV       QA(47)     STATE VARIABLE VECTOR
CV       P(14)      MATERIAL CONSTANT
CV
CV                  SPECIAL CASE 1 : "VMISES" MODEL
CV                  P(3)=1.0D0
CV                  QA(1)      YIELD INDICATOR (0.0D0=ELASTIC)
CV                  QA(2)      VON MISES EQUIVALENT STRAIN
CV                  QA(3)      YIELD LIMIT
CV                  P(1)       YOUND MODULUS
CV                  P(2)       POISSON RATIO
CV                  P(3)       CHOICE, 1=VMISES, 2=TEXISO, 3=TEXMIC
CV                  P(4)       COFK
CV                  P(5)       COFN ... SIGV=COFK*(EPS0+EPS_VM)**COFN
CV                  P(6)       EPS0
CV                  SET P( 7..16) TO 99.0D0
CV
CV                  SPECIAL CASE 2: "TEXISO" MODEL
CV                             ROUTINE TEXHAR BUT WITH
CV                             ISOTROPIC EXPONENTIAL HARDENING
CV                  P(3)=2.0D0
CV                  QA(1)      YIELD INDICATOR (0=ELASTIC)
CV                  QA(2)      CRYSTALLOGRAPHIC PLASTIC STRAIN GAM
CV                  QA(3)      CRITICAL RESOLVED SHEAR STRESS
CV                  P(1)       YOUND MODULUS
CV                  P(2)       POISSON RATIO
CV                  P(3)       CHOICE: 1=VMISES, 2=TEXISO, 3=TEXMIC
CV                  P(4)       COFK
CV                  P(5)       COFN ... TAU=COFK*(GAM0+GAM)**COFN
CV                  P(6)       GAM0
CV                  SET P( 7..16) TO 99.0D0
CV
CV                  GENERAL CASE  : "TEXMIC" MODEL
CV                                  ROUTINE TEXHAR WITH
CV                                  COMBINED TEXTURE-MICROSTRUCTURE
CV                                  HARDENING MODEL GENERALISED FOR
CV                                  STEEL AND ALUMINIUM
CV                  P(3).EQ.3.0D0
CV                  QA(1)      YIELD INDICATOR (0=ELASTIC)
CV                  QA(2)      MEASURE OF EQUIVALENT PLASTIC STRAIN
CV                  QA(3)      MEASURE OF SIZE OF YIELD LOCUS
CV                  QA(4)      STATE VARIABLE R
CV                  QA( 5.. 9) BACK STRESS
CV                  QA(10..14) POLARITY
CV                  QA(15..39) STRENGTH OF DISLOCATION STRUCTURE
CV                  QA(40)     STRENGTH OF THE DISLOCATION
CV                             STRUCTURE CORRESPONDING TO THE LATENT
CV                             SLIP SYSTEM AT THE BEGINNING OF THE
CV                             INCREMENT
CV                  QA(42..46) THE PLASTIC STRAIN MODE ENCOUNTERED
CV                             THE LAST TIME THE POINT WAS PLASTIC
CV                  QA(47)     THE LENGTH OF THE BACK-STRESS
CV                  P(1)       YOUND MODULUS
CV                  P(2)       POISSON RATIO
CV                  P(3)       CHOICE: 1=VMISES, 2=TEXISO, 3=TEXMIC
CV                  P(4)       TAU0: INITIAL CRIT. RESOLVED SHEAR STRESS
CV                  P(5)       X0  : INITIAL BACK-STRESS
CV                  P(6)       SSAT: ACTIVE DISLOCATION SATURATION
CV                  P(7)       RSAT: SATURATION FOR STATE VARIABLE R
CV                  P(8)       CP  : POLARITY COEFFICIENT
CV                  P(9)       CSL : LATENT DISLOCATION COEFFICIENT
CV                  P(10)      CSD : ACTIVE DISLOCATION COEFFICIENT
CV                  P(11)      CX  : BACK-STRESS COEFFICIENT CX
CV                  P(12)      CR  : PACING RATE FOR STATE VARIABLE R
CV                  P(13)      NP  : EXP. FOR HP IS P:A>=0
CV                  P(14)      NL  : LATENT DISLOCATION EXPONENT
CV                  P(15)      M   : ISOTROPIC FRACTION
CV                  P(16)      R   : LATENT CONTRIBUTION TO XSAT
CV
CV                  TO RETRIEVE THE FIRST VERSION OF THE MODEL (CASE
CV                  OF MILD STEEL), SET CSL=CSD, RSAT=0 AND CR=0
CV
CV                  ALUMINIUM CASE  : "TEXMIC" MODEL FOR ALUMINIUM
CV
CV                  P(3).EQ.4.0D0
CV                  QA(1)      YIELD INDICATOR (0=ELASTIC)
CV                  QA(2)      MEASURE OF EQUIVALENT PLASTIC STRAIN
CV                  QA(3)      MEASURE OF SIZE OF YIELD LOCUS
CV                  QA(4)      STATE VARIABLE R
CV                  QA( 5.. 9) BACK STRESS
CV                  QA(10..14) POLARITY (0 FOR ALUMINIUM)
CV                  QA(15..39) QA(15) IS SCALAR STATE VARIABLE S
CV                             QA(16..39)=0
CV                  QA(40)     STRENGTH OF THE DISLOCATION
CV                             STRUCTURE CORRESPONDING TO THE LATENT
CV                             SLIP SYSTEM AT THE BEGINNING OF THE
CV                             INCREMENT, 0 FOR ALUMINIUM
CV                  QA(42..46) THE PLASTIC STRAIN MODE ENCOUNTERED
CV                             THE LAST TIME THE POINT WAS PLASTIC
CV                  QA(47)     THE LENGTH OF THE BACK-STRESS
CV                  P(1)       YOUND MODULUS
CV                  P(2)       POISSON RATIO
CV                  P(3)       CHOICE: 1=VMISES, 2=TEXISO, 3=TEXMIC
CV                  P(4)       TAU0: INITIAL CRIT. RESOLVED SHEAR STRESS
CV                  P(5)       X0  : INITIAL BACK-STRESS
CV                  P(6)       SSAT: ACTIVE DISLOCATION SATURATION
CV                  P(7)       RSAT: SATURATION FOR STATE VARIABLE R
CV                  P(8)       CP  : 0
CV                  P(9)       CSL : 0
CV                  P(10)      CSD : ACTIVE DISLOCATION COEFFICIENT
CV                  P(11)      CX  : BACK-STRESS COEFFICIENT CX
CV                  P(12)      CR  : PACING RATE FOR STATE VARIABLE R
CV                  P(13)      NP  : 0
CV                  P(14)      NL  : 0
CV                  P(15)      M   : ISOTROPIC FRACTION
CV                  P(16)      R   : 0
CV
CV                  SPECIAL VALUES FOR P
CV
CV                  P( 7)=-99  KVMISES ROUTINE USES ELASTIC MODULUS WHEN
CV                             VERY SMALL PLASTIC STEP
CV                  P( 8)=-99  KVMISES LINEARISES THE EXPONENTIAL
CV                             HARDENING CURVE
CV                  P( 9)=-98  LOAX3D USES MODCOR2 INSTEAD OF MODCOR
CV                       =-97  LOAX3D USES NEITHER MODCOR2 NOR MODCOR
CV                  P(10)=-99  STRESS CORRECTION OF COMPLIANCE MATRIX IN
CV                             BLZ3DB.F ARE SKIPPED WHEN ABS(ILAW)=504
CV                  P(11)=-99  LOAX3D USES ROT99EH INSTEAD OF ROT9X9
CV
CV
CV       DELTAT     TIME STEP INCREMENT
CV       LPARA(*)   INTEGER VECTOR WITH LAW OPTIONS
CV                  ABS(LPARA(5)) NUMBER OF SUBSTEP FOR THE
CV                             CONSTITUTIVE INTEGRATION
CV       STRAT(*)   REAL EXECUTION PARAMETERS
CV                  STRAT(1)   PERTEPS WHEN NUMERICAL COMPLIANCE
CV                             IS ASKED IN KANI3VH
CV       ISTRA(*)   INTEGER EXECUTION PARAMETERS AND VARIABLES
CV                  ISTRA( 1)  .NE.0 COMPUTE COMPLIANCE TENSOR
CV                             .EQ.0 COMPLIANCE TENSOR IS NOT REQUIRED
CV                  ISTRA( 2)  CURRENT ITERATION NUMBER
CV                  ISTRA(11)  CURRENT STEP NUMBER (A STEP=SEVERAL ITER)
CV                  ISTRA(12)  CURRENT ELEMENT NUMBER
CV                  ISTRA(13)  CURRENT INTEGRATION POINT NUMBER
CV       S(*)       VECTOR WITH SERIES EXPANSION INFO AND COEF
CV       NT6        PRINT UNIT (NOT USED)
CV       IDENT      USER IDENTITIFIER DEFINED IN INPUT FILE.  IT CAN BE 
CV                  USED TO PERSONALISE THE ROUTINES, FOR INSTANCE TO
CV                  CONTROL IW FROM THE INPUT FILE
CV
CV    ARGUMENTS OUT
CV       SIGMB(6)   UPDATED CAUCHY STRESSES
CV       QB(47)     UPDATED STATE VARIABLE VECTOR
CV       C(9,9)     COMPLIANCE MATRIX
CV       ISTRA(20)  IF 1, IMPOSES TIME STEP REDUCTION
C     ------------------------------------------------------------------
      IMPLICIT  NONE
      !
      ! INPUT
      !
      INTEGER   LPARA(*),NT6,ISTRA(*),IDENT
      REAL*8    VGRAD(3,3),SIGMA(6),QA(47),P(*),S(*),DELTAT,STRAT(*)
      !
      ! OUTPUT
      !
      REAL*8    C(9,9),SIGMB(6),QB(47)
      !
      ! LOCAL
      !
      INTEGER   I,J,ISTART,ILOOP,IINT,NINT,NINT1,METH,IW,ISTEP,ITERA
      REAL*8    DCOROT(6),SUBDELTAT,SUBSIGMA(6),
     .          YIELDA,TAUA,GAMA,RADA,BCKA(5),POLA(5),STGA(5,5),
     .          NRMSLA,APREV(5),
     .          YIELDB,TAUB,GAMB,RADB,BCKB(5),POLB(5),STGB(5,5),
     .          NRMSLB,THETA,ANEW(5),
     .          CNUM(9,9),SIGPERT(6,6),PERTEPS,VGRADORI(3,3),CTEST(9,9),
     .          VEC(9),TMP
      INTEGER   IELEM
      LOGICAL   LAGA
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT

C_ICO REAL*8   HISTORY(656,6)
C_ICO COMMON   /KICOTOM/HISTORY

CTO   INTEGER MAXEL3
CTO   PARAMETER(MAXEL3=20000)
CTO   REAL*8 OLDTAU(MAXEL3)
CTO   COMMON /KOLDTAU/ OLDTAU

      LOGICAL NEWSTP,NEWITE
      COMMON /KNEWSTP/ NEWSTP,NEWITE

      REAL*8 XSAT
      COMMON /KXSAT/   XSAT

CTO   INTEGER INDELE
CTO   COMMON /KINDELE/ INDELE
C     ------------------------------------------------------------------
      LAGA=.FALSE.
      IF(LAGA) THEN
         OUT6     =  6
         OUT9     =  9
         OUTOPT   =  9
      ELSE
         OUT6     = 16
         OUT9     = 16
         OUTOPT   = 16
      ENDIF

      IW       = 0             ! CO OPTION KANI3VH
      IF(IDENT.EQ.1245)IW=1
      IELEM    = ISTRA(12)
      NINT     = ABS(LPARA(5))
      NINT1    = NINT
      METH     = ABS(LPARA(6))
      PERTEPS  = STRAT(1)
      IF(IW.EQ.1) THEN
        WRITE(OUT6,9999)
      ENDIF

C     ------------------------------------------------------------------
C     SOME CONSTANTS 
C     ------------------------------------------------------------------
      ITERA    = ISTRA(2)
      ISTEP    = ISTRA(11)
      IELEM    = ISTRA(12)

C     ------------------------------------------------------------------
C     GET THE INDEX NUMBER OF THE CURRENT ELEMENT
C     ------------------------------------------------------------------
CTO   IF(LAGA) INDELE=IELEM
      IF(IW.EQ.1.AND.NEWSTP) WRITE(OUT6,9998) IELEM

C     ------------------------------------------------------------------
C     PERTURBATION TO COMPUTE LOCAL COMPLIANCE TENSOR
C     (ON IF ISTART=1, OFF IF ISTART=7)
C     IF ISTART=1, TOL (IN VMISES ROUTINE) MUST BE SMALLER THAN ITS
C     USUAL VALUE (I.E. 1.0D-6 INSTEAD OF 1.0D-3).  THIS ENSURES 
C     NUMERICAL ACCURACY FOR THE NUMERICAL PERTURBATIONS.
C     ------------------------------------------------------------------
      DO I=1,3
         DO J=1,3
            VGRADORI(I,J)=VGRAD(I,J)
         ENDDO
      ENDDO

      ISTART=7 ! ISTART=1 NEEDS TOL 1.0D-3 IN VMISES
      DO ILOOP=ISTART,7
         DO I=1,3
            DO J=1,3
                 VGRAD(I,J)=VGRADORI(I,J)
            ENDDO
         ENDDO      
         IF (ILOOP.EQ.1) VGRAD(1,1)=VGRADORI(1,1)+PERTEPS/DELTAT !KL=11
         IF (ILOOP.EQ.2) VGRAD(1,2)=VGRADORI(1,2)+PERTEPS/DELTAT !KL=12
         IF (ILOOP.EQ.3) VGRAD(1,3)=VGRADORI(1,3)+PERTEPS/DELTAT !KL=13
         IF (ILOOP.EQ.4) VGRAD(2,2)=VGRADORI(2,2)+PERTEPS/DELTAT !KL=22
         IF (ILOOP.EQ.5) VGRAD(2,3)=VGRADORI(2,3)+PERTEPS/DELTAT !KL=23
         IF (ILOOP.EQ.6) VGRAD(3,3)=VGRADORI(3,3)+PERTEPS/DELTAT !KL=33

C        ---------------------------------------------------------------
C        VELOCITY GRADIENT 'VGRAD' IS SYMMETRIC I.E. EQUAL TO DCOROT
C        ---------------------------------------------------------------
         DCOROT(1)=VGRAD(1,1)
         DCOROT(2)=VGRAD(2,2)
         DCOROT(3)=VGRAD(3,3)
         DCOROT(4)=VGRAD(1,2)
         DCOROT(5)=VGRAD(1,3)
         DCOROT(6)=VGRAD(2,3)

         DO I=1,9
            DO J=1,9
               C(I,J)=0.0D0
            ENDDO
         ENDDO

C        ---------------------------------------------------------------
C        STATE VARIABLES AT THE BEGINNING OF THE INCREMENT
C        INITIALISATION OF TAUA: QUID IF STG INI IS NOT ZERO ?
C        ---------------------------------------------------------------
         YIELDA = QA(1)
         GAMA   = QA(2)
         TAUA   = QA(3)
         RADA   = QA(4)
         DO I=1,5
            BCKA(I)=QA(4+I)                 !Q(5),...,Q(9)
            POLA(I)=QA(9+I)                 !Q(10),...,Q(14)
            DO J=1,5
               STGA(I,J)=QA(14+(I-1)*5+J)   !Q(15),...,Q(39)
            ENDDO
         ENDDO
         NRMSLA=QA(40)
         DO I=1,5
            APREV(I)=QA(41+I)               !Q(42),...,Q(46)
         ENDDO

         IF (TAUA.EQ.0.0D0) THEN
            IF(P(3).EQ.1.0D0.OR.P(3).EQ.2.0D0) THEN
               TAUA=P(4)*P(6)**P(5)         ! VMISES OR TEXISO
            ELSE
               TAUA=P(4)+RADA               ! TEXMIC MODEL : TAU0=P(4)
            ENDIF
         ENDIF

C        ---------------------------------------------------------------
C        CHECKS IF SOFTENING
C        ---------------------------------------------------------------
CTO      IF(ISTEP.EQ.1) THEN
CTO         IF(INDELE.GT.MAXEL3) THEN
CTO            WRITE(OUT6,9994) INDELE,MAXEL3
CTO            CALL XIT
CTO            STOP
CTO         ENDIF
CTO      ENDIF
CTO      IF(NEWSTP.AND.ISTEP.GT.1) THEN
CTO         IF(TAUA.LT.0.99D0*OLDTAU(INDELE)) THEN
CTO            WRITE(OUT6,9995) ISTEP,IELEM,OLDTAU(INDELE),TAUA
CTO         ENDIF
CTO      ENDIF
CTO      OLDTAU(INDELE)=TAUA

C        ---------------------------------------------------------------
C        PRINT STATE IN THE REFERENCE CONFIGURATION
C        ---------------------------------------------------------------
         IF(IW.EQ.1) THEN
            WRITE(OUT6,9990) ISTEP,ITERA,IELEM,ISTRA(13)
            WRITE(OUT6,9980) (DCOROT(I),I=1,6),DELTAT
            WRITE(OUT6,8990) YIELDA,GAMA,TAUA
            IF(P(3).GT.2.0D0) THEN
               WRITE(OUT6,8985)  RADA
               WRITE(OUT6,8980) (BCKA(I),I=1,5)
               WRITE(OUT6,8970) (POLA(I),I=1,5)
               WRITE(OUT6,8960) ((STGA(I,J),J=1,5),I=1,5)
               WRITE(OUT6,8950)  NRMSLA
               WRITE(OUT6,8940) (APREV(I),I=1,5)
            ENDIF
            WRITE(OUT6,8930) (SIGMA(I),I=1,6)
            IF(IW.EQ.1.AND.ISTART.NE.7) WRITE(OUT6,8920) ILOOP
         ENDIF

C        ---------------------------------------------------------------
C        CALL TO CONSTITUTIVE ROUTINE (KVMISES OR KTEXHAR)
C        ---------------------------------------------------------------
         SUBDELTAT = DELTAT/NINT
         DO I=1,6
            SUBSIGMA(I) = SIGMA(I)
         ENDDO
         IF(IW.EQ.1) WRITE(OUT6,7990) NINT, SUBDELTAT

         DO IINT=1,NINT
            IF(IW.EQ.1)WRITE(OUT6,7980) (SUBSIGMA(I),I=1,6)
            IF(P(3).EQ.1.0D0) THEN
               CALL KVMISES(SUBSIGMA,YIELDA,GAMA,TAUA,DCOROT,
     .                      SUBDELTAT,P,IDENT,
     .                      ISTRA,SIGMB,YIELDB,GAMB,TAUB,C)
            ELSE
               call KTEXHAR(ISTRA,IDENT,
     .                      SUBSIGMA,YIELDA,GAMA,TAUA,
     .                      RADA,BCKA,POLA,STGA,NRMSLA,APREV,
     .                      DCOROT,SUBDELTAT,P,S,
     .                      SIGMB,YIELDB,GAMB,TAUB,
     .                      RADB,BCKB,POLB,STGB,NRMSLB,ANEW,
     .                      THETA,C)
            ENDIF

C        ---------------------------------------------------------------
C        NOTE:
C           ONE MUST INTRODUCE HERE THE VARIABLES SUBSIGMA AND SUBDELTAT
C           SINCE THEY ARE ARGUMENT OF THE ROUTINE.  IF WE WOULD USE
C           SIGMA AND DELTAT, THEY VALUE WHEN KANI3VH IS COMPLETED WOULD
C           HAVE BEEN MODIFIED W.R.T THEIR ORIGINAL VALUES.  THIS IS
C           OBVIOUSLY INACCEPTABLE FOR DELTAT AND FOR SIGMA.  THE REST
C           OF THE VARIABLES (YIELDA, GAMA, ...) DOES NOT APPEAR ANYMORE
C           HERE AFTER, THEY ARE COMPLETELY LOCAL VARIABLES, THEIR
C           VALUES DO NOT IMPORT IN THE REST OF THIS ROUTINE.
C        ---------------------------------------------------------------
            DO I=1,6
               SUBSIGMA(I)=SIGMB(I)
            ENDDO
            YIELDA = YIELDB
            GAMA   = GAMB
            TAUA   = TAUB
            RADA   = RADB
            DO I=1,5
               BCKA(I) = BCKB(I)              !Q(5),...,Q(9)
               POLA(I) = POLB(I)              !Q(10),...,Q(14)
               DO J=1,5
                  STGA(I,J) = STGB(I,J)       !Q(15),...,Q(39)
               ENDDO
            ENDDO
            NRMSLA = NRMSLB
C           QB(41) = THETA                    ! NOTHING FOR THETA SINCE
                                              ! DOES NOT GO IN ASOLVER
            DO I=1,5
               APREV(I)=ANEW(I)               ! MODIFIED ONLY IF PL.INC.
            ENDDO
         ENDDO ! END OF SUBSTEPPING

C        ---------------------------------------------------------------
C        UPDATE ONLY WHEN ILOOP=7
C        ---------------------------------------------------------------
         IF(ILOOP.LE.6) THEN
            DO I=1,6
               SIGPERT(I,ILOOP)=SIGMB(I)
            ENDDO
         ELSE
C           STATE VARIABLES AT THE END OF THE INCREMENT
            QB(1) = YIELDB
            QB(2) = GAMB
            QB(3) = TAUB
            QB(4) = RADB
            DO I=1,5
               QB(4+I) = BCKB(I)              !Q(5),...,Q(9)
               QB(9+I) = POLB(I)              !Q(10),...,Q(14)
               DO J=1,5
                  QB(14+(I-1)*5+J) = STGB(I,J)!Q(15),...,Q(39)
               ENDDO
            ENDDO
            QB(40) = NRMSLB
            QB(41) = THETA
            DO I=1,5
               QB(41+I) = ANEW(I)             ! MODIFIED ONLY IF PL.INC.
            ENDDO
            CALL KLENGTH(BCKB,5,1, QB(47))
            QB(47)=XSAT
         ENDIF ! ILOOP<=6
      ENDDO ! DO ILOOP=ISTART,7

C     ------------------------------------------------------------------
C     COMPUTATION OF LOCAL COMPLIANCE TENSOR BY PERTURBATION
C     (ON IF ISTART=1, OFF IF ISTART=7)
C     ------------------------------------------------------------------
      IF (ISTART.EQ.1) THEN
         DO J=1,3
            CNUM(1,J)=(SIGPERT(1,J)-SIGMB(1))/PERTEPS
            CNUM(2,J)=(SIGPERT(4,J)-SIGMB(4))/PERTEPS
            CNUM(3,J)=(SIGPERT(5,J)-SIGMB(5))/PERTEPS
            CNUM(4,J)=CNUM(2,J)
            CNUM(5,J)=(SIGPERT(2,J)-SIGMB(2))/PERTEPS
            CNUM(6,J)=(SIGPERT(6,J)-SIGMB(6))/PERTEPS
            CNUM(7,J)=CNUM(3,J)
            CNUM(8,J)=CNUM(6,J)
            CNUM(9,J)=(SIGPERT(3,J)-SIGMB(3))/PERTEPS
         ENDDO
C        MINOR SYMMETRY OF COMPLIANCE 21=12
         CNUM(1,4)=CNUM(1,2)
         CNUM(2,4)=CNUM(2,2)
         CNUM(3,4)=CNUM(3,2)
         CNUM(4,4)=CNUM(4,2)
         CNUM(5,4)=CNUM(5,2)
         CNUM(6,4)=CNUM(6,2)
         CNUM(7,4)=CNUM(7,2)
         CNUM(8,4)=CNUM(8,2)
         CNUM(9,4)=CNUM(9,2)
         DO J=5,6
            CNUM(1,J)=(SIGPERT(1,J-1)-SIGMB(1))/PERTEPS
            CNUM(2,J)=(SIGPERT(4,J-1)-SIGMB(4))/PERTEPS
            CNUM(3,J)=(SIGPERT(5,J-1)-SIGMB(5))/PERTEPS
            CNUM(4,J)=CNUM(2,J)
            CNUM(5,J)=(SIGPERT(2,J-1)-SIGMB(2))/PERTEPS
            CNUM(6,J)=(SIGPERT(6,J-1)-SIGMB(6))/PERTEPS
            CNUM(7,J)=CNUM(3,J)
            CNUM(8,J)=CNUM(6,J)
            CNUM(9,J)=(SIGPERT(3,J-1)-SIGMB(3))/PERTEPS
         ENDDO
C        MINOR SYMMETRY 31=13
         CNUM(1,7)=CNUM(1,3)
         CNUM(2,7)=CNUM(2,3)
         CNUM(3,7)=CNUM(3,3)
         CNUM(4,7)=CNUM(4,3)
         CNUM(5,7)=CNUM(5,3)
         CNUM(6,7)=CNUM(6,3)
         CNUM(7,7)=CNUM(7,3)
         CNUM(8,7)=CNUM(8,3)
         CNUM(9,7)=CNUM(9,3) 
C        MINOR SYMMETRY 32=23 
         CNUM(1,8)=CNUM(1,6)
         CNUM(2,8)=CNUM(2,6)
         CNUM(3,8)=CNUM(3,6)
         CNUM(4,8)=CNUM(4,6)
         CNUM(5,8)=CNUM(5,6)
         CNUM(6,8)=CNUM(6,6)
         CNUM(7,8)=CNUM(7,6)
         CNUM(8,8)=CNUM(8,6)
         CNUM(9,8)=CNUM(9,6)
 
         CNUM(1,9)=(SIGPERT(1,9-3)-SIGMB(1))/PERTEPS
         CNUM(2,9)=(SIGPERT(4,9-3)-SIGMB(4))/PERTEPS
         CNUM(3,9)=(SIGPERT(5,9-3)-SIGMB(5))/PERTEPS
         CNUM(4,9)=CNUM(2,9)
         CNUM(5,9)=(SIGPERT(2,9-3)-SIGMB(2))/PERTEPS
         CNUM(6,9)=(SIGPERT(6,9-3)-SIGMB(6))/PERTEPS
         CNUM(7,9)=CNUM(3,9)
         CNUM(8,9)=CNUM(6,9)
         CNUM(9,9)=(SIGPERT(3,9-3)-SIGMB(3))/PERTEPS
 
         DO J=2,4
            DO I=1,9
               CNUM(I,J)=CNUM(I,J)/2.0D0
            ENDDO
         ENDDO
         DO J=6,8
            DO I=1,9
               CNUM(I,J)=CNUM(I,J)/2.0D0
            ENDDO
         ENDDO

         IF(IW.EQ.1) THEN
            DO J=1,9
               VEC(J)=0.0D0
            ENDDO
            VEC(3)=PERTEPS
            VEC(7)=PERTEPS
            TMP=0.0D0
            DO J=1,9
               TMP=TMP+C(3,J)*VEC(J)
            ENDDO
            TMP=SIGMB(5)+TMP
            WRITE(OUT6,'(A,E25.16)') 'STRESS 13 ILOOP 3 C ...',TMP
            TMP=0.0D0
            DO J=1,9
               TMP=TMP+CNUM(3,J)*VEC(J)
            ENDDO
            TMP=SIGMB(5)+TMP
            WRITE(OUT6,6993) TMP
            WRITE(OUT6,6992) SIGPERT(5,3)
            WRITE(OUT6,6991) SIGMB(5)
         ENDIF

      ENDIF ! IF(ISTART.EQ.1)

C     ------------------------------------------------------------------
C     PRINTINGS
C     ------------------------------------------------------------------
      IF(IW.EQ.1) THEN
         WRITE(OUT6,6980) ((C(I,J),J=1,9),I=1,9)
         IF(ISTART.EQ.1) THEN
            WRITE(OUT6,6990) ((CNUM(I,J),J=1,9),I=1,9)
            DO I=1,9
               DO J=1,9
                  CTEST(I,J)=C(I,J)-CNUM(I,J)
               ENDDO
            ENDDO
            WRITE(OUT6,5001) ((CTEST(I,J),J=1,9),I=1,9)
         ENDIF
      ENDIF

      IF(IW.EQ.1) THEN
         WRITE(OUT6,5990) YIELDB,GAMB,TAUB
         IF(P(3).NE.1.0D0) THEN
            WRITE(OUT6,5985)  RADB
            WRITE(OUT6,5980) (BCKB(I),I=1,5)
            WRITE(OUT6,5970) (POLB(I),I=1,5)
            WRITE(OUT6,5960) ((STGB(I,J),J=1,5),I=1,5)
            WRITE(OUT6,5950)  NRMSLB
            WRITE(OUT6,5940) (ANEW(I),I=1,5)
         ENDIF
         WRITE(OUT6,5930) (SIGMB(I),I=1,6)
         WRITE(OUT6,4990) IELEM,ISTRA(13)
      ENDIF

C     ------------------------------------------------------------------
C     FORMATS
C     ------------------------------------------------------------------
C     FORMAT 9XXX : INTRODUCTION
9999  FORMAT(/,50('"'),'KANI3VH',50('"'),/)
9998  FORMAT(  1X,'[KANI3VH] THIS IS A NEW STEP FOR IELEM: ',I5,/)
9995  FORMAT(  !X,'[KANI3VH] KWARNING!'               ,/,
     .         3X,'          SOFTENING AT STEP:',I5   ,/,
     .         3X,'          IELEM............:',I5   ,/,
     .         3X,'          OLDTAU...........:',E13.5,/,
     .         3X,'          TAUA.............:',E13.5,/)
9994  FORMAT(  1X,'[KANI3VH] INDELE SHOUD BE <MAXEL3 WHEREAS'     ,/,
     .         1X,'          WHEREAS INDEX=',I5,' AND MAXEL3=',I5 ,/)
9990  FORMAT(  1X,'[KANI3VH] BEGIN OF KANI3VH.F'    ,/,
     .         1X,'          ISTEP          : ', I5 ,/,
     .         1X,'          ITERA          : ', I5 ,/,
     .         1X,'          IELEM          : ', I5 ,/,
     .         1X,'          INTEGRATION PT : ', I5 ,/)
9980  FORMAT(  1X,'STRAIN RATE          :',6E13.5,/,
     .         1X,'DELTAT               :', E13.5)

C     FORMAT 8XXX : INITIAL STATE 

8990  FORMAT(  1X,'PL. INDICATOR.......A:', E13.5,/,
     .         1X,'EQ. PL. STRAIN......A:', E13.5,/,
     .         1X,'SIZE OF YL..........A:', E13.5)
8985  FORMAT(  1X,'STATE VAR R.........A:', E13.5)
8980  FORMAT(  1X,'DEV. BACK STRESS....A:',5E13.5)
8970  FORMAT(  1X,'POLARITY............A:',5E13.5)
8960  FORMAT(  1X,'STRENGTH OF DISL....A:',/,5(1X,5E13.5,/))
8950  FORMAT(  1X,'NORM OF LATENT PART.A:', E13.5)
8940  FORMAT(  1X,'LAST PLASTIC MODE...A:',5E13.5)
8930  FORMAT(  1X,'CAUCHY STRESSES.....A:',6E13.5)
8920  FORMAT(  1X,'ILOOP                :', I5,/,
     .         1X,'=====================')

C     FORMAT 7XXX : SUB-STEPPING

7990  FORMAT(  1X,'NUMBER OF SUB-STEPS  :', I5   ,/,
     .         1X,'SUB-DELTAT           :', E13.5)
7980  FORMAT(  1X,'CURRENT SUBSIGMA     :',6E13.5,/)

C     FORMAT 1XXX : NUMERICAL OR ANALYTICAL COMPLIANCE

6993  FORMAT(  1X,'STRESS 13 ILOOP 3 CNUM=',E25.16) 
6992  FORMAT(  1X,'SIGPERT(5,3)=',E25.16)
6991  FORMAT(  1X,'SIGMB(5)=',E25.16)
6990  FORMAT(  1X,'COMPLIANCE CNUM      :',/,9(1X,9E13.5,/)) 
6980  FORMAT(  1X,'COMPLIANCE C         :',/,9(1X,9E13.5,/)) 

C     FORMAT 5XXX : UPDATED STATE (SAME INFO AS FORMAT 8XXX)

5990  FORMAT(  1X,'PL. INDICATOR.......B:', E13.5,/,
     .         1X,'EQ. PL. STRAIN......B:', E13.5,/,
     .         1X,'SIZE OF YL..........B:', E13.5)
5985  FORMAT(  1X,'STATE VAR R.........B:', E13.5)
5980  FORMAT(  1X,'DEV. BACK STRESS....B:',5E13.5)
5970  FORMAT(  1X,'POLARITY............B:',5E13.5)
5960  FORMAT(  1X,'STRENGTH OF DISL....B:',/,5(1X,5E13.5,/))
5950  FORMAT(  1X,'NORM OF LATENT PART.B:', E13.5)
5940  FORMAT(  1X,'LAST PLASTIC MODE...B:',5E13.5)
5930  FORMAT(  1X,'CAUCHY STRESSES.....B:',6E13.5)
5920  FORMAT(  1X,'CAUCHY STRESSES.....B:',6E13.5)
5001  FORMAT(  6X,'DIFFERENCE         :',/,9(6X,9E13.5,/)) 
C     FORMAT 4XXX : EXIT

4990  FORMAT(  1X,'ELEMENT              :', I5,/,
     .         1X,'INTEGRATION POINT    :', I5,/,
     .         1X,'END OF KANI3VH.F',/)
C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KTEXHAR(ISTRA,IDENT,
     .                    SIGA,YIELDA,GAMA,TAUA,
     .                    RADA,BCKA,POLA,STGA,NRMSLA,APREV,
     .                    DCOROT,DELTAT,P,S,
     .                    SIGB,YIELDB,GAMB,TAUB,
     .                    RADB,BCKB,POLB,STGB,NRMSLB,ANEW,
     .                    THETA,C)
C     ------------------------------------------------------------------
CU    SUBROUTINE KTEXHAR USES SUBROUTINES
CU       KINISTA
CU       KPRTSTA
CU       F3DER
CU       KLENGTH
CU       KLENGT6
CU       KX6_2X5
CU       KMODELA
CU       KMODCON
CU       KDETECT
CU       KYLPMUL
CU       KMAKE66
CU       XIT
CU       KINIDPL
CU       KINIARG
CU       KBEULER
CU       KPRODUC
CU       KX5_2X6
CU       KDETECT
C     ------------------------------------------------------------------
CA    CALL KTEXHAR(ISTRA,IDENT,
CA   .             SIGA,YIELDA,GAMA,TAUA,
CA   .             RADA,BCKA,POLA,STGA,NRMSLA,APREV,
CA   .             DCOROT,DELTAT,P,S,
CA   .             SIGB,YIELDB,GAMB,TAUB,
CA   .             RADB,BCKB,POLB,STGB,NRMSLB,ANEW,
CA   .             THETA,C)
C     ------------------------------------------------------------------
CB    SUBROUTINE KTEXHAR USES COMMON BLOCKS
CB       KNCHOUT
CB       K_OKOPT
CB       KN_UREF
CB       KSTATIS
CB       KNEWSTP
CB       KELATR5
CB       K_ITRY3
CB       K_HIST1
CB       K_HIST2
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      !
      ! INPUT
      !
      INTEGER   ISTRA(*),IDENT
      REAL*8    SIGA(6),YIELDA,GAMA,TAUA,RADA,BCKA(5),POLA(5),STGA(5,5),
     .          NRMSLA,APREV(5),DCOROT(6),DELTAT,P(*),S(600)
      !
      ! OUTPUT
      !
      REAL*8    SIGB(6),YIELDB,GAMB,TAUB,RADB,BCKB(5),POLB(5),STGB(5,5),
     .          NRMSLB,ANEW(5),THETA,C(9,9)
      !
      ! LOCAL
      !
      INTEGER   I,J,IW,ICOMPL,IERRC,ITERA,ISTEP,IELEM,IOFF,IWVEC(5)
      INTEGER   IDER,NCFE,NCHLST
      REAL*8    TEMP
      REAL*8    DEPS(6),DEVDEPS(6)
      REAL*8    EMOD,KMOD,GMOD,TWOG,ANU,MATCOF(14)
      REAL*8    PRESA,PRESB
      REAL*8    ESSA6(6),A2TR6(6),ESSTR6(6),ESSTR5(5),PHITR5(5),NPHITR5,
     .          ESSB6(6),PHIA5(5),NPHIA5,A2TR5(5),NA2TR5,DPL5(5),NRMREF,
     .          NRMINI,BCK6(6),SIGTAY(6),AONSET(5),ELAFRA
      REAL*8    FVAL,H,HPRIM(3,3) ! OUTPUT OF CALL KHARDEN
      REAL*8    PI,ROOT2,ROOT6,ROOT23,RTD,DTR
      REAL*8    SOV5(5),ELASOV5,ELATR5,APHITR5(5)
      REAL*8    MCON99(9,9),MCON66(6,6),INCPL6(6),GELAFR
      REAL*8    FVALR,DFR(5),DDFR(5,5),AVR(5),  UVR(5),N_UREF
      REAL*8    NDEPS,NBIG
      PARAMETER (NBIG=0.025D0)
      REAL*8    DPL6(6)

      LOGICAL   USEELA,CALTR5,EXPLIC
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT

      LOGICAL OK_OPT
      COMMON /K_OKOPT/OK_OPT

      COMMON   /KN_UREF/ N_UREF

      INTEGER ELETAY
      COMMON /K_HIST1/ ELETAY
      REAL*8  HISTORY(6)
      COMMON /K_HIST2/ HISTORY

      INTEGER   LSTSTP,ITETOT,KELAS,KPLAS,KBIG,KTEXH,KDTEC,
     .          KTRIV,KPHIA1,KPHIA2,KOFFS,KPROD,KTRIA1,KTRIA2,KBISS,
     .          KYLPER,KYLP0,KYLP0A,KYLP0B,KYLP0C,
     .          KYLP1,KYLP1A,KYLP1B,KYLP1C
      COMMON   /KSTATIS/ LSTSTP,ITETOT,KELAS,KPLAS,KBIG,KTEXH,KDTEC,
     .          KTRIV,KPHIA1,KPHIA2,KOFFS,KPROD,KTRIA1,KTRIA2,KBISS,
     .          KYLPER,KYLP0,KYLP0A,KYLP0B,KYLP0C,
     .          KYLP1,KYLP1A,KYLP1B,KYLP1C

      LOGICAL   NEWSTP,NEWITE
      COMMON   /KNEWSTP/ NEWSTP,NEWITE
      COMMON   /KELATR5/ ELATR5,ISTEP,ITERA

      INTEGER   ITRY3
      COMMON   /K_ITRY3/ ITRY3

      CHARACTER*100 KFACETID1, KFACETID2
      INTEGER KTYPEXPR, KTYPFACET, KSTGFACET, KCTRi, KCTRj
      INTEGER KNUMFACET, KORDFACET
      DOUBLE PRECISION KLAMFACET(2500), KSNSFACET(2500,5)
      COMMON /KFACETPAR1/ KFACETID1, KFACETID2
      COMMON /KFACETPAR2/ KTYPFACET, KSTGFACET, KNUMFACET,
     &                    KORDFACET, KTYPEXPR
      COMMON /KFACETPAR3/ KLAMFACET
      COMMON /KFACETPAR4/ KSNSFACET

C     ------------------------------------------------------------------
C     variables introduced specially to cut the fishtails
C     ------------------------------------------------------------------
cft   logical chk(3)
cft   integer isol,itry
cft   real*8  beutr(4),beutr2(4),unilen,adpl(5),len(3),lenmin,tmp5(5)
cft   real*8  dpl5x(5),gambx,taubx,radbx,bckbx(5),polbx(5),stgbx(5,5),
cft  .        essb6x(6),nrmslbx,anewx(5),cx(9,9)
cft   real*8  dpl5y(5),gamby,tauby,radby,bckby(5),polby(5),stgby(5,5),
cft  .        essb6y(6),nrmslby,anewy(5),cy(9,9)
cft   real*8  dpl5z(5),gambz,taubz,radbz,bckbz(5),polbz(5),stgbz(5,5),
cft  .        essb6z(6),nrmslbz,anewz(5),cz(9,9)
      logical check
      common /kcheck/ check
C     ------------------------------------------------------------------

C     ------------------------------------------------------------------
C     SOME CONSTANTS 
C     ------------------------------------------------------------------
      ITERA    = ISTRA(2)
      ISTEP    = ISTRA(11)
      IELEM    = ISTRA(12)

C     ------------------------------------------------------------------
C     OPTION
C     ------------------------------------------------------------------
      IW = 0                ! CO OPTION KTEXHAR
      IF(IDENT.EQ.1245)IW=1
      IWVEC(1)=0            ! CO OPTION KTEXHAR FOR ROUTINE KHARDEN
      IWVEC(2)=0            ! CO OPTION KTEXHAR FOR ROUTINE KDETECT
      IWVEC(3)=0            ! CO OPTION KTEXHAR FOR ROUTINE KYLPMUL
      IWVEC(4)=0            ! CO OPTION KTEXHAR FOR ROUTINE KINIDPL
      IWVEC(5)=0            ! CO OPTION KTEXHAR FOR ROUTINE KINIARG
      IF(IDENT.EQ.1245) THEN
         IWVEC(2)=1
         IWVEC(3)=1
      ENDIF

      USEELA=.TRUE.         ! CO OPTION KTEXHAR
      EXPLIC=.FALSE.        ! CO OPTION KTEXHAR
      ITRY3=1               ! CO OPTION KTEXHAR

      IF(.NOT.OK_OPT) THEN
        WRITE(OUTOPT,892) USEELA,EXPLIC,ITRY3
        IF(ITRY3.NE.1)  WRITE(OUTOPT,891)
        OK_OPT=.TRUE.
      ENDIF

C     ------------------------------------------------------------------
C     SOME CONSTANTS 
C     ------------------------------------------------------------------
      IF(ISTEP.EQ.0)LSTSTP=0
      IF(NEWSTP.AND.LSTSTP.NE.ISTEP-1) THEN
         IF(ISTEP.GT.1) CALL KPRTSTA(OUT6,ISTEP-1)
         CALL KINISTA
         NEWSTP=.FALSE.
      ENDIF
      IF(NEWITE) THEN
         KELAS=0
         KPLAS=0
         KBIG =0
         NEWITE=.FALSE.
      ENDIF

      KTEXH    = KTEXH+1
      ICOMPL   = ABS(MOD(ISTRA(5),10)/1)        ! DEF FROM LOAX3D.F 
      PI       = DACOS(-1.0D0) 
      ROOT2    = DSQRT(2.0D0) 
      ROOT6    = DSQRT(6.0D0) 
      ROOT23   = DSQRT(2.0D0/3.0D0) 
      RTD      = 180.0D0/PI 
      DTR      = PI/180.0D0 
      EMOD     = P(1)                           ! YOUNG'S MODULUS
      ANU      = P(2)                           ! POISSON RATIO
      KMOD     = EMOD/(3.0D0*(1.0D0-2.0D0*ANU)) ! COMPRESSIBILITY MOD
      GMOD     = EMOD/(2.0D0*(1.0D0+ANU))       ! SHEAR MODULUS
      TWOG     = 2.0D0*GMOD                     ! 2 TIMES SHEAR MODULUS
      DO I=1,14
         MATCOF(I)=P(I+2) 
      ENDDO 
      IF(IW.EQ.1)WRITE(OUT6,900)ISTEP,IELEM,(MATCOF(I),I=1,14),EMOD,ANU 

C     ------------------------------------------------------------------
C     COMPUTE THE ORDER OF MAGNITUDE OF THE LENGTH OF THE DEVIATORIC
C     NORMALISED YIELD LOCUS POINT
C     ------------------------------------------------------------------
      IF(ISTEP.EQ.1.AND.N_UREF.LT.0.0D0.AND.IELEM.EQ.1) THEN
        AVR(1)= DSQRT(0.75D0)
        AVR(2)= 0.5D0
        AVR(3)= 0.0D0
        AVR(4)= 0.0D0
        AVR(5)= 0.0D0
        IDER  =1
        NCHLST=OUT6
        IF (P(3) .EQ. 2.D0) THEN
          NCFE  =S(2)
          CALL KF3DER(FVALR,DFR,DDFR,AVR,S(9),S(9+NCFE),5,IDER,NCHLST,0)
        END IF
        IF(P(3) .EQ. 30.D0) THEN
          IF (KTYPFACET==2) 
     &      CALL KFacetPOT(FVALR,DFR,DDFR,AVR,IDER,NCHLST,0)
        END IF
        TEMP=0.0D0
        DO I=1,5
          TEMP=TEMP+DFR(I)*AVR(I)
        ENDDO
        TEMP=FVALR-TEMP
        DO I=1,5
          UVR(I)=DFR(I)+AVR(I)*TEMP
        ENDDO
        CALL KLENGTH(UVR,5,1, N_UREF)
        WRITE(OUT6,890) N_UREF
      END IF
C     ------------------------------------------------------------------
C     HYPOTHETICAL COROTATIONAL TOTAL ELASTIC STRAIN (11,22,33,12,13,23) 
C     PAS DE FACTEUR 2 POUR LES TERMES HORS DIAGONALE CAR ON NE CHERCHE 
C     PAS A APPLIQUER LA LOI DE HOOKE SUR LES CONTRAINTES TOTALES 
C     ------------------------------------------------------------------
      DEPS(1)=DELTAT*DCOROT(1)
      DEPS(2)=DELTAT*DCOROT(2)
      DEPS(3)=DELTAT*DCOROT(3)
      DEPS(4)=DELTAT*DCOROT(4)
      DEPS(5)=DELTAT*DCOROT(5)
      DEPS(6)=DELTAT*DCOROT(6)
      CALL KLENGT6(DEPS, NDEPS)
      IF(NDEPS.GT.NBIG)KBIG=KBIG+1
C     write(out6,'(a,i5,a,e13.5)') 'ielem=',ielem,', ndeps=',ndeps

C     ------------------------------------------------------------------
C     DEVIATORIC PART OF COROTATIONAL STRAIN 
C     ------------------------------------------------------------------
      TEMP=(DEPS(1)+DEPS(2)+DEPS(3))/3.0D0 
      DEVDEPS(1)=DEPS(1)-TEMP 
      DEVDEPS(2)=DEPS(2)-TEMP 
      DEVDEPS(3)=DEPS(3)-TEMP 
      DEVDEPS(4)=DEPS(4) 
      DEVDEPS(5)=DEPS(5) 
      DEVDEPS(6)=DEPS(6) 
 
C     ------------------------------------------------------------------
C     HYDROSTATIC PRESSURE AT THE END OF INCREMENT 
C     ------------------------------------------------------------------
      PRESA  =(SIGA(1)+SIGA(2)+SIGA(3))/3.0D0
      IF(IW.EQ.1)WRITE(OUT6,880 )(SIGA(I),I=1,6),PRESA
      PRESB  =PRESA+KMOD*(DEPS(1)+DEPS(2)+DEPS(3)) 
 
C     ------------------------------------------------------------------
C     DEVIATORIC STRESS TENSOR 
C     ------------------------------------------------------------------
      ESSA6(1)=SIGA(1)-PRESA 
      ESSA6(2)=SIGA(2)-PRESA 
      ESSA6(3)=SIGA(3)-PRESA 
      ESSA6(4)=SIGA(4) 
      ESSA6(5)=SIGA(5) 
      ESSA6(6)=SIGA(6) 
 
C     ------------------------------------------------------------------
C     ELASTIC INCREMENT OF DEVIATORIC STRESS (11,22,33,12,13,23) 
C     ------------------------------------------------------------------
      A2TR6(1)=TWOG*DEVDEPS(1) 
      A2TR6(2)=TWOG*DEVDEPS(2) 
      A2TR6(3)=TWOG*DEVDEPS(3) 
      A2TR6(4)=TWOG*DEVDEPS(4) 
      A2TR6(5)=TWOG*DEVDEPS(5) 
      A2TR6(6)=TWOG*DEVDEPS(6) 
 
C     ------------------------------------------------------------------
C     ELASTIC DEVIATORIC TRIAL STRESS (11,22,33,12,13,23) 
C     ------------------------------------------------------------------
      ESSTR6(1)=ESSA6(1)+A2TR6(1) 
      ESSTR6(2)=ESSA6(2)+A2TR6(2) 
      ESSTR6(3)=ESSA6(3)+A2TR6(3) 
      ESSTR6(4)=ESSA6(4)+A2TR6(4) 
      ESSTR6(5)=ESSA6(5)+A2TR6(5) 
      ESSTR6(6)=ESSA6(6)+A2TR6(6) 
 
C     ------------------------------------------------------------------
C     5-DIM VECTORS NEEDED WHATEVER THE VALUE OF YIELDA 
C     WHATEVER YIELDA : PHITR5 SERT POUR DEFINIR LA DIR. DE RECHERCHE
C     IF YIELDA.EQ.1  : A2TR5  SERT POUR CALCULER PHITR_DOT*NORMALE 
C     ------------------------------------------------------------------
      CALL KX6_2X5(ESSA6, PHIA5) 
      DO I=1,5 
         PHIA5(I)=PHIA5(I)-BCKA(I) 
      ENDDO
      CALL KLENGTH(PHIA5,5,1, NPHIA5)
      CALL KX6_2X5(A2TR6, A2TR5) 
      CALL KLENGTH(A2TR5,5,1, NA2TR5) 
      DO I=1,5 
         PHITR5(I)=PHIA5(I)+A2TR5(I) 
      ENDDO 
      CALL KLENGTH(PHITR5,5,1, NPHITR5) 
      DO I=1,5 
         ESSTR5(I)=BCKA(I)+PHIA5(I)+A2TR5(I) 
      ENDDO 
      IF(IW.EQ.1)WRITE(OUT6,860) (PHIA5(I),I=1,5),NPHIA5,
     .  (DCOROT(I),I=1,6),(A2TR6(I),I=1,6),(A2TR5(I),I=1,5),NA2TR5,
     .  (PHITR5(I),I=1,5),NPHITR5,(APREV(I),I=1,5),YIELDA
 
C     ------------------------------------------------------------------
C     TOO SMALL INCREMENT -> AVOID CALLING KDETECT
C     ------------------------------------------------------------------
      IF(NA2TR5/TAUA.LT.1.0D-5) THEN
         IF(IW.EQ.1)WRITE(OUT6,800)
         IF(YIELDA.EQ.0.0D0) THEN 
            IF(IW.EQ.1)WRITE(OUT6,780)
            YIELDB =0.0D0
            KELAS  =KELAS+1
            SIGB(1)=ESSTR6(1)+PRESB 
            SIGB(2)=ESSTR6(2)+PRESB 
            SIGB(3)=ESSTR6(3)+PRESB 
            SIGB(4)=ESSTR6(4) 
            SIGB(5)=ESSTR6(5) 
            SIGB(6)=ESSTR6(6) 
            GAMB   =GAMA
            TAUB   =TAUA  
            RADB   =RADA
            DO I=1,5 
               BCKB(I)=BCKA(I) 
               POLB(I)=POLA(I) 
               DO J=1,5 
                  STGB(I,J)=STGA(I,J) 
               ENDDO 
            ENDDO 
            NRMSLB=NRMSLA 
            THETA=0.0D0 
            DO I=1,5 
               ANEW(I)=APREV(I) 
            ENDDO 
            IF(ICOMPL.NE.0) CALL KMODELA(GMOD,KMOD, C) 
            IF(IW.EQ.1)WRITE(OUT6,301)
            RETURN 
         ELSE 
            IF(IW.EQ.1)WRITE(OUT6,760) IELEM
            YIELDB=1.0D0
            KPLAS =KPLAS+1
            DO I=1,6
               SIGB(I)=SIGA(I) 
            ENDDO 
            GAMB   =GAMA 
            TAUB   =TAUA
            RADB   =RADA  
            DO I=1,5 
               BCKB(I)=BCKA(I) 
               POLB(I)=POLA(I) 
               DO J=1,5 
                  STGB(I,J)=STGA(I,J) 
               ENDDO 
            ENDDO 
            NRMSLB=NRMSLA 
            THETA=0.0D0 
            DO I=1,5 
               ANEW(I)=APREV(I) 
            ENDDO
            IF (ICOMPL.NE.0) THEN
               IF(USEELA) THEN
                  IF(IW.EQ.1)WRITE(OUT6,740)
                  CALL KMODELA(GMOD,KMOD,C) 
               ELSE
                  IF(IW.EQ.1)WRITE(OUT6,720)
                  CALL KHARDEN(IWVEC(1),MATCOF,GAMA,ANEW,S,
     .                         RADA,BCKA,POLA,STGA, FVAL,H,HPRIM)
                  CALL KMODCON(GMOD,KMOD,ANEW,FVAL,H,HPRIM, C) 
               ENDIF
            ENDIF
            IF(IW.EQ.1)WRITE(OUT6,302)
            RETURN 
         ENDIF 
      ENDIF 
 
C     ------------------------------------------------------------------
C     CHECK THE VALUE OF SSTAR 
C     ------------------------------------------------------------------
CHK   BEU(1)=0.0D0 
CHK   BEU(2)=0.0D0 
CHK   BEU(3)=0.0D0 
CHK   BEU(4)=60.0D0*DTR 
CHK   IOFF=0 
CHK   DO I=1,5 
CHK      SOV5(I)=0.0D0 
CHK   ENDDO 
CHK   UNILEN=1.0D0 
CHK   ITRY3  =1 
CHK   IFUN   =3 
CHK   NCFE   =S(2) 
CHK   BEA0(1)=BEU(1) 
CHK   BEA0(2)=BEU(2) 
CHK   BEA0(3)=BEU(3) 
CHK   BEA0(4)=BEU(4) 
CHK   CALL KYLP(RL,SCALS,POT,APREV,BEA,ANG,COSANG,NSTEPS, 
CHK  .         SOV5,TAUA,UNILEN,BEU,BEA0,s(9),s(9+NCFE), 
CHK  .         IOFF,ITRY3,IFUN,IERR,0) 
CHK   WRITE(OUT6,'(6X,A, E13.5)') '[ANI4VH] SSTAR  :',RL 
CHK   WRITE(OUT6,'(6X,A, E13.5)') '[ANI4VH] SSTAR  :',SCALS 
 
C     ------------------------------------------------------------------
C     DETERMINES PLASTIC STATE AFTER THIS INCREMENT 
C     OUTPUT:
C        YIELDB : THE PLASTICITY INDICATOR
C        ELAFRA : THE FRACTION OF THE STRESS VECTOR (PHIA5,PHITR5) THAT
C                 HAS BEEN DONE IN THE ELASTIC DOMAIN
C        AONSET : THE YL NORMAL AT THE INTERSECTION WITH THE LINE BASED
C                 ON THE VECTOR (PHIA5,PHITR5)
C     ------------------------------------------------------------------
      CALL KDETECT(IWVEC(2),IELEM,ISTEP,
     .             PHIA5,NPHIA5,PHITR5,NPHITR5,A2TR5,NA2TR5,
     .             YIELDA,TAUA,APREV,S, YIELDB,ELAFRA,AONSET,
     .             CALTR5,ELATR5,APHITR5)

C     ------------------------------------------------------------------
C     YIELDB=0, ELASTIC UPDATE 
C     ------------------------------------------------------------------
      IF(YIELDB.EQ.0.0D0) THEN
         IF(IW.EQ.1)WRITE(OUT6,700)IELEM,YIELDA,YIELDB, 
     .      NA2TR5,NPHITR5,ELAFRA 
         KELAS  =KELAS+1
         SIGB(1) = ESSTR6(1)+PRESB 
         SIGB(2) = ESSTR6(2)+PRESB 
         SIGB(3) = ESSTR6(3)+PRESB 
         SIGB(4) = ESSTR6(4) 
         SIGB(5) = ESSTR6(5) 
         SIGB(6) = ESSTR6(6) 
         GAMB    = GAMA 
         TAUB    = TAUA  
         RADB    = RADA
         DO I=1,5 
            BCKB(I)=BCKA(I) 
            POLB(I)=POLA(I) 
            DO J=1,5 
               STGB(I,J)=STGA(I,J) 
            ENDDO 
         ENDDO 
         NRMSLB=NRMSLA 
         THETA=0.0D0 ! THETA NE PEUT ETRE CALCULE QUE PAR KBEULER 
         DO I=1,5 
            ANEW(I)=APREV(I) 
         ENDDO 
         IF(IW.EQ.1)WRITE(OUT6,680)(SIGB(I),I=1,6) 
         IF(ICOMPL.NE.0) CALL KMODELA(GMOD,KMOD,C) 
         IF(IW.EQ.1)WRITE(OUT6,660) 
         IF(IW.EQ.1)WRITE(OUT6,303)
         RETURN 
      ENDIF 
 
C     ------------------------------------------------------------------
C     IF YIELDB IS ZERO, THERE IS A RETURN BEFORE AND THE PROGRAM DOES 
C     NOT COME TILL HERE !  IN OTHER WORDS, IT IS SURE NOW THAT YIELDB=1
C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,640) 
      KPLAS=KPLAS+1
 
      IF(.NOT.CALTR5) THEN
         IOFF=0
         DO I=1,5
            SOV5(I)=0.0D0
         ENDDO
       ELASOV5=99.9
         CALL KYLPMUL(IWVEC(3),IELEM,IOFF,SOV5,ELASOV5,TAUA,PHITR5,S,
     .                ELATR5,APHITR5)
      ENDIF
 
      IF(ELATR5.GT.0.99D0) THEN 
         IF(IW.EQ.1) WRITE(OUT6,620) IELEM
C        ---------------------------------------------------------------
C        STATUS QUO FOR STATE VARIABLES
C        NOTE THAT YIELDB IS 1 !
C        ---------------------------------------------------------------
         GAMB    = GAMA 
         TAUB    = TAUA  
         RADB    = RADA
         DO I=1,5 
            BCKB(I) = BCKA(I) 
            POLB(I) = POLA(I) 
            DO J=1,5 
               STGB(I,J) = STGA(I,J) 
            ENDDO 
         ENDDO 
         NRMSLB = NRMSLA 
         THETA  = 0.0D0 
         DO I=1,5 
            ANEW(I) = AONSET(I) ! SURE THAT AONSET IS NOT 0 
         ENDDO 

C        ---------------------------------------------------------------
C        ELASTIC STRESS UPDATE
C        ---------------------------------------------------------------
         IF(.NOT.EXPLIC) THEN
            SIGB(1) = ESSTR6(1)+PRESB 
            SIGB(2) = ESSTR6(2)+PRESB 
            SIGB(3) = ESSTR6(3)+PRESB 
            SIGB(4) = ESSTR6(4) 
            SIGB(5) = ESSTR6(5) 
            SIGB(6) = ESSTR6(6) 

            IF(ICOMPL.NE.0) THEN 
            IF(USEELA) THEN
               IF(IW.EQ.1)WRITE(OUT6,600)
               CALL KMODELA(GMOD,KMOD, C)
            ELSE
               IF(IW.EQ.1)WRITE(OUT6,580)
               CALL KHARDEN(IWVEC(1),MATCOF,GAMA,AONSET,S,
     .                      RADA,BCKA,POLA,STGA, FVAL,H,HPRIM)
               CALL KMODCON(GMOD,KMOD,AONSET,FVAL,H,HPRIM, C) 
            ENDIF
         ENDIF 

C        ---------------------------------------------------------------
C        EXPLICIT STRESS UPDATE
C        ---------------------------------------------------------------
         ELSE

            CALL KHARDEN(IWVEC(1),MATCOF,GAMA,AONSET,S,
     .                   RADA,BCKA,POLA,STGA, FVAL,H,HPRIM)
            IF(IW.EQ.1) THEN
               WRITE(OUT6,'(A,E13.5)') 'H AFTER KHARDEN=',H
               WRITE(OUT6,'(A,E13.5)') 'ELAFRA AFTER KHARDEN=',ELAFRA
            ENDIF

            CALL KMODCON(GMOD,KMOD,AONSET,FVAL,H,HPRIM, MCON99)   
            CALL KMAKE66(MCON99, MCON66)

            DO I=1,6
               DO J=1,6
                  INCPL6(I)=MCON66(I,J)*(1.0D0-ELAFRA)*DEVDEPS(J)
               ENDDO
            ENDDO
            IF(ELAFRA.GT.1.0D0) THEN
               WRITE(OUT6,'(A,E13.5)')'STOP, ELAFRA CANT BE >1: ',ELAFRA
               CALL XIT
               STOP
            ENDIF

            SIGB(1) = ESSA6(1)+ELAFRA*A2TR6(1)+INCPL6(1)+PRESB 
            SIGB(2) = ESSA6(2)+ELAFRA*A2TR6(2)+INCPL6(2)+PRESB 
            SIGB(3) = ESSA6(3)+ELAFRA*A2TR6(3)+INCPL6(3)+PRESB 
            SIGB(4) = ESSA6(4)+ELAFRA*A2TR6(4)+INCPL6(4)
            SIGB(5) = ESSA6(5)+ELAFRA*A2TR6(5)+INCPL6(5)
            SIGB(6) = ESSA6(6)+ELAFRA*A2TR6(6)+INCPL6(6)

            IF(ICOMPL.NE.0) THEN 
               GELAFR=ELAFRA*GMOD
               CALL KMODELA(GELAFR,KMOD, C)
               DO I=1,9
                  DO J=1,9
                     C(I,J)=C(I,J)+(1.0D0-ELAFRA)*MCON99(I,J)
                  ENDDO
               ENDDO
            ENDIF

         ENDIF
         IF(IW.EQ.1)WRITE(OUT6,304)
         RETURN 
      ENDIF 
 
C     ------------------------------------------------------------------
C     INITIALISES PLASTIC STRAIN RATE FOR BACKWARD EULER 
C     ------------------------------------------------------------------
      CALL KINIDPL(IWVEC(4),MATCOF,GAMA,AONSET,S,RADA,BCKA,POLA,STGA,
     .             ELAFRA,DCOROT,GMOD, NRMINI)

      DO I=1,5 
         DPL5(I)=NRMINI*PHITR5(I)/NPHITR5 
      ENDDO 
      NRMREF=NRMINI 
 
cft   ------------------------------------------------------------------
cft   starts cutting the fishtails
cft   ------------------------------------------------------------------
cft   CALL KINIARG(IWVEC(5),APREV,PHIA5,ESSA6,ESSTR5,NRMREF,TWOG,
cft  .             DELTAT,S,GAMA,TAUA,RADA,BCKA,POLA,STGA,
cft  .             AONSET,MATCOF)
cft
cft   do i=1,5
cft      dpl5x(i)=dpl5(i)
cft   enddo
cft
cft   write(out6,'(a,i5)') 'call beuler isol=1, ielem=',ielem
cft   CALL KBEULER(IELEM,IDENT,ICOMPL,DPL5x,GMOD,KMOD, 
cft  .             GAMBx,TAUBx,RADBx,BCKBx,POLBx,STGBx,ESSB6x,NRMSLBx,
cft  .             ANEWx,Cx,IERRC) 
cft   write(out6,'(a,l5)') '    check is=',check
cft   chk(1)=check
cft
cft   call kbetap(beutr,unilen,phitr5,0)
cft   do i=1,3
cft      beutr2(i)=beutr(i)
cft   enddo
cft   beutr2(4)=beutr(4)+10.0d0*3.141592d0/180.0d0
cft   call kapbea(adpl,beutr2,0)
cft   do i=1,5
cft      dpl5y(i)=NRMINI*adpl(i)
cft   enddo
cft
cft   write(out6,'(a,i5)') 'call beuler isol=2, ielem=',ielem
cft   CALL KBEULER(IELEM,IDENT,ICOMPL,DPL5y,GMOD,KMOD, 
cft  .             GAMBy,TAUBy,RADBy,BCKBy,POLBy,STGBy,ESSB6y,NRMSLBy,
cft  .             ANEWy,Cy,IERRC) 
cft   write(out6,'(a,l5)') '    check is=',check
cft   chk(2)=check
cft
cft   do i=1,3
cft      beutr2(i)=beutr(i)
cft   enddo
cft   beutr2(4)=beutr(4)-10.0d0*3.141592d0/180.0d0
cft   call kapbea(adpl,beutr2,0)
cft   do i=1,5
cft      dpl5z(i)=NRMINI*adpl(i)
cft   enddo
cft
cft   write(out6,'(a,i5)') 'call beuler isol=3, ielem=',ielem
cft   CALL KBEULER(IELEM,IDENT,ICOMPL,DPL5z,GMOD,KMOD, 
cft  .             GAMBz,TAUBz,RADBz,BCKBz,POLBz,STGBz,ESSB6z,NRMSLBz,
cft  .             ANEWz,Cz,IERRC) 
cft   write(out6,'(a,l5)') '    check is=',check
cft   chk(3)=check
cft
cft   if(.not.chk(1)) then
cft      call kx6_2x5(essb6x, tmp5)
cft      do i=1,5
cft          tmp5(i)=tmp5(i)-bckbx(i)
cft      enddo
cft      call klength(tmp5,5,1, len(1))
cft   endif
cft   if(.not.chk(2)) then
cft      call kx6_2x5(essb6y, tmp5)
cft      do i=1,5
cft          tmp5(i)=tmp5(i)-bckby(i)
cft      enddo
cft      call klength(tmp5,5,1, len(2))
cft   endif
cft   if(.not.chk(3)) then
cft      call kx6_2x5(essb6z, tmp5)
cft      do i=1,5
cft          tmp5(i)=tmp5(i)-bckbz(i)
cft      enddo
cft      call klength(tmp5,5,1, len(3))
cft   endif
cft   if(chk(1).and.chk(2).and.chk(3)) then
cft      isol=1
cft      write(out6,'(a)') '[ktexhar] none of the kbeuler converged'
cft   else
cft      do itry=1,3
cft         if(.not.chk(itry)) then
cft            isol=itry
cft            lenmin=len(itry)      
cft         endif
cft      enddo
cft      do itry=1,3
cft         if(.not.chk(itry)) then
cft            write(out6,'(a,i5,a,e13.5)') 'itry=',itry,
cft  .         ', len=',len(itry)
cft             if(len(itry).lt.lenmin) then
cft                isol=itry
cft                lenmin=len(itry)
cft             endif
cft         endif
cft      enddo
cft   endif
cft   if(isol.eq.1) then
cft      gamb=gambx
cft      taub=taubx
cft      radb=radbx
cft      nrmslb=nrmslbx 
cft      do i=1,5
cft         dpl5(i)=dpl5x(i)
cft         polb(i)=polbx(i)
cft         bckb(i)=bckbx(i)
cft         do j=1,5
cft            stgb(i,j)=stgbx(i,j)
cft         enddo
cft         anew(i)=anewx(i)
cft      enddo
cft      do i=1,6
cft         essb6(i)=essb6x(i)
cft      enddo
cft      do i=1,9
cft         do j=1,9
cft            c(i,j)=cx(i,j)
cft         enddo
cft      enddo
cft   endif
cft   if(isol.eq.2) then
cft      gamb=gamby
cft      taub=tauby
cft      radb=radby
cft      nrmslb=nrmslby 
cft      do i=1,5
cft         dpl5(i)=dpl5y(i)
cft         polb(i)=polby(i)
cft         bckb(i)=bckby(i)
cft         do j=1,5
cft            stgb(i,j)=stgby(i,j)
cft         enddo
cft         anew(i)=anewy(i)
cft      enddo
cft      do i=1,6
cft         essb6(i)=essb6y(i)
cft      enddo
cft      do i=1,9
cft         do j=1,9
cft            c(i,j)=cy(i,j)
cft         enddo
cft      enddo
cft   endif
cft   if(isol.eq.3) then
cft      gamb=gambz
cft      taub=taubz
cft      radb=radbz
cft      nrmslb=nrmslbz 
cft      do i=1,5
cft         dpl5(i)=dpl5z(i)
cft         polb(i)=polbz(i)
cft         bckb(i)=bckbz(i)
cft         do j=1,5
cft            stgb(i,j)=stgbz(i,j)
cft         enddo
cft         anew(i)=anewz(i)
cft      enddo
cft      do i=1,6
cft         essb6(i)=essb6z(i)
cft      enddo
cft      do i=1,9
cft         do j=1,9
cft            c(i,j)=cz(i,j)
cft         enddo
cft      enddo
cft   endif
cft   ------------------------------------------------------------------
cft   end of cutting the fishtails
cft   ------------------------------------------------------------------

C     ------------------------------------------------------------------
C     YIELDB=1 AND RL<1.0 : BACKWARD EULER PROJECTION 
C     KBEULER : EVERYTHING IS 5-DIM, SDS-AXIS. 
C     KBEULER UPDATES THE STATE VARIABLES AND COMPUTES ESSB6 (6-DIM) 
C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,560)
           
      CALL KINIARG(IWVEC(5),APREV,PHIA5,ESSA6,ESSTR5,NRMREF,TWOG,
     .             DELTAT,S,GAMA,TAUA,RADA,BCKA,POLA,STGA,
     .             AONSET,MATCOF)

     
      CALL KBEULER(IELEM,IDENT,ICOMPL,DPL5,GMOD,KMOD, 
     .             GAMB,TAUB,RADB,BCKB,POLB,STGB,ESSB6,NRMSLB,
     .             ANEW,C,IERRC) 

     
C     ------------------------------------------------------------------
C     IF PROBLEM DURING KMODPLA, LOAX3D WILL REDO 
C     THIS ITERATION BUT THIS TIME, WITH NUMERICAL PERTURBATIONS TO 
C     COMPUTE THE TANGENT MODULUS.  LOAX3D DETECTS THIS POSSIBILITY BY 
C     CHECKING IF ISTRA(5) IS MODIFIED AFTER CALL LOI2.  ANI4VH GOES 
C     ON EVEN IF THE ERROR IN MODPLA WAS DETECTED (I.E. NO RETURN) 
C     ------------------------------------------------------------------
      IF (IERRC.EQ.1) ISTRA(5)=ISTRA(5)+1 
 
      CALL KLENGTH(APREV,5,1, TEMP)
      IF (TEMP.EQ.0.0D0) THEN 
 
C        THE ELEMENT HAS NEVER BEEN PLASTIC. THETA IS THE 
C        ANGLE BETWEEN BEGIN AND END OF INCREMENT. 

         CALL KPRODUC(ANEW,AONSET,5, THETA)

      ELSE 
 
C        ONE USES ANOTHER FORMULA THAT CAN HAVE TWO SIGNIFICATIONS : 
C        * IF YIELDA WAS 1, APREV AND AONSET DESIGNATES THE SAME NORMAL, 
C        THEREFORE, THETA IS THE ANGLE BETWEEN BEGIN AND END INCREMENT. 
C        * IF YIELDA WAS 0, AONSET IS NOT THE SAME AS APREV (DEFINED THE 
C        LAST TIME THE ELEMENT WAS PLASTIC).  THETA IS THE ANGLE BETWEEN 
C        THE LAST NORMAL AND THE NORMAL AT THE END OF THE INCREMENT. 
 
         CALL KPRODUC(ANEW,APREV,5, THETA)

      ENDIF 
 
      IF(ABS(THETA).LE.1.0D0) THETA=DACOS(THETA)*RTD
      IF(THETA.GT.1.0D0.AND.THETA.LT.1.001D0) THETA=  0.0D0
      IF(THETA.LT.-1.0D0.AND.THETA.GT.-1.001D0) THETA=180.0D0
 
      SIGB(1)  = ESSB6(1)+PRESB 
      SIGB(2)  = ESSB6(2)+PRESB 
      SIGB(3)  = ESSB6(3)+PRESB 
      SIGB(4)  = ESSB6(4) 
      SIGB(5)  = ESSB6(5) 
      SIGB(6)  = ESSB6(6) 
      CALL KX5_2X6(BCKB,BCK6) 
      DO I=1,6 
         SIGTAY(I)= (ESSB6(I)-BCK6(I))/TAUB 
      ENDDO

      IF(IW.EQ.1)WRITE(OUT6,540)
     .  (APREV(I),I=1,5),(ANEW(I) ,I=1,5),TAUB-TAUA,
     .  (ESSB6(I),I=1,6),NRMSLB ,THETA,(SIGB(I),I=1,6),
     .  (SIGTAY(I),I=1,6)

      IF(ELETAY.GT.0.AND.IELEM.EQ.ELETAY) THEN
         CALL KX5_2X6(DPL5, DPL6)
         DO I=1,6
            HISTORY(I)=DPL6(I)*DELTAT
         ENDDO
      ENDIF
C     ------------------------------------------------------------------
C     FORMATS 
C     ------------------------------------------------------------------
900   FORMAT(/,50('>'),'KANI4VH',50('<')           ,/,
     .       3X,'[KTEXHAR] BEGIN OF KANI4VH'       ,/,
     .       3X,'          ISTEP         : ', I5   ,/,
     .       3X,'          IELEM         : ', I5   ,/, 
     .       3X,'          MATCOF(1...7) : ',7E13.5,/, 
     .       3X,'          MATCOF(8..14) : ',7E13.5,/, 
     .       3X,'          EMOD,ANU      : ',2E13.5,/) 
890   FORMAT(3X,'[KTEXHAR] LENGTH OF UREF: ', E13.5,/)
892   FORMAT(3X,'[KTEXHAR] USEELA : ',L1,/,
     .       3X,'          EXPLIC : ',L1,/,
     .       3X,'          ITRY3  : ',I5,/)
891   FORMAT(9X,'[KYLPMUL] KWARNING! YOU DONT CUT THE FISHTAILS',/)
880   FORMAT(3X,'[KTEXHAR] SIGA          : ',6E13.5,/, 
     .       3X,'          PRESA         : ',6E13.5,/) 
860   FORMAT(3X,'[KTEXHAR] PHIA5         : ',5E13.5,/,
     .       3X,'          NPHIA5        : ', E13.5,/,/,
     .       3X,'          DCOROT        : ',6E13.5,/, 
     .       3X,'          A2TR6         : ',6E13.5,/,
     .       3X,'          A2TR5         : ',5E13.5,/,
     .       3X,'          NA2TR5        : ', E13.5,/,/,
     .       3X,'          PHITR5        : ',5E13.5,/,
     .       3X,'          NPHITR5       : ', E13.5,/,/,
     .       3X,'          APREV         : ',5E13.5,/,
     .       3X,'          YIELDA        : ', E13.5,/)
800   FORMAT(3X,'[KTEXHAR] NA2TR5/TAUA<1.0D-5') 
780   FORMAT(3X,'[KTEXHAR]    YIELDA WAS 0 -> ELASTIC UPDATE ',/,
     .       3X,'                          -> ELASTIC MODULUS',/,
     .       3X,'          -> RETURN (NA2TR5/TAUA<1.0D-5 + KMODELA)',/)
760   FORMAT(3X,'[KTEXHAR]    YIELDA WAS 1 -> STATUS QUO @ IELEM: ',I5)
740   FORMAT(3X,'                          -> ELASTIC MODULUS',/,
     .       3X,'                          -> RETURN',/)
720   FORMAT(3X,'                          -> CONTINU MODULUS',/,
     .       3X,'          -> RETURN (NA2TR5/TAUA<1.0D-5 + KMODCON)',/)
700   FORMAT(3X,'[KTEXHAR] NEW STRESS STATE IS ELASTIC',/, 
     .       3X,'          IELEM        : ', I5   ,/, 
     .       3X,'          YIELDA       : ', E13.5,/, 
     .       3X,'          YIELDB       : ', E13.5,/,
     .       3X,'          NA2TR5       : ', E13.5,/,
     .       3X,'          NPHITR5      : ', E13.5,/,
     .       3X,'          ELAFRA       : ', E13.5,/) 
680   FORMAT(3X,'[KTEXHAR] SIGB         : ',6E13.5  ) 
660   FORMAT(3X,'[KTEXHAR] -> RETURN (ELASTIC UPDATE)',/) 
640   FORMAT(3X,'[KTEXHAR] NEW STRESS STATE IS PLASTIC',/) 
620   FORMAT(3X,'[KTEXHAR] ELATR5 > 0.99,  -> EL. UPDATE @ IELEM: ',I5)
600   FORMAT(3X,'                          -> ELASTIC MODULUS',/,
     .       3X,'          -> RETURN (ELATR5 > 0.99 + KMODELA)',/)
580   FORMAT(3X,'                          -> CONTINU MODULUS',/,
     .       3X,'          -> RETURN (ELATR5 > 0.99 + KMODCON)',/)
560   FORMAT(3X,'[KTEXHAR] PREPARING AND CALLING KBEULER',/)
540   FORMAT(3X,'[KTEXHAR] RESULTS OF KBEULER',/,
     .       3X,'          APREV        : ',5E13.5,/,
     .       3X,'          ANEW         : ',5E13.5,/,
     .       3X,'          TAUB-TAUA    : ', E13.5,/,
     .       3X,'          ESSB6        : ',6E13.5,/,
     .       3X,'          NRMSLB       : ', E13.5,/,
     .       3X,'          THETA        : ', E13.5,/,
     .       3X,'          SIGB         : ',6E13.5,/,
     .       3X,'          SIGTAY       : ',6E13.5,/,
     .       3X,'          -> RETURN (AFTER KBEULER)',/)
301   FORMAT(3X,'[KTEXHAR] EXIT 1: YIELDA=0 AND SMALL NA2TR5'        ,/,
     .       3X,18X,'->SIGB  =SIGA+INCEL'                            ,/,
     .       3X,18X,'  YIELDB=0'                                     ,/,
     .       3X,18X,'  QB    =QA'                                    ,/,
     .       3X,18X,'  MODELA'                                       ,/)
302   FORMAT(3X,'[KTEXHAR] EXIT 2: YIELDA=1 AND SMALL NA2TR5'        ,/,
     .       3X,18X,'->SIGB  =SIGA'                                  ,/,
     .       3X,18X,'  YIELDB=1'                                     ,/,
     .       3X,18X,'  QB   =QA'                                     ,/,
     .       3X,18X,'  IF USEELA  MODELA'                            ,/,
     .       3X,18X,'  ELSE       MODCON'                            ,/)
303   FORMAT(3X,'[KTEXHAR] EXIT 3: KDETECT-> YIELDB=0'               ,/,
     .       3X,18X,'->SIGB=SIGA+INCEL'                              ,/,
     .       3X,18X,'  QB  =QA'                                      ,/,
     .       3X,18X,'  MODELA'                                       ,/)
304   FORMAT(3X,'[KTEXHAR] EXIT 4: KDETECT->YIELDB=0 AND ELATR5>0.99',/,
     .       3X,18X,'IF .NOT.EXPLIC'                                 ,/,
     .       3X,18X,'->SIGB=SIGA+INCEL'                              ,/,
     .       3X,18X,'  QB  =QA'                                      ,/,
     .       3X,18X,'  MODELA'                                       ,/,
     .       3X,18X,'ELSE:'                                          ,/,
     .       3X,18X,'->SIGB=SIGA+ELAFRA*INCEL+(1-ELAFRA)*INCPL'      ,/,
     .       3X,18X,'  QB  =QA'                                      ,/,
     .       3X,18X,'  ELAFRA*MODELA+(1-ELAFRA)*MODCON'              ,/)
305   FORMAT(3X,'[KTEXHAR] EXIT 5: KDETECT->YIELDB=0 AND ELATR5<0.99',/,
     .       3X,18X,'->SIGB=BACKWARD PROJECTION OF TRIAL'            ,/,
     .       3X,18X,'  QB  =UPDATED VARIALBES'                       ,/,
     .       3X,18X,'  MODPLA'                                       ,/)
C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,305)
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KBEULER(IELEM,IDENT,ICOMPL,DPL5,GMOD,KMOD,
     .                    YAMB,YAUB,YADB,YCKB,YOLB,YTGB,YSSB6,YRMSLB,
     .                    YNEW,C,IERRC)
C     ------------------------------------------------------------------
CU    SUBROUTINE KBEULER USES SUBROUTINES
CU       KNEWT
CU       KYLPMUL
CU       KHARDEN
CU       KMODPLA
C     ------------------------------------------------------------------
CA    CALL KBEULER(IELEM,IDENT,ICOMPL,DPL5,GMOD,KMOD,
CA   .             YAMB,YAUB,YADB,YCKB,YOLB,YTGB,YSSB6,YRMSLB,
CA   .             YNEW,C,IERRC)
C     ------------------------------------------------------------------
CB    SUBROUTINE KBEULER USES COMMON BLOCKS
CB       KNCHOUT
CB       KARGUM1
CB       KARGUM2
CB       KTAUOLD
CB       KELATR5
C     ------------------------------------------------------------------
CT    COMMENTS FOR SUBROUTINE KBEULER
CT       THE FIRST TWO LINES OF /KARGUM1/ ARE INITIALISED IN ANI4VH
CT       BEFORE THE CALL TO KBEULER.
CT       THE LAST LINE OF /KARGUM1/ IS COMPUTED WHEN FUNCV IS CALLED
CT       EVERYTHING IS 5-DIMENSIONAL EXCEPT THE OUTPUT ESSB6(6).
CT       STRESSES ARE IN 'SIMULATION DEVIATORIC STRESS'-AXIS
C     ------------------------------------------------------------------
      IMPLICIT  NONE
      !
      ! INPUT
      !
      INTEGER   IELEM,IDENT,ICOMPL
      REAL*8    DPL5(5),GMOD,KMOD
      REAL*8    YAMB,YAUB,YADB,YCKB(5),YOLB(5),YTGB(5,5),YSSB6(6),YRMSLB
      !
      ! OUTPUT
      !
      INTEGER   IERRC
      REAL*8    YNEW(5),C(9,9)
      !
      ! LOCAL
      !
      INTEGER   I,J
      INTEGER   IW,IOFF
      REAL*8    PHITR5(5),RL,SOV5(5),ELASOV5,ACHECK(5)
      REAL*8    INIT(5),RTD
      REAL*8    H,HPRIM(3,3)
      LOGICAL   CHECK
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT

      INTEGER   IWB
      REAL*8    AVA(5),FVALA,DFA(5),DDFA(5,5),UVA(5)
      REAL*8    PHIA5(5),ESSA6(6),ESSTR5(5),NRMREF,TWOG,
     .          DELTAT,S(600),GAMA,RADA,TAUA,BCKA(5),POLA(5),STGA(5,5),
     .          AONSET(5),MATCOF(14)
      REAL*8    GAMB,TAUB,RADB,BCKB(5),POLB(5),STGB(5,5),
     .          ESSB6(6),NRMSLB,ANEW(5),
     .          FVALB,DFB(5),DDFB(5,5),UVB(5),HPB,HXB,SDB,XSATB,NRMEPL
      COMMON   /KARGUM1/ !INPUT
     .          AVA,FVALA,DFA,DDFA,UVA,
     .          PHIA5,ESSA6,ESSTR5,NRMREF,TWOG,DELTAT,S,
     .          GAMA,TAUA,RADA,BCKA,POLA,STGA,AONSET,MATCOF,
     .          !18 RESULTS
     .          GAMB,TAUB,RADB,BCKB,POLB,STGB,ESSB6,NRMSLB,ANEW,
     .          FVALB,DFB,DDFB,UVB,HPB,HXB,SDB,XSATB,NRMEPL
      COMMON   /KARGUM2/ IWB

      REAL*8    TAUOLD
      COMMON   /KTAUOLD/ TAUOLD

      INTEGER   ISTEP,ITERA      
      REAL*8    ELATR5
      COMMON   /KELATR5/ ELATR5,ISTEP,ITERA
C     ------------------------------------------------------------------
C     NEWTON-RAPHSON MINIMIZATION
C     ------------------------------------------------------------------
      IW=IWB
      TAUOLD=TAUA
      RTD=180.0D0/DACOS(-1.0D0)
      IF(IW.EQ.1)WRITE(OUT6,900)
      DO I=1,5
         INIT(I)=DPL5(I)/NRMREF
      ENDDO

      IF(IW.EQ.1)WRITE(OUT6,880) (DPL5(I),I=1,5),(INIT(I),I=1,5)
      CALL KNEWT(INIT,5,CHECK,IW)

C     ------------------------------------------------------------------
C     THE SOLUTION DPL5 IS SEND BACK TO ANI4VH.  NEVERTHELESS, NOTE THAT
C     DPL5_SOLUTION IS NOT ACTUALLY NEEDED AFTER CALL KBEULER.
C     ------------------------------------------------------------------
      DO I=1,5
         DPL5(I)=INIT(I)*NRMREF
      ENDDO

      IF (NRMEPL.LT.1.0D-7) THEN
         WRITE(OUT6,865) IELEM,ISTEP,ITERA
         WRITE(OUT6,860) NRMEPL,ELATR5,(ANEW(I),I=1,5)
         IOFF=0
         DO I=1,5
            SOV5(I)=0.0D0
         ENDDO
       ELASOV5=99.9
         DO I=1,5
            PHITR5(I)=ESSTR5(I)-BCKA(I)
         ENDDO
         CALL KYLPMUL(IW,IELEM,IOFF,SOV5,ELASOV5,TAUA,PHITR5,S,
     .                RL,ACHECK)
         WRITE(OUT6,840) (ACHECK(I),I=1,5)
      ENDIF

C     ------------------------------------------------------------------
C     COPY THE RESULTS FROM THE COMMON BLOCK /KARGUM1/ (FILLED 
C     IN FUNCV DURING THE NEWTON-RAPHSON PROCEDURE) ONTO THE 8 FIRST
C     OUT-ARGUMENTS OF SUBROUTINE BEULER(IN,OUT)
C     ------------------------------------------------------------------
      YAMB=GAMB
      YAUB=TAUB
      YADB=RADB
      DO I=1,5
         YCKB(I)=BCKB(I)
         YOLB(I)=POLB(I)
         DO J=1,5
            YTGB(I,J)=STGB(I,J)
         ENDDO
      ENDDO
      DO I=1,6
         YSSB6(I)=ESSB6(I)
      ENDDO
      YRMSLB=NRMSLB
      DO I=1,5
         YNEW(I)=ANEW(I)
      ENDDO

C     ------------------------------------------------------------------
C     COMPUTE THE CONSISTENT ELASTO-PLASTIC TANGENT MODULUS (LAST OUT-
C     ARGUMENT OF BEULER(IN,OUT)
C     ------------------------------------------------------------------
      IF(ICOMPL.NE.0) THEN
         CALL KHARDEN(IW,MATCOF,GAMB,ANEW,S,
     .                RADB,BCKB,POLB,STGB, FVALB,H,HPRIM)
         CALL KMODPLA(IW,IDENT, NRMEPL,ANEW,FVALB,DFB,DDFB,UVB,TAUB,
     .                H,HPRIM,GMOD,KMOD,
     .                IERRC,C)
      ENDIF
      IF(IW.EQ.1)WRITE(OUT6,820)

C     ------------------------------------------------------------------
C     FORMATS
C     ------------------------------------------------------------------
900   FORMAT(9X,'[KBEULER] BEGIN')
880   FORMAT(9X,'[KBEULER] DPL5 : ',5E13.5,/,
     .       9X,'          INIT : ',5E13.5  )
865   FORMAT(9X,'[KBEULER] IELEM      : ',I5,/,
     .       9X,'          ISTEP      : ',I5,/,
     .       9X,'          ITERA      : ',I5)
860   FORMAT(9X,'[KBEULER] KWARNING! SMALL NRMEPL AFTER NEWT: ',E13.5,/,
     .       9X,'          ELATR5 WAS : ', E13.5,/,
     .       9X,'          ANEW...... : ',5E13.5)
840   FORMAT(9X,'[KBEULER] ACHECK.... : ',5E13.5)
820   FORMAT(9X,'[KBEULER] EXIT',/)
C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KDETECT(IW,IELEM,ISTEP,
     .                    PHIA5,NPHIA5,PHITR5,NPHITR5,A2TR5,NA2TR5,
     .                    YIELDA,TAU,APREV,S, YIELDB,ELAFRA,AONSET,
     .                    CALTR5,ELATR5,APHITR5)
C     ------------------------------------------------------------------
CU    SUBROUTINE KDETECT USES SUBROUTINES 
CU       KPRODUC
CU       XIT
CU       KYLPMUL
CU       KPRODUC
CU       KYLPBIS
CU       KLENGTH
C     ------------------------------------------------------------------
CA    CALL KDETECT(IW,IELEM,ISTEP,
CA   .             PHIA5,NPHIA5,PHITR5,NPHITR5,A2TR5,NA2TR5,
CA   .             YIELDA,TAU,APREV,S, YIELDB,ELAFRA,AONSET)
CA   .             CALTR5,ELATR5,APHITR5)
C     ------------------------------------------------------------------
CB    SUBROUTINE KDETECT USES COMMON BLOCKS
CB       KNCHOUT
CB       KSTATIS
C     ------------------------------------------------------------------
      IMPLICIT  NONE
      !
      ! INPUT 
      !
      INTEGER   IW,IELEM,ISTEP
      REAL*8    PHIA5(5),NPHIA5,PHITR5(5),NPHITR5,A2TR5(5),NA2TR5,
     .          YIELDA,TAU,APREV(5),S(600) 
      !
      ! OUTPUT 
      !
      REAL*8    YIELDB,ELAFRA,AONSET(5)
      REAL*8    ELATR5,APHITR5(5)
      LOGICAL   CALTR5
      !
      ! LOCAL 
      !
      INTEGER   I,IOFF
      REAL*8    PROD,SOV5(5),ELASOV5,APHIA5(5),ELPHIA5
      REAL*8    PROJC5(5),PR2TR5(5),NPR2TR5,APROJ5(5),PHIYLP(5)
      REAL*8    TOL001,TOL010,TOL099,TOL101,TOL50
      PARAMETER(TOL001=0.01D0,TOL010=0.10D0,
     .          TOL099=0.99D0,TOL101=1.01D0,TOL50=50.0D0)
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT

      INTEGER   LSTSTP,ITETOT,KELAS,KPLAS,KBIG,KTEXH,KDTEC,
     .          KTRIV,KPHIA1,KPHIA2,KOFFS,KPROD,KTRIA1,KTRIA2,KBISS,
     .          KYLPER,KYLP0,KYLP0A,KYLP0B,KYLP0C,
     .          KYLP1,KYLP1A,KYLP1B,KYLP1C
      COMMON   /KSTATIS/ LSTSTP,ITETOT,KELAS,KPLAS,KBIG,KTEXH,KDTEC,
     .          KTRIV,KPHIA1,KPHIA2,KOFFS,KPROD,KTRIA1,KTRIA2,KBISS,
     .          KYLPER,KYLP0,KYLP0A,KYLP0B,KYLP0C,
     .          KYLP1,KYLP1A,KYLP1B,KYLP1C
C     ------------------------------------------------------------------
C     BEGIN
C     ------------------------------------------------------------------

      IF(IW.EQ.1)WRITE(OUT6,920)IELEM,ISTEP
      CALTR5=.FALSE.
      KDTEC=KDTEC+1
      IF(IW.EQ.1)WRITE(OUT6,900) (NPHITR5/TAU.LT.TOL010)

      IF(NPHITR5/TAU.LT.TOL010) THEN 
         KTRIV=KTRIV+1
         YIELDB=0.0D0    ! OUTPUT 2 (ELAFRA) AND 3 (AONSET) ARE FILLED
         ELAFRA=1.0D0    ! ALTHOUGH THEIR VALUES DONT MATTER IN KTEXHAR
         DO I=1,5        ! WHEN YIELDB=0
            AONSET(I)=0.0D0 
         ENDDO
         IF(IW.EQ.1)WRITE(OUT6,880) IELEM
         RETURN ! RETURN 1, 0 CALL TO KYLP
      ENDIF

C     ------------------------------------------------------------------
C     TEMPORARY VERSION 10
C     ------------------------------------------------------------------
C11   IOFF=0 
C11   DO I=1,5 
C11      SOV5(I)=0.0D0 
C11   ENDDO 
C11   ELASOV5=99.9
C11   CALL KYLPMUL(IW,IELEM,IOFF,SOV5,ELASOV5,TAU,PHITR5,S, 
C11  .             ELATR5,APHITR5)
C11
C11   IF(ELATR5.GE.1.0D0) THEN
C11      YIELDB=0.0D0    ! OUTPUT 2 (ELAFRA) AND 3 (AONSET) ARE FILLED
C11      ELAFRA=1.0D0    ! ALTHOUGH THEIR VALUES DONT MATTER IN KTEXHAR
C11      DO I=1,5        ! WHEN YIELDB=0
C11         AONSET(I)=0.0D0 
C11      ENDDO
C11   ELSE
C11      YIELDB=1.0D0
C11      ELAFRA=0.0D0
C11      DO I=1,5
C11         AONSET(I)=APHITR5(I)
C11      ENDDO
C11   ENDIF
C11   RETURN

C     ------------------------------------------------------------------
C     CHECK THE QUALITY OF THE OFFSET PHIA5
C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,860) (NPHIA5/TAU.LT.TOL001)
      IF(NPHIA5/TAU.LT.TOL001) THEN
         KPHIA1=KPHIA1+1
         ELPHIA5=2.0D0*TOL50 ! I.E. SMALL OFFSET, LARGE ELASTIC FRACTION
         IF(IW.EQ.1)WRITE(OUT6,840) ELPHIA5
      ELSE
         KPHIA2=KPHIA2+1
         IOFF=0 
         DO I=1,5 
            SOV5(I)=0.0D0 
         ENDDO 
       ELASOV5=99.9
         IF(IW.EQ.1)WRITE(OUT6,820)
         CALL KYLPMUL(IW,IELEM,IOFF,SOV5,ELASOV5,TAU,PHIA5,S, 
     .                ELPHIA5,APHIA5)
         IF(IW.EQ.1)WRITE(OUT6,800) ELPHIA5,(APHIA5(I),I=1,5)
      ENDIF

C     ------------------------------------------------------------------
C     POSSIBLE STOP STATEMENTS
C     ------------------------------------------------------------------
      IF(ELPHIA5.LT.TOL099) THEN
         WRITE(OUT6,780) ELPHIA5
         CALL XIT
         STOP
      ENDIF
      IF(ELPHIA5.GT.TOL101.AND.YIELDA.EQ.1.D0) THEN
         WRITE(OUT6,760) ELPHIA5
         CALL XIT
         STOP
      ENDIF

C     ------------------------------------------------------------------
C     KDETECT GOES ON
C     ------------------------------------------------------------------
      IF(ELPHIA5.GT.TOL101) THEN
         KOFFS=KOFFS+1
         IF(NPHIA5.EQ.0.0D0) THEN
            IOFF=0 
         ELSE
            IOFF=1 
         ENDIF
         DO I=1,5 
            SOV5(I)=PHIA5(I) 
         ENDDO
         ELASOV5=ELPHIA5
         IF(IW.EQ.1)WRITE(OUT6,740)IOFF,(SOV5(I),I=1,5),(A2TR5(I),I=1,5)
         CALL KYLPMUL(IW,IELEM,IOFF,SOV5,ELASOV5,TAU,A2TR5,S,
     .                ELAFRA,AONSET) 
         YIELDB=0.0D0
         IF(ELAFRA.LT.1.0D0)YIELDB=1.0D0 ! LT SUCH THAT THE INIT DP.NE.0
         IF(IW.EQ.1)WRITE(OUT6,720)YIELDB,ELAFRA,(AONSET(I),I=1,5),IELEM
         RETURN ! RETURN 2, 2 CALL TO KYLP                         
      ENDIF

C     ------------------------------------------------------------------
C     FROM NOW ON, SURE THAT ELPHIA5.LE.1.01
C     ------------------------------------------------------------------
      IF(YIELDA.EQ.1.0D0) THEN
         DO I=1,5
            PROJC5(I)=ELPHIA5*PHIA5(I)
            PR2TR5(I)=PHITR5(I)-PROJC5(I)
            APROJ5(I)=APHIA5(I)
         ENDDO
         CALL KPRODUC(APROJ5,PR2TR5,5, PROD)
         IF(IW.EQ.1)WRITE(OUT6,700) ELPHIA5,(PROD.EQ.0.0D0)
         IF(PROD.GE.0.0D0) THEN
            YIELDB=1.0D0
            ELAFRA=0.0D0
            DO I=1,5
               AONSET(I)=APROJ5(I)
            ENDDO
            IF(IW.EQ.1)WRITE(OUT6,680)YIELDB,ELAFRA,(AONSET(I),I=1,5),
     .                 IELEM
            KPROD=KPROD+1
            RETURN ! RETURN 3, 1 CALL TO KYLP
         ENDIF
      ENDIF

C     ------------------------------------------------------------------
C     SURE THAT ELPHIA5.LE.1.01
C     IF YIELDA.EQ.1.0, PHIA5 AS BEEN PROJECTED ON THE YL TO COMPENSATE
C                       EXACTLY POSSIBLE DEFAULT DUE TO TOLERANCES
C     IF YIELDA.EQ.0.0, THE POINT SHOULD BE INSIDE THE YL
C     ------------------------------------------------------------------
      IOFF=0 
      DO I=1,5 
         SOV5(I)=0.0D0 
      ENDDO 
      ELASOV5=99.9
      IF(IW.EQ.1)WRITE(OUT6,660)
      KTRIA1=KTRIA1+1
      CALL KYLPMUL(IW,IELEM,IOFF,SOV5,ELASOV5,TAU,PHITR5,S, 
     .             ELATR5,APHITR5)
      CALTR5=.TRUE.
      IF(IW.EQ.1)WRITE(OUT6,640)ELATR5,(APHITR5(I),I=1,5)

      IF(ELATR5.GE.1.0D0) THEN
         YIELDB=0.0D0    ! OUTPUT 2 (ELAFRA) AND 3 (AONSET) ARE FILLED
         ELAFRA=1.0D0    ! ALTHOUGH THEIR VALUES DONT MATTER IN KTEXHAR
         DO I=1,5        ! WHEN YIELDB=0
            AONSET(I)=0.0D0 
         ENDDO
         IF(IW.EQ.1)WRITE(OUT6,620)YIELDB,ELAFRA,(AONSET(I),I=1,5),IELEM
         KTRIA2=KTRIA2+1
         RETURN ! RETURN 4, 2 CALL TO KYLP
      ENDIF

C     ------------------------------------------------------------------
C     SURE THAT ELPHIA5.LE.1.01
C     YIELDA CAN BE 0 OR 1
C     THE TRIAL STRESS IS OUTSIDE THE YL
C     -> YIELDB IS 1.0
C     -> STILL NEED TO COMPUTE ELAFRA AND AONSET
C     ------------------------------------------------------------------
      YIELDB=1.0D0
      KBISS=KBISS+1
      IF(ELPHIA5.GE.1.0D0) THEN
         IF(IW.EQ.1)WRITE(OUT6,600)
         CALL KYLPBIS(IW,IELEM,TAU,PHIA5,PHITR5,NA2TR5,S,
     .                ELAFRA,AONSET,PHIYLP)
         IF(IW.EQ.1)WRITE(OUT6,580) ELAFRA,(AONSET(I),I=1,5),IELEM
         RETURN ! RETURN 5, 2 CALL TO KYLP + BISSECTION
      ELSE
         DO I=1,5
            PROJC5(I)=ELPHIA5*PHIA5(I)
            PR2TR5(I)=PHITR5(I)-PROJC5(I)
         ENDDO
         CALL KLENGTH(PR2TR5,5,1, NPR2TR5)
         IF(IW.EQ.1)WRITE(OUT6,560)
         CALL KYLPBIS(IW,IELEM,TAU,PROJC5,PHITR5,NPR2TR5,S,
     .                ELAFRA,AONSET,PHIYLP)
         IF(IW.EQ.1)WRITE(OUT6,540) ELAFRA,(AONSET(I),I=1,5),IELEM
         RETURN ! RETURN 6, 2 CALL TO KYLP + BISSECTION
      ENDIF

C     ------------------------------------------------------------------
C     FORMATS 
C     ------------------------------------------------------------------
920   FORMAT(6X,'[KDETECT] BEGINS DETECTION, IELEM: ',I5,', ISTEP: ',I5)
900   FORMAT(6X,'[KDETECT] IS THIS A VERY SMALL TRIAL STRESS  ? ',L1   ) 
880   FORMAT(6X,'[KDETECT] VERY SMALL PHITR5 -> ELASTIC'             ,/,
     .       6X,'          EXIT1, IELEM WAS:',I5                     ,/) 
860   FORMAT(6X,'[KDETECT] IS THIS A VERY SMALL OFFSET STRESS ? ',L1   ) 
840   FORMAT(6X,'[KDETECT] VERY SMALL OFFSET -> SETS ELPHIA5=',E13.5   ) 
820   FORMAT(6X,'[KDETECT] NOT VERY SMALL OFFSET -> CALL KYLPMUL'      ,
     .                     ' TO DETERMINE ELPHIA5'                   ,/)
800   FORMAT(6X,'[KDETECT] NOT VERY SMALL OFFSET -> KYLPMUL GIVES:'  ,/,
     .       6X,'          ELPHIA5: ', E13.5                         ,/,
     .       6X,'          APHIA5 : ',5E13.5                           ) 
780   FORMAT(6X,'[KDETECT] KWARNING! ELPHIA5 IS < 0.99'              ,/,
     .       6X,'          MEANS THAT THE INITIAL STRESS POINT IS'   ,/,
     .       6X,'          QUITE BEYOND THE YL, IMPOSSIBLE!, STOP'     )
760   FORMAT(6X,'[KDETECT] KWARNING!  YIELDA=1 AND ELPHIA5=',E13.5   ,/,
     .       6X,'          >1.01 MEANS THAT THE INITIAL STRESS POINT',/,
     .       6X,'          IS QUITE INSIDE THE YL WHEREAS IT SHOULD' ,/,
     .       6X,'          BE ON THE YL, IMPOSSIBLE!, STOP'            )
740   FORMAT(6X,'[KDETECT] ELPHIA5.GT.1.01 -> CALL KYLP WITH:'       ,/,
     .       6X,'          IOFF            :', I5                    ,/,
     .       6X,'          SOV5            :',5E13.5                 ,/,
     .       6X,'          SEARCH VECTOR   :',5E13.5                   )
720   FORMAT(6X,'[KDETECT] YLP FINDS:'                               ,/,
     .       6X,'          YIELDB          : ', E13.5                ,/,
     .       6X,'          ELAFRA          : ', E13.5                ,/,
     .       6X,'          AONSET          : ',5E13.5                ,/,
     .       6X,'          EXIT2, IELEM WAS: ', I5                   ,/)
700   FORMAT(6X,'[KDETECT] YIELDA=1 AND ELPHIA IN [0.99,1.01]:',E13.5,/,
     .       6X,'          IS PROJC5*PR2TR5 >= 0 ? ',L1                )
680   FORMAT(6X,'[KDETECT] SINCE PROD >=0, BY CONVEXITY, ONE FINDS'  ,/,
     .       6X,'          YIELDB          : ', E13.5                ,/,
     .       6X,'          ELAFRA          : ', E13.5                ,/,
     .       6X,'          AONSET          : ',5E13.5                ,/,
     .       6X,'          EXIT3, IELEM WAS: ', I5                   ,/)
660   FORMAT(6X,'[KDETECT] CALL KYLP WITHOUT OFFSET FOR PHITR5')
640   FORMAT(6X,'[KDETECT] YLP FINDS:'                               ,/,
     .       6X,'          ELATR5          : ', E13.5                ,/,
     .       6X,'          APHITR5         : ',5E13.5                ,/)
620   FORMAT(6X,'[KDETECT] ELATR5>=1, TRIAL STRESS IS ELASTIC'       ,/,
     .       6X,'          YIELDB          : ', E13.5                ,/,
     .       6X,'          ELAFRA          : ', E13.5                ,/,
     .       6X,'          AONSET          : ',5E13.5                ,/,
     .       6X,'          EXIT4, IELEM WAS: ', I5                   ,/)
600   FORMAT(6X,'[KDETECT] ELATR5<1, YIELDB SET TO 1'                ,/,
     .       6X,'          CALL BISSECTION WITH'                     ,/,
     .       6X,'          PTA=PHIA5 AND PTB=PHITR5'                   )
580   FORMAT(6X,'[KDETECT] BISSECTION FOUND:'                        ,/,
     .       6X,'          ELAFRA          : ', E13.5                ,/,
     .       6X,'          AONSET          : ',5E13.5                ,/,
     .       6X,'          EXIT5, IELEM WAS: ', I5                   ,/)
560   FORMAT(6X,'[KDETECT] ELATR5<1, YIELDB SET TO 1'                ,/,
     .       6X,'          CALL BISSECTION WITH'                     ,/,
     .       6X,'          PTA=ELPHIA5*PHIA5 AND PTB=PHITR5'           )
540   FORMAT(6X,'[KDETECT] BISSECTION FOUND:'                        ,/,
     .       6X,'          ELAFRA          : ', E13.5                ,/,
     .       6X,'          AONSET          : ',5E13.5                ,/,
     .       6X,'          EXIT6, IELEM WAS: ', I5                   ,/)
C     ------------------------------------------------------------------
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KYLPMUL(IW,IELEM,IOFF,SOV5,ELASOV5,TAU,VEC5,S, 
     .                    ELAFRA,AONSET) 
C     ------------------------------------------------------------------
C     INTERFACE TO CALL KYLP WITH MULTIPLE ATTEMPTS IN CASE OF PROBLEMS 
C     ------------------------------------------------------------------
CU    SUBROUTINE KYLPMUL USES SUBROUTINES 
CU       BETAP 
CU       KBE13VM
CU       BE4INI 
CU       YLP 
CU       KANGCTR
CU       KYLPERR
CU       KLENGTH 
CU       KYLPBIS 
C     ------------------------------------------------------------------
CA    CALL KYLPMUL(IW,IELEM,IOFF,SOV5,ELASOV5,TAU,VEC5,S, 
CA   .             ELAFRA,AONSET)
C     ------------------------------------------------------------------
CB    SUBROUTINE KYLPMUL USES COMMON BLOCKS
CB       KNCHOUT
CB       K_ITRY3
CB       KSTATIS
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      !
      ! INPUT 
      !
      INTEGER   IW,IELEM,IOFF 
      REAL*8    SOV5(5),ELASOV5,TAU,VEC5(5),S(600) 
      !
      ! OUTPUT 
      !
      REAL*8    ELAFRA,AONSET(5) 
      !
      ! LOCAL 
      !
      INTEGER   I,J,IERR,NWARN,IW2
      PARAMETER (NWARN=80)
      INTEGER   IFUN,NCFE,NSTEPS 
      REAL*8    UNILEN,BEU(4),BEA0(4),BEAS,BEA(4),RTD 
      REAL*8    SCALS,POT,ANG,COSANG,SDUM,FDUM,AMDUM(3,3)
      REAL*8    ANGCTR,PHIYLP(5) 
      REAL*8    PTA(5),PTB(5),DISTAB
      REAL*8    ANGTOL
      REAL*8    BEUVM(4)
      PARAMETER(ANGTOL=5.0D0)
      !
      ! COMMON BLOCKS
      !
      REAL*8 PARAM(16)  
      COMMON /KPARAM/ PARAM

      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT

      INTEGER   ITRY3
      COMMON   /K_ITRY3/ ITRY3

      INTEGER   LSTSTP,ITETOT,KELAS,KPLAS,KBIG,KTEXH,KDTEC,
     .          KTRIV,KPHIA1,KPHIA2,KOFFS,KPROD,KTRIA1,KTRIA2,KBISS,
     .          KYLPER,KYLP0,KYLP0A,KYLP0B,KYLP0C,
     .          KYLP1,KYLP1A,KYLP1B,KYLP1C
      COMMON   /KSTATIS/ LSTSTP,ITETOT,KELAS,KPLAS,KBIG,KTEXH,KDTEC,
     .          KTRIV,KPHIA1,KPHIA2,KOFFS,KPROD,KTRIA1,KTRIA2,KBISS,
     .          KYLPER,KYLP0,KYLP0A,KYLP0B,KYLP0C,
     .          KYLP1,KYLP1A,KYLP1B,KYLP1C
C
      CHARACTER*100 KFACETID1, KFACETID2
      INTEGER KTYPEXPR, KTYPFACET, KSTGFACET, KCTRi, KCTRj
      INTEGER KNUMFACET, KORDFACET
      DOUBLE PRECISION KLAMFACET(2500), KSNSFACET(2500,5)
      COMMON /KFACETPAR1/ KFACETID1, KFACETID2
      COMMON /KFACETPAR2/ KTYPFACET, KSTGFACET, KNUMFACET,
     &                    KORDFACET, KTYPEXPR
      COMMON /KFACETPAR3/ KLAMFACET
      COMMON /KFACETPAR4/ KSNSFACET
C
C     ------------------------------------------------------------------
C     OPTIONS
C     ------------------------------------------------------------------
      IW2  =0 ! CO OPTION KYLPMUL
cc      IW2  =1 ! CO OPTION KYLPMUL
C     ------------------------------------------------------------------
C     INITIALISATION
C     ------------------------------------------------------------------
      IF(IOFF.EQ.0) THEN
         KYLP0=KYLP0+1
      ELSE
         KYLP1=KYLP1+1
      ENDIF
      IF(IW.EQ.1)WRITE(OUT6,990)
      IF (PARAM(3) .EQ. 30.D0) THEN 
        IFUN=30
      ELSE
        IFUN=3
      END IF
      NCFE=S(2) 
      RTD =180.0D0/DACOS(-1.0D0)
      SDUM=0.0D0 
      FDUM=0.0D0 
      DO I=1,3 
         DO J=1,3 
            AMDUM(I,J)=0.0D0 
         ENDDO 
      ENDDO 
 
C     ------------------------------------------------------------------
C     CALL KYLP WITH OPTIMISED INITIALISATION FOR BEA 
C     OUTPUT: ELAFRA AND AONSET
C
C     WARNING:
C        IF(ITRY3.EQ.1) YLP WILL MODIFY THE VALUE OF BEA0(4) BY + 60 DEG
C        THEREFORE, AFTER YLP, BEA0 IS NOT ANYMORE BEUVM !
C     ------------------------------------------------------------------
      CALL KBETAP(BEU,UNILEN,VEC5,0)
      IF(IW.EQ.1)WRITE(OUT6,920)IOFF,(SOV5(I),I=1,5),(VEC5(I),I=1,5),
     .  (BEU(I)*RTD,I=1,4),UNILEN
      DO I=1,4 
         BEA0(I)=BEU(I) 
      ENDDO 

      CALL KBE13VM(IW,SOV5,VEC5,TAU, BEUVM) 
      DO I=1,4
         BEA0(I)=BEUVM(I)
      ENDDO

      CALL KBE4INI(SDUM,FDUM,AMDUM,BEA0,BEU,TAU,SOV5,VEC5,S,IFUN,IW2)
      BEAS=BEA0(4)

      IF(IW.EQ.1)WRITE(OUT6,910) BEAS*RTD
      IF (PARAM(3) .EQ. 30.D0) THEN
        IF (KTYPFACET==2) THEN
          CALL KYLPFacet(ELAFRA,SCALS,POT,AONSET,BEA,ANG,COSANG,NSTEPS,
     x     SOV5,TAU,UNILEN,BEU,BEA0,IOFF,ITRY3,IFUN,IERR,IW2,PHIYLP,
     x     0.2D0,200)
        END IF
      ELSE IF( (PARAM(3) .EQ. 2.D0) .OR. (PARAM(3) .EQ. 3.D0) .OR.
     &           (PARAM(3) .EQ. 4.D0) ) THEN
        CALL KYLP(ELAFRA,SCALS,POT,AONSET,BEA,ANG,COSANG,NSTEPS, 
     .         SOV5,TAU,UNILEN,BEU,BEA0,S(9),S(9+NCFE), 
     .         IOFF,ITRY3,IFUN,IERR,IW2,PHIYLP)
      END IF 
C     IF(NSTEPS.GT.NWARN) WRITE(OUT6,900) NSTEPS,NWARN,IELEM
C     IF(NSTEPS.EQ.1)     WRITE(OUT6,899) IELEM

CC    IF(IERR.EQ.1) THEN
CC       IF(IOFF.EQ.0) THEN
CC          ANGCTR=ANG
CC       ELSE
CC          CALL KANGCTR(IW,SOV5,VEC5,PHIYLP,ANG, ANGCTR)
CC       ENDIF
CC    ENDIF
      ANGCTR=ANG  ! KANGCTR DOES NOT WORK FINE

C     ------------------------------------------------------------------
C     IF FAILED, TRIES WITH NORMAL VON MISES INITIALISATION 
C     OUTPUT: ELAFRA AND AONSET
C     ------------------------------------------------------------------
      IF(IERR.EQ.2.OR.(IERR.EQ.1.AND.ANGCTR.GT.ANGTOL)) THEN 
        WRITE(OUT6,850) IELEM 
        IF(IERR.EQ.2)WRITE(OUT6,830) IOFF,(PHIYLP(I),I=1,5),
     .                               ANG,ANGCTR,POT
        IF(IERR.EQ.1)WRITE(OUT6,820) IOFF,(PHIYLP(I),I=1,5),
     .                               ANG,ANGCTR,POT
        IF(IOFF.EQ.1)WRITE(OUT6,819) ELASOV5
        DO I=1,4
          BEA0(I)=BEUVM(I)
        ENDDO

CCC     WRITE(OUT6,800)(BEU(I)*RTD,I=1,4),(BEA0(I)*RTD,I=1,3),BEAS 
        WRITE(OUT6,800)(BEUVM(I)*RTD,I=1,4),(BEA0(I)*RTD,I=1,3),BEAS 
CCC     DO I=1,4 
CCC       BEA0(I)=BEU(I) 
CCC     ENDDO 
        IF (PARAM(3) .EQ. 30.D0) THEN
          IF (KTYPFACET==2) THEN
           CALL KYLPFacet(ELAFRA,SCALS,POT,AONSET,BEA,ANG,COSANG,NSTEPS,
     x     SOV5,TAU,UNILEN,BEU,BEA0,IOFF,ITRY3,IFUN,IERR,IW2,PHIYLP,
     x     0.2D0,200)
          END IF
        ELSE IF( (PARAM(3) .EQ. 2.D0) .OR. (PARAM(3) .EQ. 3.D0) .OR.
     &             (PARAM(3) .EQ. 4.D0) ) THEN
          CALL KYLP(ELAFRA,SCALS,POT,AONSET,BEA,ANG,COSANG,NSTEPS, 
     .            SOV5,TAU,UNILEN,BEU,BEA0,S(9),S(9+NCFE), 
     .            IOFF,ITRY3,IFUN,IERR,IW2,PHIYLP)
        END IF 
C        IF(NSTEPS.GT.NWARN) WRITE(OUT6,750) NSTEPS,NWARN,IELEM
C        IF(NSTEPS.EQ.1)     WRITE(OUT6,749) IELEM

CC       IF(IERR.EQ.1) THEN
CC          IF(IOFF.EQ.0) THEN
CC             ANGCTR=ANG
CC          ELSE
CC             CALL KANGCTR(IW,SOV5,VEC5,PHIYLP,ANG, ANGCTR)
CC          ENDIF
CC       ENDIF
         ANGCTR=ANG ! KANGCTR DOES NOT WORK FINE
      ELSE
        IF(IOFF.EQ.0) THEN
          KYLP0A=KYLP0A+1
        ELSE
          KYLP1A=KYLP1A+1
        ENDIF
      ENDIF

C     ------------------------------------------------------------------
C     IF STILL FAILED THEN 
C        IF WITHOUT OFFSET, MUST USE YLPERROR 
C        IF WITH    OFFSET, TRIES THE BISSECTION 
C     OUTPUT: ELAFRA AND AONSET
C     ------------------------------------------------------------------
      IF(IERR.EQ.2.OR.(IERR.EQ.1.AND.ANGCTR.GT.ANGTOL)) THEN 
         WRITE(OUT6,810) IELEM 
         IF(IOFF.EQ.0) THEN 
            IF(IERR.EQ.2)WRITE(OUT6,830) IOFF,(PHIYLP(I),I=1,5),
     .                                   ANG,ANGCTR,POT
            IF(IERR.EQ.1)WRITE(OUT6,820) IOFF,(PHIYLP(I),I=1,5),
     .                                   ANG,ANGCTR,POT
            IF(IOFF.EQ.1)WRITE(OUT6,819) ELASOV5
            WRITE(OUT6,700) IELEM 
            BEA0(4)=BEAS
            KYLP0C=KYLP0C+1
            KYLPER=KYLPER+1 
            CALL KYLPERR(IW,IELEM,VEC5,BEA0,TAU,UNILEN,S,
     .                   AONSET,ELAFRA,PHIYLP)
            RETURN 
         ELSE 
            IF(IERR.EQ.2)WRITE(OUT6,830) IOFF,(PHIYLP(I),I=1,5),
     .                                   ANG,ANGCTR,POT
            IF(IERR.EQ.1)WRITE(OUT6,820) IOFF,(PHIYLP(I),I=1,5),
     .                                   ANG,ANGCTR,POT
            IF(IOFF.EQ.1)WRITE(OUT6,819) ELASOV5
            WRITE(OUT6,650) IELEM 
            DO I=1,5
               PTA(I)=SOV5(I)
               PTB(I)=PTA(I)+VEC5(I) 
            ENDDO 
            CALL KLENGTH(VEC5,5,1, DISTAB) 
            KYLP1C=KYLP1C+1
            CALL KYLPBIS(IW,IELEM,TAU,PTA,PTB,DISTAB,S,
     .                   ELAFRA,AONSET,PHIYLP) 
         ENDIF
      ELSE
         IF(IOFF.EQ.0) THEN
            KYLP0B=KYLP0B+1
         ELSE
            KYLP1B=KYLP1B+1
         ENDIF
      ENDIF

C     ------------------------------------------------------------------
C     FORMATS 
C     ------------------------------------------------------------------
990   FORMAT(9X,'[KYLPMUL] BEGIN')
920   FORMAT(9X,'[KYLPMUL] IOFF             : ', I5   ,/,
     .       9X,'          SOV5(1..5)       : ',5E13.5,/,
     .       9X,'          VEC5(1..5)       : ',5E13.5,/,
     .       9X,'          BEU(1..4)        : ',4E13.5,/,
     .       9X,'          UNILEN           : ', E13.5,/,
     .       9X,'          TRIES WITH THE OPTIMISED INITIALISATION') 
910   FORMAT(9X,'[KYLPMUL] OPTIMISED BEA4   : ', E13.5) 
900   FORMAT(9X,'[KYLPMUL] ATTEMPT 1, NSTEP : ',I5,'>',I3,' @ELEM ',I5) 
899   FORMAT(9X,'[KYLPMUL] ATTEMPT 1 VERY FAST NSTEP = 1 @ELEM ',I5) 
830   FORMAT(/,9X,'[KYLPMUL] KWARNING! ROUTINE YLP, IERR=2:         ',/,
C    .       9X,'          NONE OF THE 3 ITE PROCEDURES LEADS TO A  ',/,
C    .       9X,'          SOLUTION SATISFYING THE CONV. CRITERION'  ,/,
C    .       9X,'          OR A STABLE SOLUTION'                     ,/,
     .       9X,'          IOFF WAS         : ', I5                  ,/,
     .       9X,'          PHIYLP           : ',5E13.5               ,/,
     .       9X,'          ANG IS           : ', E13.5               ,/,
     .       9X,'          ANGCTR IS        : ', E13.5               ,/,
     .       9X,'          SERIES EXP IS    : ', E13.5                 )
820   FORMAT(9X,'[KYLPMUL] KWARNING! ROUTINE YLP, IERR=1'            ,/,
C    .       9X,'          NONE OF THE 3 ITE PROCEDURES LEADS TO A'  ,/,
C    .       9X,'          SOLUTION SATISFYING THE CONV. CRITERION, ',/,
C    .       9X,'          BUT AT LEAST ONE LEADS TO A STABLE SOL   ',/,
     .       9X,'          IOFF WAS         : ', I5                  ,/,
     .       9X,'          PHIYLP           : ',5E13.5               ,/,
     .       9X,'          ANG IS           : ', E13.5               ,/,
     .       9X,'          ANGCTR IS        : ', E13.5               ,/,
     .       9X,'          SERIES EXP IS    : ', E13.5                 )
819   FORMAT(9X,'[KYLPMUL] ELASOV5 WAS      : ', E13.5               ,/)
850   FORMAT(9X,'[KYLPMUL] @ELEM: ',I5,' OPTIMISED INIT. FAILED') 
800   FORMAT(9X,'[KYLPMUL] BEUVM WAS        : ',4E13.5,/, 
     .       9X,'          BEA0 WAS         : ',4E13.5,/, 
     .       9X,'          TRIES NOW VON MISES GUESS' ,/) 
810   FORMAT(9X,'[KYLPMUL] @ELEM: ',I5,' VON MISES INIT. ALSO FAILED') 
750   FORMAT(9X,'[KYLPMUL] ATTEMPT 2, NSTEP : ',I5,'>',I3,' @ELEM ',I5) 
749   FORMAT(9X,'[KYLPMUL] ATTEMPT 2 VERY FAST NSTEP = 1 @ELEM ',I5) 
700   FORMAT(/,9X,'[KYLPMUL] KWARNING! IOFF=0 ATTEMPT 1 & 2 FAILED, ',
     .          'YLPERROR @ELEM ',I5,/) 
650   FORMAT(/,9X,'[KYLPMUL] IOFF=1 ATTEMPT 1 & 2 FAILED, ',
     .          'KYLPBIS @ELEM',I5,/) 
600   FORMAT(9X,'[KYLPMUL] EXIT',/)
C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,600)
      RETURN 
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KBE13VM(IW,SOV5,VEC5,TAU, BEUVM) 
C     ------------------------------------------------------------------
CU    SUBROUTINE KBE13VM USES SUBROUTINES 
CU       KLENGTH 
CU       KPRODUC
CU       BETAP
C     ------------------------------------------------------------------
CA    CALL KBE13VM(IW,SOV5,VEC5,TAU, BEUVM) 
C     ------------------------------------------------------------------
CB    SUBROUTINE KBE13VM COMMON BLOCKS
CB       KNCHOUT
CB       KN_UREF
C     ------------------------------------------------------------------
CT    COMMENTS FOR SUBROUTINE KBE13VM
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      !
      ! INPUT 
      !
      INTEGER   IW
      REAL*8    SOV5(5),VEC5(5),TAU
      !
      ! OUTPUT 
      !
      REAL*8    BEUVM(4)
      !
      ! LOCAL 
      !
      INTEGER   I
      REAL*8    RADVM,NSOV5
      REAL*8    COFA,COFB,COFC,DTM,TMP1,TMP2
      REAL*8    LAMBDA,PHIVM(5)
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT

      REAL*8    N_UREF
      COMMON   /KN_UREF/ N_UREF
C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,920)
      RADVM=N_UREF*TAU

C     ------------------------------------------------------------------
C     COMPUTES THE POINT ON THE VM YL DEFINED BY
C     - RADIUS VON MISES =TAU*N_UREF
C     - OFFSET STRESS SOV5
C     - DIR OF THE STRESS INCREMENT VEC5
C     ------------------------------------------------------------------
      CALL KLENGTH(SOV5,5,1, NSOV5)
      IF(NSOV5.EQ.0.0D0) THEN
         DO I=1,5
            PHIVM(I)=VEC5(I)
         ENDDO
      ELSE
         CALL KLENGTH(VEC5,5,1, COFA) ! IF 0, YLP WOULD FAILED
         COFA=COFA*COFA
         IF(COFA.EQ.0.0D0) WRITE(OUT6,900)
         CALL KPRODUC(VEC5,SOV5,5, COFB)
         COFB=2.D0*COFB
         CALL KLENGTH(SOV5,5,1, COFC)
         COFC=COFC*COFC-RADVM*RADVM
         IF(COFB.EQ.0.0D0) THEN
            TMP1=-DSQRT(-COFC/COFA)
            TMP2= DSQRT(-COFC/COFA)
         ELSE
            DTM=COFB*COFB-4.0D0*COFA*COFC
            IF(DTM.LT.0.0D0) THEN
               WRITE(OUT6,880) DTM
               LAMBDA=0.0D0
            ELSE
               TMP1=(-COFB-DSQRT(DTM))/(2.0D0*COFA)
               TMP2=(-COFB+DSQRT(DTM))/(2.0D0*COFA)
            ENDIF
         ENDIF

         LAMBDA=MAX(TMP1,TMP2)

         IF(IW.EQ.1)WRITE(OUT6,860) LAMBDA
         IF(LAMBDA.LT.0.0D0) THEN
            WRITE(OUT6,840) LAMBDA
            LAMBDA=0.0D0
         ENDIF
         DO I=1,5
            PHIVM(I)=SOV5(I)+LAMBDA*VEC5(I)
         ENDDO
      ENDIF

C     ------------------------------------------------------------------
C     COMPUTES THE BETA ANGLES OF PHIVM AND STORE THEM INTO BEUVM
C     ------------------------------------------------------------------
      CALL KBETAP(BEUVM,TMP1,PHIVM,0)
      IF(IW.EQ.1) THEN
         WRITE(OUT6,820) (PHIVM(I),I=1,5)
         WRITE(OUT6,800) (BEUVM(I)*180.0D0/DACOS(-1.0D0),I=1,4)
      ENDIF

C     ------------------------------------------------------------------
920   FORMAT(/,6X,'[KBE13VM] BEGIN')
900   FORMAT(6X,'[KBE13VM] KWARNING! COFA IS NEGATIVE AND SHOULD NOT')
880   FORMAT(6X,'[KBE13VM] KWARNING! DTM IS NEGATIVE,'    ,/,
     .       6X,'           LAMBDA SET TO 0, DTM WAS: ',E13.5)
860   FORMAT(6X,'[KBE13VM] ELASTIC VM FRACTION OF VEC5: ',E13.5)
840   FORMAT(6X,'[KBE13VM] KWARNING! LAMBDA SHOULD NOT BE <0',
     .       6X,'           LAMBDA IS SET TO 0, IT WAS: ',E13.5)
820   FORMAT(6X,'[KBE13VM] PHIVM=SOV5+LAMBDA*VEC5: ',5E13.5)
800   FORMAT(6X,'[KBE13VM] BEUVM OF PHIVM        : ',4E13.5,/,
     .       6X,'           EXIT',/)
C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KANGCTR(IW,SOV5,VEC5,PHIYLP,ANG, ANGCTR) 
C     ------------------------------------------------------------------
CU    SUBROUTINE KANGCTR USES SUBROUTINES 
CU       KLENGTH 
CU       KPRODUC
C     ------------------------------------------------------------------
CA    CALL KANGCTR(IW,PHIYLP,SOV5,VEC5,ANG, ANGCTR)  
C     ------------------------------------------------------------------
CB    SUBROUTINE KANGCTR USES COMMON BLOCKS
CB       KNCHOUT
C     ------------------------------------------------------------------
CT    COMMENTS FOR SUBROUTINE KANGCTR
CT       - COMPUTES THE RADIUS OF A VON MISES YL PASSING BY PHIYLP
CT       - COMPUTES THE INTERSECTION PHIVM BETWEEN THE LINE DEFINED BY
CT         (ORIGIN=SOV5,DIR=VEC5), PHIVM=SOV5+LAMBDA*VEC5
CT         LAMBDA SHOULD BE CLOSE TO THE ELASTIC FRACTION FOUND BY THE
CT         ROUTINE YLP IF THE ANISOTROPY IS WEAK
CT       - COMPUTES THE ANGLE BETWEEN PHIVM AND PHIYLP
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      !
      ! INPUT 
      !
      INTEGER   IW
      REAL*8    SOV5(5),VEC5(5),PHIYLP(5),ANG
      !
      ! OUTPUT 
      !
      REAL*8    ANGCTR
      !
      ! LOCAL 
      !
      INTEGER   I
      REAL*8    RADVM,COFA,COFB,COFC,DTM,TMP1,TMP2,LAMBDA,PHIVM(5)
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT
C     ------------------------------------------------------------------
C     COMPUTES THE RADIUS OF A VON MISES YL PASSING BY PHIYLP
C     ------------------------------------------------------------------
      CALL KLENGTH(PHIYLP,5,1, RADVM)
      CALL KLENGTH(SOV5,5,1, TMP1)
      IF(TMP1.GT.RADVM)WRITE(OUT6,995) RADVM,TMP1

C     ------------------------------------------------------------------
C     COMPUTES THE INTERSECTION PHIVM AND THE ANGLE (PHIVM,PHIYLP)
C     ------------------------------------------------------------------
      CALL KLENGTH(VEC5,5,1, COFA) ! IF 0, YLP WOULD HAVE FAILED
      COFA=COFA*COFA
      CALL KPRODUC(VEC5,SOV5,5, COFB)
      COFB=2.D0*COFB
      CALL KLENGTH(SOV5,5,1, COFC)
      COFC=COFC*COFC-RADVM*RADVM

      DTM=COFB*COFB-4.0D0*COFA*COFC

      IF(DTM.LT.0.0D0) THEN
         WRITE(OUT6,900) DTM
         ANGCTR=ANG
      ELSE
         TMP1=(-COFB-DSQRT(DTM))/(2.0D0*COFA)
         TMP2=(-COFB+DSQRT(DTM))/(2.0D0*COFA)
         LAMBDA=MAX(TMP1,TMP2)
         IF(LAMBDA.LT.0.0D0) WRITE(OUT6,880) LAMBDA
         DO I=1,5
            PHIVM(I)=SOV5(I)+LAMBDA*VEC5(I)
         ENDDO
         CALL KLENGTH(PHIVM,5,1, TMP1)
         IF(IW.EQ.1)WRITE(OUT6,870) (PHIYLP(I),I=1,5),RADVM,LAMBDA,
     .                              (SOV5(I),I=1,5),(VEC5(I),I=1,5),
     .                              (PHIVM(I),I=1,5),TMP1

         CALL KPRODUC(PHIVM,PHIYLP,5, ANGCTR)
         ANGCTR=ANGCTR/(RADVM*RADVM)
         IF(ANGCTR.LT.-1.0D0) THEN
            ANGCTR=180.0D0
         ELSE IF(ANGCTR.GT.1.0D0) THEN
           ANGCTR=0.0D0
         ELSE
           ANGCTR=DACOS(ANGCTR)*180.0D0/DACOS(-1.0D0)
         ENDIF

      ENDIF
      IF(IW.EQ.1)WRITE(OUT6,860) ANGCTR,ANG

C     ------------------------------------------------------------------
995   FORMAT(/,6X,'[KANGCTR] KWARNING! LENGTH OF SOV5 IS > RADVM ',/,
     .         6X,'          RADVM          : ',E13.5             ,/,
     .         6X,'          LENGTH OF SOV5 : ',E13.5               )
900   FORMAT(/,6X,'[KANGCTR] DTM IS NEGATIVE:',E13.5)
880   FORMAT(/,6X,'[KANGCTR] KWARNING! LAMBDA<0 : ',E13.5)
870   FORMAT(/,6X,'[KANGCTR] PHIYLP                      : ',5E13.5,/,
     .         6X,'          RADVM FROM PHIYLP           : ', E13.5,/,
     .         6X,'          ELASTIC FRAC OF INC IF VM YL: ', E13.5,/,
     .         6X,'          OFFSET WAS                  : ',5E13.5,/,
     .         6X,'          INCREMENT WAS               : ',5E13.5,/,
     .         6X,'          PHIVM FROM THIS FRACTION    : ',5E13.5,/,
     .         6X,'          LENGTH OF PHIVM             : ', E13.5  )
860   FORMAT(  6X,'[KANGCTR] ANGCTR                      : ',E13.5,/,
     .         6X,'          ANG                         : ',E13.5,/)
C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KYLPBIS(IW,IELEM,TAU,PTA,PTB,DISTAB,S,
     .                    ELAFRA,AONSET,PHIYLP) 
C     ------------------------------------------------------------------
CU    SUBROUTINE KYLPBIS USES SUBROUTINES 
CU       KLENGTH 
CU       KPRODUC
CU       KYLPMUL 
C     ------------------------------------------------------------------
CA    CALL KYLPBIS(IW,IELEM,TAU,PTA,PTB,DISTAB,S,
CA   .             ELAFRA,AONSET,PHIYLP) 
C     ------------------------------------------------------------------
CB    SUBROUTINE KYLPBIS USES COMMON BLOCKS
CB       KNCHOUT
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      !
      ! INPUT 
      !
      INTEGER   IW,IELEM 
      REAL*8    TAU,PTA(5),PTB(5),DIRAB(5),DISTAB,S(600) 
      !
      ! OUTPUT 
      !
      REAL*8    ELAFRA,AONSET(5),PHIYLP(5)
      !
      ! LOCAL 
      !
      INTEGER   I,ISTEP,IOFF
      REAL*8    ORI(5),END(5),AEND(5),ELAEND,BIS(5),ELABIS,TOLBIS, 
     .          DIF5(5),TEMP,RTD,SOV5(5),ELASOV5
      REAL*8    RADIUS,COFA,COFB,COFC,DTM,SOL1,SOL2,MU
      PARAMETER(TOLBIS=0.0001D0) 
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT
C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,900) 
      RTD=180.0D0/DACOS(-1.0D0) 
 
C     ------------------------------------------------------------------
C     MUST BE SURE THAT THE END IS OUTSIDE THE YIELD LOCUS 
C     IF VON MISES YIELD LOCUS, 'END' IS ON THE YL IF 
C     NORM_OF_(PTA+MU*DIRAB)=RADIUS
C     ------------------------------------------------------------------
      DO I=1,5 
         END(I)=PTB(I) 
         DIRAB(I)=(PTB(I)-PTA(I))/DISTAB 
      ENDDO 
      CALL KLENGTH(END,5,1, TEMP) 
      IF(IW.EQ.1)WRITE(OUT6,850) (PTA(I),I=1,5),(PTB(I),I=1,5),DISTAB, 
     .                         (DIRAB(I),I=1,5),(END(I),I=1,5),TEMP, 
     .                          TAU,TEMP/TAU
 
      RADIUS=DSQRT(2.0D0/3.0D0)*TAU
      COFA=1.0D0
      CALL KPRODUC(PTA,DIRAB,5, COFB)
      COFB=2.0D0*COFB
      CALL KLENGTH(PTA,5,1, COFC)
      COFC=COFC-RADIUS*RADIUS
      DTM=COFB*COFB-4.0D0*COFA*COFC
      IF(DTM.GE.0.0D0) THEN
         SOL1=(-COFB-DSQRT(DTM))/(2.0D0*COFA)
         SOL2=(-COFB+DSQRT(DTM))/(2.0D0*COFA)
         MU=MAX(SOL1,SOL2)
         IF(MU.GT.0.0D0) THEN
            DO I=1,5
               END(I)=PTA(I)+1.5D0*MU*DIRAB(I)
            ENDDO
         ENDIF
      ENDIF

05    IOFF=0 
      DO I=1,5 
         SOV5(I)=0.0D0 
      ENDDO 
      ELASOV5=99.9
      CALL KYLPMUL(IW,IELEM,IOFF,SOV5,ELASOV5,TAU,END,S, 
     .             ELAEND,AEND) 
      IF(IW.EQ.1)WRITE(OUT6,800) ELAEND 
 
      IF(ELAEND.GT.0.99D0) THEN 
         DO I=1,5 
            DIF5(I)=END(I)-PTA(I) 
         ENDDO 
         CALL KLENGTH(DIF5,5,1, TEMP) 
         DO I=1,5 
            END(I)=PTA(I)+5.0D0*TEMP*DIRAB(I) 
         ENDDO 
         GOTO 05 
      ENDIF 
 
C     ------------------------------------------------------------------
C     END IS NOW SUFFICIENTLY OUTSIDE THE YIELD LOCUS 
C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,750) ELAEND 
      DO I=1,5 
         ORI(I)=PTA(I) 
      ENDDO 
      ISTEP=0 
 
10    DO I=1,5 
         BIS(I)=ORI(I)+0.5D0*(END(I)-ORI(I)) 
      ENDDO 
 
      ISTEP=ISTEP+1 
      IF(IW.EQ.1) THEN 
         WRITE(OUT6,700)ISTEP
         WRITE(OUT6,650)(ORI(I),I=1,5),(END(I),I=1,5),(BIS(I),I=1,5) 
      ENDIF 
 
      IOFF=0 
      DO I=1,5 
         SOV5(I)=0.0D0 
      ENDDO 
      ELASOV5=99.9 
      CALL KYLPMUL(IW,IELEM,IOFF,SOV5,ELASOV5,TAU,BIS,S, 
     .             ELABIS,AONSET) 
      DO I=1,5 
         DIF5(I)=BIS(I)-PTA(I) 
      ENDDO 
      CALL KLENGTH(DIF5,5,1, TEMP) 
      ELAFRA=TEMP/DISTAB 
      IF(IW.EQ.1)WRITE(OUT6,600)ELABIS,ELAFRA
 
      IF(ELABIS.GT.1.0D0) THEN 
         DO I=1,5 
            ORI(I)=BIS(I) 
         ENDDO 
      ENDIF 
      IF(ELABIS.LT.1.0D0) THEN 
         DO I=1,5 
            END(I)=BIS(I) 
         ENDDO 
      ENDIF 
      DO I=1,5 
         DIF5(I)=END(I)-ORI(I) 
      ENDDO 
      CALL KLENGTH(DIF5,5,1, TEMP) 
      IF(TEMP/TAU.LT.TOLBIS) THEN 
         IF(IW.EQ.1)WRITE(OUT6,550) 
         GOTO 20 
      ENDIF 
      IF ((ELABIS.GE.0.999D0).AND.(ELABIS.LE.1.001D0)) THEN 
         GOTO 20 
      ELSE 
         GOTO 10 
      ENDIF 
20    CONTINUE 
      DO I=1,5
         PHIYLP(I)=BIS(I)
      ENDDO
      IF(IW.EQ.1)WRITE(OUT6,500) 
 
C     ------------------------------------------------------------------
C     FORMATS
C     ------------------------------------------------------------------
900   FORMAT(9X,'[KYLPBIS] BEGIN') 
850   FORMAT(9X,'[KYLPBIS] PTA...................... : ',5E13.5,/, 
     .       9X,'          PTB...................... : ',5E13.5,/, 
     .       9X,'          DISTAB................... : ', E13.5,/, 
     .       9X,'          DIRAB.................... : ',5E13.5,/, 
     .       9X,'          END...................... : ',5E13.5,/, 
     .       9X,'          LENGTH................... : ', E13.5,/, 
     .       9X,'          TAU...................... : ', E13.5,/, 
     .       9X,'          LENGTH/TAU............... : ', E13.5,/) 
800   FORMAT(9X,'[KYLPBIS] SCALING, ELAFRA OF END... : ', E13.5  ) 
750   FORMAT(9X,'[KYLPBIS] CAN START BISSECTIION RL. : ', E13.5  ) 
700   FORMAT(9X,'[KYLPBIS] BISSECTION STEP.......... : ', I5     ) 
650   FORMAT(9X,'[KYLPBIS] ORI...................... : ',5E13.5,/, 
     .       9X,'          END...................... : ',5E13.5,/, 
     .       9X,'          -> BIS................... : ',5E13.5,/) 
600   FORMAT(9X,'[KYLPBIS] ELAFRA OF BIS. POINT..... : ', E13.5,/, 
     .       9X,'          ELAFRA OF STRESS INC..... : ', E13.5,/) 
550   FORMAT(9X,'[KYLPBIS] END=ORI -> END') 
500   FORMAT(9X,'[KYLPBIS] END OF KYLPBIS') 
C     ------------------------------------------------------------------
      RETURN 
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KYLPERR(IW,IELEM,PHIA5,BEA0,TAUA,UNILEN,S,
     .                    APHIA5,RLPHIA5,PHIYLP)
C     ------------------------------------------------------------------
CU    SUBROUTINE KYLERR USES SUBROUTINES 
CU       APBEA
CU       F3DER
CU       KLENGTH
CU       KPRODUC
C     ------------------------------------------------------------------
CA    CALL KYLPERR(IW,IELEM,PHIA5,BEA0,TAUA,UNILEN,S,
CA   .             APHIA5,RLPHIA5,PHIYLP)
C     ------------------------------------------------------------------
CB    SUBROUTINE KYLPERR USES COMMON BLOCKS
CB       KNCHOUT
C     ------------------------------------------------------------------
CT    COMMENTS FOR SUBROUTINE KYLPERR
CT       PHIA5 : 5D-VECTOR DEFINING THE SEARCH DIRECTION IN STRESS SPACE
CT               FROM CENTRE TO PHIA5, NO OFFSET IS CONSIDERED.  BEA0 IS
CT               BEU1,BEU2,BEU3,BEASTAR OBTAINED BY MINIMISATION.
CT       UNILEN   : THE LENGTH OF PHIA5
CT       APHIA(5) : 5D-VECTOR NORMAL DEFINED BY BEA0
CT       RLPHIA5  : LENGTH OF YLPOINT/UNILEN
C     ------------------------------------------------------------------
      IMPLICIT NONE
      !
      ! INPUT
      !
      INTEGER   IW,IELEM
      REAL*8    PHIA5(5),BEA0(4),TAUA,UNILEN,S(600)
      !
      ! OUTPUT
      !
      REAL*8    APHIA5(5),RLPHIA5,PHIYLP(5)
      !
      ! LOCAL
      !
      INTEGER   IDER,NCFE,NCHLST,I
      REAL*8    FVAL,DF(5),DDF(5,5),TEMP,UV(5),NRMU,ANGLE,RTD
      !
      ! COMMON BLOCKS
      !
      REAL*8 PARAM(16)  
      COMMON /KPARAM/ PARAM

      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT
C     ------------------------------------------------------------------
      WRITE(OUT6,920) IELEM

C     ------------------------------------------------------------------
C     COMPUTE THE NORMAL CORRESPONDING TO BEU1,BEU2,BEU3,BEUSTAR
C     ------------------------------------------------------------------
      RTD=180.0D0/DACOS(-1.0D0)
      CALL KAPBEA(APHIA5,BEA0,0)

C     ------------------------------------------------------------------
C     COMPUTE THE CORRESPONDING YIELD LOCUS POINT
C     ------------------------------------------------------------------
      IDER  =1
      NCFE  =S(2)
      NCHLST=OUT6
      IF (PARAM(3).EQ.30.D0) THEN
        CALL KFacetPOT(FVAL,DF,DDF,APHIA5,IDER,NCHLST,0)
      ELSE
        CALL KF3DER(FVAL,DF,DDF,APHIA5,S(9),S(9+NCFE),5,IDER,NCHLST,0)
      END IF
      TEMP=0.0D0
      DO I=1,5
         TEMP=TEMP+DF(I)*APHIA5(I)
      ENDDO
      TEMP=FVAL-TEMP
      DO I=1,5
         UV(I)=DF(I)+APHIA5(I)*TEMP
         PHIYLP(I)=TAUA*UV(I)
      ENDDO

C     ------------------------------------------------------------------
C     COMPUTE RLPHIA5
C     ------------------------------------------------------------------
      CALL KLENGTH(UV,5,1, NRMU)
      RLPHIA5=TAUA*NRMU/UNILEN
      CALL KPRODUC(UV,PHIA5,5, ANGLE)
      ANGLE=ANGLE/(NRMU*UNILEN)
      ANGLE=DACOS(ANGLE)*RTD
      IF(IW.EQ.1)WRITE(OUT6,900) TAUA,UNILEN,(BEA0(I)*RTD,I=1,4),
     .  (APHIA5(I),I=1,5),(UV(I),I=1,5),RLPHIA5,
     .  ((UV(I)/PHIA5(I)),I=1,5),ANGLE

C     ------------------------------------------------------------------
C     FORMATS
C     ------------------------------------------------------------------
920   FORMAT(12X,'[KYLPERR] BEGIN @ IELEM: ',I5)
900   FORMAT(12X,'[KYLPERR] TAUA           : ',1E13.5,/,
     .       12X,'          UNILEN         : ',1E13.5,/,
     .       12X,'          BEA0           : ',4E13.5,/,
     .       12X,'          -> APHIA5      : ',5E13.5,/,
     .       12X,'          -> UV          : ',5E13.5,/,
     .       12X,'          -> RLPHIA5     : ',1E13.5,/,
     .       12X,'          -> UV/PHIA     : ',5E13.5,/,
     .       12X,'          -> ANGLE ERROR : ',1E13.5,/)
880   FORMAT(12X,'[KYLPERR] EXIT',/)

C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,880)
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KMODELA(GMOD,KMOD, HOOK) 
C     ------------------------------------------------------------------
CU    SUBROUTINE KMODELA USES SUBROUTINES 
CU       NONE 
C     ------------------------------------------------------------------
CA    CALL KMODELA(GMOD,KMOD, HOOK) 
C     ------------------------------------------------------------------
CB    SUBROUTINE KMODELA USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      INTEGER   I,J
      REAL*8    GMOD,KMOD
      REAL*8    HOOK(9,9)
      REAL*8    GMOD23,GMOD43
C     ------------------------------------------------------------------
C     CONSTRUCTION OF HOOKE FOURTH ORDER TENSOR (ISOTROPIC CASE)
C     ------------------------------------------------------------------
      DO I=1,9
         DO J=1,9
            HOOK(I,J)=0.0D0
         ENDDO
      ENDDO
      GMOD23   = 2.0D0*GMOD/3.0D0
      GMOD43   = 2.0D0*GMOD23
      HOOK(1,1)= GMOD43+KMOD
      HOOK(5,1)=-GMOD23+KMOD
      HOOK(9,1)=-GMOD23+KMOD
      HOOK(2,2)= GMOD
      HOOK(4,2)= GMOD
      HOOK(3,3)= GMOD
      HOOK(7,3)= GMOD
      HOOK(2,4)= GMOD
      HOOK(4,4)= GMOD
      HOOK(1,5)=-GMOD23+KMOD
      HOOK(5,5)= GMOD43+KMOD
      HOOK(9,5)=-GMOD23+KMOD
      HOOK(6,6)= GMOD
      HOOK(8,6)= GMOD
      HOOK(3,7)= GMOD
      HOOK(7,7)= GMOD
      HOOK(6,8)= GMOD
      HOOK(8,8)= GMOD
      HOOK(1,9)=-GMOD23+KMOD
      HOOK(5,9)=-GMOD23+KMOD
      HOOK(9,9)= GMOD43+KMOD

C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KMODPLA(IW,IDENT, NRMEPL,AV,FVAL,DF,DDF,UV,TAU,
     .                    H,HPRIM,GMOD,KMOD,
     .                    IERRC,MCOR) 
C     ------------------------------------------------------------------
CU    SUBROUTINE KMODPLA USES SUBROUTINES 
CU       KCALMPL 
CU       KCALMEP 
CU       KMAKE99
CU       KPRTMAT
C     ------------------------------------------------------------------
CA    CALL KMODPLA(IW,IDENT, NRMEPL,AV,FVAL,DF,DDF,UV,TAU,
CA   .             H,HPRIM,GMOD,KMOD,
CA   .             IERRC,MCOR) 
C     ------------------------------------------------------------------
CB    SUBROUTINE KMODPLA USES COMMON BLOCKS
CB       KNCHOUT
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      !
      ! INPUT 
      !
      INTEGER   IW,IDENT
      REAL*8    NRMEPL,AV(5),FVAL,DF(5),DDF(5,5),UV(5),TAU, 
     .          H,HPRIM(3,3),GMOD,KMOD 
      !
      ! OUTPUT 
      !
      INTEGER   IERRC 
      REAL*8    MCOR(9,9) 
      !
      ! LOCAL 
      !
      INTEGER   I,J,K,L
      REAL*8    ONETHD,TWOTHD,ROOT2,ROOT23,ROOT32,IROOT2,IROOT6, 
     .          T56(5,6),T65(6,5),OPDEV(6,6)
      REAL*8    MPL55(5,5),MPL66(6,6),MEPD55(5,5),MEPD66(6,6),MEP66(6,6)
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT
C     ------------------------------------------------------------------
C     INITIALISES CONSTANTS: 
C        (S1 S2 S3 S4 S5)=T56*(S11 S22 S33 S12 S13 S23) 
C        (S11 S22 S33 S12 S13 S23)=T65*(S1 S2 S3 S4 S5) 
C        DEV(X11 X22 X33 X12 X13 X23)=OPDEV*(X11 X22 X33 X12 X13 X23) 
C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,950) 
      ONETHD= 1.0D0/3.0D0 
      TWOTHD= 2.0D0*ONETHD 
      ROOT2 =SQRT(2.0D0) 
      ROOT23=SQRT(2.0D0/3.0D0) 
      ROOT32=SQRT(3.0D0/2.0D0) 
      IROOT2=1.0D0/ROOT2 
      IROOT6=1.0D0/SQRT(6.0D0) 
 
      DO I=1,5 
         DO J=1,6 
            T56(I,J)=0.0D0 
         ENDDO 
      ENDDO 
      T56(1,1)= IROOT2 
      T56(1,2)=-IROOT2 
      T56(2,1)= ROOT32 
      T56(2,2)= ROOT32 
      T56(3,6)= ROOT2 
      T56(4,5)= ROOT2 
      T56(5,4)= ROOT2 
 
      DO I=1,6 
         DO J=1,5 
            T65(I,J)=0.0D0 
         ENDDO 
      ENDDO 
      T65(1,1)= IROOT2 
      T65(1,2)= IROOT6 
      T65(2,1)=-IROOT2 
      T65(2,2)= IROOT6 
      T65(3,2)=-ROOT23 
      T65(4,5)= IROOT2 
      T65(5,4)= IROOT2 
      T65(6,3)= IROOT2 
 
      DO I=1,6 
         DO J=1,6 
            OPDEV(I,J)=0.0D0 
         ENDDO 
      ENDDO 
      OPDEV(1,1)= TWOTHD 
      OPDEV(1,2)=-ONETHD 
      OPDEV(1,3)=-ONETHD 
      OPDEV(2,1)=-ONETHD 
      OPDEV(2,2)= TWOTHD 
      OPDEV(2,3)=-ONETHD 
      OPDEV(3,1)=-ONETHD 
      OPDEV(3,2)=-ONETHD 
      OPDEV(3,3)= TWOTHD 
      OPDEV(4,4)= 1.0D0 
      OPDEV(5,5)= 1.0D0 
      OPDEV(6,6)= 1.0D0 
 
C     ------------------------------------------------------------------
C     COMPUTES THE PLASTIC MODULUS MPL55 
C     D(S1 S2 S3 S4 S5)=MPL55*D(DELTA_EPS_PL1...DELTA_EPS_PL5) 
C     ------------------------------------------------------------------
      CALL KCALMPL(NRMEPL,AV,FVAL,DF,DDF,H,HPRIM,UV,TAU, MPL55) 
 
C     ------------------------------------------------------------------
C     COMPUTES THE ELASTO-PLASTIC DEVIATORIC MODULUS 
C     D(S1 S2 S3 S4 S5)=MEPD55*D(DELTA_DEVEPS_TOT1...DELTA_DEVEPS_TOT5) 
C     ------------------------------------------------------------------
      CALL KCALMEP(IW,GMOD,MPL55, IERRC,MEPD55)
 
C     ------------------------------------------------------------------
C     PERFORMS THE TRANSFORMATION MEPD55 -> MEPD66 
C     D(S11 S22 S33 S12 S13 S23) 
C        =MEPD66*D(DELTA_DEVEPS_TOT11...DELTA_DEVEPS_TOT23) 
C     ------------------------------------------------------------------
      DO I=1,6 
         DO J=1,6 
            MPL66(I,J)=0.0D0 
            MEPD66(I,J)=0.0D0 
            DO K=1,5 
               DO L=1,5 
                  MPL66(I,J) =MPL66(I,J) +T65(I,K)* MPL55(K,L)*T56(L,J) 
                  MEPD66(I,J)=MEPD66(I,J)+T65(I,K)*MEPD55(K,L)*T56(L,J) 
               ENDDO 
            ENDDO 
         ENDDO 
      ENDDO 
 
C     ------------------------------------------------------------------
C     PERFORMS THE TRANSFORMATION DEVIATORIC -> TOTAL STRAINS 
C     D(S11 S22 S33 S12 S13 S23) 
C        =MEP66*D(DELTA_EPS_TOT11...DELTA_EPS_TOT23) 
C     ------------------------------------------------------------------
      DO I=1,6 
         DO J=1,6 
            MEP66(I,J)=0.0D0 
            DO K=1,6 
               MEP66(I,J)=MEP66(I,J)+MEPD66(I,K)*OPDEV(K,J) 
            ENDDO 
         ENDDO 
      ENDDO 
 
C     ------------------------------------------------------------------
C     ADDS THE CONTRIBUTION FOR THE PRESSURE 
C     D(SIG11 SIG22 SIG33 SIG12 SIG13 SIG23) 
C        =D(S11+P S22+P S33+P S12 S13 S23) 
C     ------------------------------------------------------------------
      DO I=1,3 
         DO J=1,3 
            MEP66(I,J)=MEP66(I,J)+KMOD 
         ENDDO 
      ENDDO 
 
C     ------------------------------------------------------------------
C     PERFORMS THE TRANSFORMATION MEP66 -> MEP99 
C     ------------------------------------------------------------------
      CALL KMAKE99(MEP66, MCOR) 

      IF(IW.EQ.1) THEN 
         WRITE(OUT6,'(A)') '[KMODPLA] MPL55' 
         CALL KPRTMAT(OUT6,MPL55 ,5,5,'E13.5',1) 
         WRITE(OUT6,'(A)') '[KMODPLA] MPL66' 
         CALL KPRTMAT(OUT6,MPL66 ,6,6,'E13.5',1) 
         WRITE(OUT6,'(A)') '[KMODPLA] MEPD55' 
         CALL KPRTMAT(OUT6,MEPD55,5,5,'E13.5',1) 
         WRITE(OUT6,'(A)') '[KMODPLA] MEPD66' 
         CALL KPRTMAT(OUT6,MEPD66,6,6,'E13.5',1) 
         WRITE(OUT6,'(A)') '[KMODPLA] MEP66' 
         CALL KPRTMAT(OUT6,MEP66 ,6,6,'E13.5',1) 
         WRITE(OUT6,'(A)') '[KMODPLA] MCOR' 
         CALL KPRTMAT(OUT6,MCOR  ,9,9,'E13.5',1) 
         WRITE(OUT6,800) 
      ENDIF 
 
C     ------------------------------------------------------------------
C     FORMATS 
C     ------------------------------------------------------------------
950   FORMAT(/,'[KMODPLA] BEGIN') 
900   FORMAT('[KMODPLA] KSWIFT : ',E13.5,/, 
     .       '          GSWIFT : ',E13.5,/, 
     .       '          NSWIFT : ',E13.5,/, 
     .       '          GAM    : ',E13.5,/, 
     .       '          --> H  : ',E13.5   ) 
850   FORMAT('[KMODPLA] CS     : ',E13.5,/, 
     .       '          NL     : ',E13.5,/, 
     .       '          SSAT   : ',E13.5,/, 
     .       '          CX     : ',E13.5,/, 
     .       '          M      : ',E13.5,/, 
     .       '          GAM    : ',E13.5,/, 
     .       '          --> H  : ',E13.5   ) 
800   FORMAT('[KMODPLA] EXIT',/) 

C     ------------------------------------------------------------------
      RETURN 
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KCALMPL(NRMEPL,AV,FVAL,DF,DDF,H,HPRIM,UV,TAU, MPL55) 
C     ------------------------------------------------------------------
CU    SUBROUTINE KCALMPL USES SUBROUTINES 
CU       NONE 
C     ------------------------------------------------------------------
CA    CALL KCALMPL(NRMEPL,AV,FVAL,DF,DDF,H,HPRIM,UV,TAU, MPL55) 
C     ------------------------------------------------------------------
CB    SUBROUTINE KCALMPL USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
C     D(S1 S2 S3 S4 S5)=MPL55*D(DELTA_EPS_PL1...DELTA_EPS_PL5) 
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      !
      ! INPUT 
      !
      REAL*8    NRMEPL,AV(5),FVAL,DF(5),DDF(5,5),H,HPRIM(3,3),UV(5),TAU 
      !
      ! OUTPUT 
      !
      REAL*8    MPL55(5,5) 
      !
      ! LOCAL 
      !
      INTEGER   I,J,K,L 
      REAL*8    UNI(5,5),A(5,5),HPRIM5(5) 
C     ------------------------------------------------------------------
C     COMPUTES THE ANISOTROPIC TENSOR 
C       A_IJ=DU_I/D_(DELTAT_EPS_PL_J) 
C       A_IJ=1/DELTAT*DU_I/D_(D_PL_J) 
C     ------------------------------------------------------------------
      DO I=1,5 
         DO J=1,5 
            UNI(I,J)=0.0D0 
         ENDDO 
         UNI(I,I)=1.0D0 
      ENDDO 
 
      DO I=1,5 
         DO J=1,5 
            A(I,J)=(UNI(I,J)-AV(I)*AV(J))*FVAL 
         ENDDO 
      ENDDO 
 
      DO I=1,5 
         DO J=1,5 
            DO K=1,5 
               A(I,J)=A(I,J)+AV(I)*DF(K)*(UNI(K,J)-AV(K)*AV(J)) 
            ENDDO 
         ENDDO 
      ENDDO 
 
      DO I=1,5 
         DO J=1,5 
            DO K=1,5 
               A(I,J)=A(I,J)+DDF(I,K)*(UNI(K,J)-AV(K)*AV(J)) 
            ENDDO 
         ENDDO 
      ENDDO 
 
      DO I=1,5 
         DO J=1,5 
            DO K=1,5 
               A(I,J)=A(I,J)-(UNI(I,J)-AV(I)*AV(J))*DF(K)*AV(K) 
            ENDDO 
         ENDDO 
      ENDDO 
 
      DO I=1,5 
         DO J=1,5 
            DO K=1,5 
               A(I,J)=A(I,J)-AV(I)*(UNI(K,J)-AV(K)*AV(J))*DF(K) 
            ENDDO 
         ENDDO 
      ENDDO 
 
      DO I=1,5 
         DO J=1,5 
            DO K=1,5 
               DO L=1,5 
                  A(I,J)=A(I,J) 
     .                  -AV(I)*AV(K)*DDF(L,K)*(UNI(L,J)-AV(L)*AV(J)) 
               ENDDO 
            ENDDO 
         ENDDO 
      ENDDO 
 
C     ------------------------------------------------------------------
C     COMPUTES MPL55 
C     ------------------------------------------------------------------
      CALL KX332X5(HPRIM, HPRIM5) 

      DO I=1,5 
         DO J=1,5 
            A(I,J)=A(I,J)/NRMEPL 
            MPL55(I,J)=H*UV(I)*UV(J)+HPRIM5(I)*UV(J)+TAU*A(I,J) 
         ENDDO 
      ENDDO 

C     ------------------------------------------------------------------
      RETURN 
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KCALMEP(IW,GMOD,MPL55, IERRC,MEPD55) 
C     ------------------------------------------------------------------
CU    SUBROUTINE KCALMEP USES SUBROUTINES 
CU       KDMATIN 
C     ------------------------------------------------------------------
CA    CALL KCALMEP(IW,GMOD,MPL55, IERRC,MEPD55) 
C     ------------------------------------------------------------------
CB    SUBROUTINE KCALMEP USES COMMON BLOCKS
CB       KNCHOUT
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      !
      ! INPUT
      !
      INTEGER   IW
      REAL*8    GMOD,MPL55(5,5) 
      !
      ! OUTPUT 
      !
      INTEGER   IERRC 
      REAL*8    MEPD55(5,5) 
      !
      ! LOCAL 
      !
      INTEGER   I,J,K,LIG(5) 
      REAL*8    TWOG,UNI(5,5),SUM(5,5),INVSUM(5,5),CHECK(5,5) 
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT
C     ------------------------------------------------------------------
C     INITIALISES CONSTANT 
C     ------------------------------------------------------------------
      DO I=1,5 
         DO J=1,5 
            UNI(I,J)=0.0D0 
         ENDDO 
         UNI(I,I)=1.0D0 
      ENDDO

C     ------------------------------------------------------------------
C     COMPUTES THE MATRIX TO INVERT 'SUM' AND INITIALISES INVSUM 
C     ------------------------------------------------------------------
      TWOG=2.0D0*GMOD 
      DO I=1,5 
         DO J=1,5 
            SUM(I,J)=MPL55(I,J)+TWOG*UNI(I,J) 
            INVSUM(I,J)=SUM(I,J) 
         ENDDO 
      ENDDO 
 
C     ------------------------------------------------------------------
C     CALL THE INVERSION 
C     IF THE FIFTH ARG IS 1.E-6 INSTEAD OF 1.0D-6, IT DOES NOT WORK !
C     BEFORE THE CALL: THE INPUT IS INVSUM, THE MATRIX TO INVERT 
C     AFTER  THE CALL: INVSUM IS THE INVERTED MATRIX 
C     ------------------------------------------------------------------
      CALL KDMATIN(INVSUM,5,5,LIG,1.0D-6,0) 
 
C     ------------------------------------------------------------------
C     CHECKS THE INVERSION 
C     ------------------------------------------------------------------
      IERRC=0 
      DO I=1,5 
         DO J=1,5 
            CHECK(I,J)=0.0D0 
            DO K=1,5 
               CHECK(I,J)=CHECK(I,J)+SUM(I,K)*INVSUM(K,J) 
            ENDDO 
         ENDDO 
         IF(CHECK(I,I)-1.0D0.GT.0.02D0) IERRC=1 
      ENDDO 
 
      IF(IERRC.EQ.1) THEN 
         WRITE(OUT6,980)
      ENDIF 
 
C     ------------------------------------------------------------------
C     COMPUTE THE 5X5 DEVIATORIC ELASTO-PLASTIC MODULUS 
C     ------------------------------------------------------------------
      DO I=1,5 
         DO J=1,5 
            MEPD55(I,J)=TWOG*UNI(I,J)-TWOG*TWOG*INVSUM(I,J) 
         ENDDO 
      ENDDO 
 
C     ------------------------------------------------------------------
C     FORMAT
C     ------------------------------------------------------------------
980   FORMAT(6X,'[KCALMEP] KWARNING! INV FAILED -> IERRC=1')
C     ------------------------------------------------------------------
      RETURN 
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KHARDEN(IW,MATCOF,GAM,AV,S,
     .                    RAD,BCK,POL,STG, FVAL,H,HPRIM)
C     ------------------------------------------------------------------
CU    SUBROUTINE KHARDEN USES SUBROUTINES 
CU       F3DER
CU       KX52X33
CU       KLENGTH
CU       KPRODUC
CU       XIT
C     ------------------------------------------------------------------
CA    CALL KHARDEN(IW,MATCOF,GAM,AV,S,
CA   .             RAD,BCK,POL,STG, FVAL,H,HPRIM)
C     ------------------------------------------------------------------
CB    SUBROUTINE KHARDEN USES COMMON BLOCKS
CB       KNCHOUT
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      !
      ! INPUT 
      !
      INTEGER   IW
      REAL*8    MATCOF(14),GAM,AV(5),S(600),RAD,BCK(5),POL(5),STG(5,5)
      !
      ! OUTPUT 
      !
      REAL*8    FVAL,H,HPRIM(3,3) 
      !
      ! LOCAL 
      !
      INTEGER   I,J,IDER,NCFE
      REAL*8    POT,DF(5),DDF(5,5),TEMP,CST0
      REAL*8    UV(5),NRMSL
      REAL*8    AV33(3,3),UV33(3,3),X33(3,3)
      REAL*8    TAU0,X0,SSAT,RSAT,CP,CSL,CSD,CX,CR,NP,NL,M,R
      REAL*8    KSWIFT,NSWIFT,GSWIFT
      REAL*8    SD,SL(5,5),XSAT,HX,HP
      REAL*8    TMP1,TMP2
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT
C     ------------------------------------------------------------------
C     NORMALISED DEVIATORIC YL POINT CORRESPONDING TO AV
C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,920)

C     ------------------------------------------------------------------
C     TRANSFORMATION TO MATRICES 
C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,900) GAM,(AV(I),I=1,5),(UV(I),I=1,5) 
      CALL KX52X33(AV , AV33)
      CALL KX52X33(UV , UV33)
      CALL KX52X33(BCK, X33 )

C     ------------------------------------------------------------------
C     PREPARES HP,HX,SD,XSAT IF TEXMIC MODEL
C     ------------------------------------------------------------------
      IF ( (MATCOF(1).EQ.2.0D0)  .OR. (MATCOF(1).EQ.30.0D0) ) THEN
         KSWIFT=MATCOF(2) 
         NSWIFT=MATCOF(3) 
         GSWIFT=MATCOF(4) 
         IF(IW.EQ.1)WRITE(OUT6,880) KSWIFT,GSWIFT,NSWIFT 
         H=NSWIFT*KSWIFT*(GSWIFT+GAM)**(NSWIFT-1.0D0) 
         DO I=1,3 
            DO J=1,3 
               HPRIM(I,J)=0.0D0 
            ENDDO 
         ENDDO 
         RETURN
C
      ELSE IF(MATCOF(1).EQ.3.0D0.OR.MATCOF(1).EQ.4.0D0) THEN
      IDER=2
      NCFE=S(2)
      CALL KF3DER(POT,DF,DDF,AV,s(9),s(9+NCFE),5,IDER,OUT6,0)
      FVAL=POT
      TEMP=0.0D0
      DO I=1,5
         TEMP=TEMP+DF(I)*AV(I)
      ENDDO
      TEMP=POT-TEMP
      DO I=1,5
         UV(I)=DF(I)+AV(I)*TEMP
      ENDDO
C
C        ---------------------------------------------------------------
C        MATERIAL COEFFICIENTS
C        ---------------------------------------------------------------
         TAU0=MATCOF(2)
         X0  =MATCOF(3)
         SSAT=MATCOF(4)
         RSAT=MATCOF(5)
         CP  =MATCOF(6)
         CSL =MATCOF(7)
         CSD =MATCOF(8)
         CX  =MATCOF(9)
         CR  =MATCOF(10)
         NP  =MATCOF(11)
         NL  =MATCOF(12)
         M   =MATCOF(13)
         R   =MATCOF(14)

C        ---------------------------------------------------------------
C        COMPUTES SD, SL(5,5) AND NRMSL
C        ---------------------------------------------------------------
         IF(MATCOF(1).EQ.3.0D0) THEN
            SD=0.0D0
            DO I=1,5
               SD=SD+AV(I)*(STG(I,1)*AV(1)
     .                     +STG(I,2)*AV(2)
     .                     +STG(I,3)*AV(3)
     .                     +STG(I,4)*AV(4)
     .                     +STG(I,5)*AV(5))
            ENDDO
            DO I=1,5
               DO J=1,5
                  SL(I,J)=STG(I,J)-SD*AV(I)*AV(J)
               ENDDO
            ENDDO
            CALL KLENGTH(SL,5,5, NRMSL)
         ELSE IF(MATCOF(1).EQ.4.0D0) THEN
            SD=STG(1,1)
            DO I=1,5
               DO J=1,5
                  SL(I,J)=0.0D0
               ENDDO
            ENDDO
            NRMSL=0.0D0
         ENDIF      

C        ---------------------------------------------------------------
C        COMPUTES XSAT
C        ---------------------------------------------------------------
         XSAT=X0+(1-M)*DSQRT(SD**2.0D0+R*NRMSL**2.0D0)

C        ---------------------------------------------------------------
C        DERIVE THE VALUE OF HX
C        ---------------------------------------------------------------
         IF (XSAT.NE.0.0D0) THEN
            CALL KPRODUC(BCK,AV,5, HX)
C           HX=0.5D0*(1-HX/(XSAT*NRMU))
            HX=0.5D0*(1-HX/(XSAT*POT))
         ELSE
            ! ISOTROPIC HARDENING : NO BAUSCHINGER (BCK=0) AND
            ! NO MICRO-BAUSCHINGER (HXB=0) CORRECT ?
            HX=0.0D0
         ENDIF

C        ---------------------------------------------------------------
C        COMPUTES HP
C        ---------------------------------------------------------------
         IF(CP+CSD.EQ.0.D0) THEN
            WRITE(OUT6,910)
            CALL XIT 
         ELSE
            CST0=CP/(CP+CSD)
         ENDIF

         CALL KPRODUC(AV,POL,5, HP)
         IF (HP.GE.0) THEN
            HP=1.0D0-CST0*ABS(SD/SSAT-HP)
         ELSE
            HP=(1.0D0+HP)**NP*(1.0D0-CST0*DABS(SD)/SSAT)
         ENDIF
         IF(IW.EQ.1) THEN 
            WRITE(OUT6,860) CSD,CSL,NL,SSAT,CX,M 
            WRITE(OUT6,840) HP,HX,SD,XSAT,NRMSL,(BCK(I),I=1,5) 
         ENDIF 

C        ---------------------------------------------------------------
C        COMPUTES H AND HPRIM
C        H=D_TAU/D_GAMMA 
C        IF THE ELEMENT IS PLASTIC FOR THE FIRST TIME, ONE CAN SEE IT AS
C        THE FIRST STEP OF A MONOTONIC DEFORMATION.  SO TAU=TAU0+M*SD
C        AND D_TAU/D_GAM=M*D_SD/D_GAM
C        ---------------------------------------------------------------
         IF(GAM.NE.0.D0) THEN
            H   = M/DSQRT(NRMSL**2.0D0+SD**2.0D0) 
            TMP1= (-CSL*NRMSL**(NL+2.0D0))/(SSAT**NL) 
            TMP2= SD*CSD*(HP*(SSAT-SD)-HX*SD) 
            H   = H*(TMP1+TMP2) + CR*(RSAT-RAD)
         ELSE
            H   = M*CSD*(HP*(SSAT-SD)-HX*SD)  + CR*(RSAT-RAD)
         ENDIF

         DO I=1,3 
            DO J=1,3 
               HPRIM(I,J)=CX*(XSAT*UV33(I,J)-X33(I,J)) 
            ENDDO 
         ENDDO 

      ELSE

         WRITE(OUT6,820) MATCOF(1)
         CALL XIT
         STOP

      ENDIF
      IF(IW.EQ.1)WRITE(OUT6,800) H,((HPRIM(I,J),J=1,3),I=1,3) 

C     ------------------------------------------------------------------
C     FORMATS 
C     ------------------------------------------------------------------
920   FORMAT(9X,'[KHARDEN]BEGIN')
910   FORMAT(9X,'[KHARDEN] KWARNING! STOP, ERROR DUE TO CP+CSD=0')
900   FORMAT(9X,'[KHARDEN]GAM .... : ',E13.5 ,/, 
     .       9X,'         AV ..... : ',5E13.5,/,  
     .       9X,'         UV ..... : ',5E13.5  ) 
880   FORMAT(9X,'[KHARDEN]KSWIFT.. : ', E13.5,/, 
     .       9X,'         GSWIFT.. : ', E13.5,/,  
     .       9X,'         NSWIFT.. : ', E13.5  ) 
860   FORMAT(9X,'[KHARDEN]CSD..... : ', E13.5,/, 
     .       9X,'         CSL..... : ', E13.5,/,  
     .       9X,'         NL...... : ', E13.5,/,  
     .       9X,'         SSAT.... : ', E13.5,/,  
     .       9X,'         CX...... : ', E13.5,/,  
     .       9X,'         M....... : ', E13.5,/) 
840   FORMAT(9X,'[KHARDEN]HP...... : ', E13.5,/, 
     .       9X,'         HX...... : ', E13.5,/,  
     .       9X,'         SD...... : ', E13.5,/,  
     .       9X,'         XSAT.... : ', E13.5,/,  
     .       9X,'         NRMSL... : ', E13.5,/, 
     .       9X,'         BCK..... : ',5E13.5,/) 
820   FORMAT(9X,'[KHARDEN] KWARNING! STOP, OUT OF RANGE MATCOF(1)',I5)
800   FORMAT(9X,'[KHARDEN]--> H... : ', E13.5,/, 
     .       9X,'         --> HPRIM: ',       /,  
     .       3(9X,9X,3E13.5,/)                 ) 
780   FORMAT(9X,'[KHARDEN]EXIT')
C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,780)
      RETURN
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KASOLVE(IW,MATCOF,AV,UV,DGAM,GAMB,POT,
     .                    RADA,BCKA,POLA,SDA,NRMSLA,
     .                    RADB,BCKB,POLB,SDB,NRMSLB,
     .                    ESSH5,TAUB,HPB,HXB,XSATB)
C     ------------------------------------------------------------------
CU    SUBROUTINE KASOLVE USES SUBROUTINES 
CU       KLENGTH
CU       KPRODUC
CU       XIT
C     ------------------------------------------------------------------
CA    CALL KASOLVE(IW,MATCOF,AV,UV,DGAM,GAMB,POT,
CA   .             RADA,BCKA,POLA,SDA,NRMSLA,
CA   .             RADB,BCKB,POLB,SDB,NRMSLB,
CA   .             ESSH5,TAUB,HPB,HXB,XSATB)
C     ------------------------------------------------------------------
CB    SUBROUTINE KASOLVE USES COMMON BLOCKS
CB       KNCHOUT
CB       KXSAT
C     ------------------------------------------------------------------
      IMPLICIT  NONE
      !
      ! INPUT
      !
      INTEGER   IW
      REAL*8    MATCOF(14),AV(5),UV(5),DGAM,GAMB,POT,
     .          RADA,BCKA(5),POLA(5),SDA,NRMSLA
      !
      ! OUTPUT
      !
      REAL*8    RADB,BCKB(5),POLB(5),SDB,NRMSLB
      REAL*8    ESSH5(5),TAUB,HPB,HXB,XSATB
      !
      ! LOCAL
      !
      INTEGER   I
      REAL*8    TAU0,X0,SSAT,RSAT,CP,CSL,CSD,CX,CR,NP,NL,M,R
      REAL*8    KSWIFT,NSWIFT,GSWIFT
      REAL*8    CST0,CST1,CST2,CST3,CST4(5),CST5(5),CST6(5)
      REAL*8    XSATA,NRMU,TEMP
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT

      REAL*8    XSAT
      COMMON   /KXSAT/   XSAT
C     ------------------------------------------------------------------
      IF( (MATCOF(1).EQ.2.0D0) .OR. (MATCOF(1).EQ.30.0D0) )THEN
         KSWIFT=MATCOF(2)
         NSWIFT=MATCOF(3)
         GSWIFT=MATCOF(4)
         RADB  =0.0D0
         DO I=1,5
            BCKB(I)=0.0D0
            POLB(I)=0.0D0
         ENDDO
         NRMSLB = 0.0D0
         TAUB   = KSWIFT*(GSWIFT+GAMB)**NSWIFT
         HPB    = 0.0D0
         HXB    = 0.0D0
         SDB    = 0.0D0
         XSATB  = 0.0D0
         DO I=1,5
            ESSH5(I)=TAUB*UV(I)
         END DO
         IF(IW.EQ.1)WRITE(OUT6,900) TAUB,(AV(I),I=1,5),(UV(I),I=1,5),
     .      DGAM,GAMB,POT,
     .      RADA,(BCKA(I),I=1,5),(POLA(I),I=1,5),SDA,NRMSLA,
     .      RADB,(BCKB(I),I=1,5),(POLB(I),I=1,5),SDB,NRMSLB,
     .      (ESSH5(I),I=1,5)
         RETURN
      ELSE IF(MATCOF(1).EQ.3.0D0.OR.MATCOF(1).EQ.4.0D0) THEN
C        ---------------------------------------------------------------
C        MATERIAL COEFFICIENTS
C        ---------------------------------------------------------------
         TAU0=MATCOF(2)
         X0  =MATCOF(3)
         SSAT=MATCOF(4)
         RSAT=MATCOF(5)
         CP  =MATCOF(6)
         CSL =MATCOF(7)
         CSD =MATCOF(8)
         CX  =MATCOF(9)
         CR  =MATCOF(10)
         NP  =MATCOF(11)
         NL  =MATCOF(12)
         M   =MATCOF(13)
         R   =MATCOF(14)

C        ---------------------------------------------------------------
C        NORM OF NORMALISED DEVIATORIC YIELD LOCUS STRESS
C        ---------------------------------------------------------------
         CALL KLENGTH(UV,5,1, NRMU)

C        ---------------------------------------------------------------
C        POLARITY
C        ---------------------------------------------------------------
         DO I=1,5
            POLB(I)=POLA(I)+(AV(I)-POLA(I))*(1.0D0-EXP(-CP*DGAM))
         ENDDO

C        ---------------------------------------------------------------
C        STATE VARIABLE R
C        ---------------------------------------------------------------
         RADB=RADA+(RSAT-RADA)*(1.0D0-EXP(-CR*DGAM))

C        ---------------------------------------------------------------
C        CONTRIBUTION OF LATENT DISLOCATIONS TO THE RESISTANCE TO GLIDE
C        ---------------------------------------------------------------
         IF(NRMSLA.EQ.0.0D0) THEN
            NRMSLB=0.0D0
         ELSE
            IF(NL.EQ.0.0D0) THEN
               NRMSLB=NRMSLA+CSL*DGAM
            ELSE
               IF(CSL.EQ.0.0D0) THEN
                  NRMSLB=NRMSLA
               ELSE
                  TEMP=1.0D0/(CSL*NL)*(SSAT/NRMSLA)**NL
C OLD             NRMSLB=SSAT
C OLD.                  *(CS*NL*DGAM+(SSAT/NRMSLA)**NL)**(-1.0D0/NL)
                  NRMSLB=SSAT
     .                  *(CSL*NL)**(-1.0D0/NL)*(DGAM+TEMP)**(-1.0D0/NL)
               ENDIF
            ENDIF
         ENDIF

C        ---------------------------------------------------------------
C        CONTRIBUTION OF CURRENT DISLOCATIONS TO THE RESISTANCE TO GLIDE
C        HXB=FUNCTION OF BCKA,XSATA
C        HPB=FUNCTION OF POLB,SDA
C        ---------------------------------------------------------------
         XSATA=X0+(1.0D0-M)*DSQRT(SDA**2.0D0+R*NRMSLA**2.0D0)
         IF (XSATA.NE.0.0D0) THEN
            CALL KPRODUC(BCKA,AV,5, HXB) 
C           HXB=0.5D0*(1.0D0-HXB/(XSATA*NRMU))
            HXB=0.5D0*(1.0D0-HXB/(XSATA*POT))
         ELSE
            ! ISOTROPIC HARDENING : NO BAUSCHINGER (BCK=0) AND
            ! NO MICRO-BAUSCHINGER (HXB=0) CORRECT ?
            HXB=0.0D0
         ENDIF
      
         IF(CP+CSD.EQ.0.D0) THEN
            WRITE(OUT6,910)
            CALL XIT
         ELSE
            CST0=CP/(CP+CSD)
         ENDIF

         CALL KPRODUC(POLB,AV,5, HPB)
         IF (HPB.GE.0.0D0) THEN
            HPB=1.0D0-CST0*ABS(SDA/SSAT-HPB)
         ELSE
            HPB=(1.0D0+HPB)**NP*(1.0D0-CST0*DABS(SDA)/SSAT)
         ENDIF

         CST3=HPB*SSAT/(HPB+HXB)
         CST1=SDA-CST3
         CST2=CSD*(HPB+HXB)
         SDB =CST1*EXP(-CST2*DGAM)+CST3

C        ---------------------------------------------------------------
C        CONTRIBUTION OF THE AVERAGE RESISTANCE TO GLIDE FOR THE 
C        ISOTROPIC HARDENING
C        ---------------------------------------------------------------
         TAUB=TAU0+RADB+M*DSQRT(SDB**2.0D0+NRMSLB**2.0D0)

C        ---------------------------------------------------------------
C        BACK-STRESS DUE TO CONSTANT (PRECIPITATES ...) AND
C        VARIABLE (DISLOCATIONS WALLS) COMPOSITE EFFECT
C        ---------------------------------------------------------------
         DO I=1,5
            CST4(I)=CX*(X0+(1.0D0-M)*CST3)*UV(I)
            CST5(I)=CX*(1.0D0-M)*CST1*UV(I)
            CST6(I)=BCKA(I)-CST4(I)/CX-CST5(I)/(CX-CST2)
            BCKB(I)=CST6(I)*EXP(-CX*DGAM)+CST4(I)/CX
     .             +CST5(I)/(CX-CST2)*EXP(-CST2*DGAM)
         ENDDO
C_TEST
C        FORMULE FAX TEODOSIU
C_TEST
         XSATB=X0+(1.0D0-M)*DSQRT(SDB**2.0D0+R*NRMSLB**2.0D0)
         XSAT=XSATB
         DO I=1,5
            BCKB(I)=BCKA(I)
     .             +0.5D0*CX*DGAM*(XSATA*UV(I)-BCKA(I)+XSATB*UV(I))
            BCKB(I)=BCKB(I)/(1.0D0+0.5D0*CX*DGAM)
         ENDDO
C_TEST
C_TEST  WRITE(OUT6,'(12X,A)') 'ASOLVER SET BCKB TO ZERO'
      ELSE
         WRITE(OUT6,920) MATCOF(1)
         CALL XIT
         STOP
      ENDIF

C     ------------------------------------------------------------------
C     DEVIATORIC STRESS VECTOR
C     ------------------------------------------------------------------
      DO I=1,5
         ESSH5(I)=BCKB(I)+TAUB*UV(I)
      END DO
      IF(IW.EQ.1)WRITE(OUT6,900) TAUB,(AV(I),I=1,5),(UV(I),I=1,5),
     .      DGAM,GAMB,POT,
     .      RADA,(BCKA(I),I=1,5),(POLA(I),I=1,5),SDA,NRMSLA,
     .      RADB,(BCKB(I),I=1,5),(POLB(I),I=1,5),SDB,NRMSLB,
     .      (ESSH5(I),I=1,5)

C     ------------------------------------------------------------------
920   FORMAT(15X,'[KASOLVE] KWARNING! STOP, OUT OF RANGE MATCOF(1)',I5)
910   FORMAT(15X,'[KASOLVE] KWARNING! STOP, ERROR DUE TO CP+CSD=0')
900   FORMAT(15X,'[KASOLVE] TAU   =',1E13.5,/,
     .       15X,'          AV    =',5E13.5,/,
     .       15X,'          UV    =',5E13.5,/,
     .       15X,'          DGAM  =',1E13.5,/,
     .       15X,'          GAMB  =',1E13.5,/,
     .       15X,'          POT   =',1E13.5,/,
     .       15X,'          RADA  =', E13.5,/,
     .       15X,'          BCKA  =',5E13.5,/,
     .       15X,'          POLA  =',5E13.5,/,
     .       15X,'          SDA   =',1E13.5,/,
     .       15X,'          NRMSLA=',1E13.5,/,
     .       15X,'          RADB  =', E13.5,/,
     .       15X,'          BCKB  =',5E13.5,/,
     .       15X,'          POLB  =',5E13.5,/,
     .       15X,'          SDB   =',1E13.5,/,
     .       15X,'          NRMSLB=',1E13.5,/,
     .       15X,'          ESSH5 =',5E13.5,/)
C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KLENGTH(X,M,N, L) 
C     ------------------------------------------------------------------
CU    SUBROUTINE KLENGTH USES SUBROUTINES 
CU       NONE 
C     ------------------------------------------------------------------
CA    CALL KLENGTH(X,M,N, L)
C     ------------------------------------------------------------------
CB    SUBROUTINE KLENGTH USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      INTEGER   I,J,M,N 
      REAL*8    X(M,N),L 
       
      L=0.0D0 
      DO I=1,M
         DO J=1,N
            L=L+X(I,J)*X(I,J) 
         ENDDO
      ENDDO 
      L=DSQRT(L) 

C     ------------------------------------------------------------------
      RETURN 
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KLENGT6(X6, L) 
C     ------------------------------------------------------------------
CU    SUBROUTINE KLENGT6 USES SUBROUTINES 
CU       NONE 
C     ------------------------------------------------------------------
CA    CALL KLENGT6(X6, L)
C     ------------------------------------------------------------------
CB    SUBROUTINE KLENGT6 USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
      IMPLICIT  NONE
      REAL*8    X6(6),L 
       
      L=      X6(1)**2.0D0+      X6(2)**2.0D0+      X6(3)**2.0D0
     . +2.0D0*X6(4)**2.0D0+2.0D0*X6(5)**2.0D0+2.0D0*X6(6)**2.0D0
      L=DSQRT(L) 

C     ------------------------------------------------------------------
      RETURN 
      END 

************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KMODCON(GMOD,KMOD,AV,FVAL,H,HPRIM, MCOR) 
C     ------------------------------------------------------------------
CU    SUBROUTINE KMODCON USES SUBROUTINES 
CU       KX52X33
CU       KMODELA
C     ------------------------------------------------------------------
CA    CALL KMODCON(GMOD,KMOD,AV,FVAL,H,HPRIM, MCOR)
C     ------------------------------------------------------------------
CB    SUBROUTINE KMODCON USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
      IMPLICIT  NONE
      !
      ! INPUT
      !
      REAL*8    GMOD,KMOD,AV(5),FVAL,H,HPRIM(3,3)
      !
      ! OUTPUT
      !
      REAL*8    MCOR(9,9)
      !
      ! LOCAL
      !
      INTEGER   I,J,K,L,M,N 
      REAL*8    NOR(3,3),TWOG,DENOM 
C     ------------------------------------------------------------------
      TWOG=2.0D0*GMOD 
      CALL KX52X33(AV, NOR) 

      DENOM=0.0D0 
      DO I=1,3 
         DO J=1,3 
            DENOM=DENOM+NOR(I,J)*HPRIM(I,J) 
         ENDDO 
      ENDDO 
      DENOM=1.0D0+DENOM*FVAL/TWOG+FVAL*FVAL*H/TWOG
    
      CALL KMODELA(GMOD,KMOD, MCOR) 

C              KL ... 
C            IJ
C            .
C          N .   1  2   3   4   5   6   7   8   9
C        M   .  11 12  13  21  22  23  31  32  33
C        1   11
C        2   12
C        3   13
C        4   21
C        5   22
C        6   23
C        7   31
C        8   32
C        9   33
 
      M=0 
      DO I=1,3 
         DO J=1,3 
            M=M+1 
            N=0 
            DO K=1,3 
               DO L=1,3 
                  N=N+1 
                  MCOR(M,N)=MCOR(M,N)-TWOG*NOR(I,J)*NOR(K,L)/DENOM 
               ENDDO 
            ENDDO 
         ENDDO 
      ENDDO 

C     ------------------------------------------------------------------
      RETURN 
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KX5_2X6(X5, X6)
C     ------------------------------------------------------------------
CU    SUBROUTINE KX5_2X6 USES SUBROUTINES 
CU       NONE 
C     ------------------------------------------------------------------
CA    CALL KX5_2X6(X5, X6)
C     ------------------------------------------------------------------
CB    SUBROUTINE KX5_2X6 USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      REAL*8    X5(5), X6(6) 
      REAL*8    IROOT2,IROOT6,ROOT23 
 
      IROOT2=1.0D0/SQRT(2.0D0) 
      IROOT6=1.0D0/SQRT(6.0D0) 
      ROOT23=SQRT(2.0D0/3.0D0) 
 
      X6(1)= IROOT2*X5(1)+IROOT6*X5(2) 
      X6(2)=-IROOT2*X5(1)+IROOT6*X5(2) 
      X6(3)=-ROOT23*X5(2) 
      X6(4)= IROOT2*X5(5) 
      X6(5)= IROOT2*X5(4) 
      X6(6)= IROOT2*X5(3) 

C     ------------------------------------------------------------------
      RETURN 
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KX52X33(X5, X33)
C     ------------------------------------------------------------------
CU    SUBROUTINE KX52X33 USES SUBROUTINES 
CU       KX52X33 
CU       KX62X33 
C     ------------------------------------------------------------------
CA    CALL KX52X33(X5, X33)
C     ------------------------------------------------------------------
CB    SUBROUTINE KX52X33 USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      REAL*8    X5(5), X33(3,3) 
      REAL*8    X6(6) 
 
      CALL KX5_2X6(X5, X6) 
      CALL KX62X33(X6, X33) 

C     ------------------------------------------------------------------
      RETURN 
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KX6_2X5(X6, X5)
C     ------------------------------------------------------------------
CU    SUBROUTINE KX6_2X5 USES SUBROUTINES 
CU       NONE 
C     ------------------------------------------------------------------
CA    CALL KX6_2X5(X6, X5)
C     ------------------------------------------------------------------
CB    SUBROUTINE KX6_2X5 USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      REAL*8    X6(6), X5(5) 
      REAL*8    ROOT2,IROOT2,ROOT32 
 
      ROOT2 = SQRT(2.0D0) 
      IROOT2= 1.0D0/ROOT2 
      ROOT32= SQRT(3.0D0/2.0D0) 
 
      X5(1) =IROOT2*(X6(1)-X6(2)) 
      X5(2) =ROOT32*(X6(1)+X6(2)) 
      X5(3) =ROOT2 * X6(6) 
      X5(4) =ROOT2 * X6(5) 
      X5(5) =ROOT2 * X6(4) 

C     ------------------------------------------------------------------
      RETURN 
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KX62X33(X6, X33)
C     ------------------------------------------------------------------
CU    SUBROUTINE KX62X33 USES SUBROUTINES 
CU       NONE 
C     ------------------------------------------------------------------
CA    CALL KX62X33(X6, X33)
C     ------------------------------------------------------------------
CB    SUBROUTINE KX52X33 USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      REAL*8    X6(6), X33(3,3) 
 
      X33(1,1)=X6(1) 
      X33(1,2)=X6(4) 
      X33(1,3)=X6(5) 
      X33(2,1)=X6(4) 
      X33(2,2)=X6(2) 
      X33(2,3)=X6(6) 
      X33(3,1)=X6(5) 
      X33(3,2)=X6(6) 
      X33(3,3)=X6(3) 

C     ------------------------------------------------------------------
      RETURN 
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KX332X5(X33, X5)
C     ------------------------------------------------------------------
CU    SUBROUTINE KX332X5 USES SUBROUTINES 
CU       KX332X6 
CU       KX6_2X5
C     ------------------------------------------------------------------
CA    CALL KX332X5(X33, X5)
C     ------------------------------------------------------------------
CB    SUBROUTINE KX52X33 USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      REAL*8    X33(3,3), X5(5) 
      REAL*8    X6(6) 
 
      CALL KX332X6(X33, X6) 
      CALL KX6_2X5(X6, X5) 

C     ------------------------------------------------------------------
      RETURN 
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KX332X6(X33, X6)
C     ------------------------------------------------------------------
CU    SUBROUTINE KX332X6 USES SUBROUTINES 
CU       NONE 
C     ------------------------------------------------------------------
CB    SUBROUTINE KX332X6 USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
CA    CALL KX332X6(X33, X6)
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      REAL*8    X33(3,3), X6(6) 
 
      X6(1)=X33(1,1) 
      X6(2)=X33(2,2) 
      X6(3)=X33(3,3) 
      X6(4)=X33(1,2) 
      X6(5)=X33(1,3) 
      X6(6)=X33(2,3) 

C     ------------------------------------------------------------------
      RETURN 
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KMAKE66(A9, A6)
C     ------------------------------------------------------------------
CU    SUBROUTINE KMAKE66 USES
CU       NONE
C     ------------------------------------------------------------------
CA    CALL KMAKE66(A9, A6)
C     ------------------------------------------------------------------
CB    SUBROUTINE KMAKE66 USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
      IMPLICIT  NONE
      INTEGER   I,J,INDEX(6)
      REAL*8    A9(9,9),A6(6,6)
C     ------------------------------------------------------------------
      INDEX(1)=1
      INDEX(2)=5
      INDEX(3)=9
      INDEX(4)=2
      INDEX(5)=3
      INDEX(6)=6
      DO I=1,6
         DO J=1,6
            A6(I,J)=A9(INDEX(I),INDEX(J))
            IF (J.EQ.4) A6(I,J)=A6(I,J)+A9(INDEX(I),4)
            IF (J.EQ.5) A6(I,J)=A6(I,J)+A9(INDEX(I),7)
            IF (J.EQ.6) A6(I,J)=A6(I,J)+A9(INDEX(I),8)
         ENDDO
      ENDDO

C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KMAKE99(A6, A9)
C     ------------------------------------------------------------------
CU    SUBROUTINE KMAKE99
CU       NONE
C     ------------------------------------------------------------------
CA    CALL KMAKE99(A6, A9)
C     ------------------------------------------------------------------
CB    SUBROUTINE KMAKE99 USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
      IMPLICIT  NONE
      INTEGER   I,J,INDEX(9)
      REAL*8    A6(6,6),A9(9,9)
C     ------------------------------------------------------------------
      INDEX(1)=1
      INDEX(2)=4
      INDEX(3)=5
      INDEX(4)=4
      INDEX(5)=2
      INDEX(6)=6
      INDEX(7)=5
      INDEX(8)=6
      INDEX(9)=3
C
      DO I=1,9
         DO J=1,9
            A9(I,J)=A6(INDEX(I),INDEX(J))
            IF((J.EQ.2).OR.(J.EQ.3).OR.(J.EQ.4).OR.
     .         (J.EQ.6).OR.(J.EQ.7).OR.(J.EQ.8)) A9(I,J)=A9(I,J)/2.0D0
         ENDDO
      ENDDO

C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KPRODUC(X1,X2,N, PROD)
C     ------------------------------------------------------------------
CU    SUBROUTINE KPRODUC USES SUBROUTINES 
CU       NONE 
C     ------------------------------------------------------------------
CA    CALL KPRODUC(X1,X2,N, PROD)
C     ------------------------------------------------------------------
CB    SUBROUTINE KPRODUC USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      INTEGER   I,N 
      REAL*8    X1(N),X2(N), PROD
 
      PROD=0.0D0 
      DO I=1,N 
         PROD=PROD+X1(I)*X2(I) 
      ENDDO 

C     ------------------------------------------------------------------
      RETURN 
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KPRTMAT(UNIT,MAT,M,N,FRMT,LINE)
C     ------------------------------------------------------------------
CU    SUBROUTINE KPRTMAT USES SUBROUTINES 
CU       NONE 
C     ------------------------------------------------------------------
CA    CALL KPRTMAT(UNIT,MAT,M,N,FRMT,LINE)
C     ------------------------------------------------------------------
CB    SUBROUTINE KPRTMAT USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
      INTEGER UNIT,M,N,I,J,LINE 
      REAL*8  MAT(M,N) 
      CHARACTER*6 FRMT 
C       
      IF (FRMT(1:5).EQ.'f10.2')  FRMT(1:5)='F10.2' 
      IF (FRMT(1:5).EQ.'e13.5')  FRMT(1:5)='E13.5' 
      IF (FRMT(1:6).EQ.'e25.16') FRMT(1:6)='E25.16' 
C       
      IF (FRMT(1:5).NE.'F10.2') GOTO 20 
      DO I=1,M 
         DO J=1,N 
             WRITE(UNIT,510) MAT(I,J) 
         ENDDO 
      WRITE(UNIT,*) 
      ENDDO 
      IF (LINE.EQ.1) WRITE(UNIT,*) 
      RETURN 
C       
20    IF (FRMT(1:5).NE.'E13.5') GOTO 30 
      DO I=1,M 
         DO J=1,N 
             WRITE(UNIT,520) MAT(I,J) 
         ENDDO 
      WRITE(UNIT,*) 
      ENDDO 
      IF (LINE.EQ.1) WRITE(UNIT,*) 
      RETURN  
C       
30    IF (FRMT(1:6).NE.'E25.16') GOTO 40 
      DO I=1,M 
         DO J=1,N 
             WRITE(UNIT,530) MAT(I,J)      
         ENDDO 
      WRITE(UNIT,*)  
      ENDDO 
      IF (LINE.EQ.1) WRITE(UNIT,*) 
      RETURN  
C       
40    WRITE(UNIT,540) 
      DO I=1,M 
         DO J=1,N 
             WRITE(UNIT,520) MAT(I,J) 
         ENDDO 
      WRITE(UNIT,*)  
      ENDDO 
      IF (LINE.EQ.1) WRITE(UNIT,*) 

C     ------------------------------------------------------------------
510   FORMAT(F10.2,$) 
520   FORMAT(E13.5,$) 
530   FORMAT(E25.16,$) 
540   FORMAT('REQUIRED FORMAT NOT SUPPORTED BY PRTMAT.F',/, 
     .       'DEFAULT FORMAT IS USED') 
C     ------------------------------------------------------------------
      RETURN 
      END 
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KCOFBCK(STG,BCK,POL,AV,MATCOF,S, XSAT,HX,HP)
C     ------------------------------------------------------------------
CU    SUBROUTINE KCOFBCK USES SUBROUTINES 
CU       F3DER
CU       KLENGTH
CU       KPRODUC
C     ------------------------------------------------------------------
CA    CALL KCOFBCK(STG,BCK,POL,AV,MATCOF,S, XSAT,HX,HP)
C     ------------------------------------------------------------------
CB    SUBROUTINE KCOFBCK USES COMMON BLOCKS
CB       KNCHOUT
C     ------------------------------------------------------------------
CT    COMMENTS FOR SUBROUTINE KCOFBCK
CT       DECOMPOSITION OF STG ONTO AV
CT       IF CALL KCOFBCK(STGA,BCKA,POLA,AONSET)     !EXPLE: IN INIDPL
CT          THEN SDA,SLA,NRMSLA
CT               ->XSATA=FCT(SDA,NRMSLA)
CT               ->HXA  =FCT(BCKA,AONSET,XSATA)
CT               ->HPA  =FCT(POLA,AONSET,SDA)
CT       ENDIF
CT       IF CALL KCOFBCK(STGA,BCKA,POLB,AV_CURRENT) !EXPLE: IN ASOLVER
CT          THEN SDA,SLA,NRMSLA
CT               ->XSATA=FCT(SDA,NRMSLA)
CT               ->HXA  =FCT(BCKA,AV_CURRENT,XSATA)
CT               ->HPA  =FCT(POLB,AV_CURRENT,SDA)
CT       ENDIF
C     ------------------------------------------------------------------
      IMPLICIT  NONE
      !
      ! INPUT
      !
      REAL*8    STG(5,5),BCK(5),POL(5),AV(5),MATCOF(14),S(600)
      !
      ! OUTPUT
      !
      REAL*8    XSAT,HX,HP
      !
      ! LOCAL
      !
      INTEGER   IDER,NCFE
      REAL*8    FVAL,TEMP,POT,DF(5),DDF(5,5),UV(5)
      INTEGER   I,J
      REAL*8    TAU0,X0,SSAT,RSAT,CP,CSL,CSD,CX,CR,NP,NL,M,R
      REAL*8    SD,SL(5,5),NRMSL
      REAL*8    CST0
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT
C     ------------------------------------------------------------------
C     NORMALISED DEVIATORIC YL POINT CORRESPONDING TO AV
C     ------------------------------------------------------------------
      IDER=2
      NCFE=S(2)
      IF (MATCOF(1).EQ.30.D0) THEN
        CALL KFacetPOT(POT,DF,DDF,AV,IDER,OUT6,0)
      ELSE
        CALL KF3DER(POT,DF,DDF,AV,s(9),s(9+NCFE),5,IDER,OUT6,0)
      END IF
      FVAL=POT
      TEMP=0.0D0
      DO I=1,5
         TEMP=TEMP+DF(I)*AV(I)
      ENDDO
      TEMP=POT-TEMP
      DO I=1,5
         UV(I)=DF(I)+AV(I)*TEMP
      ENDDO

C     ------------------------------------------------------------------
C     DECOMPOSITION OF STG ONTO AV
C     ------------------------------------------------------------------
      TAU0=MATCOF(2)
      X0  =MATCOF(3)
      SSAT=MATCOF(4)
      RSAT=MATCOF(5)
      CP  =MATCOF(6)
      CSL =MATCOF(7)
      CSD =MATCOF(8)
      CX  =MATCOF(9)
      CR  =MATCOF(10)
      NP  =MATCOF(11)
      NL  =MATCOF(12)
      M   =MATCOF(13)
      R   =MATCOF(14)

      IF(MATCOF(1).EQ.3.0D0) THEN
         SD=0.0D0
         DO I=1,5
            SD=SD+AV(I)*(STG(I,1)*AV(1)
     .                  +STG(I,2)*AV(2)
     .                  +STG(I,3)*AV(3)
     .                  +STG(I,4)*AV(4)
     .                  +STG(I,5)*AV(5))
         ENDDO
         DO I=1,5
            DO J=1,5
               SL(I,J)=STG(I,J)-SD*AV(I)*AV(J)
            ENDDO
         ENDDO
         CALL KLENGTH(SL,5,5, NRMSL)
      ELSE IF(MATCOF(1).EQ.4.0D0) THEN
         SD=STG(1,1)
         DO I=1,5
            DO J=1,5
               SL(I,J)=0.0D0
            ENDDO
         ENDDO
         NRMSL=0.0D0
      ENDIF

C     ------------------------------------------------------------------
C     OUTPUT 1: XSAT
C     ------------------------------------------------------------------
      XSAT=X0+(1-M)*DSQRT(SD**2.0D0+R*NRMSL**2.0D0)

C     ------------------------------------------------------------------
C     OUTPUT 2: HX
C     ------------------------------------------------------------------
      IF (XSAT.NE.0.0D0) THEN
         CALL KPRODUC(BCK,AV,5, HX)
C        HX=0.5D0*(1-HX/(XSAT*NRMU))
         HX=0.5D0*(1-HX/(XSAT*POT))
      ELSE
         ! ISOTROPIC HARDENING : NO BAUSCHINGER (BCK=0) AND
         ! NO MICRO-BAUSCHINGER (HXB=0) CORRECT ?
         HX=0.0D0
      ENDIF

C     ------------------------------------------------------------------
C     OUTPUT 3: HP
C     ------------------------------------------------------------------
      CST0=CP/(CP+CSD)
      CALL KPRODUC(AV,POL,5, HP)
      IF (HP.GE.0) THEN
         HP=1.0D0-CST0*ABS(SD/SSAT-HP)
      ELSE
         HP=(1.0D0+HP)**NP*(1.0D0-CST0*DABS(SD)/SSAT)
      ENDIF

C     ------------------------------------------------------------------
      RETURN 
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KINIDPL(IW,MATCOF,GAM,AV,S,RAD,BCK,POL,STG,
     .                    ELAFRA,DCOR6,GMOD, NRMINI)
C     ------------------------------------------------------------------
CU    SUBROUTINE KINIDPL USES SUBROUTINES 
CU       KHARDEN
CU       KX62X33
CU       KX52X33
C     ------------------------------------------------------------------
CA    CALL KINIDPL(IW,MATCOF,GAM,AV,S,RAD,BCK,POL,STG,
CA   .             ELAFRA,DCOR6,GMOD, NRMINI)
C     ------------------------------------------------------------------
CB    SUBROUTINE KINIDPL USES COMMON BLOCKS
CB       KNCHOUT
C     ------------------------------------------------------------------
CT    COMMENTS FOR SUBROUTINE KINIDPL
CT       -1ST LINE OF INPUT: IDENTICAL TO THE ONE USED FOR KHARDEN
CT       -2ND LINE OF INPUT: EXTRA INPUT SPECIAL FOR HERE
CT       -OUTPUT IS ACTUALLY THE CONSISTENCY PARAMETER
CT        N{MN}H{MNPQ}DTOT{PQ}/
CT        N{MN}H{MNPQ}N{PQ}+N{MN}HPRIM{MN}FVAL+FVAL**2*H
C     ------------------------------------------------------------------
      IMPLICIT  NONE
      !
      ! INPUT
      !
      INTEGER   IW
      REAL*8    MATCOF(14),GAM,AV(5),S(600)
      REAL*8    RAD,BCK(5),POL(5),STG(5,5),ELAFRA,DCOR6(6),GMOD
      !
      ! OUTPUT
      !
      REAL*8    NRMINI
      !
      ! LOCAL
      !
      INTEGER   I,J
      REAL*8    FRAC
      REAL*8    FVAL,H,HPRIM(3,3)
      REAL*8    DTOT6(6),DTOT33(3,3),NOR(3,3),NUME,DENO
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT
C     ------------------------------------------------------------------
C     COMPUTES H AND HPRIM
C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,920)
      IF(IW.EQ.1)WRITE(OUT6,900) ELAFRA,(AV(I),I=1,5),(DCOR6(I),I=1,6)
      CALL KHARDEN(IW,MATCOF,GAM,AV,S,
     .             RAD,BCK,POL,STG, FVAL,H,HPRIM)

C     ------------------------------------------------------------------
C     COMPUTES THE CONSISTENCY PARAMETER LAMBDA (HERE CALLED NRMINI)
C     ------------------------------------------------------------------
      FRAC=ELAFRA
      IF(FRAC.GE.1.0D0) FRAC=1.0D0-1.0D-4
      DO I=1,6
         DTOT6(I)=(1.0D0-FRAC)*DCOR6(I)
      ENDDO

      CALL KX62X33(DTOT6, DTOT33)
      CALL KX52X33(AV, NOR)
      NUME=0.0D0
      DO I=1,3
         DO J=1,3
            NUME=NUME+NOR(I,J)*DTOT33(I,J)
         ENDDO
      ENDDO
      NUME=2.0D0*GMOD*NUME
      DENO=0.0D0
      DO I=1,3
         DO J=1,3
            DENO=DENO+NOR(I,J)*HPRIM(I,J)
         ENDDO
      ENDDO
      DENO=2.0D0*GMOD+DENO*FVAL+FVAL*FVAL*H
      NRMINI=NUME/DENO
      IF(IW.EQ.1)WRITE(OUT6,880) NUME,DENO,NRMINI

C     ------------------------------------------------------------------
C     FORMAT
C     ------------------------------------------------------------------
920   FORMAT(6X,'[KINIDPL] BEGIN')
900   FORMAT(6X,'[KINIDPL] ELAFRA : ',1E13.5,/,
     .       6X,'          AV     : ',5E13.5,/,
     .       6X,'          DCOR6  : ',6E13.5  )
880   FORMAT(6X,'[KINIDPL] NUME   : ',1E13.5,/,
     .       6X,'          DENO   : ',1E13.5,/,
     .       6X,'          NRMINI : ',1E13.5  )
800   FORMAT(6X,'[KINIDPL] EXIT',/)
C     ------------------------------------------------------------------
      IF(IW.EQ.1)WRITE(OUT6,800)
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KINIARG(ZIW,ZAVA,ZHIA5,ZSSA6,ZSSTR5,ZRMREF,ZWOG,
     .                    ZELTAT,ZS,ZAMA,ZAUA,ZADA,ZCKA,ZOLA,ZTGA,
     .                    ZONSET,ZATCOF)
C     ------------------------------------------------------------------
CU    SUBROUTINE KINIARG USES SUBROUTINES 
CU       F3DER
C     ------------------------------------------------------------------
CA    CALL KINIARG(ZIW,ZAVA,ZHIA5,ZSSA6,ZSSTR5,ZRMREF,ZWOG,
CA   .             ZELTAT,ZS,ZAMA,ZAUA,ZADA,ZCKA,ZOLA,ZTGA,
CA   .             ZONSET,ZATCOF)
C     ------------------------------------------------------------------
CB    SUBROUTINE KINIARG USES COMMON BLOCKS
CB       KNCHOUT
CB       KARGUM1
CB       KARGUM2
C     ------------------------------------------------------------------
      IMPLICIT  NONE
      !
      ! INPUT
      !
      INTEGER   ZIW
      REAL*8    ZAVA(5)
      REAL*8    ZHIA5(5),ZSSA6(6),ZSSTR5(5),ZRMREF,ZWOG,
     .          ZELTAT,ZS(600),ZAMA,ZAUA,ZADA,ZCKA(5),ZOLA(5),ZTGA(5,5),
     .          ZONSET(5),ZATCOF(14)
      !
      ! LOCAL
      !
      INTEGER   I,J,IDER,NCFE,NCHLST
      REAL*8    TEMP
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT

      INTEGER   IWB
      REAL*8    AVA(5),FVALA,DFA(5),DDFA(5,5),UVA(5)
      REAL*8    PHIA5(5),ESSA6(6),ESSTR5(5),NRMREF,TWOG,
     .          DELTAT,S(600),GAMA,RADA,TAUA,BCKA(5),POLA(5),STGA(5,5),
     .          AONSET(5),MATCOF(14)
      REAL*8    GAMB,TAUB,RADB,BCKB(5),POLB(5),STGB(5,5),
     .          ESSB6(6),NRMSLB,ANEW(5),
     .          FVALB,DFB(5),DDFB(5,5),UVB(5),HPB,HXB,SDB,XSATB,NRMEPL

      COMMON   /KARGUM1/ !INPUT
     .          AVA,FVALA,DFA,DDFA,UVA,
     .          PHIA5,ESSA6,ESSTR5,NRMREF,TWOG,DELTAT,S,
     .          GAMA,TAUA,RADA,BCKA,POLA,STGA,AONSET,MATCOF,
     .          !18 RESULTS
     .          GAMB,TAUB,RADB,BCKB,POLB,STGB,ESSB6,NRMSLB,ANEW,
     .          FVALB,DFB,DDFB,UVB,HPB,HXB,SDB,XSATB,NRMEPL
      COMMON   /KARGUM2/ IWB
C     ------------------------------------------------------------------
C     INPUT 1: NORMAL
C     ------------------------------------------------------------------
      DO I=1,5
         AVA(I)=ZAVA(I)
      ENDDO

C     ------------------------------------------------------------------
C     INPUT 2..5: FVALA,DFA,DDFA,UVA CORRESPONDING TO AVA
C     COMMENTS:
C        -AVA IS
C         EITHER THE NORMAL WHEN THE POINT SWITCHES FORM EL TO PL
C         OR     THE NORMAL THE LAST TIME THE POINT WAS PLASTIC
C        -FVALA,DFA,DDFA,UVA ARE NOT STORED AS STATE VARIABLES
C     ------------------------------------------------------------------
      IDER  =2
      NCFE  =S(2)
      NCHLST=OUT6
      IF (MATCOF(1).EQ.30.D0) THEN
        CALL KFacetPOT(FVALA,DFA,DDFA,AVA,IDER,NCHLST,0)
      ELSE
        CALL KF3DER(FVALA,DFA,DDFA,AVA,s(9),s(9+NCFE),5,IDER,NCHLST,0)
      END IF
      TEMP=0.0D0
      DO I=1,5
         TEMP=TEMP+DFA(I)*AVA(I)
      ENDDO
      TEMP=FVALA-TEMP
      DO I=1,5
         UVA(I)=DFA(I)+AVA(I)*TEMP
      ENDDO

C     ------------------------------------------------------------------
C     INPUT 6..20
C     ------------------------------------------------------------------
      DO I=1,5
         PHIA5(I)=ZHIA5(I)
      ENDDO
      DO I=1,6
         ESSA6(I)=ZSSA6(I)
      ENDDO
      DO I=1,5
         ESSTR5(I)= ZSSTR5(I)
      ENDDO
      NRMREF=ZRMREF
      TWOG= ZWOG
      DELTAT = ZELTAT
      DO I=1,600
         S(I)=ZS(I)
      ENDDO
      IWB=ZIW
      GAMA= ZAMA
      TAUA= ZAUA
      RADA= ZADA
      DO I=1,5
         BCKA(I)= ZCKA(I)
         POLA(I)= ZOLA(I)
         DO J=1,5
            STGA(I,J)= ZTGA(I,J)
         ENDDO
         AONSET(I)= ZONSET(I)
      ENDDO
      DO I=1,14
         MATCOF(I)= ZATCOF(I)
      ENDDO

C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KFDJAC(N,X,FVEC,NP,DF)
C     ------------------------------------------------------------------
CU    SUBROUTINE KFDJAC USES SUBROUTINES 
CU       KAFDJAC
C     ------------------------------------------------------------------
CA    CALL KFDJAC(N,X,FVEC,NP,DF)
C     ------------------------------------------------------------------
CB    SUBROUTINE KFDJAC USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
      !
      ! INPUT
      !
      INTEGER   N,NP
      REAL*8    X(N),FVEC(N)
      !
      ! OUTPUT
      !
      REAL*8    DF(NP,NP)
      !
      ! LOCAL
      !
      INTEGER   I,J,NMAX
      REAL*8    EPS,JACO(5,5)
      PARAMETER(NMAX=40,EPS=1.0D-4)
C     ------------------------------------------------------------------
      CALL KAFDJAC(JACO)
      DO I=1,5
         DO J=1,5
            DF(I,J)=JACO(I,J)
         ENDDO
      ENDDO
C     ------------------------------------------------------------------
      RETURN
      END
C  (C) Copr. 1986-92 Numerical Recipes Software D04-4-+5Z5{..
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KAFDJAC(JACO)
C     ------------------------------------------------------------------
CU    SUBROUTINE KAFDJAC USES SUBROUTINES 
CU       KHARDEN
CU       KCALMPL
C     ------------------------------------------------------------------
CA    CALL KAFDJAC(JACO)
C     ------------------------------------------------------------------
CB    SUBROUTINE KAFDJAC USES COMMON BLOCKS
CB       KNCHOUT
CB       KARGUM1
CB       KARGUM2
C     ------------------------------------------------------------------
CT    COMMENTS:
CT       - ANALYTICAL COMPUTATION OF THE JACOBIAN FOR THE NEWTON-RAPHSON
CT         MINIMIZATION OF FVEC(5)=S_RELAXATION(5)-S_HARDENING(5)
C     ------------------------------------------------------------------
      IMPLICIT  NONE
      !
      ! OUTPUT
      !
      REAL*8    JACO(5,5)
      !
      ! LOCAL
      !
      INTEGER   I,J,IW
      REAL*8    UNI(5,5)
      REAL*8    FDUM,H,HPRIM(3,3),MPL55(5,5)
      !
      ! COMMON BLOCKS
      !
      INTEGER   IWB
      REAL*8    AVA(5),FVALA,DFA(5),DDFA(5,5),UVA(5)
      REAL*8    PHIA5(5),ESSA6(6),ESSTR5(5),NRMREF,TWOG,
     .          DELTAT,S(600),GAMA,RADA,TAUA,BCKA(5),POLA(5),STGA(5,5),
     .          AONSET(5),MATCOF(14)
      REAL*8    GAMB,TAUB,RADB,BCKB(5),POLB(5),STGB(5,5),
     .          ESSB6(6),NRMSLB,ANEW(5),
     .          FVALB,DFB(5),DDFB(5,5),UVB(5),HPB,HXB,SDB,XSATB,NRMEPL

      COMMON   /KARGUM1/ !INPUT
     .          AVA,FVALA,DFA,DDFA,UVA,
     .          PHIA5,ESSA6,ESSTR5,NRMREF,TWOG,DELTAT,S,
     .          GAMA,TAUA,RADA,BCKA,POLA,STGA,AONSET,MATCOF,
     .          !18 RESULTS
     .          GAMB,TAUB,RADB,BCKB,POLB,STGB,ESSB6,NRMSLB,ANEW,
     .          FVALB,DFB,DDFB,UVB,HPB,HXB,SDB,XSATB,NRMEPL
      COMMON   /KARGUM2/ IWB
C     ------------------------------------------------------------------
C     CONSTANT 
C     ------------------------------------------------------------------
      IW=IWB
      DO I=1,5
         DO J=1,5
            UNI(I,J)=0.0D0
         ENDDO
         UNI(I,I)=1.0D0
      ENDDO

C     ------------------------------------------------------------------
C     COMPUTES H=DTAU/DGAMMA, HPRIM(I,J)=DX{IJ}/DGAMMA
C     ------------------------------------------------------------------
      CALL KHARDEN(IW,MATCOF,GAMB,ANEW,S,
     .             RADB,BCKB,POLB,STGB, FDUM,H,HPRIM)

C     ------------------------------------------------------------------
C     COMPUTES THE PLASTIC MODULUS MPL55 
C     D(S1 S2 S3 S4 S5)=MPL55*D(DELTA_EPS_PL1...DELTA_EPS_PL5) 
C     ------------------------------------------------------------------
      CALL KCALMPL(NRMEPL,ANEW,FVALB,DFB,DDFB,H,HPRIM,UVB,TAUB, MPL55) 

C     ------------------------------------------------------------------
C     JACOBIAN
C     ------------------------------------------------------------------
      DO I=1,5
         DO J=1,5
            JACO(I,J)=-TWOG*DELTAT*UNI(I,J)-DELTAT*MPL55(I,J)
         ENDDO
      ENDDO

C     ------------------------------------------------------------------
C     NORMALISATION: DF{I}/DXADIM{J}=DF{I}/DDPL{J}*DDPL{J}/DXADIM{J}
C     WITH XADIM{I}=DPL{I}/NRMREF
C     ------------------------------------------------------------------
      DO I=1,5
         DO J=1,5
            JACO(I,J)= JACO(I,J)*NRMREF
         ENDDO
      ENDDO

C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KFUNCV5(N2,DPL_ADIM,F)
C     ------------------------------------------------------------------
CU    SUBROUTINE KFUNCV5 USES SUBROUTINES 
CU       KLENGTH
CU       F3DER
CU       KASOLVE
CU       KX5_2X6
C     ------------------------------------------------------------------
CA    CALL KFUNCV5(N2,DPL_ADIM,F)
C     ------------------------------------------------------------------
CB    SUBROUTINE KFUNCV5 USES COMMON BLOCKS
CB       KNCHOUT
CB       KARGUM1
CB       KARGUM2
C     ------------------------------------------------------------------
      IMPLICIT  NONE
      !
      ! INPUT
      !
      INTEGER   N2
      REAL*8    DPL_ADIM(N2)
      !
      ! OUTPUT
      !
      REAL*8    F(N2)
      !
      ! LOCAL
      !
      INTEGER   I,J,IW
      INTEGER   IDER,NCHLST,NCFE
      REAL*8    DPL5(5),NRMDPL,DGAM,ESSR5(5),ESSH5(5)
      REAL*8    POT,TEMP
      REAL*8    SDA,SLA(5,5),NRMSLA,MODSLA(5,5),SLB(5,5)
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT

      INTEGER   IWB
      REAL*8    AVA(5),FVALA,DFA(5),DDFA(5,5),UVA(5)
      REAL*8    PHIA5(5),ESSA6(6),ESSTR5(5),NRMREF,TWOG,
     .          DELTAT,S(600),GAMA,RADA,TAUA,BCKA(5),POLA(5),STGA(5,5),
     .          AONSET(5),MATCOF(14)
      REAL*8    GAMB,TAUB,RADB,BCKB(5),POLB(5),STGB(5,5),
     .          ESSB6(6),NRMSLB,ANEW(5),
     .          FVALB,DFB(5),DDFB(5,5),UVB(5),HPB,HXB,SDB,XSATB,NRMEPL

      COMMON   /KARGUM1/ !INPUT
     .          AVA,FVALA,DFA,DDFA,UVA,
     .          PHIA5,ESSA6,ESSTR5,NRMREF,TWOG,DELTAT,S,
     .          GAMA,TAUA,RADA,BCKA,POLA,STGA,AONSET,MATCOF,
     .          !18 RESULTS
     .          GAMB,TAUB,RADB,BCKB,POLB,STGB,ESSB6,NRMSLB,ANEW,
     .          FVALB,DFB,DDFB,UVB,HPB,HXB,SDB,XSATB,NRMEPL
      COMMON   /KARGUM2/ IWB
C     ------------------------------------------------------------------
C     RESTORE THE DIMENSIONIAL DPL
C     ------------------------------------------------------------------
      IW=IWB
      IF(IW.EQ.1)WRITE(OUT6,990)
      DO I=1,5
         DPL5(I)=NRMREF*DPL_ADIM(I)
      ENDDO

C     ------------------------------------------------------------------
C     DEVIATORIC STRESS POINT FROM PLASTIC RELAXATION
C     ------------------------------------------------------------------
      DO I=1,5
         ESSR5(I)=ESSTR5(I)-TWOG*DPL5(I)*DELTAT
      ENDDO

C     ------------------------------------------------------------------
C     DEVIATORIC STRESS POINT FROM HARDENING MODEL
C     FILLS THE FOLLOWING ITEMS OF /ARGUM1/:  ITEM  MEANING
C                                             17    NRMEPL
C                                              8    ANEW
C                                              9    FVALB
C                                             10    DFB
C                                             11    DDFB
C                                             12    UVB
C                                              1    GAMB
C     ------------------------------------------------------------------
      CALL KLENGTH(DPL5,5,1, NRMDPL)
      NRMEPL=NRMDPL*DELTAT
      DO I=1,5
         ANEW(I)=DPL5(I)/NRMDPL
      ENDDO
      IDER=2
      NCFE=S(2)
      IF (MATCOF(1).EQ.30.0D0) THEN
        CALL KFacetPOT(POT,DFB,DDFB,ANEW,IDER,NCHLST,0)
      ELSE
        CALL KF3DER(POT,DFB,DDFB,ANEW,S(9),S(9+NCFE),5,IDER,NCHLST,0)
      END IF
      FVALB=POT
      TEMP=0.0D0
      DO I=1,5
         TEMP=TEMP+DFB(I)*ANEW(I)
      ENDDO
      TEMP=POT-TEMP
      DO I=1,5
         UVB(I)=DFB(I)+ANEW(I)*TEMP
      ENDDO
      DGAM=NRMDPL*FVALB*DELTAT
      GAMB=GAMA+DGAM
      IF(IW.EQ.1)WRITE(OUT6,980) (DPL_ADIM(I),I=1,5),(DPL5(I),I=1,5),
     .  (ESSR5(I),I=1,5),NRMDPL,NRMEPL,POT,(DFB(I),I=1,5),
     .  ((DDFB(I,J),J=1,5),I=1,5),(ANEW(I),I=1,5),(UVB(I),I=1,5)

C     ------------------------------------------------------------------
C     DECOMPOSITION OF THE STRENGTH OF DISLOCATION STRUCTURES
C     ------------------------------------------------------------------
      IF(MATCOF(1).EQ.3.0D0) THEN
         SDA=0.0D0
         DO I=1,5
            SDA=SDA+AONSET(I)*(STGA(I,1)*AONSET(1)
     .                        +STGA(I,2)*AONSET(2)
     .                        +STGA(I,3)*AONSET(3)
     .                        +STGA(I,4)*AONSET(4)
     .                        +STGA(I,5)*AONSET(5))
         ENDDO

         DO I=1,5
            DO J=1,5
               SLA(I,J)=STGA(I,J)-SDA*AONSET(I)*AONSET(J)
            ENDDO
         ENDDO
         CALL KLENGTH(SLA,5,5, NRMSLA)
         IF (NRMSLA.EQ.0.0D0) THEN
            DO I=1,5
               DO J=1,5
                  MODSLA(I,J)=0.0D0
               ENDDO
            ENDDO
         ELSE
            DO I=1,5
               DO J=1,5
                  MODSLA(I,J)=SLA(I,J)/NRMSLA
               ENDDO
            ENDDO
         ENDIF
      ELSE IF(MATCOF(1).EQ.4.0D0) THEN
         SDA=STGA(1,1)
         DO I=1,5
            DO J=1,5
               SLA(I,J)=0.0D0
               MODSLA(I,J)=0.0D0
            ENDDO
         ENDDO
         NRMSLA=0.0D0
      ENDIF

C     ------------------------------------------------------------------
C     INTEGRATION OF EVOLUTION EQUATIONS
C     FILLS THE FOLLOWING ITEMS OF /ARGUM1/:  ITEM  MEANING
C                                              4    POLB
C                                              3    BCKB
C                                              7    NRMSLB
C                                              2    TAUB
C                                             13    HPB
C                                             14    HXB
C                                             15    SDB
C                                             16    XSATB
C     ------------------------------------------------------------------
      CALL KASOLVE(IW,MATCOF,ANEW,UVB,DGAM,GAMB,POT,
     .             RADA,BCKA,POLA,SDA,NRMSLA,
     .             RADB,BCKB,POLB,SDB,NRMSLB,
     .             ESSH5,TAUB,HPB,HXB,XSATB)

C     ------------------------------------------------------------------
C     CONSTRUCTION OF SLB, COMPUTATION OF STGB
C     FILLS THE FOLLOWING ITEMS OF /ARGUM1/:  ITEM  MEANING
C                                              5    STGB
C     ------------------------------------------------------------------
      IF(MATCOF(1).EQ.3.0D0) THEN
         DO I=1,5
            DO J=1,5
               SLB(I,J)  = NRMSLB*MODSLA(I,J)
               STGB(I,J) = SLB(I,J)+SDB*ANEW(I)*ANEW(J) 
           ENDDO
         ENDDO
      ELSEIF(MATCOF(1).EQ.4.0D0) THEN
         DO I=1,5
            DO J=1,5
               SLB(I,J)  = NRMSLB*MODSLA(I,J)
               STGB(I,J) = 0.0D0
            ENDDO
         ENDDO
         STGB(1,1)=SDB
      ENDIF

C     ------------------------------------------------------------------
C     COST VECTOR
C     ------------------------------------------------------------------
      DO I=1,5
         F(I)=(ESSR5(I)-ESSH5(I))       ! DON'T FORGET TO DIVIDE JACO
      ENDDO                             ! BY THE SAME VALUE AS TEMP
      IF(IW.EQ.1)WRITE(OUT6,970)SDA,SDB,(ESSH5(I),I=1,5),(F(I),I=1,5)

C     ------------------------------------------------------------------
C     DEVIATORIC STRESS IF DP IS THE REAL SOLUTION
C     FILLS THE FOLLOWING ITEMS OF /ARGUM1/:  ITEM  MEANING
C                                              6    ESSB6
C     ------------------------------------------------------------------
      CALL KX5_2X6(ESSH5, ESSB6)
      IF(IW.EQ.1)WRITE(OUT6,960)

C     ------------------------------------------------------------------
990   FORMAT(12X,'[KFUNCV5] BEGIN')
980   FORMAT(12X,'[KFUNCV5] DPL_ADIM:',5E13.5,/,
     .       12X,'          DPL5    :',5E13.5,/,
     .       12X,'          ESSR5   :',5E13.5,/,
     .       12X,'          NRMDPL  :',1E13.5,/,
     .       12X,'          NRMEPL  :',1E13.5,/,
     .       12X,'          POT     :',1E13.5,/,
     .       12X,'          DFB     :',5E13.5,/,
     .       12X,'          DDFB    :',/,5(12X,5E13.5,/),/,
     .       12X,'          ANEW    :',5E13.5,/,
     .       12X,'          UVB     :',5E13.5,/)
970   FORMAT(12X,'[KFUNCV5] SDA     :',1E13.5,/,
     .       12X,'          SDB     :',1E13.5,/,
     .       12X,'          ESSH5   :',5E13.5,/,
     .       12X,'          F       :',5E13.5,/)
960   FORMAT(12X,'[KFUNCV5] EXIT',/)

C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KVMISES(SIGMA,YIELDA,EVMA,SIGVA,DCOROT,
     .                    DELTAT,P,IDENT,ISTRA,
     .                   ! 5 OUTPUT
     .                    SIGMB,YIELDB,EVMB,SIGVB,C)
C     ------------------------------------------------------------------
CU    SUBROUTINE KVMISES USES SUBROUTINES 
CU       KMODELA
CU       KLENGTH
CU       XIT
C     ------------------------------------------------------------------
CA    CALL KVMISES(SIGMA,YIELDA,EVMA,SIGVA,DCOROT,
CA   .             DELTAT,P,IDENT,ISTRA,
CA                 ! 5 OUTPUT
CA   .             SIGMB,YIELDB,EVMB,SIGVB,C)
C     ------------------------------------------------------------------
CB    SUBROUTINE KVMISES USES COMMON BLOCKS
CB       KNCHOUT
CB       K_HIST1
CB       K_HIST2
C     ------------------------------------------------------------------
CT    COMMENTS FOR SUBROUTINE KVMISES 
CT       -ARGUMENTS : SEE ANI3VH FOR DETAILS
CT       -TOLERANCE USED FOR NEWTON-RAPHSON AND BISSECTION:
CT        THE COST FUNCTION SHOULD BE COMPARED TO TOL*SIGVA OTHERWISE,
CT        *WHEN WORKING IN PASCAL RATHER THAN MPA, IT IS NOT THE SAME
CT         REQUIREMENTS
CT        *WHEN THE PREDICTOR IS VERY FAR, THE ORDER OF MAGNITUDE OF THE
CT         STRESSES CAN BE 1E8 MPA AND CANNOT BE COMPARED TO 1E-6
C     ------------------------------------------------------------------
      IMPLICIT  NONE
      !
      ! INPUT
      !
      INTEGER   IDENT,ISTRA(*)
      REAL*8    SIGMA(6),YIELDA,EVMA,SIGVA,DCOROT(6),DELTAT,P(*)
      !
      ! OUTPUT
      !
      REAL*8    SIGMB(6),YIELDB,EVMB,SIGVB,C(9,9)
      !
      ! LOCAL
      !
      INTEGER   IELEM,I,J,K,L,M,N,ICOMPL,ITE,IW,IW_BIS
C               MATERIAL STATE
      REAL*8    SIGA(3,3),P0, SIGB(3,3),PE
C               MATERIAL LAW (DIRECT AND DERIVED INPUT)
      REAL*8    EMOD,ANU,COFK,COFN,EPS0,
     .          KMOD,GMOD,TWOG,COFK_COFN,COFN1,EPS0_EVMA
C               DEFORMATION
      REAL*8    DEC(3,3),NRMDEC,TR_DEC,DEV_DEC(3,3)
C               RADIAL RETURN
      REAL*8    SE(3,3),S0(3,3),S1(3,3),MU,NOR(3,3),NOR6(6),
     .          D_EPL_L,D_EPL_R,D_EPL,
     .          SIGV,TMP,FVAL,DFVAL,TOL
C               COMPLIANCE
      REAL*8    SLOPH,SLOPH_TIL,BETA,GMOD_TIL,GTIL,
C               CONSTANTS
     .          ROOT23,ROOT32
      PARAMETER(TOL=1.0D-6)
      LOGICAL   SHINFO,ELAPLA,LINEAR,NEWTON
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT

      INTEGER ELETAY
      COMMON /K_HIST1/ ELETAY
      REAL*8  HISTORY(6)
      COMMON /K_HIST2/ HISTORY

C     ------------------------------------------------------------------
      IW=0
      IF(IDENT.EQ.1245)IW=1
      IF(IW.EQ.1)WRITE(OUT6,9990)

      SHINFO=.FALSE.
      IF(ISTRA(11).EQ.1.AND.ISTRA(12).EQ.1) THEN
         IF(ISTRA(13).EQ.1) SHINFO=.TRUE.
      ENDIF
      
      ELAPLA=.FALSE.
      IF(P(7).EQ.-99.0D0) ELAPLA=.TRUE.

      LINEAR=.FALSE.
      IF(P(8).EQ.-99.0D0) LINEAR=.TRUE.

      NEWTON=.TRUE.
      
      IF(SHINFO) WRITE(OUT6,9980) ELAPLA,LINEAR,NEWTON

C     ------------------------------------------------------------------
C     TRANSLATES THE INPUT
C     ------------------------------------------------------------------
      IELEM     = ISTRA(12)
      ICOMPL    = ABS(MOD(ISTRA(5),10)/1)     ! DEFINITION FROM LOAX3D.F 
      EMOD      = P(1)
      ANU       = P(2)
      COFK      = P(4)
      COFN      = P(5)
      EPS0      = P(6)
      COFK_COFN = COFK*COFN
      EPS0_EVMA = EPS0+EVMA
      COFN1     = COFN-1.0D0
      KMOD      = EMOD/(3*(1.0D0-2.0D0*ANU))   ! COMPRESSIBILITY MODULUS 
      GMOD      = EMOD/(2.0D0*(1.0D0+ANU))     ! SHEAR MODULUS
      TWOG      = 2.0D0*GMOD
      ROOT23    = DSQRT(2.0D0/3.0D0)
      ROOT32    = DSQRT(3.0D0/2.0D0)

      DEC(1,1)  = DCOROT(1)*DELTAT
      DEC(2,2)  = DCOROT(2)*DELTAT
      DEC(3,3)  = DCOROT(3)*DELTAT
      DEC(1,2)  = DCOROT(4)*DELTAT
      DEC(1,3)  = DCOROT(5)*DELTAT
      DEC(2,3)  = DCOROT(6)*DELTAT
      DEC(2,1)  = DEC(1,2)
      DEC(3,1)  = DEC(1,3)
      DEC(3,2)  = DEC(2,3)
      CALL KLENGTH(DEC,3,3, NRMDEC)
      TR_DEC    = DEC(1,1)+DEC(2,2)+DEC(3,3)

      SIGA(1,1) = SIGMA(1)
      SIGA(2,2) = SIGMA(2)
      SIGA(3,3) = SIGMA(3)
      SIGA(1,2) = SIGMA(4)
      SIGA(1,3) = SIGMA(5)
      SIGA(2,3) = SIGMA(6)
      SIGA(2,1) = SIGA(1,2)
      SIGA(3,1) = SIGA(1,3)
      SIGA(3,2) = SIGA(2,3)
      P0        = (SIGA(1,1)+SIGA(2,2)+SIGA(3,3))/3.0D0

C     ------------------------------------------------------------------
C     COMPUTE DEVIATOR OF STRAIN INC AND DEVIATOR OF INITIAL STRESSES
C     ------------------------------------------------------------------
      DO I=1,3
         DO J=1,3
            DEV_DEC(I,J) = DEC(I,J)
            S0(I,J)      = SIGA(I,J)
         ENDDO
         DEV_DEC(I,I) = DEV_DEC(I,I)-TR_DEC/3.0D0
         S0(I,I)      = S0(I,I)-P0
      ENDDO
      IF(IW.EQ.1) THEN
         WRITE(OUT6,8990) YIELDA,EVMA,SIGVA
         WRITE(OUT6,8980) ((SIGA(I,J),J=1,3),I=1,3)
         TMP = DSQRT(    S0(1,1)**2.0D0+S0(2,2)**2.0D0+S0(3,3)**2.0D0
     .       +      2.0D0*S0(1,2)**2.0D0
     .       +      2.0D0*S0(1,3)**2.0D0
     .       +      2.0D0*S0(2,3)**2.0D0)
         TMP = ROOT32*TMP
         WRITE(OUT6,8970) P0,((S0(I,J),J=1,3),I=1,3),TMP
         WRITE(OUT6,8960) EMOD,ANU,KMOD,GMOD,COFK,COFN,EPS0
         WRITE(OUT6,8950) ((DEC(I,J),J=1,3),I=1,3),NRMDEC,TR_DEC
      ENDIF

C     ------------------------------------------------------------------
C     UPDATE ANALYTICALLY THE PRESSURE
C     ------------------------------------------------------------------
      PE = P0+KMOD*TR_DEC

C     ------------------------------------------------------------------
C     COMPUTE ELASTIC DEVIATORIC STRESSES
C     ------------------------------------------------------------------
      DO I=1,3
         DO J=1,3
            SE(I,J) = S0(I,J)+TWOG*DEV_DEC(I,J)
         ENDDO
      ENDDO
      IF(IW.EQ.1)WRITE(OUT6,7990) PE,((SE(I,J),J=1,3),I=1,3)

C     ------------------------------------------------------------------
C     ELASTIC OR PLASTIC ?
C     ------------------------------------------------------------------
      CALL KLENGTH(SE,3,3, MU)

      IF(ROOT32*MU.LE.SIGVA) THEN
CC    IF(ROOT32*MU.LT.SIGVA) THEN
         YIELDB = 0.0D0
         DO I=1,3
            DO J=1,3
               SIGB(I,J) = SE(I,J)
            ENDDO
            SIGB(I,I) = SIGB(I,I)+PE
         ENDDO
         SIGMB(1) = SIGB(1,1)
         SIGMB(2) = SIGB(2,2)
         SIGMB(3) = SIGB(3,3)
         SIGMB(4) = SIGB(1,2)
         SIGMB(5) = SIGB(1,3)
         SIGMB(6) = SIGB(2,3)
         SIGVB    = SIGVA
         EVMB     = EVMA ! STATUS QUO FOR THE UNIQUE STATE VARIABLE
         CALL KMODELA(GMOD,KMOD, C)

C        NEW BLOCK STARTS

CC       IF(EVMA.NE.0.0D0.AND.ROOT32*MU.GT.0.99D0*SIGVA) THEN
CC          WRITE(OUT6,*) IELEM,'EL WITH PL MODULUS',ROOT32*MU/SIGVA
CC          DO I=1,3
CC             DO J=1,3
CC                NOR(I,J) = SE(I,J)/MU
CC             ENDDO
CC          ENDDO
CC          BETA     = 1.0D0
CC          GMOD_TIL = BETA*GMOD
CC          SLOPH    = COFK_COFN*(EPS0+EVMB)**COFN1
CC          SLOPH_TIL= SLOPH/(1.0D0+(BETA-1.0D0)*SLOPH/(3.0D0*GMOD_TIL))
CC          GTIL     = 1.0D0/(1.0D0+SLOPH_TIL/(3.0D0*GMOD_TIL))
CC          CALL KMODELA(GMOD_TIL,KMOD,C)
CC          M=0
CC          DO I=1,3
CC             DO J=1,3
CC                M=M+1
CC                N=0
CC                DO K=1,3
CC                   DO L=1,3
CC                      N=N+1
CC                      C(M,N) = C(M,N)
CC   .                         - 2.0D0*GMOD_TIL*GTIL*NOR(I,J)*NOR(K,L)
CC                   ENDDO
CC                ENDDO
CC             ENDDO
CC          ENDDO
CC       ENDIF
CC    IF(IELEM.EQ.810) WRITE(OUT6,*) '810,YIELDA=',YIELDA,
CC   .   ' YIELDB=',YIELDB
C        NEW BLOCK ENDS

         IF(IW.EQ.1) THEN
            WRITE(OUT6,6990) YIELDB,EVMB,SIGVB
            WRITE(OUT6,6980) (SIGMB(I),I=1,6)
            WRITE(OUT6,6970) ((C(I,J),J=1,9),I=1,9)
         ENDIF
         RETURN
      ENDIF

C     ------------------------------------------------------------------
C     PREPARE THE EQUATION FOR THE RADIAL RETURN
C     ------------------------------------------------------------------
      YIELDB = 1.0D0
      DO I=1,3
         DO J=1,3
            NOR(I,J) = SE(I,J)/MU
         ENDDO
      ENDDO

C     ------------------------------------------------------------------
C     PIECEWIZE LINEARISATION
C     ------------------------------------------------------------------
      IF(LINEAR.AND.EVMA.GT.0.02D0) THEN
         COFK      = COFK*COFN*(EPS0+EVMA)**COFN1
         EPS0      = SIGVA/COFK-EVMA
         EPS0_EVMA = EPS0+EVMA
         COFN      = 1.0D0
         COFN1     = 0.0D0
         COFK_COFN = COFK
         IF(IW.EQ.1)WRITE(OUT6,5990) COFK,EPS0
      ENDIF

C     ------------------------------------------------------------------
C     NEWTON-RAPHSON
C     ------------------------------------------------------------------
      IF(NEWTON) THEN
         ITE   = 0
         TMP   = ROOT23*COFK_COFN*(EPS0_EVMA)**COFN1
         D_EPL = ROOT32*MU/(TWOG+TMP)
20       CONTINUE
         ITE   = ITE+1
         IF(ITE.GT.10) THEN
            WRITE(OUT6,5985)
            GOTO 30
         ENDIF
         SIGV  = COFK*(EPS0_EVMA+ROOT23*D_EPL)**COFN
         TMP   = ROOT32*(MU-TWOG*D_EPL)
         FVAL  = TMP - SIGV
         IF(DABS(FVAL).LE.TOL*SIGVA) GOTO 200
         IF(D_EPL.GE.0.0D0) THEN
            TMP = 1.0D0
         ELSE
            TMP =-1.0D0
         ENDIF
         DFVAL = 
     .         - ROOT32*TWOG*TMP
     .         - COFK_COFN*ROOT23*TMP*(EPS0_EVMA+ROOT23*D_EPL)**COFN1
         D_EPL = D_EPL - FVAL/DFVAL
         D_EPL = DABS(D_EPL)
         GOTO 20
      ENDIF

C     ------------------------------------------------------------------
C     BISSECTION
C     INTERSECTION BETWEEN PARABOLA AND EXPONENTIAL CURVE, 2 ROOTS EXIT.
C     BISSECTION IS VERY STRAIGTHFORWARD METHOD.
C
C     EQUATION: 3/2*(MU-2*G*D_EPL)**2=(K*(EPS0+EVMA+SQRT(2/3)*D_EPL))**2
C
C     MINIMUM PL. COR. : 0
C     MAXIMUM PL. COR. : IF RIGID PLASTIC, SIGV REMAINS SIGVA,
C                        -> D_EPL=-(SQRT(2/3)*SIGVA-MU)/(2*G)
C     ------------------------------------------------------------------
30    CONTINUE

      IF(COFN.EQ.1.0D0) THEN
         D_EPL= (ROOT32*MU-COFK*(EPS0+EVMA))/(COFK*ROOT23+2*ROOT32*GMOD)
         SIGV = COFK*(EPS0_EVMA+ROOT23*D_EPL)**COFN
         TMP  = ROOT32*(MU-TWOG*D_EPL)
         FVAL = TMP - SIGV
         ITE  = 0
         GOTO 200
      ENDIF

      IW_BIS  = 0
110   CONTINUE
      ITE     = 0
      D_EPL_L = 0.0D0
      D_EPL_R = (MU-ROOT23*SIGVA)/TWOG
      IF(IW_BIS.EQ.1) THEN
         WRITE(OUT6,5980) ((NOR(I,J),J=1,3),I=1,3)
         WRITE(OUT6,5970) MU,SIGVA
         WRITE(OUT6,5960) COFK,COFN,EPS0,EPS0_EVMA
      ENDIF

      IF(IW.EQ.1)WRITE(OUT6,5002) ((NOR(I,J),J=1,3),I=1,3)

150   CONTINUE
      ITE   = ITE+1
      D_EPL = (D_EPL_L+D_EPL_R)/2.0D0
      SIGV  = COFK*(EPS0_EVMA+ROOT23*D_EPL)**COFN
      TMP   = ROOT32*(MU-TWOG*D_EPL)
      FVAL  = TMP - SIGV
      IF(IW.EQ.1)WRITE(OUT6,5950) ITE,D_EPL,SIGV,TMP,FVAL
      IF(IW_BIS.EQ.1) THEN
         WRITE(OUT6,5940) D_EPL_L,D_EPL_R,D_EPL
         WRITE(OUT6,5930) SIGV,TMP,FVAL
      ENDIF

      IF((FVAL.GE.0.0D0).AND.(FVAL.LE.TOL*SIGVA)) GOTO 200

      IF(ITE.GT.250) THEN
         IF(IW_BIS.EQ.1) THEN
            ISTRA(20)=1
            WRITE(OUT6,5920)
            RETURN
         ENDIF    
         IW_BIS=1
         GOTO 110
      ENDIF

      IF(FVAL.GT.0.0D0) THEN
         D_EPL_L=D_EPL
      ELSE
         D_EPL_R=D_EPL
      ENDIF
      GOTO 150

200   CONTINUE
      IF(IW.EQ.1)WRITE(OUT6,5910) ITE,FVAL,D_EPL

C     ------------------------------------------------------------------
C     STORE THE PLASTIC HISTORY FOR TAYLOR SIMULATION
C     ------------------------------------------------------------------
      IF(ELETAY.GT.0.AND.IELEM.EQ.ELETAY) THEN
         CALL KX332X6(NOR, NOR6)
         DO I=1,6
            HISTORY(I)=D_EPL*NOR6(I)
         ENDDO
      ENDIF

C     ------------------------------------------------------------------
C     PROJECTED STRESSES
C     ------------------------------------------------------------------
      DO I=1,3
         DO J=1,3
           S1(I,J)   = SE(I,J)-TWOG*D_EPL*NOR(I,J)
           SIGB(I,J) = S1(I,J)
         ENDDO
         SIGB(I,I) = SIGB(I,I)+PE
      ENDDO
      EVMB=EVMA+ROOT23*D_EPL

C     ------------------------------------------------------------------
C     COMPUTES THE TANGENT MODULUS CONSISTENT WITH THE RADIAL RETURN
C     ------------------------------------------------------------------
      IF(ICOMPL.NE.0) THEN
         CALL KLENGTH(S1,3,3, BETA)
         BETA=BETA/MU
         IF(BETA.GT.1.0D0) THEN
            WRITE(OUT6,'(A)') '[VMISES] BETA CANNOT BE > 1.0D0'
            CALL XIT
            STOP
         ENDIF
         GMOD_TIL = BETA*GMOD
         SLOPH    = COFK_COFN*(EPS0+EVMB)**COFN1
         SLOPH_TIL= SLOPH/(1.0D0+(BETA-1.0D0)*SLOPH/(3.0D0*GMOD_TIL))
         GTIL     = 1.0D0/(1.0D0+SLOPH_TIL/(3.0D0*GMOD_TIL))

         IF(ELAPLA.AND.NRMDEC.LT.1.0D-5) THEN
            GTIL     = 0.0D0    ! PLASTIC UPDATE WITH ELA MODULUS
            GMOD_TIL = GMOD
            WRITE(OUT6,4990) IELEM,NRMDEC
         ENDIF

C              KL ... 
C            IJ
C            .
C          N .   1  2   3   4   5   6   7   8   9
C        M   .  11 12  13  21  22  23  31  32  33
C        1   11
C        2   12
C        3   13
C        4   21
C        5   22
C        6   23
C        7   31
C        8   32
C        9   33

C        ---------------------------------------------------------------
C        PREPARES THE APPARENT HOOK MATRIX
C        ---------------------------------------------------------------
         CALL KMODELA(GMOD_TIL,KMOD,C)

C        ---------------------------------------------------------------
C        IF GTIL.NE.0 -> PLASTIC CORRECTION
C        ---------------------------------------------------------------
         M=0
         DO I=1,3
            DO J=1,3
               M=M+1
               N=0
               DO K=1,3
                  DO L=1,3
                     N=N+1
                     C(M,N)=C(M,N)-2.0D0*GMOD_TIL*GTIL*NOR(I,J)*NOR(K,L)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDIF
 
      SIGMB(1) = SIGB(1,1)
      SIGMB(2) = SIGB(2,2)
      SIGMB(3) = SIGB(3,3)
      SIGMB(4) = SIGB(1,2)
      SIGMB(5) = SIGB(1,3)
      SIGMB(6) = SIGB(2,3)
      SIGVB    = SIGV

      IF(IW.EQ.1) THEN
         WRITE(OUT6,3990) YIELDB,EVMB,SIGVB
         WRITE(OUT6,3980) (SIGMB(I),I=1,6)
         WRITE(OUT6,3970) ((C(I,J),J=1,9),I=1,9)
         WRITE(OUT6,3960) BETA
         WRITE(OUT6,2990) IELEM
      ENDIF

C     ------------------------------------------------------------------
C     FORMAT 9XXX : INTRODUCTION

9990  FORMAT(/,6X,'[VMISES] BEGIN')
9980  FORMAT(  9X,'ELA. MOD. FOR SMALL PL. INC.',L5,/,
     .         9X,'PIECEWIZE LINEARIZATION     ',L5,/,
     .         9X,'NEWTON RAPHSON              ',L5)

C     FORMAT 8XXX : INITIAL STATE

8990  FORMAT(  9X,'PLASTIC INDICATOR....A:', E13.5,/,
     .         9X,'EQUIVALENT STRAIN....A:', E13.5,/,
     .         9X,'YIELD LIMIT..........A:', E13.5)
8980  FORMAT(  9X,'CAUCHY STRESSES......A:',/,3(9X,3E13.5,/))
8970  FORMAT(  9X,'HYDROSTATIC PRESSURE.A:', E13.5,/,
     .         9X,'DEVIATORIC STRESSES..A:',/,3(9X,3E13.5,/),
     .         9X,'VON MISES NORM.......A:', E13.5,/)
8960  FORMAT(  9X,'YOUNG MODULUS         :', E13.5,/,
     .         9X,'POISSON RATIO         :', E13.5,/,
     .         9X,'COMPRESSIBILITY       :', E13.5,/,
     .         9X,'SHEAR MODULUS         :', E13.5,/,
     .         9X,'COFK                  :', E13.5,/,
     .         9X,'COFN                  :', E13.5,/,
     .         9X,'EPS0                  :', E13.5)
8950  FORMAT(  9X,'COROT. STRAIN INC.    :',/,3(9X,3E13.5,/),/,
     .         9X,'NORM                  :', E13.5,/,
     .         9X,'TRACE                 :', E13.5) 

C     FORMAT 7XXX : ELASTIC PREDICTOR

7990  FORMAT(  9X,'UPDATED PRESSURE      :', E13.5,/,
     .         9X,'ELASTIC DEV. STRESSES :',/,3(9X,3E13.5,/))

C     FORMAT 6XXX : ELASTIC UPDATE

6990  FORMAT(  9X,'PLASTIC INDICATOR....B:', E13.5,/,
     .         9X,'EQUIVALENT STRAIN....B:', E13.5,/,
     .         9X,'YIELD LIMIT .........B:', E13.5)
6980  FORMAT(  9X,'CAUCHY STRESSES......B:',6E13.5)
6970  FORMAT(  9X,'COMPLIANCE            :',/,9(9X,9E13.5,/))

C     FORMAT 5XXX : RADIAL RETURN

5990  FORMAT(  9X,'LINEARIZATION OF EXPONENTIAL LAW',/,
     .         9X,'COFK                  :', E13.5,/,
     .         9X,'COFN                  :', E13.5,/,
     .         9X,'EPS0                  :', E13.5)
5985  FORMAT(  9X,'NEWTON FAILED -> USES BISSECTION')
5980  FORMAT(  9X,'NORMAL RADIAL RETURN  :',/,3(9X,3E13.5,/))
5970  FORMAT(  9X,'LENGTH OF PREDICTOR   :', E13.5,/,
     .         9X,'INITIAL YIELD LIMIT   :', E13.5)
5960  FORMAT(  9X,'COFK                  :', E13.5,/,
     .         9X,'COFN                  :', E13.5,/,
     .         9X,'EPS0                  :', E13.5,/,
     .         9X,'EPS0_EVMA             :', E13.5)
5950  FORMAT(  9X,'ITERATION             :', I5   ,/,
     .         9X,'CURRENT D_EPL         :', E13.5,/,
     .         9X,'YIELD LIMIT           :', E13.5,/,
     .         9X,'RELAXED ELASTIC       :', E13.5,/,
     .         9X,'RESIDU                :', E13.5)
5940  FORMAT(  9X,'D_EPL_L,D_EPL_R,D_EPL :',3E13.5)
5930  FORMAT(  9X,'-> SIGV,TMP,FVAL      :',3E13.5)
5920  FORMAT(  9X,'BISSECTION FAILED -> REDUCE DELTAT')
5910  FORMAT(  9X,'NUMBER OF ITERATIONS  :',    I5,/,
     .         9X,'OBTAINED ACCURACY     :', E13.5,/,
     .         9X,'D_EPL                 :', E13.5)
5002  FORMAT(  9X,'NORMAL FOR RADIAL RET.:',/,3(9X,3E13.5,/))

C     FORMAT 4XXX : ELASTIC MODULUS FOR SMALL PLASTIC UPDATE

4990  FORMAT(  9X,'ELASTIC MODULUS FOR   :',I5,E13.5)

C     FORMAT 3XXX : ELASTO-PLASTIC UPDATE

3990  FORMAT(  9X,'PLASTIC INDICATOR....B:', E13.5,/,
     .         9X,'EQUIVALENT STRAIN....B:', E13.5,/,
     .         9X,'YIELD LIMIT..........B:', E13.5)
3980  FORMAT(  9X,'CAUCHY STRESSES......B:',6E13.5)
3970  FORMAT(  9X,'COMPLIANCE            :',/,9(9X,9E13.5,/))
3960  FORMAT(  9X,'BETA                  :', E13.5)

C     FORMAT 2XXX : EXIT

2990  FORMAT(  9X,'ELEMENT               :', I5,/,
     .         6X,'END OF VMISES',/)
C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  ZXIT
C     ------------------------------------------------------------------
CU    SUBROUTINE XIT USES SUBROUTINES 
CU       NONE 
C     ------------------------------------------------------------------
CA    CALL XIT
C     ------------------------------------------------------------------
CB    SUBROUTINE XIT USES COMMON BLOCKS
CB       NONE
C     ------------------------------------------------------------------
CT    COMMENTS FOR SUBROUTINE XIT
CT       -THIS ROUTINES REPLACES THE EXIT ROUTINE OF ABAQUS WHEN ABAQUS
CT        IS NOT USED
CT       -TO USE THIS ROUTINE, RENAME IT 'SUBROUTINE XIT'
CT       -THE 'IF(1.EQ.1)' AVOID A FASTIDIOUS WARNING WHILE COMPILING
C     ------------------------------------------------------------------
      IF(1.EQ.1)STOP
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KINISTA
C     ------------------------------------------------------------------
CU    SUBROUTINE KINISTA USES SUBROUTINES 
CU       NONE 
C     ------------------------------------------------------------------
CA    CALL KINISTA
C     ------------------------------------------------------------------
CB    SUBROUTINE KINISTA USES COMMON BLOCKS
CB       KSTATIS
C     ------------------------------------------------------------------
CV    SOME VARIABLES FOR SUBROUTINE KINISTA
CV       KTEXH  NUMBER OF CALL TO THE INTEGRATION ROUTINE KTEXHAR
CV       KDTEC  NUMBER OF TIMES THAT E/P DETECTION USED KDETECT
CV
CV       --> KDTEC=KINTE
CV
CV       STATISTICS ONLY RELATED TO SUBROUTINE KDETECT:
CV
CV       KTRIV  > EXIT1 OF KDETECT <
CV              NUMBER OF E/P DETECTION DIRECTLY BASED ON THE FACT
CV              THAT THE TRIAL STRESS IS VERY SMALL
CV       KPHIA1 NUMBER OF TIMES THAT THE ANALYSIS OF THE QUALITY
CV              OF PHIA5 WAS TRIVIAL
CV       KPHIA2 NUMBER OF TIMES THAT THE ANALYSIS OF THE QUALITY
CV              OF PHIA5 REQUIRED KYLPMUL
CV       KOFFS  > EXIT2 OF KDETECT <
CV              NUMBER OF TIMES THAT THE E/P DETECTION CONCLUDED
CV              BY MEANS OF A CALL TO KYLPMUL WITH OFFSET
CV       KPROD  > EXIT3 OF KDETECT <
CV              NUMBER OF TIMES THAT THE E/P DETECTION CONCLUDED
CV              BY MEANS OF (YIELDA+1 AND PROD>=0)
CV       KTRIA1 NUMBER OF CALL TO YLP(NO OFFSET,PHITR5)
CV       KTRIA2 > EXIT4 OF KDETECT <
CV              NUMBER OF TIMES THAT THE E/P DETECTION CONCLUDED
CV              THAT IT IS ELASTIC ON THE BASIS OF ELATR5
CV       KBISS  > EXIT 5 OR 6 OF KDETECT <
CV              NUMBER OF TIMES THAT THE E/P DETECTION CONCLUDED
CV              BY MEANS OF THE BISSEC(PHIA5,PHITR5) OR 
CV              (PROJECTED PHIA5,PHITR5)
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      !
      ! COMMON BLOCKS
      !
      INTEGER   LSTSTP,ITETOT,KELAS,KPLAS,KBIG,KTEXH,KDTEC,
     .          KTRIV,KPHIA1,KPHIA2,KOFFS,KPROD,KTRIA1,KTRIA2,KBISS,
     .          KYLPER,KYLP0,KYLP0A,KYLP0B,KYLP0C,
     .          KYLP1,KYLP1A,KYLP1B,KYLP1C
      COMMON   /KSTATIS/ LSTSTP,ITETOT,KELAS,KPLAS,KBIG,KTEXH,KDTEC,
     .          KTRIV,KPHIA1,KPHIA2,KOFFS,KPROD,KTRIA1,KTRIA2,KBISS,
     .          KYLPER,KYLP0,KYLP0A,KYLP0B,KYLP0C,
     .          KYLP1,KYLP1A,KYLP1B,KYLP1C
C     ------------------------------------------------------------------
      KELAS=0
      KPLAS=0
      KBIG =0
      KTEXH=0
      KDTEC=0

C     ------------------------------------------------------------------
C     STATISTICS ONLY FOR SUBROUTINE KDETECT
C     ------------------------------------------------------------------
      KTRIV =0
      KPHIA1=0
      KPHIA2=0
      KOFFS =0
      KPROD =0
      KTRIA1=0
      KTRIA2=0
      KBISS =0
      KYLPER=0

C     ------------------------------------------------------------------
C     STATISTICS ONLY FOR SUBROUTINE KYLPMUL
C     ------------------------------------------------------------------
      KYLP0 =0
      KYLP0A=0
      KYLP0B=0
      KYLP0C=0

      KYLP1 =0
      KYLP1A=0
      KYLP1B=0
      KYLP1C=0
C     ------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KPRTSTA(NCHOUT,ISTEP)
C     ------------------------------------------------------------------
CU    SUBROUTINE KPRISTA USES SUBROUTINES 
CU       NONE 
C     ------------------------------------------------------------------
CA    CALL KPRISTA(NCHOUT)
C     ------------------------------------------------------------------
CB    SUBROUTINE KINISTA USES COMMON BLOCKS
CB       KSTATIS
C     ------------------------------------------------------------------
      IMPLICIT  NONE 
      !
      ! INPUT
      !
      INTEGER   NCHOUT,ISTEP
      !
      ! LOCAL
      !
      !
      ! COMMON BLOCKS
      !
      INTEGER   LSTSTP,ITETOT,KELAS,KPLAS,KBIG,KTEXH,KDTEC,
     .          KTRIV,KPHIA1,KPHIA2,KOFFS,KPROD,KTRIA1,KTRIA2,KBISS,
     .          KYLPER,KYLP0,KYLP0A,KYLP0B,KYLP0C,
     .          KYLP1,KYLP1A,KYLP1B,KYLP1C
      COMMON   /KSTATIS/ LSTSTP,ITETOT,KELAS,KPLAS,KBIG,KTEXH,KDTEC,
     .          KTRIV,KPHIA1,KPHIA2,KOFFS,KPROD,KTRIA1,KTRIA2,KBISS,
     .          KYLPER,KYLP0,KYLP0A,KYLP0B,KYLP0C,
     .          KYLP1,KYLP1A,KYLP1B,KYLP1C
C     ------------------------------------------------------------------
      LSTSTP=ISTEP
      WRITE(NCHOUT,980) ISTEP,KELAS,KPLAS,KBIG,KTEXH
      WRITE(NCHOUT,960) KDTEC,KTRIV,KPHIA1,KPHIA2,KOFFS,
     .                  KPROD,KTRIA1,KTRIA2,KBISS,KYLPER
      WRITE(NCHOUT,940) KYLP0,KYLP0A,KYLP0B,KYLP0C,
     .                  KYLP1,KYLP1A,KYLP1B,KYLP1C

C     ------------------------------------------------------------------
C     FORMATS
C     ------------------------------------------------------------------
980   FORMAT(/,4X,'[KPRTSTA] STATISTICS FOR STEP: ',I5               ,/,
C    .       14X,'TOTAL NUMBER OF FE ITERATIONS                : ',I5,/,
     .       14X,'NUMBER OF ELASTIC ELEMENTS                   : ',I5,/,
     .       14X,'NUMBER OF PLASTIC ELEMENTS                   : ',I5,/,
     .       14X,'NUMBER OF ELEMENTS WITH LARGE STRAIN INCR    : ',I5,/,
     .       14X,'KTEXH CALL TO KTEXHAR                        : ',I5,/)
960   FORMAT(14X,'KDTEC CALL TO KDETECT                        : ',I5,/,
     .       14X,'   KTRIV  ANSWERS WITH (PHITR5 VERY SMALL)   : ',I5,/,
     .       14X,'   KPHIA1 TRIVIAL ANALYSIS OF PHIA           : ',I5,/,
     .       14X,'   KPHIA2 ANALYSIS OF PHIA WITH KYLPMUL      : ',I5,/,
     .       14X,'   KOFFS  ANSWERS WITH YLP(PHIA,A2TR5)       : ',I5,/,
     .       14X,'   KPROD  ANSWERS WITH (YIELDA=1,PROD>=0)    : ',I5,/,
     .       14X,'   KTRIA1 CALL YLP(SOV=0,PHITR5)             : ',I5,/,
     .       14X,'   KTRIA2 ANSWERS WITH (ELATR5>1->ELASTIC)   : ',I5,/,
     .       14X,'   KBISS  ANSWERS WITH ({PR}PHIA,PHITR5)     : ',I5,/,
     .       14X,'   KYLPER ANSWERS WITH KYLPERROR             : ',I5,/)
940   FORMAT(14X,'KYLP0 CALL TO KYLPMUL WITHOUT OFFSET         : ',I5,/,
     .       14X,'   KYLP0A ANSWER WITH OPTIM. INITIALISATION  : ',I5,/,
     .       14X,'   KYLP0B ANSWER WITH VMISES INITIALISATION  : ',I5,/,
     .       14X,'   KYLP0C ANSWER WITH YLPERROR               : ',I5,/,
     .       14X,'KYLP1 CALL TO KYLPMUL WITH OFFSET            : ',I5,/,
     .       14X,'   KYLP1A ANSWER WITH OPTIM. INITIALISATION  : ',I5,/,
     .       14X,'   KYLP1B ANSWER WITH VMISES INITIALISATION  : ',I5,/,
     .       14X,'   KYLP1C ANSWER WITH BISSECTION             : ',I5,/)
C-----------------------------------------------------------------------
      RETURN
      END
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KDMATIN (A,N,NDIM,LIG,PREC,IOP)                                 
C                                                                       
C-----------------------------------------------------------------------
C0  DMATINF   DMATIN                                                    
C1  BUT       INVERSION D'UNE MATRICE NON-SYMETRIQUE (REAL*8)           
C1            PAR LA METHODE DE GAUSS-JORDAN AVEC CHOIX DU PIVOT MAXIMUM
C1                                                                      
C2  APPEL     CALL KDMATIN (A,N,NDIM,LIG,PREC,IOP)                       
C2                                                                      
C3  ARGUMENTS A(NDIM,N)           = MATRICE A INVERSER (REAL*8).        
C3                                  LA MATRICE ORIGINALE EST DETRUITE ET
C3                                  REMPLACEE PAR SON INVERSE           
C3            N                   = DIMENSION DE LA MATRICE A INVERSER  
C3            NDIM                = NOMBRE DE LIGNES APPARAISSANT       
C3                                  DANS L'ORDRE "DIMENSION"            
C3                                  DU PROGRAMME APPELANT               
C3            LIG(N)              = VECTEUR DE TRAVAIL (INTEGER*4)      
C3            PREC                = SI UN PIVOT EST PLUS PETIT QUE      
C3                                  PREC, LE SOUS-PROGRAMME IMPRIME     
C3                                  UN MESSAGE PUIS "RETURN'            
C3            IOP                 = OPTION D'IMPRESSION DU PIVOT        
C3                                  0  PAS D'IMPRESSION                 
C3                                  1  IMPRESSION                       
C3                                                                      
C6  UTILISE   DABS.                                                     
C6                                                                      
C8  REMARQUE  PAR UN CHOIX JUDICIEUX DE N ET NDIM, ON PEUT              
C8            INVERSER UNE PARTIE DE LA MATRICE.                        
C8            EXAMPLE: CALL KDMATIN (A(2,3),2,4,LIG,PREC,IOP)            
C8                     0  0  0  0                                       
C8                     0  0  *  *                                       
C8                     0  0  *  *                                       
C8                     0  0  0  0                                       
CLDMATIN INVERSION D'UNE MATRICE REAL*8                                 
C-----------------------------------------------------------------------
C                                                                       
C     IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
C     DIMENSION A(*),LIG(*)
C
      IMPLICIT  NONE
      INTEGER   J,N,LIG(*),KN,NDIM,K,KK,L,IX,LM,LL,LX,IK,KJ,M,KM,I,IOP
      REAL*8    PVT,A(*),P1,PREC,AP,COM
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT
C
      DO 010 J=1,N                                                      
  010 LIG(J)=0                                                          
      KN=N*NDIM                                                         
C                                                                       
C     ARRANGEMENT DES PIVOTS                                            
C                                                                       
      DO 080 K=1,N                                                      
      KK=(K-1)*NDIM+K                                                   
      PVT=DABS(A(KK))                                                   
      DO 030 L=KK,KN,NDIM                                               
      P1=DABS(A(L))                                                     
      IF (P1-PVT) 030,030,020                                           
  020 PVT=P1                                                            
      LIG(K)=(L-K)/NDIM+1                                               
  030 CONTINUE                                                          
C     IF (IOP.EQ.1) WRITE (OUT6,1000) PVT                                  
      IF (PVT.LT.PREC) GO TO 120                                        
      IF (LIG(K).EQ.0.0D0) GO TO 050                                     
      IX=(LIG(K)-K)*NDIM                                                
      LM=(LIG(K)-1)*NDIM+1                                              
      LL=LM+N-1                                                         
      DO 040 L=LM,LL                                                    
      AP=A(L)                                                           
      LX=L-IX                                                           
      A(L)=A(LX)                                                        
  040 A(LX)=AP                                                          
C                                                                       
C     INVERSION                                                         
C                                                                       
  050 COM=A(KK)                                                         
      A(KK)=1.0D0                                                        
      DO 060 J=K,KN,NDIM                                                
  060 A(J)=A(J)/COM                                                     
      DO 080 I=1,N                                                      
      IX=I-K                                                            
      IF (IX.EQ.0) GO TO 080                                            
      IK=(K-1)*NDIM+I                                                   
      COM=A(IK)                                                         
      A(IK)=0.0D0                                                        
      DO 070 J=I,KN,NDIM                                                
      KJ=J-IX                                                           
  070 A(J)=A(J)-COM*A(KJ)                                               
  080 CONTINUE                                                          
C                                                                       
C     REARRANGEMENT DES LIGNES ET DES COLONNES                          
C                                                                       
      K=N                                                               
  090 IF (LIG(K).EQ.0) GO TO 110                                        
      L=LIG(K)                                                          
      DO 100 M=L,KN,NDIM                                                
      AP=A(M)                                                           
      KM=M-L+K                                                          
      A(M)=A(KM)                                                        
  100 A(KM)=AP                                                          
  110 K=K-1                                                             
      IF (K.NE.0) GO TO 090                                             
      LIG(1)=0                                                          
      RETURN                                                            
  120 LIG(1)=1                                                          
      WRITE (OUT6,1010) PREC                                               
      RETURN                                                            
 1000 FORMAT (' PIVOT = ',1PD13.6)                                      
 1010 FORMAT ('0MATRICE DEGENEREE - LE PIVOT EST PLUS PETIT QUE ',      
     .        1PD9.2)                                                   
      END                                                               
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  KNEWT(X,N,CHECK,IW)
C     ------------------------------------------------------------------
CU    SUBROUTINE KNEWT USES SUBROUTINES/FUNCTIONS
CU       KFDJAC
CU       KLUDCMP
CU       KLUBKSB
CU       KLNSRCH
CU       KFMIN    (EXTERNAL LINK)
C     ------------------------------------------------------------------
CA    CALL KNEWT(X,N,CHECK,IW)
C     ------------------------------------------------------------------
CB    SUBROUTINE KNEWT USES COMMON BLOCKS
CB       KNCHOUT
CB       KNEWTV
CB       KTAUOLD
C     ------------------------------------------------------------------
CT    COMMENTS FOR SUBROUTINE KNEWT
CT       ORIGINAL VALUE OF MAXITS WAS 200
CT       ORIGINAL VALUE OF STPMX WAS 100
C     ------------------------------------------------------------------
      !
      ! INPUT
      !
      INTEGER   IW,N
      REAL*8    X(N)
      !
      ! OUTPUT (NOTE THAT X(N) IS ALSO OUTPUT)
      !
      LOGICAL   CHECK
      !
      ! LOCAL
      !
      INTEGER   NN,NP,MAXITS
      REAL*8    FVEC,TOLF,TOLMIN,TOLX,STPMX
      PARAMETER(NP=40,MAXITS=2000,TOLF=1.0D-4,TOLMIN=1.0D-6,TOLX=1.0D-7)
      PARAMETER(STPMX=1.0D0)
      INTEGER   I,ITS,J,INDX(NP)
      REAL*8    D,DEN,F,FOLD,STPMAX,SUM,TEMP,TEST
      REAL*8    FJAC(NP,NP),G(NP),P(NP),XOLD(NP),KFMIN
      EXTERNAL  KFMIN
      !
      ! COMMON BLOCKS
      !
      INTEGER   OUT6,OUT9,OUTOPT
      COMMON   /KNCHOUT/ OUT6,OUT9,OUTOPT

      REAL*8    TAUOLD
      COMMON   /KTAUOLD/ TAUOLD

      COMMON   /KNEWTV/ FVEC(NP),NN
      SAVE     /KNEWTV/
C     ------------------------------------------------------------------
C     common kcheck only to cut the fishtails
C     ------------------------------------------------------------------
      logical chk
      common /kcheck/ chk
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C     CHECK IF THE INITIAL GUESS IS THE REAL SOLUTION
C     ------------------------------------------------------------------
      NN=N
      IF(IW.EQ.1) WRITE(OUT6,980)
      F=KFMIN(X)
      TEST=0.0D0
      DO 11 I=1,N
         IF(ABS(FVEC(I)).GT.TEST)TEST=ABS(FVEC(I))
11    CONTINUE
      IF(TEST.LT..01D0*TOLF*TAUOLD) THEN
         IF(IW.EQ.1)WRITE(OUT6,960) ITS,TEST
C        write(out6,'(a,i5,a,5e13.5,a,e13.5)') 'At exit 1: its=',0,
C    .                    ', dpl=',(x(i),i=1,5),', test=',test
         chk=.false.
         RETURN
      ENDIF
      IF(IW.EQ.1) WRITE(OUT6,940)

      SUM=0.0D0
      DO 12 I=1,N
         SUM=SUM+X(I)**2.0D0
12    CONTINUE

C     ------------------------------------------------------------------
C     EH 10.1999
C        - CORRECTION TWENTE: STPMAX=STPMX*MAX(SQRT(SUM),FLOAT(N)) IS
C          REPLACED BY THE FOLLOWING STATEMENT
C     ------------------------------------------------------------------
      STPMAX=STPMX*MAX(SQRT(SUM),DBLE(N))

C     ------------------------------------------------------------------
C     STARTS A SERIES OF LINE SEARCH
C     ------------------------------------------------------------------
      DO 21 ITS=1,MAXITS
         CALL KFDJAC(N,X,FVEC,NP,FJAC)
         DO 14 I=1,N
            SUM=0.0D0
            DO 13 J=1,N
               SUM=SUM+FJAC(J,I)*FVEC(J)
13          CONTINUE
            G(I)=SUM
14       CONTINUE
         DO 15 I=1,N
            XOLD(I)=X(I)
15       CONTINUE
         FOLD=F
         DO 16 I=1,N
            P(I)=-FVEC(I)
16       CONTINUE
         CALL KLUDCMP(FJAC,N,NP,INDX,D)
         CALL KLUBKSB(FJAC,N,NP,INDX,P)
         IF(IW.EQ.1) WRITE(OUT6,930) ITS
         CALL KLNSRCH(N,XOLD,FOLD,G,P,X,F,STPMAX,CHECK,KFMIN)
C        ---------------------------------------------------------------
C        OUTPUT OF KLNSRCH:
C        CHECK=TRUE  : NO IMPROVEMENT IN KLNSRCH I.E. X=XOLD
C        CHECK=FALSE :    IMPROVEMENT IN KLNSRCH
C        ---------------------------------------------------------------
         TEST=0.0D0
         DO 17 I=1,N
            IF(ABS(FVEC(I)).GT.TEST)TEST=ABS(FVEC(I))
17       CONTINUE
C        write(out6,'(a,i5,a,5e13.5,a,e13.5)') 'At exit 2: its=',its,
C    .                    ', dpl=',(x(i),i=1,5),', test=',test
         IF(TEST.LT.TOLF*TAUOLD)THEN
            CHECK=.FALSE.
            IF(IW.EQ.1)WRITE(OUT6,920) ITS,TEST
            chk=check
            RETURN
         ENDIF


C        ---------------------------------------------------------------
C        IF(NO IMPROVEMENT BY KLNSRCH): EXIT OF KNEWT
C        ---------------------------------------------------------------
         IF(CHECK)THEN
            TEST=0.0D0
C           DEN=MAX(F,.5D0*N)
C           CORRECTION TWENTE
            DEN=MAX(F,0.5D0*N)
            DO 18 I=1,N
               TEMP=ABS(G(I))*MAX(ABS(X(I)),1.0D0)/DEN
               IF(TEMP.GT.TEST)TEST=TEMP
18          CONTINUE
            IF(TEST.LT.TOLMIN)THEN
              CHECK=.TRUE.
            ELSE
              CHECK=.FALSE.
            ENDIF
            F=KFMIN(X)
            TEST=0.0D0
            DO I=1,N
               IF(ABS(FVEC(I)).GT.TEST)TEST=ABS(FVEC(I))
            ENDDO
C        write(out6,'(a,i5,a,5e13.5,a,e13.5)') 'At exit 3: its=',its,
C    .                    ', dpl=',(x(i),i=1,5),', test=',test
            IF(TEST/TAUOLD.GT.0.005D0)WRITE(OUT6,900) ITS,TEST
            chk=.false.
            if(test.gt.1.d6)chk=.true.
            RETURN
         ENDIF

C        ---------------------------------------------------------------
C        IF(IMPROVEMENT BY KLNSRCH IS NOT SIGNIFICATIVE): EXIT OF KNEWT
C        ---------------------------------------------------------------
         TEST=0.0D0
         DO 19 I=1,N
            TEMP=(ABS(X(I)-XOLD(I)))/MAX(ABS(X(I)),1.0D0)
            IF(TEMP.GT.TEST)TEST=TEMP
19       CONTINUE

         IF(TEST.LT.TOLX) THEN
            F=KFMIN(X)
            TEST=0.
            DO I=1,N
               IF(ABS(FVEC(I)).GT.TEST)TEST=ABS(FVEC(I))
            ENDDO
            WRITE(OUT6,880) ITS,TEST
C        write(out6,'(a,i5,a,5e13.5,a,e13.5)') 'At exit 4: its=',its,
C    .                    ', dpl=',(x(i),i=1,5),', test=',test
            chk=.false.
            if(test.gt.1.d6)chk=.true.
            RETURN
         ENDIF
C        ---------------------------------------------------------------
C        IF(ITERATIVE PROCESS OK) GOES ON ITERATING (DO 21 ...)
C        ---------------------------------------------------------------
21    CONTINUE
C
C     ICOTOM IF NO SOLUTION AFTER MAXITS ITERATIONS, DOES NOT STOP
C     BUT EXIT PROPERLY AT THE OBTAINED POINT
C 
C     PAUSE 'MAXITS EXCEEDED IN NEWT'
      WRITE(OUT6,860) ITS,TEST
      RETURN

C     ------------------------------------------------------------------
C     FORMATS
C     ------------------------------------------------------------------
980   FORMAT(12X,'[KNEWT] IS THE CURRENT POINT THE SOLUTION?')
960   FORMAT(12X,'[KNEWT] EXIT1: ITS=',I5,' , MAX(ABS(FVEC(I)))=',E13.5)
940   FORMAT(12X,'[KNEWT] THIS IS NOT THE SOLUTION')
930   FORMAT(12X,'[KNEWT] LNSRCH NUMBER ',I5)
920   FORMAT(12X,'[KNEWT] EXIT2: ITS=',I5,' , MAX(ABS(FVEC(I)))=',E13.5)
900   FORMAT(12X,'[KNEWT] KWARNING! EXIT3: ITS=',I5,
     .           ' , MAX(ABS(FVEC(I)))=',E13.5)
880   FORMAT(12X,'[KNEWT] EXIT4: ITS=',I5,' , MAX(ABS(FVEC(I)))=',E13.5)
860   FORMAT(12X,'[KNEWT] EXIT SPECIAL: ITS=',I5,
     .           ' , MAX(ABS(FVEC(I)))=',E13.5)
C     ------------------------------------------------------------------
      END
C  (C) Copr. 1986-92 Numerical Recipes Software D04-4-+5Z5{..
************************************************************************ 
* 
************************************************************************ 
      FUNCTION  KFMIN(X)
C     ------------------------------------------------------------------
CU    FUNCTION KFMIN USES SUBROUTINES 
CU       KFUNCV5
C     ------------------------------------------------------------------
CA    XXX=KFMIN(X)
C     ------------------------------------------------------------------
CB    FUNCTION KFMIN USES COMMON BLOCKS
CB       KNEWTV
C     ------------------------------------------------------------------
      !
      ! INPUT
      !
      REAL*8    X(*)
      !
      ! OUTPUT
      !
      REAL*8    KFMIN
      !
      ! LOCAL
      !
      INTEGER   I,N,NP
      PARAMETER(NP=40)
      REAL*8    SUM
      !
      ! COMMON BLOCKS
      !
      REAL*8  FVEC
      COMMON /KNEWTV/ FVEC(NP),N
      SAVE   /KNEWTV/
C     ------------------------------------------------------------------
      CALL KFUNCV5(N,X,FVEC)
      SUM=0.0D0
      DO 11 I=1,N
        SUM=SUM+FVEC(I)**2.0D0
11    CONTINUE
      KFMIN=0.5D0*SUM
C     ------------------------------------------------------------------
      RETURN
      END
C  (C) Copr. 1986-92 Numerical Recipes Software D04-4-+5Z5{..
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  Klnsrch(n,xold,fold,g,p,x,f,stpmax,check,func)
      INTEGER n
      LOGICAL check
      REAL*8 f,fold,stpmax,g(n),p(n),x(n),xold(n),func,ALF,TOLX
      PARAMETER (ALF=1.0d-4,TOLX=1.0d-7)
      EXTERNAL func
CU    USES func
      INTEGER i
      REAL*8 a,alam,alam2,alamin,b,disc,f2,fold2,rhs1,rhs2,slope,sum,
     *temp,test,tmplam
C     ------------------------------------------------------------------
C     eh/syl: 03.05.00
C        - INITIALISE F2,ALAM2,FOLD2 TO SOMETHING TO AVOID FASTIDIOUS
C          WARNINGS DURING COMPILATION.  NO EFFECT ON THE COMPUTATIONS
C     ------------------------------------------------------------------
      f2=0.0d0
      fold2=0.0d0
      alam2=0.0d0
C     ------------------------------------------------------------------
      check=.false.
      sum=0.0D0
      do 11 i=1,n
        sum=sum+p(i)*p(i)
11    continue
      sum=sqrt(sum)
      if(sum.gt.stpmax)then
        do 12 i=1,n
          p(i)=p(i)*stpmax/sum
12      continue
      endif
      slope=0.0D0
      do 13 i=1,n
        slope=slope+g(i)*p(i)
13    continue
      test=0.0D0
      do 14 i=1,n
        temp=abs(p(i))/max(abs(xold(i)),1.0D0)
        if(temp.gt.test)test=temp
14    continue
C     ------------------------------------------------------------------
C     EH/SYL 07/04/00
C        - ALAMIN=0.1D0*TOLX/TEST INSTEAD OF TOLX/TEST TO ALLOW MORE
C          STEP REDUCTIONS DURING THE LINE SEARCH PROCEDURE
C     ------------------------------------------------------------------
      alamin=0.1D0*TOLX/test
      alam=1.0D0
1     continue
        do 15 i=1,n
          x(i)=xold(i)+alam*p(i)
15      continue
        f=func(x)
        if(alam.lt.alamin)then
          do 16 i=1,n
            x(i)=xold(i)
16        continue
          check=.true.
          return
        else if(f.le.fold+ALF*alam*slope)then
          return
        else
          if(alam.eq.1.)then
            tmplam=-slope/(2.0D0*(f-fold-slope))
          else
            rhs1=f-fold-alam*slope
            rhs2=f2-fold2-alam2*slope
            a=(rhs1/alam**2-rhs2/alam2**2)/(alam-alam2)
            b=(-alam2*rhs1/alam**2+alam*rhs2/alam2**2)/(alam-alam2)
            if(a.eq.0.0D0)then
              tmplam=-slope/(2.0D0*b)
            else
              disc=b*b-3.0D0*a*slope
              tmplam=(-b+sqrt(disc))/(3.0D0*a)
            endif
            if(tmplam.gt..5D0*alam)tmplam=.5D0*alam
          endif
        endif
        alam2=alam
        f2=f
        fold2=fold
        alam=max(tmplam,.1D0*alam)
      goto 1
      END
C  (C) Copr. 1986-92 Numerical Recipes Software D04-4-+5Z5{..
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  Kludcmp(a,n,np,indx,d)
      INTEGER n,np,indx(n),NMAX
      REAL*8 d,a(np,np),TINY
      PARAMETER (NMAX=500,TINY=1.0d-20)
      INTEGER i,imax,j,k
      REAL*8 aamax,dum,sum,vv(NMAX)
C     ------------------------------------------------------------------
C     eh/syl: 03.05.00
C        - INITIALISE IMAX TO SOMETHING TO AVOID FASTIDIOUS
C          WARNINGS DURING COMPILATION.  NO EFFECT ON THE COMPUTATIONS
C     ------------------------------------------------------------------
      imax=0
C     ------------------------------------------------------------------
      d=1.0d0
      do 12 i=1,n
        aamax=0.0D0
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        if (aamax.eq.0.0D0) pause 'singular matrix in ludcmp'
        vv(i)=1.0D0/aamax
12    continue
      do 19 j=1,n
        do 14 i=1,j-1
          sum=a(i,j)
          do 13 k=1,i-1
            sum=sum-a(i,k)*a(k,j)
13        continue
          a(i,j)=sum
14      continue
        aamax=0.0D0
        do 16 i=j,n
          sum=a(i,j)
          do 15 k=1,j-1
            sum=sum-a(i,k)*a(k,j)
15        continue
          a(i,j)=sum
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(a(j,j).eq.0.0D0)a(j,j)=TINY
        if(j.ne.n)then
          dum=1.0d0/a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software D04-4-+5Z5{..
************************************************************************ 
* 
************************************************************************ 
      SUBROUTINE  Klubksb(a,n,np,indx,b)
      INTEGER n,np,indx(n)
      REAL*8 a(np,np),b(n)
      INTEGER i,ii,j,ll
      REAL*8 sum
      ii=0
      do 12 i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            sum=sum-a(i,j)*b(j)
11        continue
        else if (sum.ne.0.0D0) then
          ii=i
        endif
        b(i)=sum
12    continue
      do 14 i=n,1,-1
        sum=b(i)
        do 13 j=i+1,n
          sum=sum-a(i,j)*b(j)
13      continue
        b(i)=sum/a(i,i)
14    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software D04-4-+5Z5{..
C VER9
************************************************************************
**** 
***
**
*    CONCATENATION OF TEXTURE SUBROUTINES FOR CASHFORM MODELS
**   (POINT2.F YLP.F TENS.F INIF3 F02ABF.F F04JDF.F X04AAF.F)
***
****
************************************************************************
ct    Modifications for cashform:
ct       10.04.00 (eh/syl)
ct       - All the subroutines printing output include now the common
ct         bloc /knchout/ and the statement 'data nout/6/' is replaced
ct         by 'nout=out6' or 'nout=out9'.
ct       - The subroutine kx04aaf includes the common bloc /knchout/
ct         and the statement 'nerr1=out6'
ct      14.04.00 (eh/syl/bvb)
ct       - Modify the test in ylp.f to determine ibesttry and hmin when
ct         itry3=1
ct      16.04.00 (eh)
ct       - ylp: Add to the output of ylp the 5-dim yield locus point 
ct         corresponding to the solution
ct      27.04.00 (eh/syl)
ct       - inidf: add declaration of identifiers iaa, ibb and ntermd.
ct       - inif3: previously, nlawm was not transmitted but it should
ct         have been.  Now, nlawm appears in the arguments and is 
ct         declared as an integer.
ct       - kpsylp: add a comma at the end of the 1st line of the format
ct         6997.
ct       - point2: add a dummy variable real*8 dum5(5) to call kylp and 
ct         being in agreement with the modification 16.04.00 in ylp.
ct       - be4ini: add a comma at the end of the first line of the 
ct         format 6997.
ct       03.05.00 (syl/eh)
ct       - all routine: add 'implicit none' statement.
ct       - point2: remove the complete routine since it was not used at
ct         all.  The subsequent call to kpsylp is therefore also removed,
ct         so does the complete routine kpsylp.
ct       - ylp: add initialisation of iscale to avoid warnings.  Note 
ct         that iscale is not used in this case since ioff is never 2.
ct         Same remark for dtau and hgold.
ct       - ylp: initialise ang1old, ang2old.
ct       - daijb: initialise i and j to something to avoid warnings.  No 
ct         effects on results.
ct       - inif3: add the declaration of identifier i.
ct         Add definition 'nodd=ncfodd' similar to 'nevn=ncfevn'.  This
ct         is an important correction.
ct       - kipnz: add the declaration integer kisodd, external kisodd to 
ct         avoid warnings during compilation.  Same for knjklst.
ct       - inif: Permute the declaration of iaa and aa(iaa,*) to avoid 
ct         warnings.  Add the declaration of identifier i and k.
ct         Add the declaration integer kipnz, external kipnz to avoid 
ct         warnings during compilation.
ct       - f02szf: Add initialisation of jj to avoid warnings.  No 
ct         effect on results since jj is defined each time wantz is true
ct         and used only when wantz is true.
ct       - f02wbz: Add initialisation of km1 to avoid warnings.  No 
ct         effect on result since km1 is defined each time k.ne.1 and 
ct         used only when k.ne.1.
ct       - read2: permute the declaration of mpame and pamet(mpame).
ct         Add the declaration of maxf, nchlst, ired, mpamec.
ct         Initialise rclass to -1.  No effect since redefined after.
ct         Adding implicit none
ct         symcla: add integer/external declaration for kisodd and knjklst
ct         reduce: add integer/external declaration for kipnz
ct       05/06/00 syl
ct       - add 'k' to subroutine/function names
ct       - set 3 spaces beteeen subroutine/function and the name
c ----------------------------------------------------------------------
c
c     set of subroutines for implementation into FE routines of JAW;
c     yield locus described by means of series expansion fitted to 1/s
c     or M;
c
c     BVB     1/04/98 : superfast by plane strain YL as initial guess
c     BVB/EH 20/06/97 : allow for iptial=2
c     BVB/EH 15/01/97 : using f3der instead of f2der in subr. point2
c
c     Subroutines:
c
c        kdhm
c        kmdv
c        kvdm
c        kupsmsp
c        kf3der
c
************************************************************************
************************************************************************
      subroutine   kdhm(dm,hydr,m)
c ----------------------------------------------------------------------
c     calculates "dm(3,3)"  , 3x3 matrix representation
c                             of the deviatoric tensor,
c            and "hydr"     , the hydrostatic part,
c     corresponding to 
c     the given  "m(3,3)"   , 3x3 matrix representation of the tensor;
c ----------------------------------------------------------------------
ct    27.04.00: eh/syl
ct    - remove the real*8 variable r which was set but not used
c ----------------------------------------------------------------------
      implicit none
      integer i,j
      real*8 dm(3,3),hydr,m(3,3)
c
      hydr=(m(1,1)+m(2,2)+m(3,3))/3.d0

      do 10 i=1,3
      do 10 j=1,3
         if (i.eq.j) then 
            dm(i,j)=m(i,j)-hydr
         else
            dm(i,j)=m(i,j)
         endif
 10   continue
c ----------------------------------------------------------------------
c     end of subroutine kdhm;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kmdv(dm,dv)
c ----------------------------------------------------------------------
c     calculates "dm(3,3)"  , 3x3 matrix representation 
c                             of the deviatoric tensor 
c     corresponding to
c     the given  "dv(5)"    , 5-dimensional vector representation
c                             of the deviatioric tensor;
c ----------------------------------------------------------------------
      implicit none
      real*8 dm(3,3),dv(5)
      real*8 root23,root2i,root6i
c ----------------------------------------------------------------------
c     calculate some constants;
c ----------------------------------------------------------------------
      root23=dsqrt(2.d0/3.d0)
      root2i=dsqrt(2.d0)/2.d0
      root6i=dsqrt(6.d0)/6.d0
c
      dm(1,1)= root2i*dv(1) + root6i*dv(2)
      dm(2,2)=-root2i*dv(1) + root6i*dv(2)
      dm(3,3)=-root23*dv(2)
      dm(2,3)= root2i*dv(3)
      dm(3,1)= root2i*dv(4)
      dm(1,2)= root2i*dv(5)
      dm(3,2)= dm(2,3)
      dm(1,3)= dm(3,1)
      dm(2,1)= dm(1,2)
c ----------------------------------------------------------------------
c     end of subroutine kmdv;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kvdm(dv,dm)
c ----------------------------------------------------------------------
c     calculates "dv(5)"    , 5-dimensional vector representation
c                             of the deviatioric tensor
c     corresponding to
c     the given  "dm(3,3)"  , 3x3 matrix representation 
c                             of the deviatoric tensor;
c ----------------------------------------------------------------------
      implicit none
      real*8 dv(5),dm(3,3)
      real*8 root2,root2i,root32
c ----------------------------------------------------------------------
c     calculate some constants;
c ----------------------------------------------------------------------
      root2 =dsqrt(2.d0)
      root2i=root2/2.d0
      root32=dsqrt(3.d0/2.d0)
c
      dv(1)= root2i*(dm(1,1)-dm(2,2))
      dv(2)= root32*(dm(1,1)+dm(2,2))
      dv(3)= root2*dm(2,3)
      dv(4)= root2*dm(3,1)
      dv(5)= root2*dm(1,2)
c ----------------------------------------------------------------------
c     end of subroutine kvdm;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kupsmsp(up,sm,sp)
c ----------------------------------------------------------------------
c     calculates   'up(1..5)' and 'sm' for a given  'sp(1..5)';
c ----------------------------------------------------------------------
ct    27.04.00: syl/eh
ct    - remove nout since it was not used
c ----------------------------------------------------------------------
      implicit none
      integer p
      real*8 sm,sp(5),sum,up(5)
c
      sum=0.d0
      do 10 p=1,5
         sum=sum+sp(p)**2
 10   continue
      sm=dsqrt(sum)
      do 20 p=1,5
         up(p)=sp(p)/sm
 20   continue
c ----------------------------------------------------------------------
c     end of subroutine kupsmsp;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
************************************************************************
      subroutine   kf3der(fval,df,ddf,uv,cfevn,cfodd,
     x     nvar,ider,nchlst,iw)
c ----------------------------------------------------------------------
ct    21.03.02: bvb/sh : allow for uv=0.d0 
ct    13.03.02: bvb/sh : modifications for ifun=-3, i.e. M^n and npoevn
ct    - npoevn declared as integer and added to common/opteao/
ct    - modifications based on f3der bvb 07/08/01
ct    - suggestion: later change df1e,df2e,...    -> dfe()
ct                               df1e0,df2e0,...  -> dfe0()
ct                               ddf11e,...       -> ddfe()
ct                               also for odd part
c     (bvb/eh 23/06/97 : also 2nd order derivatives)
c     (bvb/eh 15/01/97 : implementation for iptial=1, i.e. function+
c                        1st order derivatives)
c     (bvb 12/12/96)
c ----------------------------------------------------------------------
cc at present common only for one "nevn+nodd+symmetry class"
      implicit none
      integer mxorde,mxfe,mxdfe,mxddfe
      integer mxordo,mxfo,mxdfo,mxddfo
      parameter (mxorde=6,mxfe=210,mxdfe=126,mxddfe=70)
      parameter (mxordo=1,mxfo=5,mxdfo=1,mxddfo=1)
      integer nevn,nodd,npoevn
      integer inde(mxfe,mxorde+2),
     x   inde1(mxdfe,mxorde+1),inde2(mxdfe,mxorde+1),
     x   inde3(mxdfe,mxorde+1),inde4(mxdfe,mxorde+1),
     x   inde5(mxdfe,mxorde+1),
     x   inde11(mxddfe,mxorde),inde12(mxddfe,mxorde),
     x   inde13(mxddfe,mxorde),inde14(mxddfe,mxorde),
     x   inde15(mxddfe,mxorde),inde22(mxddfe,mxorde),
     x   inde23(mxddfe,mxorde),inde24(mxddfe,mxorde),
     x   inde25(mxddfe,mxorde),inde33(mxddfe,mxorde),
     x   inde34(mxddfe,mxorde),inde35(mxddfe,mxorde),
     x   inde44(mxddfe,mxorde),inde45(mxddfe,mxorde),
     x   inde55(mxddfe,mxorde)
      integer indo(mxfo,mxordo+2),
     x   indo1(mxdfo,mxordo+1),indo2(mxdfo,mxordo+1),
     x   indo3(mxdfo,mxordo+1),indo4(mxdfo,mxordo+1),
     x   indo5(mxdfo,mxordo+1),
     x   indo11(mxddfo,mxordo),indo12(mxddfo,mxordo),
     x   indo13(mxddfo,mxordo),indo14(mxddfo,mxordo),
     x   indo15(mxddfo,mxordo),indo22(mxddfo,mxordo),
     x   indo23(mxddfo,mxordo),indo24(mxddfo,mxordo),
     x   indo25(mxddfo,mxordo),indo33(mxddfo,mxordo),
     x   indo34(mxddfo,mxordo),indo35(mxddfo,mxordo),
     x   indo44(mxddfo,mxordo),indo45(mxddfo,mxordo),
     x   indo55(mxddfo,mxordo)
      integer ncfe,ncfe1,ncfe2,ncfe3,ncfe4,ncfe5,
     x   ncfe11,ncfe12,ncfe13,ncfe14,ncfe15,ncfe22,ncfe23,ncfe24,
     x   ncfe25,ncfe33,ncfe34,ncfe35,ncfe44,ncfe45,ncfe55,
     x   nevnd,nevndd
      integer ncfo,ncfo1,ncfo2,ncfo3,ncfo4,ncfo5,
     x   ncfo11,ncfo12,ncfo13,ncfo14,ncfo15,ncfo22,ncfo23,ncfo24,
     x   ncfo25,ncfo33,ncfo34,ncfo35,ncfo44,ncfo45,ncfo55,
     x   noddd,nodddd
      common /opteao/ nevn,nodd,npoevn,
     x     inde,inde1,inde2,inde3,inde4,inde5,
     x     inde11,inde12,inde13,inde14,inde15,inde22,inde23,inde24,
     x     inde25,inde33,inde34,inde35,inde44,inde45,inde55,
     x     ncfe,ncfe1,ncfe2,ncfe3,ncfe4,ncfe5,nevnd,
     x     ncfe11,ncfe12,ncfe13,ncfe14,ncfe15,ncfe22,ncfe23,ncfe24,
     x     ncfe25,ncfe33,ncfe34,ncfe35,ncfe44,ncfe45,ncfe55,nevndd,
     x     indo,indo1,indo2,indo3,indo4,indo5,
     x     indo11,indo12,indo13,indo14,indo15,indo22,indo23,indo24,
     x     indo25,indo33,indo34,indo35,indo44,indo45,indo55,
     x     ncfo,ncfo1,ncfo2,ncfo3,ncfo4,ncfo5,noddd,
     x     ncfo11,ncfo12,ncfo13,ncfo14,ncfo15,ncfo22,ncfo23,ncfo24,
     x     ncfo25,ncfo33,ncfo34,ncfo35,ncfo44,ncfo45,ncfo55,nodddd
c
      integer i,ider,iw,j,nchlst,nvar
      real*8 ddf(5,5),df(5),df1e,df2e,df3e,df4e,df5e,
     x     df1e0,df2e0,df3e0,df4e0,df5e0,
     x     df1o,df2o,df3o,df4o,df5o,
     x     ddf11e,ddf12e,ddf13e,ddf14e,ddf15e,ddf22e,ddf23e,ddf24e,
     x     ddf25e,ddf33e,ddf34e,ddf35e,ddf44e,ddf45e,ddf55e,
     x     ddf11o,ddf12o,ddf13o,ddf14o,ddf15o,ddf22o,ddf23o,ddf24o,
     x     ddf25o,ddf33o,ddf34o,ddf35o,ddf44o,ddf45o,ddf55o,
     x     keval,fval,fvale,fvalo,uv(5)
      real*8 cfevn(mxfe),cfodd(mxfo),fvale0,r1,r2,r3
c ----------------------------------------------------------------------
c     initialise odd part;
c ----------------------------------------------------------------------
      fvalo=0.d0
      df1o=0.d0
      df2o=0.d0
      df3o=0.d0
      df4o=0.d0
      df5o=0.d0
      ddf11o=0.d0
      ddf12o=0.d0
      ddf13o=0.d0
      ddf14o=0.d0
      ddf15o=0.d0
      ddf22o=0.d0
      ddf23o=0.d0
      ddf24o=0.d0
      ddf25o=0.d0
      ddf33o=0.d0
      ddf34o=0.d0
      ddf35o=0.d0
      ddf44o=0.d0
      ddf45o=0.d0
      ddf55o=0.d0
c ----------------------------------------------------------------------
c     function value;
c ----------------------------------------------------------------------
      fvale0=keval(cfevn,mxfe,uv,nevn,ncfe,inde,mxfe,nchlst,iw)
      fvale=fvale0**(1.d0/npoevn)
      if (ncfo.ne.0) fvalo=
     x     keval(cfodd,mxfo,uv,nodd,ncfo,indo,mxfo,nchlst,iw)
      fval=fvale+fvalo
c ----------------------------------------------------------------------
c     first order partial derivatives;
c ----------------------------------------------------------------------
      if (ider.ge.1) then
         df1e0=keval(cfevn,mxfe,uv,nevnd,ncfe1,inde1,mxdfe,
     x      nchlst,iw)
         df2e0=keval(cfevn,mxfe,uv,nevnd,ncfe2,inde2,mxdfe,
     x      nchlst,iw)
         df3e0=keval(cfevn,mxfe,uv,nevnd,ncfe3,inde3,mxdfe,
     x      nchlst,iw)
         df4e0=keval(cfevn,mxfe,uv,nevnd,ncfe4,inde4,mxdfe,
     x      nchlst,iw)
         df5e0=keval(cfevn,mxfe,uv,nevnd,ncfe5,inde5,mxdfe,
     x      nchlst,iw)
       r1=1.d0-1.d0/npoevn
         if (fvale0.eq.0.d0.or.r1.eq.0.d0) then
          r2=0.d0
       else
          r2=1.d0/(npoevn*fvale0**r1)
       endif
       df1e=r2*df1e0
       df2e=r2*df2e0
       df3e=r2*df3e0
       df4e=r2*df4e0
       df5e=r2*df5e0
         if (ncfo.ne.0) then
            df1o=keval(cfodd,mxfo,uv,noddd,ncfo1,indo1,mxdfo,
     x           nchlst,iw)
            df2o=keval(cfodd,mxfo,uv,noddd,ncfo2,indo2,mxdfo,
     x           nchlst,iw)
            df3o=keval(cfodd,mxfo,uv,noddd,ncfo3,indo3,mxdfo,
     x           nchlst,iw)
            df4o=keval(cfodd,mxfo,uv,noddd,ncfo4,indo4,mxdfo,
     x           nchlst,iw)
            df5o=keval(cfodd,mxfo,uv,noddd,ncfo5,indo5,mxdfo,
     x           nchlst,iw)
         endif
         df(1)=df1e+df1o
         df(2)=df2e+df2o
         df(3)=df3e+df3o
         df(4)=df4e+df4o
         df(5)=df5e+df5o
      endif
      if (ider.ge.2) then
         ddf11e=keval(cfevn,mxfe,uv,nevndd,ncfe11,inde11,mxddfe,
     x      nchlst,iw)
         ddf12e=keval(cfevn,mxfe,uv,nevndd,ncfe12,inde12,mxddfe,
     x      nchlst,iw)
         ddf13e=keval(cfevn,mxfe,uv,nevndd,ncfe13,inde13,mxddfe,
     x      nchlst,iw)
         ddf14e=keval(cfevn,mxfe,uv,nevndd,ncfe14,inde14,mxddfe,
     x      nchlst,iw)
         ddf15e=keval(cfevn,mxfe,uv,nevndd,ncfe15,inde15,mxddfe,
     x      nchlst,iw)
         ddf22e=keval(cfevn,mxfe,uv,nevndd,ncfe22,inde22,mxddfe,
     x      nchlst,iw)
         ddf23e=keval(cfevn,mxfe,uv,nevndd,ncfe23,inde23,mxddfe,
     x      nchlst,iw)
         ddf24e=keval(cfevn,mxfe,uv,nevndd,ncfe24,inde24,mxddfe,
     x      nchlst,iw)
         ddf25e=keval(cfevn,mxfe,uv,nevndd,ncfe25,inde25,mxddfe,
     x      nchlst,iw)
         ddf33e=keval(cfevn,mxfe,uv,nevndd,ncfe33,inde33,mxddfe,
     x      nchlst,iw)
         ddf34e=keval(cfevn,mxfe,uv,nevndd,ncfe34,inde34,mxddfe,
     x      nchlst,iw)
         ddf35e=keval(cfevn,mxfe,uv,nevndd,ncfe35,inde35,mxddfe,
     x      nchlst,iw)
         ddf44e=keval(cfevn,mxfe,uv,nevndd,ncfe44,inde44,mxddfe,
     x      nchlst,iw)
         ddf45e=keval(cfevn,mxfe,uv,nevndd,ncfe45,inde45,mxddfe,
     x      nchlst,iw)
         ddf55e=keval(cfevn,mxfe,uv,nevndd,ncfe55,inde55,mxddfe,
     x      nchlst,iw)
       if (fvale0.eq.0.d0) then
          r3=0.d0
       else
          r3=r1/fvale0
       endif
       ddf11e=r2*(ddf11e-r3*df1e0*df1e0)
       ddf12e=r2*(ddf12e-r3*df1e0*df2e0)
       ddf13e=r2*(ddf13e-r3*df1e0*df3e0)
       ddf14e=r2*(ddf14e-r3*df1e0*df4e0)
       ddf15e=r2*(ddf15e-r3*df1e0*df5e0)
       ddf22e=r2*(ddf22e-r3*df2e0*df2e0)
       ddf23e=r2*(ddf23e-r3*df2e0*df3e0)
       ddf24e=r2*(ddf24e-r3*df2e0*df4e0)
       ddf25e=r2*(ddf25e-r3*df2e0*df5e0)
       ddf33e=r2*(ddf33e-r3*df3e0*df3e0)
       ddf34e=r2*(ddf34e-r3*df3e0*df4e0)
       ddf35e=r2*(ddf35e-r3*df3e0*df5e0)
       ddf44e=r2*(ddf44e-r3*df4e0*df4e0)
       ddf45e=r2*(ddf45e-r3*df4e0*df5e0)
       ddf55e=r2*(ddf55e-r3*df5e0*df5e0)
         if (ncfo.ne.0) then
            ddf11o=keval(cfodd,mxfo,uv,nodddd,ncfo11,indo11,mxddfo,
     x           nchlst,iw)
            ddf12o=keval(cfodd,mxfo,uv,nodddd,ncfo12,indo12,mxddfo,
     x           nchlst,iw)
            ddf13o=keval(cfodd,mxfo,uv,nodddd,ncfo13,indo13,mxddfo,
     x           nchlst,iw)
            ddf14o=keval(cfodd,mxfo,uv,nodddd,ncfo14,indo14,mxddfo,
     x           nchlst,iw)
            ddf15o=keval(cfodd,mxfo,uv,nodddd,ncfo15,indo15,mxddfo,
     x           nchlst,iw)
            ddf22o=keval(cfodd,mxfo,uv,nodddd,ncfo22,indo22,mxddfo,
     x           nchlst,iw)
            ddf23o=keval(cfodd,mxfo,uv,nodddd,ncfo23,indo23,mxddfo,
     x           nchlst,iw)
            ddf24o=keval(cfodd,mxfo,uv,nodddd,ncfo24,indo24,mxddfo,
     x           nchlst,iw)
            ddf25o=keval(cfodd,mxfo,uv,nodddd,ncfo25,indo25,mxddfo,
     x           nchlst,iw)
            ddf33o=keval(cfodd,mxfo,uv,nodddd,ncfo33,indo33,mxddfo,
     x           nchlst,iw)
            ddf34o=keval(cfodd,mxfo,uv,nodddd,ncfo34,indo34,mxddfo,
     x           nchlst,iw)
            ddf35o=keval(cfodd,mxfo,uv,nodddd,ncfo35,indo35,mxddfo,
     x           nchlst,iw)
            ddf44o=keval(cfodd,mxfo,uv,nodddd,ncfo44,indo44,mxddfo,
     x           nchlst,iw)
            ddf45o=keval(cfodd,mxfo,uv,nodddd,ncfo45,indo45,mxddfo,
     x           nchlst,iw)
            ddf55o=keval(cfodd,mxfo,uv,nodddd,ncfo55,indo55,mxddfo,
     x           nchlst,iw)
         endif
         ddf(1,1)=ddf11e+ddf11o
         ddf(1,2)=ddf12e+ddf12o
         ddf(1,3)=ddf13e+ddf13o
         ddf(1,4)=ddf14e+ddf14o
         ddf(1,5)=ddf15e+ddf15o
         ddf(2,2)=ddf22e+ddf22o
         ddf(2,3)=ddf23e+ddf23o
         ddf(2,4)=ddf24e+ddf24o
         ddf(2,5)=ddf25e+ddf25o
         ddf(3,3)=ddf33e+ddf33o
         ddf(3,4)=ddf34e+ddf34o
         ddf(3,5)=ddf35e+ddf35o
         ddf(4,4)=ddf44e+ddf44o
         ddf(4,5)=ddf45e+ddf45o
         ddf(5,5)=ddf55e+ddf55o
         do 30 i=1,5
         do 30 j=i+1,5
            ddf(j,i)=ddf(i,j)
 30      continue
      endif
c ----------------------------------------------------------------------
c     echo results if asked for (i.e. if iw>=1);
c ----------------------------------------------------------------------
      if (iw.ge.1) then
         write(nchlst,6999)
         write(nchlst,6998)
         write(nchlst,6900) fval
         if (ider.ge.1) then
            write(nchlst,6997) 
            write(nchlst,6900) (df(i),i=1,nvar)
         endif
         if (ider.ge.2) then
            write(nchlst,6996) 
            write(nchlst,6900) ((ddf(i,j),j=1,nvar),i=1,nvar)
         endif
         write(nchlst,6991)
 6999    format(/,'SUBR F3DER :')
 6998    format('  f           =')
 6997    format('  df(i)       =')
 6996    format('  ddf(i)      =')
 6991    format('END OF SUBR F3DER '/)
 6900    format(15x,5e15.6)
      endif
c ----------------------------------------------------------------------
c     end of subroutine kf3der;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      real*8 function   keval(cc,icc,uv,nord,nterm,aa,iaa,nchlst,iw)
c ----------------------------------------------------------------------
c     evaluates series expansion;
c     (bvb 12/12/96)
c ----------------------------------------------------------------------
      implicit none
      integer i,iaa,icc,iord,iterm,nchlst,nord,nterm,nvar,iw
      integer aa(iaa,*)
      parameter (nvar=5)
      real*8 cc(icc),r,uv(nvar)
      keval=0.d0
      do 10 iterm=1,nterm
         r=aa(iterm,1)*cc(aa(iterm,2))
         do 12 iord=1,nord
            r=r*uv(aa(iterm,iord+2))
 12      continue
         keval=keval+r
         if (iw.ge.3) then
            write(nchlst,6000) iterm,
     x           (aa(iterm,i),i=1,nord+2)
            write(nchlst,6001) cc(aa(iterm,2)),r,keval
         endif
 6000    format(1x,3i5,5x,8i1)
 6001    format(1x,3d15.5)
 10   continue
c ----------------------------------------------------------------------
c     end of function keval;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
************************************************************************
****
***
**
*    CONCATENATION OF TEXTURE SUBROUTINES FOR CASHFORM MODELS
**
***
****
************************************************************************
c
c     bvb 23/6/97
c
c     subroutines
c
c                  kylp
c                  kdapb
c                  kdbb
c                  kdaprb
c                  kdaijb
c                  kdapbx
c                  kdfb
c                  kddisb
c                  knew
c                  kbe4ini : 13/08/98 initialisation of bea(4)
c
************************************************************************
      subroutine   kylp(hg,scalsg,fg,avg,beag,ang1,cos1,nsteps,
     x     sov,tau,unilen,beui,bea0,cfevn,cfodd,ioff,itry3,ifun,ierr,iw,
     x     svg)
c
c     version only suited for ifun=3 (i.e. using f3der) + 
c     bvb/eh 23/06/97: also passing even and odd order coefficients; 
c                      nout=9 i.e. NT6 (ex.out)
c
c ----------------------------------------------------------------------
c     calculation of yield locus point
c     for a given stress direction by an iterative procedure;
c
c     iteration loop [3]: cutting fish-tails by using different starting
c                         guesses 'bea0()';
c
c     iteration loop [2]: mean normal by improving 'avmean()';
c
c     -> 'ang2ok' : 0.2 degrees (convergence if angle between 'avmean()'
c                               'avmean()' and 'avmid()' smaller than 
c                               'ang2ok');
c     -> 'mxmean' :             (maximum number of iterations);
c 13/08/98
cc     -> 'rel2ok' : 0.0001      (minimum relative improvement);
c      -> 'rel2ok' : 0.02 degrees      (minimum improvement);
c
c     iteration loop [1]: intersection with yield locus by improving
c                         'avg()';
c
c     --> 'ang1ok' : 0.2 degrees (convergence if angle between 'uiv()'
c                                and 'uvg()' smaller than 'ang1ok');
c     --> 'maxstp' : 20          (maximum number of iterations);
c 13/08/98
cc     --> 'rel1ok' : 0.0001      (minimum relative improvement);
c     --> 'rel1ok' : 0.02 degrees      (minimum improvement);
c
c     iteration loop [4]: hardening for hyperplane approximation of
c                         yield locus corresponding to 'avg()';
c
c     --> 'maxtau' :             (maximum number of iterations);
c     --> 'rel4ok' : 0.0001      (minimum relative improvement);
c
c     SOME VARIABLES :
c
c     ioff      : if ioff=0, no offset stress;
c                 if ioff=1, offset stress  inside yield locus;
c                 if ioff=2, offset stress outside yield locus;
c     ifun      : if ifun=1, "fcf" series expansion;
c                 if ifun=2, "gen" series expansion;
c                 if ifun=3, "eao" series expansion;
c     sov(1..5) : offset stress;
ct----------------------------------------------------------------------
ct    Modifications eh/syl for cashform:
ct       10.04.00
ct       -all the empty string '' are replaced by ' ' for compilation
ct        on dec machines   
c ----------------------------------------------------------------------
      implicit none
      integer maxvar
      parameter (maxvar=5)
      integer i,iagain,ibest1(3),ibesttry,iconv1(3),iconv2(3),iconv4(3),
     x     icvg1,icvg2,icvg4,ider,ierr,ifun,ilocal,imean,imeans(3),
     x     ioff,iscale,istep,isteps(3),itau,itaus(3),itry,itry3,
     x     iw,iw1,iw2,j,maxstp,nout,nsteps,nvar
      DOUBLE PRECISION am0p(3,3),ammean(3,3),ang1,ang1ok,
     x     ang1old,ang2,ang2ok,
     x     ang2old,av0(maxvar),av0p(maxvar),avg(maxvar),avmean(maxvar),
     x     avmid(maxvar),bea0(4),beaco(4),beag(4),beui(4),besta1(3),
     x     bestav(3,5),bestbe(3,4),bestc1(3),besth(3),bestf(3),bests(3),
     x     bestr1(3),bestr4(3),bestt(3),bstacm(3,6),bstsca(3),
     x     cfevn(*),cfodd(*),cos1,cos1ok,cos2,cos2ok,
     x     davg(5,4),ddavg(5,10),ddfbeg(10),ddfg(maxvar,maxvar),
     x     ddhbeg(10),dfbeg(4),dfg(maxvar),dhbeg(4),draph,
     x     dsic(6),dsim(3,3),dtau,eg,evg(maxvar),fg,fgor,
     x     hg,hgco,hgold,hmax,hmin,hsi,lasta2(3),lastc2(3),lastr2(3),
     x     psnile,psnimi,r,r1,raph,rel1,rel1ok,rel2,rel2ok,rel4,rel4ok,
     x     scalsg,sg,sic(6),sig,sivavg,sivg(maxvar),sovavg,
     x     sov(maxvar),svg(maxvar),tau,uivavg,uiv(maxvar),uivg(maxvar),
     x     unilen,uvg(maxvar)
cc    DOUBLE PRECISION hfac(4)
      integer itab
      DOUBLE PRECISION tab(3)
      DOUBLE PRECISION dtr,pi,rtd,root23
      character wicvg1*7,wicvg2*7,wicvg4*7,wimean*7,wistep*7,witau*7,
     x     wloop1*44,wloop2*44,wloop4*52,wrel1*10,wrel2*10,wrel4*10,
     x     wstars*15
      data wstars/'***************'/
      integer ifish,maxtau,mxmean
      common /parylp/ ifish,maxtau,mxmean
      DOUBLE PRECISION ac0p(6),acmean(6),delas(6,6),ep,ep0,
     X     epfact,epi,er,
     x     fmean,scale,tmp
      common /wrkylp/ ac0p,acmean,delas,ep,ep0,epfact,epi,er,
     x     fmean,scale,tmp
c 1/4/98      parameter (ang1ok=0.2,rel1ok=0.0001)
c      parameter (ang1ok=0.00001,rel1ok=0.000001)
      parameter (ang1ok=0.0002d0,rel1ok=0.000001d0)  !temp change
c      parameter (ang2ok=0.2,rel2ok=0.0001)
      parameter (ang2ok=0.0002d0,rel2ok=0.0001d0)
      parameter (rel4ok=0.0001d0)
C EH ICOTOM parameter (maxstp=20)
      parameter (maxstp=200)
c      data nout/9/
      integer          out6,out9,outopt
      common /knchout/ out6,out9,outopt
ct----------------------------------------------------------------------
ct    eh/syl: 03.05.00
ct    - Initialise iscale to avoid warnings.  Note that this initialisa-
ct      should disappear if the unnecessary mean normal loop is cleaned 
ct      from this routine.  Same remark for dtau and hgold.
ct    - Initilialise ang1old and ang2old to something to avoid warnings.
ct      No effect of computation since they were initialised at the end
ct      of istep=1 and used only for istep>1.
ct----------------------------------------------------------------------
      iscale=0
      dtau=0.0d0
      hgold=0.0d0
      ang1old=0.0d0
      ang2old=0.0d0
c ----------------------------------------------------------------------
c     bvb 11/08/04: initialise iconv1()
c ----------------------------------------------------------------------
      iconv1(1)=-1
      iconv1(2)=-1
      iconv1(3)=-1

c ----------------------------------------------------------------------
c     calculate some constants;
c ----------------------------------------------------------------------
      nout=out9
      pi=acos(-1.d0)
      rtd=180.d0/pi
      dtr=pi/180.d0
      root23=sqrt(2.d0/3.d0)
      cos1ok=cos(ang1ok*dtr)
      cos2ok=cos(ang2ok*dtr)
      if (scale.eq.1.) iscale=0
c 13/08/98 no input data file
cc    if (ifish.eq.0) itry3=0
c ----------------------------------------------------------------------
c     'nvar=5' is assumed in various subroutines called from kylp;
c ----------------------------------------------------------------------
      nvar=5
c ----------------------------------------------------------------------
c     set sov(1..5)=0. if no offset-stress;
c     set eg=1.
c     calculate 'av0(1..5)'    corresponding to given 'bea0(1..4)';
c     calculate 'av0p(1..5)'   corresponding to given 'ac0p(1..6)';
c               'avmean(1..5)' corresponding to given 'acmean(1..6)';
c               'uiv(1..5)'    corresponding to given 'beui(1..4)';
c     initialize 'iagain';
c ----------------------------------------------------------------------
      if (ioff.eq.0) then
         do 10 i=1,nvar
            sov(i)=0.d0
 10      continue
      endif
      eg=1.d0
cc      c2=1.d0
      call kapbea(av0,bea0,0)
      if (ioff.eq.2.and.mxmean.gt.0) then
         call kmsnc(am0p,ac0p)
         call kvdm(av0p,am0p)
         call kmsnc(ammean,acmean)
         call kvdm(avmean,ammean)
      else
         call kapbea(uiv,beui,0)
      endif
      iagain=0
 9    if (iw.ge.1) then
         write(nout,6999)
         write(nout,6998) ioff
         if (ioff.ne.0) write(nout,6997) (sov(i),i=1,nvar)
         write(nout,6996) mxmean
         if (ioff.eq.2.and.mxmean.gt.0) then
            write(nout,6995) (av0p(i),i=1,nvar)
            write(nout,6994) (avmean(i),i=1,nvar)
         else
            write(nout,6993) unilen
            write(nout,6992) (beui(i)*rtd,i=1,4)
            write(nout,6991) (uiv(i),i=1,nvar)
         endif
         write(nout,6990) maxtau
         write(nout,6989) tau
         write(nout,6988) (bea0(i)*rtd,i=1,4)
         write(nout,6955) (av0(i),i=1,nvar)
         write(nout,6987) itry3
         write(nout,6986) ifun
         write(nout,6950) mxmean,1.0d0-cos2ok,ang2ok,rel2ok,maxstp,
     x        1.0d0-cos1ok,ang1ok,rel1ok,maxtau,rel4ok
         write(nout,6949) 
         write(nout,6940) wstars,wstars,wstars,wstars,wstars,
     x        wstars,wstars,wstars,wstars,wstars
      endif
c ----------------------------------------------------------------------
c     start of iteration loop [3];
c     cutting fish-tails by using different starting guesses 'bea0()';
c ----------------------------------------------------------------------
      nsteps=0
      itry=0
 3    itry=itry+1
      ilocal=0
      if (itry.eq.2) bea0(4)=bea0(4)-60.d0*dtr
      if (itry.eq.3) bea0(4)=bea0(4)+120.d0*dtr
      do 30 i=1,4
         beag(i)=bea0(i)
 30   continue
      if (iw.ge.3) then
         write(nout,6985) itry
         write(nout,6988) (bea0(i)*rtd,i=1,4)
      endif
c ----------------------------------------------------------------------
c     start of iteration loop [2];
c     mean normal by improving 'avmean()';
c ----------------------------------------------------------------------
      icvg2=-1
      imean=0
 2    imean=imean+1
      if (ioff.eq.2.and.mxmean.gt.0) then
         do 200 i=1,6
            r=0.0d0
            do 202 j=1,6
               r=r+delas(i,j)*acmean(j)
 202        continue
            sic(i)=-r
 200     continue
         call kdhc(dsic,hsi,sic)
         call kmssc(dsim,dsic)
         call kbetam(beui,r,dsim,0)
         call kapbea(uiv,beui,0)
         scale=1.0d0
         if (iscale.eq.1) scale=unilen/r
         ider=0
         iw1=0
         if (iw.ge.5) iw1=1
cc         if (ifun.eq.1) call kf1der(fmean,dfg,ddfg,avmean,ider,iw1)
cc         if (ifun.eq.2) call kf2der(fmean,dfg,ddfg,avmean,nvar,ider,iw1)
         if (ifun.eq.3) call kf3der(fmean,dfg,ddfg,avmean,cfevn,cfodd,
     x      nvar,ider,nout,iw1)
         epfact=fmean*scale
         if (iw.ge.3) then
            write(nout,6984) imean
            write(nout,6994) (avmean(i),i=1,nvar)
            write(nout,6983) fmean
            write(nout,6993) unilen
            write(nout,6992) (beui(i)*rtd,i=1,4)
            write(nout,6991) (uiv(i),i=1,nvar)
         endif
         do 204 i=1,6
            bstacm(itry,i)=acmean(i)
 204     continue
         bstsca(itry)=scale
      endif
c ----------------------------------------------------------------------
c     start of iteration loop [1];
c     intersection with yield locus by improving 'avg()';
c ----------------------------------------------------------------------
      icvg1=-1
      istep=0
 1    istep=istep+1
      nsteps=nsteps+1
c ----------------------------------------------------------------------
c     calculate 'avg(1..5)' corresponding to 'beag(1..4)';
c     calculate distortion factors in 4-d space for 'beag(1..4)';
c ----------------------------------------------------------------------
      call kapbea(avg,beag,0)
cc test
ccc      call kdistor(hfac,beag,iw)
c ----------------------------------------------------------------------
c     reverse the sign of this plastic strain rate mode
c     if at wrong side of yield locus;
c ----------------------------------------------------------------------
      call kdotv(uivavg,uiv,avg)
      if ((ioff.ne.2.and.uivavg.lt.0.d0).or.
     +     (ioff.eq.2.and.uivavg.gt.0.d0)) then
         if (beag(4).lt.pi) then
            beag(4)=beag(4)+pi
         else
            beag(4)=beag(4)-pi
         endif
         do 40 i=1,nvar
            avg(i)=-avg(i)
 40      continue
         uivavg=-uivavg
       endif
c ----------------------------------------------------------------------
c     calculate 'evg(1..5)' from 'avg(1..5)' and 'eg'; 
c ----------------------------------------------------------------------
       do 50 i=1,nvar
          evg(i)=avg(i)*eg
 50    continue
c ----------------------------------------------------------------------
c     calculate 'fg'       : value of series expansion for 'avg(1..5)';
c               'dfg(..)'  : first  order partial derivatives of series
c                            expansion;
c               'ddfg(..)' : second order partial derivatives of series
c                            expansion;
c ----------------------------------------------------------------------
      ider=2
      iw1=0
      if (iw.ge.5) iw1=1
cc      if (ifun.eq.1) call kf1der(fg,dfg,ddfg,avg,ider,iw1)
cc      if (ifun.eq.2) call kf2der(fg,dfg,ddfg,avg,nvar,ider,iw1)
      if (ifun.eq.3) call kf3der(fg,dfg,ddfg,avg,cfevn,cfodd,
     x     nvar,ider,nout,iw1)
c ----------------------------------------------------------------------
c     iteration loop [4];
c     Newton-Raphson iteration to solve 'hg' and 'tau' from non-linear
c     relation 'sov+hg*siv=tau(hg)*fg';
c ----------------------------------------------------------------------
      sivavg=unilen*uivavg
      call kdotv(sovavg,sov,avg)
      itau=0
      if (ioff.eq.2.and.maxtau.gt.0) then
         hg=0.0d0
         icvg4=-1
 4       itau=itau+1
         epi=epfact*hg
         ep=ep0+epi
cc         call kystres(ep,er,tmp,tau,dtau)
         dtau=epfact*dtau
         raph=sovavg+hg*sivavg-tau*fg
         draph=sivavg-dtau*fg
         hgco=-raph/draph
         if (itau.gt.1) rel4=hgco/hgold
         hg=hg+hgco
         hgold=hg
         if (itau.gt.1.and.abs(rel4).lt.rel4ok) icvg4=2
         if (itau.eq.maxtau) icvg4=0
         if (iw.ge.2) then
            wrel4=' '
            wicvg4=' '
            write(witau,1999) itau
            if (itau.gt.1) write(wrel4,1998) rel4
            if (icvg4.ge.0) write(wicvg4,1999) icvg4
            write(wloop4,1997) witau,hg,tau,wrel4,wicvg4
         endif
         if (icvg4.ge.0) goto 49
         if (iw.ge.2) write(nout,6948) wloop4
         goto 4
      else
         hg=(tau*fg-sovavg)/sivavg
cc         hg=(c1*fg**c2-sovavg)/sivavg
         if (iw.ge.2) then
            wrel4=' '
            wicvg4=' '
            witau=' '
            write(wloop4,1997) witau,hg,tau,wrel4,wicvg4
         endif
      endif
 49   epi=epfact*hg
      ep=ep0+epi
      psnile=scale*hg
      psnimi=root23*psnile
c ----------------------------------------------------------------------
c     store original value of 'fg' in 'fgor' and modify value of 'fg'
c     to take into account offset stress;
c ----------------------------------------------------------------------
      fgor=fg
      fg=hg*uivavg
c ----------------------------------------------------------------------
c     calculate yield stress 'svg(1..5)' corresponding to 'avg(1..5)';
c               stress mode  'uvg(1..5)' corresponding to 'svg(1..5)';
c ----------------------------------------------------------------------
cc      iw1=0
cc      if (iw.ge.5) iw1=1
cc      if (ifun.eq.1) then
cc        call kspap(svg,scf,nfoder,avg,iw1)
cc        do 51 i=1,nvar
cc           svg(i)=tau*svg(i)
cc 51     continue
cc        call kupsmsp(uvg,sg,svg)
cc      endif
      r=0.0d0
      do 52 i=1,nvar
         r=r+dfg(i)*avg(i)
 52   continue
      r=fgor-r
      do 54 i=1,nvar
cc         svg(i)=c1*(dfg(i)+avg(i)*r)
         svg(i)=tau*(dfg(i)+avg(i)*r)
 54   continue
      call kupsmsp(uvg,sg,svg)
c ----------------------------------------------------------------------
c     calculate increment  'sivg(1..5)' from 'sov(1..5)' to 'svg(1..5)';
c               stress mode 'uivg(1..5)' corresponding to 'sivg(1..5)';
c ----------------------------------------------------------------------
      do 60 i=1,nvar
         sivg(i)=svg(i)-sov(i)
 60   continue
      call kupsmsp(uivg,sig,sivg)
c ----------------------------------------------------------------------
c     calculate scalar product 'cos1' of 'uiv(1..5)' and 'uivg(1..5)',
c     i.e. the cosine of the angle between these two unit vectors;
c     also calculate this angle itself, 'ang1', and the relative 
c     change of 'ang1' with respect to its previous value 'ang1old';
c ----------------------------------------------------------------------
      call kdotv(cos1,uiv,uivg)
      if (cos1.lt.-1.0d0) cos1=-1.0d0
      if (cos1.gt.1.0d0) cos1=1.0d0
      ang1=acos(cos1)*rtd
c 13/08/98
cc      if (istep.gt.1) rel1=(ang1-ang1old)/ang1old
      if (istep.gt.1) rel1=ang1-ang1old
      ang1old=ang1
      if (iw.ge.3) then
         write(nout,6982) istep
         write(nout,6981) (evg(i),i=1,nvar)
         write(nout,6980) eg
         write(nout,6979) (avg(i),i=1,nvar)
         write(nout,6978) (beag(i)*rtd,i=1,4)
ccc         write(nout,6900) (hfac(i),i=1,4)
         write(nout,6977) fgor
         write(nout,6976) hg
         write(nout,6975) (svg(i),i=1,nvar)
         write(nout,6974) sg
         write(nout,6973) (uvg(i),i=1,nvar)
         write(nout,6972) (sivg(i),i=1,nvar)
         write(nout,6971) sig
         write(nout,6970) (uivg(i),i=1,nvar)
      endif
c ----------------------------------------------------------------------
c     keep track of best solution of iteration loop [1];
c ----------------------------------------------------------------------
      if (istep.eq.1.or.cos1.gt.bestc1(itry)) then
         ibest1(itry)=istep
         bests(itry)=sg
         bestf(itry)=fgor
         do 70 i=1,nvar
            bestav(itry,i)=avg(i)
 70      continue
         do 80 i=1,4
            bestbe(itry,i)=beag(i)
 80      continue
         bestc1(itry)=cos1
         besta1(itry)=ang1
         bestr1(itry)=rel1
         itaus(itry)=itau
         besth(itry)=hg
         bestt(itry)=tau
         bestr4(itry)=rel4
         iconv4(itry)=icvg4
      endif
c ----------------------------------------------------------------------
c     stop iteration loop [1] if convergence criterion is satisfied,
c     or if relative improvement is too small,
c     or if maximum number of iteration steps 'maxstp' reached;
c ----------------------------------------------------------------------
      if (istep.eq.maxstp) icvg1=0
      if (istep.gt.1.and.abs(rel1).lt.rel1ok) then
         if (ilocal.eq.4) icvg1=2
         if (ilocal.eq.2) ilocal=3
         if (ilocal.eq.0) ilocal=1
      endif
      if (cos1.gt.cos1ok) icvg1=1
      if (iw.ge.2) then
         wrel1=' '
         wicvg1=' '
         write(wistep,1999) istep
         if (istep.gt.1) write(wrel1,1998) rel1
         if (icvg1.ge.0) write(wicvg1,1999) icvg1
         write(wloop1,1996) wistep,1.0d0-cos1,ang1,wrel1,wicvg1
         write(nout,6969) wloop1,wloop4
      endif
      if (icvg1.ge.0) goto 19
      if (ilocal.eq.1) then
         do 82 i=1,4
            beaco(i)=0.0d0
 82      continue
cc         beaco(4)=pi/36
cc         if (iw.ge.3) write(nout,6945) ilocal
         beaco(3)=0.25d0*pi
         if (iw.ge.3) write(nout,6947) ilocal
         ilocal=2
         goto 17
      endif
      if (ilocal.eq.3) then
         if (iw.ge.3) write(nout,6946) ilocal
         ilocal=4
      endif
c ----------------------------------------------------------------------
c     modify partial derivatives 'dfg(..)' and 'ddfg(..)' to take into 
c     account offset stress 'sov(1..5)';
c ----------------------------------------------------------------------
      r1=1.0d0/unilen
cc      r2=c1*c2*fgor**(c2-1.d0)
cc      r3=(c2-1.d0)/fgor
      do 90 i=1,nvar
         do 92 j=i,nvar
cc            r=r2*(r3*dfg(i)*dfg(j)+ddfg(i,j))*r1
            r=tau*ddfg(i,j)*r1
            ddfg(i,j)=r
            ddfg(j,i)=r
 92      continue
 90   continue
      do 100 i=1,nvar
cc         dfg(i)=(r2*dfg(i)-sov(i))*r1
         dfg(i)=(tau*dfg(i)-sov(i))*r1
 100  continue
c ----------------------------------------------------------------------
c     calculate new guess 'beag(1..4)';
c ----------------------------------------------------------------------
      iw1=0
      if (iw.ge.5) iw1=1
      call kdapb(davg,ddavg,beag,iw1)
      iw1=0
      iw2=0
      if (iw.ge.5) then
         iw1=1
         iw2=1
      endif
      call kdfb(dfbeg,ddfbeg,davg,ddavg,dfg,ddfg,iw1,iw2)
      iw1=0
      iw2=0
      if (iw.ge.4) then
         iw1=1
         iw2=1
      endif
      call kddisb(dhbeg,ddhbeg,fg,dfbeg,ddfbeg,avg,davg,ddavg,uiv,
     x     iw1,iw2)
      iw1=0
      if (iw.ge.4) iw1=1
cc test 5 Jan 1994
      if (iw.ge.3.and.ilocal.gt.0) iw1=1
      call knew(beaco,dhbeg,ddhbeg,iw1)
 17   do 110 i=1,4
         beag(i)=beag(i)+beaco(i)
 110  continue
      if (iw.ge.3) write(nout,6968) (beaco(i)*rtd,i=1,4)
      goto 1
c ----------------------------------------------------------------------
c     end of iteration loop [1];
c ----------------------------------------------------------------------
 19   isteps(itry)=istep
      iconv1(itry)=icvg1
      wloop2=' '
      if (ioff.eq.2.and.mxmean.gt.0) then
         istep=ibest1(itry)
         do 210 i=1,nvar
            avg(i)=bestav(itry,i)
 210     continue
         call kdotv(r,av0p,avg)
         r=1.d0/sqrt(2.d0+2.d0*r)
         do 220 i=1,nvar
            avmid(i)=(av0p(i)+avg(i))*r
 220     continue
         call kdotv(cos2,avmean,avmid)
         if (cos2.lt.-1.0d0) cos2=-1.0d0
         if (cos2.gt.1.0d0) cos2=1.0d0
         ang2=acos(cos2)*rtd
c 13/08/98
cc         if (imean.gt.1) rel2=(ang2-ang2old)/ang2old
         if (imean.gt.1) rel2=ang2-ang2old
         ang2old=ang2
         if (iw.ge.3) then
            write(nout,6984) imean
            write(nout,6995) (av0p(i),i=1,nvar)
            write(nout,6979) (avg(i),i=1,nvar)
            write(nout,6967) (avmid(i),i=1,nvar)
            write(nout,6994) (avmean(i),i=1,nvar)
         endif
c ----------------------------------------------------------------------
c     stop iteration loop [2] if convergence criterion is satisfied,
c     or if relative improvement is too small,
c     or if maximum number of iteration steps 'maxstp' reached;
c ----------------------------------------------------------------------
         if (imean.eq.mxmean) icvg2=0
         if (imean.gt.1.and.abs(rel2).lt.rel2ok) icvg2=2
         if (cos2.gt.cos2ok) icvg2=1
         if (iw.ge.2) then
            wrel2=' '
            wicvg2=' '
            write(wimean,1999) imean
            if (imean.gt.1) write(wrel2,1998) rel2
            if (icvg2.ge.0) write(wicvg2,1999) icvg2
            write(wloop2,1996) wimean,1.0d0-cos2,ang2,wrel2,wicvg2
            wloop1=' '
            write(wloop1,1995) istep
            write(nout,6966) wloop2,wloop1
         endif
         if (icvg2.ge.0) goto 29
         do 230 i=1,5
            avmean(i)=avmid(i)
 230     continue
         call kmdv(ammean,avmean)
         call kcsnm(acmean,ammean)
         goto 2
      endif
c ----------------------------------------------------------------------
c     end of iteration loop [2];
c ----------------------------------------------------------------------
 29   imeans(itry)=imean
      iconv2(itry)=icvg2
      lastc2(itry)=cos2
      lasta2(itry)=ang2
      lastr2(itry)=rel2
      if (iw.ge.2) then
         i=isteps(itry)
         istep=ibest1(itry)
         cos1=bestc1(itry)
         ang1=besta1(itry)
         rel1=bestr1(itry)
         icvg1=iconv1(itry)
         wrel1=' '
         write(wistep,1995) istep
         if (i.gt.1) write(wrel1,1998) rel1
         write(wicvg1,1999) icvg1
         write(wloop1,1996) wistep,1-cos1,ang1,wrel1,wicvg1
         itau=itaus(itry)
         hg=besth(itry)
         tau=bestt(itry)
         rel4=bestr4(itry)
         icvg4=iconv4(itry)
         wrel4=' '
         wicvg4=' '
         if (itau.gt.1) write(wrel4,1998) rel4
         if (maxtau.gt.0) then
            write(witau,1999) itau
            write(wicvg4,1999) icvg4
         endif
         write(wloop4,1997) witau,hg,tau,wrel4,wicvg4
         write(nout,6959) itry,wloop2,wloop1,wloop4
         write(nout,6940) wstars,wstars,wstars,wstars,wstars,
     x        wstars,wstars,wstars,wstars,wstars
      endif
c ----------------------------------------------------------------------
c     stop iteration loop [3] if 1 starting guess is to be considered 
c     and convergence is reached with it,
c     or if maximum number of starting guesses is reached;
c     remark : if only one starting guess to be considered (itry3=0) 
c     and no convergence obtained with this starting guess, override 
c     original value of "itry3" and try with two other starting guesses;
c ----------------------------------------------------------------------
      if (itry3.eq.0.and.iconv1(1).eq.1) goto 39
c+1 (23/7/04 bvb sr) icvg1=2 is OK (i.e. accept stable solution)
      if (itry3.eq.0.and.iconv1(1).eq.2) goto 39
      itry3=1
      if (itry.eq.3) goto 39
      goto 3
c ----------------------------------------------------------------------
c     end of iteration loop [3];
c ----------------------------------------------------------------------
c ----------------------------------------------------------------------
c     determine error level "ierr";
c     "ierr=0" : at least one iteration procedure leads to a
c                solution satisfying the convergence criterion;
c     "ierr=1" : none of the 3 iteration procedures leads to a 
c                solution satisfying the convergence criterion,
c                but at least one of them leads to a stable solution;
c     "ierr=2" : none of the 3 iteration procedures leads to a
c                solution satisfying the convergence criterion
c                or a stable solution;
c ----------------------------------------------------------------------
 39   ierr=1
      if (iconv1(1).eq.0.and.iconv1(2).eq.0.and.iconv1(3).eq.0) ierr=2
      if (iconv1(1).eq.1.or.iconv1(2).eq.1.or.iconv1(3).eq.1) ierr=0
c ----------------------------------------------------------------------
c     if "ierr=2" : re-run the iteration procedures once, 
c     this time printing intermediate results;
c ----------------------------------------------------------------------
      if (ierr.eq.2.and.iagain.eq.0) then
         iagain=1
         iw=3
         bea0(4)=bea0(4)-60.d0*dtr
         goto 9
      endif
c ----------------------------------------------------------------------
c     if "ierr=0" : only the solutions satisfying the convergence 
c     criterion will be considered further on;
c ----------------------------------------------------------------------
      if (ierr.eq.0) then
         if (iconv1(1).eq.2) iconv1(1)=0
         if (iconv1(2).eq.2) iconv1(2)=0
         if (iconv1(3).eq.2) iconv1(3)=0
      endif
c-1 (23/3/99)
c    if (ierr.eq.0.or.ierr.eq.1) then
c bvb   
      if (ierr.eq.0.or.ierr.eq.1) then
c ----------------------------------------------------------------------
c     make a choice between the iteration runs in order to use the 
c     corresponding results as final results of this subroutine;
c ----------------------------------------------------------------------
         if (itry3.eq.0) then
            ibesttry=1
         else
c ----------------------------------------------------------------------
c     in case of 3 starting guesses (itry3=1), the one leading to the
c     stress point closest to the stress origin is chosen in order to 
c     deal with fish-tail-situations;
c ----------------------------------------------------------------------
            ibesttry=0

            if (ioff.ne.2) then

               itab=0
               do i=1,3
                  if(besth(i).ge.0.0D0) then
                     itab=itab+1
                 tab(itab)=besth(i)
                  endif
               enddo
               hmin=tab(1)
               if(itab.gt.1) then
                  do i=2,itab
                     if(tab(i).lt.hmin) hmin=tab(i)
                  enddo
               endif
               do i=1,3
                  if(besth(i).eq.hmin) ibesttry=i
               enddo

ccc            hmin=9999999.d0
ccc            do 120 i=1,3
cc test 5 Jan 1994
cc                  if (iconv1(i).ne.0.
cc     x                 and.besth(i).gt.0.d0.and.besth(i).lt.hmin) then
ccc               if (besth(i).gt.0.d0.and.besth(i).lt.hmin) then
ccc                  hmin=besth(i)
ccc                  ibesttry=i
ccc               endif
ccc 120           continue
            endif
            if (ioff.eq.2) then
               hmax=0.0d0
               do 130 i=1,3
cc test 5 Jan 1994
cc                  if (iconv1(i).ne.0.
cc     x                 and.besth(i).gt.0.d0.and.besth(i).gt.hmax) then
                  if (besth(i).gt.0.d0.and.besth(i).gt.hmax) then
                     hmax=besth(i)
                     ibesttry=i
                  endif
 130           continue
            endif
c-1 (23/3/99)
c         endif
c bvb 
         endif  
c ----------------------------------------------------------------------
c     the results of the best iteration solution
c     are taken to be the final results of this subroutine;
c ----------------------------------------------------------------------
*         WRITE(nout,*) 'HERE, ibesttry:', ibesttry
*         WRITE(nout,*) 'HERE, fg:', fg
*         WRITE(nout,*) 'HERE, sg:', sg
         hg=besth(ibesttry)
         sg=bests(ibesttry)
         scalsg=sg/tau
         fg=bestf(ibesttry)
         do 140 i=1,nvar
            avg(i)=bestav(ibesttry,i)
 140     continue
         do 150 i=1,4
            beag(i)=bestbe(ibesttry,i)
 150     continue
         cos1=bestc1(ibesttry)
         ang1=besta1(ibesttry)
         rel1=bestr1(ibesttry)
         do 152 i=1,6
            acmean(i)=bstacm(ibesttry,i)
 152     continue
         scale=bstsca(ibesttry)
      endif
      if (iw.ge.1) then
         write(nout,6954) 
         do 154 itry=1,1+itry3*2
            write(nout,6953) itry,besth(itry),bests(itry),bestf(itry),
     x           (bestav(itry,i),i=1,nvar),(bestbe(itry,i)*rtd,i=1,4)
 154     continue
         write(nout,6952) ibesttry,ierr
         write(nout,6939) 
      endif
*      WRITE(nout,*) 'AT END OF KYLP; fg=',fg
*      WRITE(nout,*) 'AT END OF KYLP; scalsg=',scalsg
 6999 format(/,'***start of subroutine KYLP***')
 6998 format(5x,'ioff        =',i15)
 6997 format(5x,'sov(i)      =',5e15.6)
 6996 format(5x,'mxmean      =',i15)
 6995 format(5x,'av0p(i)     =',5e15.6)
 6994 format(5x,'avmean(i)   =',5e15.6)
 6993 format(5x,'unilen      =',e15.6)
 6992 format(5x,'beui(i)     =',4e15.6)
 6991 format(5x,'uiv(i)      =',5e15.6)
 6990 format(5x,'maxtau      =',i15)
 6989 format(5x,'tau         =',e15.6)
 6988 format(5x,'bea0(i)     =',4e15.6)
 6955 format(5x,'av0(i)      =',5e15.6)
 6987 format(5x,'itry3       =',i15)
 6986 format(5x,'ifun        =',i15)
 6950 format(/8x,'mxmean',2x,'1-cos2ok',4x,'ang2ok',4x,'rel2ok',
     x     8x,'maxstp',2x,'1-cos1ok',4x,'ang1ok',4x,'rel1ok',
     x     8x,'maxtau',32x,'rel4ok',
     x     /7x,i7,2e10.2,f10.5,7x,i7,2e10.2,f10.5,
     x     7x,i7,28x,f10.5)
 6949 format(/3x,'itry',2x,'imean',4x,'1-cos2',6x,'ang2',6x,'rel2',2x,
     x     'icvg2',2x,'istep',4x,'1-cos1',6x,'ang1',6x,'rel1',2x,
     x     'icvg1',3x,'itau',12x,'hg',11x,'tau',6x,'rel4',2x,'icvg4')
 6985 format(5x,'**itry**      =',i4)
 6984 format(5x,'**imean**     =',i4)
 6983 format(5x,'fmean         =',e15.6)
 6948 format(95x,a52)
 6982 format(5x,'**istep**     =',i4)
 6981 format(5x,'evg(i)      =',5e15.6)
 6980 format(5x,'eg          =',e15.6)
 6979 format(5x,'avg(i)      =',5e15.6)
 6978 format(5x,'beag(i)     =',4e15.6)
ccc 6900 format(5x,'hfac(i)     =',4e15.6)
 6977 format(5x,'fg          =',e15.6)
 6976 format(5x,'hg          =',e15.6)
 6975 format(5x,'svg(i)      =',5e15.6)
 6974 format(5x,'sg          =',e15.6)
 6973 format(5x,'uvg(i)      =',5e15.6)
 6972 format(5x,'sivg(i)     =',5e15.6)
 6971 format(5x,'sig         =',e15.6)
 6970 format(5x,'uivg(i)     =',5e15.6)
 6969 format(51x,a44,a52)
 6947 format(1x,'ilocal=',i1,' : try modifiying beag(3) by 45 degrees')
 6946 format(1x,'ilocal=',i1,' : try once more')
 6945 format(1x,'ilocal=',i1,' : try modifiying beag(4) by 5 degrees')
 6968 format(5x,'beaco(i)    =',4e15.6)
 6967 format(5x,'avmid(i)    =',5e15.6)
 6966 format(7x,a44,a44)
 6964 format(i7)
 6959 format(i7,a44,a44,a52)
 6954 format(/3x,'itry',1x,'besth(itry)',4x,'bests(itry)',1x,
     x     'bestf(itry)',2x,'bestav(itry,1..5)',35x,
     x     'bestbe(itry,1..4)')
 6953 format(i7,f12.6,e15.6,f12.6,5f10.6,4f12.6)
 6952 format(5x,'ibesttry  = ',i20,
     x     /5x,'ierr      = ',i20,/)
 6940 format(3x,9a15,a9)
 6939 format('***end of subroutine KYLP***'/)
 1999 format(i7)
 1998 format(f10.5)
 1997 format(a7,2e14.5,a10,a7)
 1996 format(a7,2e10.2,a10,a7)
 1995 format(i5)
c ----------------------------------------------------------------------
c     end of subroutine kylp;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kdapb(dap,ddap,beta,iw1)

      implicit none
      integer iw1
      real*8 ap(5),dap(5,4),ddap(5,10),beta(4),
     x     b(3,3),db(3,3,3),ddb(3,3,6),
     x     apr(3),dapr(3),ddapr(3),
     x     aij(5),daij(5,4),ddaij(5,10)

      call kdbb(b,db,ddb,beta(1),beta(2),beta(3),0,0,0)
      call kdaprb(apr,dapr,ddapr,beta(4),0,0,0)
      call kdaijb(aij,daij,ddaij,b,db,ddb,apr,dapr,ddapr,0,0,0)
      call kdapbx(ap,dap,ddap,aij,daij,ddaij,iw1,iw1,iw1)

      return
      end
************************************************************************
      subroutine   kdbb(b,db,ddb,beta1,beta2,beta3,iw1,iw2,iw3)

      implicit none
      integer i,ir,irs,is,iw1,iw2,iw3,j,nout
      real*8 b(3,3),db(3,3,3),ddb(3,3,6),beta1,beta2,beta3,
     x     s1s2,s1c2,c1s2,c1c2,
     x     s1s3,s1c3,c1s3,c1c3,
     x     s2s3,s2c3,c2s3,c2c3,
     x     sss,scs,css,ccs,ssc,scc,csc,ccc,
     x     s1,c1,s2,c2,s3,c3
c     data nout/9/
      integer          out6,out9,outopt
      common /knchout/ out6,out9,outopt
c ----------------------------------------------------------------------
c     calculate some sines, cosines and product terms;
c ----------------------------------------------------------------------
      nout=out9
      s1=sin(beta1)
      c1=cos(beta1)
      s2=sin(beta2)
      c2=cos(beta2)
      s3=sin(beta3)
      c3=cos(beta3)

      s1s2=s1*s2
      s1c2=s1*c2
      c1s2=c1*s2
      c1c2=c1*c2

      s1s3=s1*s3
      s1c3=s1*c3
      c1s3=c1*s3
      c1c3=c1*c3

      s2s3=s2*s3
      s2c3=s2*c3
      c2s3=c2*s3
      c2c3=c2*c3

      sss= s1*s2*s3
      scs= s1*c2*s3
      css= c1*s2*s3
      ccs= c1*c2*s3
      ssc= s1*s2*c3
      scc= s1*c2*c3
      csc= c1*s2*c3
      ccc= c1*c2*c3
c ----------------------------------------------------------------------
c     calculate 'b';
c ----------------------------------------------------------------------
      b(1,1)= c1c3-scs
      b(2,1)=-c1s3-scc
      b(3,1)= s1s2
      b(1,2)= s1c3+ccs
      b(2,2)=-s1s3+ccc
      b(3,2)=-c1s2
      b(1,3)= s2s3
      b(2,3)= s2c3
      b(3,3)= c2
c ----------------------------------------------------------------------
c     calculate 'db';
c ----------------------------------------------------------------------
      db(1,1,1)=-s1c3-ccs
      db(2,1,1)= s1s3-ccc
      db(3,1,1)= c1s2
      db(1,2,1)= c1c3-scs
      db(2,2,1)=-c1s3-scc
      db(3,2,1)= s1s2
      db(1,3,1)= 0.d0
      db(2,3,1)= 0.d0
      db(3,3,1)= 0.d0

      db(1,1,2)= sss
      db(2,1,2)= ssc
      db(3,1,2)= s1c2
      db(1,2,2)=-css
      db(2,2,2)=-csc
      db(3,2,2)=-c1c2
      db(1,3,2)= c2s3
      db(2,3,2)= c2c3
      db(3,3,2)=-s2

      db(1,1,3)=-c1s3-scc
      db(2,1,3)=-c1c3+scs
      db(3,1,3)= 0.d0
      db(1,2,3)=-s1s3+ccc
      db(2,2,3)=-s1c3-ccs
      db(3,2,3)= 0.d0
      db(1,3,3)= s2c3
      db(2,3,3)=-s2s3
      db(3,3,3)= 0.d0
c ----------------------------------------------------------------------
c     calculate 'ddb';
c ----------------------------------------------------------------------
      ddb(1,1,1)=-c1c3+scs
      ddb(2,1,1)= c1s3+scc
      ddb(3,1,1)=-s1s2
      ddb(1,2,1)=-s1c3-ccs
      ddb(2,2,1)= s1s3-ccc
      ddb(3,2,1)= c1s2
      ddb(1,3,1)= 0.d0
      ddb(2,3,1)= 0.d0
      ddb(3,3,1)= 0.d0

      ddb(1,1,4)=+scs
      ddb(2,1,4)=+scc
      ddb(3,1,4)=-s1s2
      ddb(1,2,4)=-ccs
      ddb(2,2,4)=-ccc
      ddb(3,2,4)= c1s2
      ddb(1,3,4)=-s2s3
      ddb(2,3,4)=-s2c3
      ddb(3,3,4)=-c2

      ddb(1,1,6)=-c1c3+scs
      ddb(2,1,6)= c1s3+scc
      ddb(3,1,6)= 0.d0
      ddb(1,2,6)=-s1c3-ccs
      ddb(2,2,6)= s1s3-ccc
      ddb(3,2,6)= 0.d0
      ddb(1,3,6)=-s2s3
      ddb(2,3,6)=-s2c3
      ddb(3,3,6)= 0.d0

      ddb(1,1,2)=+css
      ddb(2,1,2)=+csc
      ddb(3,1,2)= c1c2
      ddb(1,2,2)=+sss
      ddb(2,2,2)=+ssc
      ddb(3,2,2)= s1c2
      ddb(1,3,2)= 0.d0
      ddb(2,3,2)= 0.d0
      ddb(3,3,2)= 0.d0

      ddb(1,1,3)= s1s3-ccc
      ddb(2,1,3)= s1c3+ccs
      ddb(3,1,3)= 0.d0
      ddb(1,2,3)=-c1s3-scc
      ddb(2,2,3)=-c1c3+scs
      ddb(3,2,3)= 0.d0
      ddb(1,3,3)= 0.d0
      ddb(2,3,3)= 0.d0
      ddb(3,3,3)= 0.d0

      ddb(1,1,5)=+ssc
      ddb(2,1,5)=-sss
      ddb(3,1,5)= 0.d0
      ddb(1,2,5)=-csc
      ddb(2,2,5)=+css
      ddb(3,2,5)= 0.d0
      ddb(1,3,5)= c2c3
      ddb(2,3,5)=-c2s3
      ddb(3,3,5)= 0.d0
c ----------------------------------------------------------------------
c     echo information;
c ----------------------------------------------------------------------
      if (iw1.ge.1) then
         write(nout,9999)
         do 10 j=1,3
         do 10 i=1,3
            write(nout,9998) i,j,b(i,j)
 10      continue
      endif
      if (iw2.ge.1) then
         write(nout,9997)
         do 20 ir=1,3
            do 22 j=1,3
            do 22 i=1,3
               write(nout,9996) i,j,ir,db(i,j,ir)
 22         continue
            write(nout,9995)
 20      continue
      endif
      if (iw3.ge.1) then
         write(nout,9994)
         irs=0
         do 30 ir=1,3
         do 30 is=ir,3
            irs=irs+1
            do 32 j=1,3
            do 32 i=1,3
               write(nout,9993) i,j,irs,ddb(i,j,irs),ir,is
 32         continue
            write(nout,9992)
 30      continue
      endif
 9999 format(////1x,' b(i,j)       : transformation matrix ',
     x     /21x,'between reference system of tensor a',
     x     /21x,'and     principal directions of a',
     x     /21x,'  a(i,j) = b(k,i)*b(k,j)*apr(k)    i,j,k = 1..3'/)
 9998 format(1x,'   b(',i1,',',i1,')     =',e15.6)
 9997 format(////1x,' db(i,j,r)    : 1st order partial derivatives',
     x     /21x,'of  b(i,j)     i,j = 1..3',
     x     /21x,'to  beta(r)    r   = 1..3'/)
 9996 format(1x,'   db(',i1,',',i1,',',i1,')  =',e15.6)
 9995 format(1x)
 9994 format(////1x,' ddb(i,j,rs)  : 1st order partial derivatives',
     x     /21x,'of  b(i,j)            i,j = 1..3',
     x     /21x,'to  beta(r)beta(s)    r   = 1..3',
     x     /21x,'                      s   = r..3',
     x     /21x,'                      (rs = 1..6)'/,
     x     /21x,'                 r s'/)
 9993 format(1x,'   ddb(',i1,',',i1,',',i2,') =',e15.6,i7,i2)
 9992 format(1x)
c ----------------------------------------------------------------------
c     end of subroutine kdbb;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kdaprb(apr,dapr,ddapr,beta4,iw1,iw2,iw3)
c ----------------------------------------------------------------------
c     calculate 'apr','dapr','ddapr' for a given 'beta4';
c ----------------------------------------------------------------------
      implicit none
      integer iw1,iw2,iw3,k,nout
      real*8 apr(3),b1,b2,beta4,dapr(3),ddapr(3)
      real*8 pi,root23
c     data nout/9/
      integer          out6,out9,outopt
      common /knchout/ out6,out9,outopt
c ----------------------------------------------------------------------
c     calculate some constants;
c ----------------------------------------------------------------------
      nout=out9
      pi=acos(-1.d0)
      b1=beta4-pi/3.d0
      b2=beta4+pi/3.d0
      root23=sqrt(2.d0/3.d0)
c ----------------------------------------------------------------------
c     calculate 'apr';
c ----------------------------------------------------------------------
      apr(1)=   root23*cos(b1)
      apr(2)=   root23*cos(b2)
      apr(3)=  -root23*cos(beta4)
c ----------------------------------------------------------------------
c     calculate 'dapr';
c ----------------------------------------------------------------------
      dapr(1)= -root23*sin(b1)
      dapr(2)= -root23*sin(b2)
      dapr(3)=  root23*sin(beta4)
c ----------------------------------------------------------------------
c     calculate 'ddapr';
c ----------------------------------------------------------------------
      ddapr(1)=-apr(1)
      ddapr(2)=-apr(2)
      ddapr(3)=-apr(3)
c ----------------------------------------------------------------------
c     echo information;
c ----------------------------------------------------------------------
      if (iw1.ge.1) then
         write(nout,9999)
         do 10 k=1,3
            write(nout,9998) k,apr(k)
 10      continue
      endif
      if (iw2.ge.1) then
         write(nout,9997)
         do 20 k=1,3
            write(nout,9996) k,dapr(k)
 20      continue
      endif
      if (iw3.ge.1) then
         write(nout,9995)
         do 30 k=1,3
            write(nout,9994) k,ddapr(k)
 30      continue
      endif
 9999 format(////1x,' apr(k)       : diagonal of principal ',
     x     'strain rate mode tensor',
     x     /21x,'k   = 1..3'/)
 9998 format(1x,'   apr(',i1,')      =',e15.6)
 9997 format(////1x,' dapr(k)      : 1st order partial derivatives',
     x     /21x,'of apr(k)   k=1..3',
     x     /21x,'to beta(4)'/)
 9996 format(1x,'   dapr(',i1,')     =',e15.6)
 9995 format(////1x,' ddapr(k)     : 2nd order partial derivatives',
     x     /21x,'of apr(k)   k=1..3',
     x     /21x,'to beta(4)beta(4)'/)
 9994 format(1x,'   ddapr(',i1,')    =',e15.6)
c ----------------------------------------------------------------------
c     end of subroutine kdaprb;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kdaijb(aij,daij,ddaij,b,db,ddb,apr,dapr,ddapr,
     x     iw1,iw2,iw3)

      implicit none
      integer hrs,i,ij,ir,irs,is,iw1,iw2,iw3,j,k,nout
      real*8 aij(5),apr(3),b(3,3),
     x     daij(5,4),dapr(3),ddaij(5,10),ddapr(3),
     x     db(3,3,3),ddb(3,3,6),sum
c     data nout/9/
      integer          out6,out9,outopt
      common /knchout/ out6,out9,outopt

      nout=out9
ct----------------------------------------------------------------------
ct   03.05.00: syl/eh
ct   - Initialise i and j to something to avoid warnings.  No effects on
ct     results
ct----------------------------------------------------------------------
      i=0
      j=0
C ----------------------------------------------------------------------
      do 10 ij=1,5
         if (ij.eq.1) then
            i=1
            j=1
         endif
         if (ij.eq.2) then
            i=2
            j=2
         endif
         if (ij.eq.3) then
            i=2
            j=3
         endif
         if (ij.eq.4) then
            i=1
            j=3
         endif
         if (ij.eq.5) then
            i=1
            j=2
         endif
c ----------------------------------------------------------------------
c     calculate 'aij';
c ----------------------------------------------------------------------
         sum=0.d0
         do 20 k=1,3
            sum=sum+b(k,i)*b(k,j)*apr(k)
 20      continue
         aij(ij)=sum
c ----------------------------------------------------------------------
c     calculate 'daij';
c ----------------------------------------------------------------------
         do 30 ir=1,3
            sum=0.d0
            do 32 k=1,3
               sum=sum+(db(k,i,ir)*b(k,j)+db(k,j,ir)*b(k,i))*apr(k)
 32         continue
            daij(ij,ir)=sum
 30      continue
         sum=0.d0
         do 34 k=1,3
            sum=sum+b(k,i)*b(k,j)*dapr(k)
 34      continue
         daij(ij,4)=sum
c ----------------------------------------------------------------------
c     calculate 'ddaij';
c ----------------------------------------------------------------------
         irs=0
         hrs=0
         do 40 ir=1,3
         do 40 is=ir,3
            irs=irs+1
            hrs=hrs+1
            sum=0.d0
            do 42 k=1,3
               sum=sum+(ddb(k,i,hrs)*b(k,j)+
     x              ddb(k,j,hrs)*b(k,i)+
     x              db(k,i,ir)*db(k,j,is)+
     x              db(k,j,ir)*db(k,i,is))*apr(k)
 42         continue
            ddaij(ij,irs)=sum
            if (is.eq.3) irs=irs+1
 40      continue
         do 44 ir=1,3
            if (ir.eq.1) irs=4
            if (ir.eq.2) irs=7
            if (ir.eq.3) irs=9
            sum=0.d0
            do 46 k=1,3
               sum=sum+(db(k,i,ir)*b(k,j)+db(k,j,ir)*b(k,i))*dapr(k)
 46         continue
            ddaij(ij,irs)=sum
 44      continue
         sum=0.d0
         do 48 k=1,3
            sum=sum+b(k,i)*b(k,j)*ddapr(k)
 48      continue
         ddaij(ij,10)=sum
 10   continue
c ----------------------------------------------------------------------
c     echo information;
c ----------------------------------------------------------------------
      if (iw1.ge.1) then
         write(nout,9999)
         do 50 ij=1,5
            write(nout,9998) ij,aij(ij)
 50      continue
      endif
      if (iw2.ge.1) then
         write(nout,9997)
         do 60 ir=1,4
            do 62 ij=1,5
               write(nout,9996) ij,ir,daij(ij,ir)
 62         continue
            write(nout,9995)
 60      continue
      endif
      if (iw3.ge.1) then
         write(nout,9994)
         irs=0
         do 70 ir=1,4
         do 70 is=ir,4
            irs=irs+1
            do 72 ij=1,5
               write(nout,9993) ij,irs,ddaij(ij,irs),ir,is
 72            continue
            write(nout,9992)
 70      continue
      endif
 9999 format(////1x,' aij(ij)      : elements of strain rate mode',
     x     ' tensor',
     x     /21x,'ij = 1..5',
     x     /21x,'i,j= (1,1),(2,2),(2,3),(1,3),(1,2)'/)
 9998 format(1x,'   aij(',i1,')      =',e15.6)
 9997 format(////1x,' daij(ij,r)   : 1st order partial derivatives',
     x     /21x,'of aij(ij)    ij = 1..5',
     x     /21x,'        i,j = (1,1),(2,2),(2,3),(1,3),(1,2)',
     x     /21x,'to beta(r)     r = 1..4'/)
 9996 format(1x,'   daij(',i1,',',i1,')   =',e15.6)
 9995 format(1x)
 9994 format(////1x,' ddaij(ij,rs) : 2nd order partial derivatives',
     x     /21x,'of aij(ij)           ij = 1..5',
     x     /21x,'        i,j = (1,1),(2,2),(2,3),(1,3),(1,2)',
     x     /21x,'to beta(r)beta(s)     r = 1..4',
     x     /21x,'                      s = r..4',
     x     /21x,'                    (rs = 1..10)'/,
     x     /21x,'                 r s'/)
 9993 format(1x,'   ddaij(',i1,',',i2,') =',e15.6,i7,i2)
 9992 format(1x)
c ----------------------------------------------------------------------
c     end of subroutine kdaijb;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kdapbx(ap,dap,ddap,aij,daij,ddaij,iw1,iw2,iw3)
c ----------------------------------------------------------------------
c     calculates 'ap','dap' and 'ddap';
c ----------------------------------------------------------------------
      implicit none
      integer ir,irs,is,iw1,iw2,iw3,nout,p
      real*8 aij(5),ap(5),daij(5,4),dap(5,4),ddaij(5,10),
     x     ddap(5,10)
      real*8 root2,root2i,root32
c     data nout/9/
      integer          out6,out9,outopt
      common /knchout/ out6,out9,outopt
c ----------------------------------------------------------------------
c     calculate some constants;
c ----------------------------------------------------------------------
      nout=out9
      root2 =sqrt(2.d0)
      root2i=0.5d0*root2
      root32=sqrt(1.5d0)
c ----------------------------------------------------------------------
c     calculate 'ap';
c ----------------------------------------------------------------------
      ap(1)= root2i*(aij(1)-aij(2))
      ap(2)= root32*(aij(1)+aij(2))
      ap(3)= root2 * aij(3)
      ap(4)= root2 * aij(4)
      ap(5)= root2 * aij(5)
c ----------------------------------------------------------------------
c     calculate 'dap';
c ----------------------------------------------------------------------
      do 10 ir=1,4
         dap(1,ir)= root2i*(daij(1,ir)-daij(2,ir))
         dap(2,ir)= root32*(daij(1,ir)+daij(2,ir))
         dap(3,ir)= root2 * daij(3,ir)
         dap(4,ir)= root2 * daij(4,ir)
         dap(5,ir)= root2 * daij(5,ir)
10    continue
c ----------------------------------------------------------------------
c     calculate 'ddap';
c ----------------------------------------------------------------------
      do 20 irs=1,10
         ddap(1,irs)= root2i*(ddaij(1,irs)-ddaij(2,irs))
         ddap(2,irs)= root32*(ddaij(1,irs)+ddaij(2,irs))
         ddap(3,irs)= root2 * ddaij(3,irs)
         ddap(4,irs)= root2 * ddaij(4,irs)
         ddap(5,irs)= root2 * ddaij(5,irs)
20    continue
c ----------------------------------------------------------------------
c     echo information;
c ----------------------------------------------------------------------
      if (iw1.ge.1) then
         write(nout,9999)
         do 50 p=1,5
            write(nout,9998) p,ap(p)
 50      continue
      endif
      if (iw2.ge.1) then
         write(nout,9997)
         do 60 ir=1,4
            do 62 p=1,5
               write(nout,9996) p,ir,dap(p,ir)
 62         continue
            write(nout,9995)
 60      continue
      endif
      if (iw3.ge.1) then
         write(nout,9994)
         irs=0
         do 70 ir=1,4
         do 70 is=ir,4
            irs=irs+1
            do 72 p=1,5
               write(nout,9993) p,irs,ddap(p,irs),ir,is
 72         continue
            write(nout,9992)
 70      continue
      endif
 9999 format(////1x,' ap(p)        : elements of strain rate mode',
     x     ' vector',
     x     /21x,'p = 1..5'/)
 9998 format(1x,'   ap(',i1,')       =',e15.6)
 9997 format(////1x,' dap(p,r)     : 1st order partial derivatives',
     x     /21x,'of ap(p)       p = 1..5',
     x     /21x,'to beta(r)     r = 1..4'/)
 9996 format(1x,'   dap(',i1,',',i1,')    =',e15.6)
 9995 format(1x)
 9994 format(////1x,' ddap(p,rs)   : 2nd order partial derivatives',
     x     /21x,'of ap(p)              p = 1..5',
     x     /21x,'to beta(r)beta(s)     r = 1..4',
     x     /21x,'                      s = r..4',
     x     /21x,'                    (rs = 1..10)'/,
     x     /21x,'                 r s'/)
 9993 format(1x,'   ddap(',i1,',',i2,')  =',e15.6,i7,i2)
 9992 format(1x)
c ----------------------------------------------------------------------
c     end of subroutine kdapbx;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kdfb(dfbe,ddfbe,dap,ddap,df,ddf,iw1,iw2)
c ----------------------------------------------------------------------
c     calculates 'dfbe(1..4)' and 'ddfbe(1..10)';
c ----------------------------------------------------------------------
      implicit none
      integer i,ij,ir,irs,is,iw1,iw2,j,p,q,m,nout
      real*8 dap(5,4),ddap(5,10),df(5),dfbe(4),ddf(5,5),
     x     ddfbe(10),sum
c     data nout/9/
      integer          out6,out9,outopt
      common /knchout/ out6,out9,outopt
c ----------------------------------------------------------------------
c     calculate 'dfbe(1..4)';
c ----------------------------------------------------------------------
      nout=out9
      do 10 i=1,4
         sum=0.d0
         do 12 p=1,5
            sum=sum+df(p)*dap(p,i)
 12      continue
         dfbe(i)=sum
 10   continue
c ----------------------------------------------------------------------
c     calculate 'ddfbe(1..10)';
c ----------------------------------------------------------------------
      ij=0
      do 20 i=1,4
      do 20 j=i,4
         ij=ij+1
         sum=0.d0
         do 24 p=1,5
         do 24 q=p,5
            m=1
            if (p.eq.q) m=0
            sum=sum+ddf(p,q)*(dap(q,j)*dap(p,i)+m*dap(p,j)*dap(q,i))
 24      continue
         do 28 p=1,5
            sum=sum+df(p)*ddap(p,ij)
 28      continue
         ddfbe(ij)=sum
 20   continue
c ----------------------------------------------------------------------
c     echo information;
c ----------------------------------------------------------------------
      if (iw1.ge.1) then
         write(nout,9999)
         do 50 ir=1,4
            write(nout,9998) ir,dfbe(ir)
 50      continue
      endif
      if (iw2.ge.1) then
         write(nout,9997)
         irs=0
         do 60 ir=1,4
         do 60 is=ir,4
            irs=irs+1
            write(nout,9996) irs,ddfbe(irs),ir,is
 60      continue
      endif
 9999 format(////1x,' dfbe(r)      : 1st order partial derivatives',
     x     /21x,'of f',
     x     /21x,'to beta(r)     r = 1..4'/)
 9998 format(1x,'   dfbe(',i1,')     =',e15.6)
 9997 format(////1x,' ddfbe(rs)    : 2nd order partial derivatives',
     x     /21x,'of f',
     x     /21x,'to beta(r)beta(s)     r = 1..4',
     x     /21x,'                      s = r..4',
     x     /21x,'                    (rs = 1..10)'/,
     x     /21x,'                 r s'/)
 9996 format(1x,'   ddfbe(',i2,')   =',e15.6,i7,i2)
c ----------------------------------------------------------------------
c     end of subroutine kdfb;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kddisb(dhbe,ddhbe,f,dfbe,ddfbe,ap,dap,ddap,up,
     x     iw1,iw2)

      implicit none
      integer i,ij,ir,irs,is,iw1,iw2,j,nout,p
      real*8 ap(5),dap(5,4),ddap(5,10),ddfbe(10),ddhbe(10),
     x     dfbe(4),dhbe(4),f,sum1,up(5),upap,upapi,updap(4)
c     data nout/9/
      integer          out6,out9,outopt
      common /knchout/ out6,out9,outopt
c ----------------------------------------------------------------------
c     calculate 'upap' and its inverse 'upapi';
c ----------------------------------------------------------------------
      nout=out9
      call kdotv(upap,up,ap)
      upapi=1.d0/upap
c ----------------------------------------------------------------------
c     calculate 'dhbe';
c ----------------------------------------------------------------------
      do 20 i=1,4
         sum1=0.d0
         do 22 p=1,5
            sum1=sum1+up(p)*dap(p,i)
 22      continue
         dhbe(i)=(dfbe(i)-f*sum1*upapi)*upapi
         updap(i)=sum1
 20   continue
c ----------------------------------------------------------------------
c     calculate 'ddhbe';
c ----------------------------------------------------------------------
      ij=0
      do 30 i=1,4
      do 30 j=i,4
         ij=ij+1
         sum1=0.d0
         do 34 p=1,5
            sum1=sum1+up(p)*ddap(p,ij)
 34      continue
         ddhbe(ij)=(ddfbe(ij)-
     x        (dfbe(i)*updap(j)+dfbe(j)*updap(i)+f*sum1)*upapi+
     x        2*f*updap(i)*updap(j)*upapi**2)*upapi
 30   continue
c ----------------------------------------------------------------------
c     echo information;
c ----------------------------------------------------------------------
      if (iw1.ge.1) then
         write(nout,9999)
         do 50 ir=1,4
            write(nout,9998) ir,dhbe(ir)
 50      continue
      endif
      if (iw2.ge.1) then
         write(nout,9997)
         irs=0
         do 60 ir=1,4
         do 60 is=ir,4
            irs=irs+1
            write(nout,9996) irs,ddhbe(irs),ir,is
 60      continue
      endif
 9999 format(////1x,' dhbe(r)      : 1st order partial derivatives',
     x     /21x,'of h',
     x     /21x,'to beta(r)     r = 1..4'/)
 9998 format(1x,'   dhbe(',i1,')     =',e15.3)
 9997 format(////1x,' ddhbe(rs)    : 2nd order partial derivatives',
     x     /21x,'of h',
     x     /21x,'to beta(r)beta(s)     r = 1..4',
     x     /21x,'                      s = r..4',
     x     /21x,'                    (rs = 1..10)'/,
     x     /21x,'                 r s'/)
 9996 format(1x,'   ddhbe(',i2,')   =',e15.6,i7,i2)
c ----------------------------------------------------------------------
c     end of subroutine kddisb;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   knew(beaco,dhbe,ddhbe,iw)

      implicit none
      integer i,ifail,irank,iw,j,k,ki,lwork,m,n,nout,nra
      real*8 beaco(4),ddhbe(10),dhbe(4)
c     following variables always double precision ! (nag-routine)
      double precision a(4,4),b(4),tol,sigma,work(32)
c     data nout/9/
      integer          out6,out9,outopt
      common /knchout/ out6,out9,outopt

      nout=out9
      m=4
      n=4
      nra=4
      lwork=32
      ifail=0
      tol=5.0d-3

      ki=0
      do 10 k=1,4
         b(k)=-dhbe(k)
         do 20 i=k,4
            ki=ki+1
            a(k,i)=ddhbe(ki)
            a(i,k)=a(k,i)
 20      continue
 10   continue

      if (iw.ge.1) then
         write(nout,9999)
         write(nout,9998) ((a(i,j),j=1,n),i=1,m)
         write(nout,9997)
         write(nout,9996) (b(i),i=1,m)
      endif
      call kf04jdf(m,n,a,nra,b,tol,sigma,irank,work,lwork,ifail)

      do 30 i=1,4
         beaco(i)=b(i)
 30   continue

      if (iw.ge.1) then
         write(nout,9995)
         write(nout,9996) (b(i),i=1,n)
         write(nout,9994) sigma,irank
      endif

 9999 format(/1x,'matrix a',/)
 9998 format(1x,4d15.6)
 9997 format(/1x,'vector b',/)
 9996 format(1x,4d15.6)
 9995 format(/1x,'solution vector',/)
 9994 format(/1x,'standard error =',e15.6,'      rank =',i4/)
c ----------------------------------------------------------------------
c     end of subroutine knew;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
c  13/08/98
c  Based on kpsylp without the option ifun=4
c
c  (Bert Van Bael)
c
c  March 24, 1998 : ifun = 4 --> as in mcub
c  March 13, 1998 : ifun = 3 --> series expansion iptial=2
c
************************************************************************
      subroutine   kbe4ini(sdum,f,am,bea,beu,tau,sov,vec5,s,ifun,iw)
c ----------------------------------------------------------------------
c     calculates point on "principal strain yield locus" for given beu;
c     variables:
c        sdum     = deviatoric yield stress length
c        f     = normalised rate of work per unit volume
c        am    = plastic strain rate mode (3x3 matrix)
c        bea   = plastic strain rate mode (4D-angular)
c        beu   = deviatoric stress mode (4D-angular)
c        ifun  = 3 for EAO series expansion in strain rate space 
c                  (using kf3der)
c ----------------------------------------------------------------------
ct    Modifications eh/syl for cashform:
ct    eh 07.03.00
ct       - commenting unsused variables
ct    eh 31.03.00
ct       local interpolation around minimum:
ct       - if imin=2, sstar1 might be actually less then sstar2, so the 
ct         parabolic interpolation does not work.
ct       - change the format f9.4 into e13.5
ct       - test the value of c before dividing by c
ct     eh 15.04.00
ct       - adding implicit none -> adding declarations of
ct            integer: ifun,iw,nout,nbea4,imin,ibea4,ncfe
ct            real*8 : bea40,step4
ct          and initialise nbea4=0, bea40=0.0d0, step4=0.0d0.  This does
ct          not change anything since only ifun=3 is used.
ct       -  comment the line bea(1..3)=beu(1..3)
ct       -  Correct the computation of up*ap using now vec5
ct       -  Loop from 0 to 360 and skip the symmetry trick
c ----------------------------------------------------------------------

c ----------------------------------------------------------------------
c     integer j,nchwag,nn
c     real*8  argu,ccf2(2985),cm1(186),scp,
c     integer i,ibea4,ifun,imin,iw,nout,ncfe
c     integer ic,ic2,ic3
      implicit none
c
      integer  ifun,iw,nout,nbea4,i,imin,ibea4,ncfe
      real*8   bea40,step4
c
      real*8   vec5(5)
c
      real*8 a,am(3,3),ap(5),
     x     b,bea(4),beu(4),c,df(5),ddf(5,5),dtr,pi,
     x     f,r1,root23,rtd,sdum,smin,sstar(72),sstar1,sstar2,sstar3,
     x     upap,xmin,
     x     tau,sov(5),s(600)
c     data nout/9/
      integer          out6,out9,outopt
      common /knchout/ out6,out9,outopt
c ----------------------------------------------------------------------
c     calculate some constants;
c ----------------------------------------------------------------------
      nout=out9
      root23=dsqrt(2.d0/3.d0)
      pi=acos(-1.d0)
      dtr=pi/180.d0
      rtd=1.d0/dtr
c ----------------------------------------------------------------------
c     initialise grid if ifun=3: -2.5, 2.5, ... 182.5 (38 values bea(4))
c ----------------------------------------------------------------------
      nbea4=0
      bea40=0.0d0
      step4=0.0d0
      if ( (ifun.eq.3) .or. (ifun.eq.30) ) then
         bea40=-2.5d0*dtr
         step4=5.0d0*dtr
         nbea4=74 ! instead of 38, -5.0+74*5=365
      endif
c ----------------------------------------------------------------------
c     principal strain yield locus assumption;
c ----------------------------------------------------------------------
CCC   bea(1)=beu(1)
CCC   bea(2)=beu(2)
CCC   bea(3)=beu(3)
      if (iw.ge.1) write(nout,6999) (beu(i)*rtd,i=1,4)
 6999 format(' beu(1..4) =',4f8.2)
c ----------------------------------------------------------------------
c     calculate sstar(1..nbea4);
c ----------------------------------------------------------------------
c     startvalue for i if sstar not calculated on whole grid
cc         i=int((beu(4)-bea40)/step4)
      imin=0
      smin=1.d99
      do 100 ibea4=1,nbea4
         bea(4)=bea40+(ibea4-1.0d0)*step4      
c ----------------------------------------------------------------------
c     taking abs(upap) only valid for centro-symmetric yield loci;
c     idem for dabuf(,,1);
c ----------------------------------------------------------------------
         upap=abs(cos(bea(4)-beu(4)))
         if (ifun.eq.3) then
            call kambea(am,bea)
            call kvdm(ap,am)
            ncfe  =s(2)
            call kf3der(f,df,ddf,ap,s(9),s(9+ncfe),5,0,nout,0)
         endif
c
         if (ifun.eq.30) then
            call kambea(am,bea)
            call kvdm(ap,am)
              CALL KFACETPOT(f,df,ddf,ap,0,nout,0)
         endif
c
c        ---------------------------------------------------------------
c        computes up of vec5*ap
c        ---------------------------------------------------------------
         upap=ap(1)*vec5(1)+ap(2)*vec5(2)
     .       +ap(3)*vec5(3)+ap(4)*vec5(4)+ap(5)*vec5(5)

c        ---------------------------------------------------------------
         r1=(tau*f-sov(1)*ap(1)-sov(2)*ap(2)
     .            -sov(3)*ap(3)-sov(4)*ap(4)
     .            -sov(5)*ap(5))/upap
         if (r1.gt.0.and.r1.le.smin.and.ibea4.ne.1.and.ibea4.ne.nbea4) 
     x        then
            imin=ibea4
            smin=r1
         endif
         sstar(ibea4)=r1
         if (iw.ge.3) write(nout,6998) ibea4,bea(4)*rtd,
     x        upap,f,r1,imin,smin
 6998    format(1x,'ibea4 =',i3,'; bea(4) =',f7.2,'; upap =',e13.5,
     x        '; f =',e13.5,'; sstar =',e13.5,'; imin =',i3,
     x        '; smin =',e13.5)
 100  continue

      IF(IMIN.EQ.2) THEN
         XMIN=0.D0
      ELSE
         sstar1=sstar(imin-1)
         sstar2=sstar(imin)
         sstar3=sstar(imin+1)
         a=(sstar1+6.d0*sstar2+sstar3)/8.d0
         b=(sstar3-sstar1)/4.d0
         c=(sstar1-2.d0*sstar2+sstar3)/8.d0
         IF(C.EQ.0.D0) THEN
          XMIN=0.D0
         ELSE
            xmin=-b/(2.d0*c)
         ENDIF
      ENDIF
      bea(4)=bea40+(imin-1+0.5d0*xmin)*step4
c ----------------------------------------------------------------------
c     at wrong side for centro-symmetric yield loci;
c     now skipped
c ----------------------------------------------------------------------
cc    if (cos(bea(4)-beu(4)).lt.0.d0) bea(4)=bea(4)+pi
c ----------------------------------------------------------------------
      IF(C.EQ.0.D0) THEN
         SDUM=0.D0
      ELSE
         sdum=a-b**2/(4.d0*c)
      ENDIF
      if (iw.ge.2) then
         write(nout,6997) 
         write(nout,6996) sstar1,sstar2,sstar3,a,b,c,xmin,bea(4)*rtd,
     .                    sdum
      endif
 6997 format(1x,'   sstar1    sstar2    sstar3         a,',
     x     '        b         c      xmin     bea(4)         s')
 6996 format(9f10.5)
c ----------------------------------------------------------------------
c     calculate 3x3 plastic strain rate mode from bea(1..4)
c ----------------------------------------------------------------------
      call kambea(am,bea)
c ----------------------------------------------------------------------
c     end of function kpsylp;
c ----------------------------------------------------------------------
      return
      end
************************************************************************


************************************************************************
************************************************************************
****
***
**
*    CONCATENATION OF TEXTURE SUBROUTINES FOR CASHFORM MODELS
**
***
****
************************************************************************
c     bvb 23/6/97
c
c     subroutines
c
c                  kcsnm
c                  kmssc
c                  kmsnc
c                  kdhc
c                  kdotv
c                  kambea
c                  kapbea
c                  kbetam
c                  krm2eul
c                  kbetap
c                  kbbe
c
************************************************************************
      subroutine   kcsnm(snc,snm)
c ----------------------------------------------------------------------
c     calculates "snc(6)"   , 6-dimensional column matrix representation
c                             of a strain-related tensor,
c     corresponding to 
c     the given  "snm(3,3)" , 3x3 matrix representation of the tensor;
c ----------------------------------------------------------------------
      implicit none
      real*8 snc(6),snm(3,3)
c
      snc(1)=snm(1,1)
      snc(2)=snm(2,2)
      snc(3)=snm(3,3)
      snc(4)=2.d0*snm(1,2)
      snc(5)=2.d0*snm(2,3)
      snc(6)=2.d0*snm(3,1)
c ----------------------------------------------------------------------
c     end of subroutine kcsnm;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kmssc(ssm,ssc)
c ----------------------------------------------------------------------
c     calculates "ssm(3,3)" , 3x3 matrix representation 
c                             of a stress-related tensor
c     corresponding to
c     the given  "ssc(6)"   , 6-dimensional column matrix 
c                             representation of the tensor;
c ----------------------------------------------------------------------
      implicit none
      real*8 ssc(6),ssm(3,3)
c
      ssm(1,1)=ssc(1)
      ssm(2,2)=ssc(2)
      ssm(3,3)=ssc(3)
      ssm(1,2)=ssc(4)
      ssm(2,3)=ssc(5)
      ssm(3,1)=ssc(6)
      ssm(2,1)=ssm(1,2)
      ssm(3,2)=ssm(2,3)
      ssm(1,3)=ssm(3,1)
c ----------------------------------------------------------------------
c     end of subroutine kmssc;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kmsnc(snm,snc)
c ----------------------------------------------------------------------
c     calculates "snm(3,3)" , 3x3 matrix representation 
c                             of a strain-related tensor
c     corresponding to
c     the given  "snc(6)"   , 6-dimensional column matrix representation
c                             of the tensor;
c ----------------------------------------------------------------------
      implicit none
      real*8 snc(6),snm(3,3)
c
      snm(1,1)=snc(1)
      snm(2,2)=snc(2)
      snm(3,3)=snc(3)
      snm(1,2)=snc(4)*0.5d0
      snm(2,3)=snc(5)*0.5d0
      snm(3,1)=snc(6)*0.5d0
      snm(2,1)=snm(1,2)
      snm(3,2)=snm(2,3)
      snm(1,3)=snm(3,1)
c ----------------------------------------------------------------------
c     end of subroutine kmsnc;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kdhc(dc,hydr,c)
c ----------------------------------------------------------------------
c     calculates "dc(6)"    , 6-dimensional column matrix representation
c                             of the deviatoric tensor,
c            and "hydr"     , the hydrostatic part,
c     corresponding to 
c     the given  "c(6)"     , 6-dimensional column matrix representation
c                             of the tensor;
c ----------------------------------------------------------------------
      implicit none
      integer i
      real*8 c(6),dc(6),hydr,r
c
      hydr=(c(1)+c(2)+c(3))/3.d0
      r=0.0d0
      do 10 i=1,6
         if (i.le.3) then 
            dc(i)=c(i)-hydr
         else
            dc(i)=c(i)
         endif
 10   continue
c ----------------------------------------------------------------------
c     end of subroutine kdhc;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kdotv(dotprod,adv,bdv)
c ----------------------------------------------------------------------
c     calculates "dotprod"  , the dot product
c     corresponding to
c     the given  "adv(5)"   , 5-dimensional vector representation
c                             of a deviatoric tensor
c           and  "bdv(5)"   , 5-dimensional vector representation
c                             of a deviatoric tensor
c                             (e.g. deviatoric stress or
c                                   deviatoric strain rate tensor);
c ----------------------------------------------------------------------
      implicit none
      integer i
      real*8 adv(5),bdv(5),dotprod
c
      dotprod = 0.d0
      do 10 i=1,5
         dotprod = dotprod + adv(i)*bdv(i)
 10   continue
c ----------------------------------------------------------------------
c     end of subroutine kdotv;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kambea(am,bea) 
c ----------------------------------------------------------------------
c     calculates 'am(1..3,1..3)' for a given 'bea(1..4)';
c
c     some variables :
c
c     'bea(1..4)'     : angular representation of deviatoric tensor;
c     'apr(3)'        : principal deviatoric values, 
c                       (only dependent on 'bea(4)');
c     'b(1..3,1..3)'  : rotation matrix,
c                       (depends on 'bea(1..3)');
c     'am(1..3,1..3)' : 3x3 matrix representation;
c ----------------------------------------------------------------------
      implicit none
      integer i,j,k,nout
      real*8 am(3,3),apr(3),b(3,3),b1,b2,bea(4),bea4,s
      real*8 pi,root2,root2i,root23,root32
c     data nout/6/
      integer          out6,out9,outopt
      common /knchout/ out6,out9,outopt

c ----------------------------------------------------------------------
c     calculate some constants;
c ----------------------------------------------------------------------
      nout=out6
      pi=dacos(-1.d0)
      root2 =dsqrt(2.d0)
      root2i=0.5d0*root2
      root23=dsqrt(2.d0/3.d0)
      root32=dsqrt(1.5d0)
c ----------------------------------------------------------------------
c     calculate 'apr(1..3)';
c ----------------------------------------------------------------------
      bea4=bea(4)
      b1=bea4-pi/3.d0
      b2=bea4+pi/3.d0
      apr(1)=   root23*dcos(b1)
      apr(2)=   root23*dcos(b2)
      apr(3)=  -root23*dcos(bea4)
c ----------------------------------------------------------------------
c     calculate 'b(1..3,1..3)';
c ----------------------------------------------------------------------
      call kbbe(b,bea(1),bea(2),bea(3))
c ----------------------------------------------------------------------
c     calculate 'am(1..3,1..3)';
c ----------------------------------------------------------------------
      do 10 i=1,3
      do 10 j=i,3
         s=0.d0
         do 20 k=1,3
            s=s+b(k,i)*b(k,j)*apr(k)
 20      continue
         am(i,j)=s
         am(j,i)=s
 10   continue
c ----------------------------------------------------------------------
c     end of subroutine kambea;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kapbea(ap,bea,iw)
c ----------------------------------------------------------------------
c     calculates 'ap(1..5)' for a given 'bea(1..4)';
c
c     some variables :
c
c     'bea(1..4)'     : angular representation of deviatoric tensor;
c     'am(1..3,1..3)' : 3x3 matrix representation;
c     'ap(1..5)'      : 5-dimensional vector representation;
c ----------------------------------------------------------------------
      implicit none
      integer iw,nout
      real*8 am(3,3),ap(5),bea(4)
c     data nout/6/
      integer          out6,out9,outopt
      common /knchout/ out6,out9,outopt
c ----------------------------------------------------------------------
c     calculate 'am(1..3,1..3)' and convert into 'ap(1..5)';
c ----------------------------------------------------------------------
      nout=out6
      call kambea(am,bea)
      call kvdm(ap,am)
c ----------------------------------------------------------------------
c     end of subroutine kapbea;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kbetam(beta,length,m,iw)
c ----------------------------------------------------------------------
c     calculates    "beta(1..4)" , 4 beta angles characterising 
c                                  the mode of the deviatoric part 
c                                  of a tensor,
c                                  with beta(1) in range   0..360
c                                       beta(2) in range   0.. 90
c                                       beta(3) in range   0..180
c                                       beta(4) in range   0.. 30
c                                                     or 180..210
c            and    "length"     , "length" of the deviatoric part
c                                  of the tensor,
c
c     corresponding to
c        the given  "m(3,3)"     , 3x3 matrix representation
c                                  of the tensor
c                                  (e.g. stress or strain rate tensor)
c
c     SOME VARIABLES :
c
c     'iw'            : control switch for amount of output;
c                      (1 = output; else no output)
c     'eigval(1..3)'  : principal values of 'm',
c                       as obtained with nag routine;
c     'odevpr(1..3)'  : deviatoric principal values of 'm',
c                       in the sequence as obtained with nag routine;
c     'ou(1..3)'      : normalised deviatoric principal values of 'm',
c                       in the sequence as obtained with nag routine;
c     'absou(1..3)'   : absolute values of 'ou(1..3)';
c     'nu(1..3)'      : normalised deviatoric principal values of 'm',
c                       in new sequence obtained from 'ou(1..3)' by 
c                       switching operations in order to find 'beta(4)' 
c                       in range 0..30 or 180..210;
c     'b(1..3,1..3)'  : rotation matrix between reference system 
c                       and principal directions;
c     'tenspr(1..3)'  : principal values of 'm' in new sequence;
c
c NEEDED SUBROUTINES :
c
c      subroutine 'kbetam' needs source files from ~/nagdir :
c                  f02abf.f
c                  x02aaf.f
c                  x04aaf.f
c                  p01aaf.f
c
c      subroutine  krm2eul
c ----------------------------------------------------------------------
      implicit none
      integer ia,ifail,iv,n,i,j,isw1,isw2,isw3,iw
c     following variables always double precision ! (nag-routine)
      double precision a(3,3),eigval(3),eigvec(3,3),work(3)
      real*8 absou(3),b(3,3),beta(4),cos4,det,fi(3),hydr,
     x     length,m(3,3),nu(3),odevpr(3),ou(3),r1,tenspr(3)
      real*8 dtr,pi,pi2,root32,rtd
c ----------------------------------------------------------------------
c     calculate some constants;
c ----------------------------------------------------------------------
      pi=dacos(-1.d0)
      root32 = dsqrt(3.d0/2.d0)
      rtd    = 180.d0/pi
      dtr    = 1.d0/rtd
      pi2    = 2.d0*pi
c ----------------------------------------------------------------------
c     initialisation of variables for nag subroutine 'kf02abf',
c     followed by call to this routine;
c ----------------------------------------------------------------------
      ia=3
      iv=3
      n=3
      ifail=0
      do 10 i=1,3
      do 10 j=1,3
         a(i,j)=m(i,j)
 10   continue
      call kf02abf(a,ia,n,eigval,eigvec,iv,work,ifail)
c ----------------------------------------------------------------------
c     Remarks :
c     The columns in 'eigvec(i,j)' represent the coordinates
c     (expressed in x1 x2 x3) of the unit vectors 
c     in the principal directions of the given tensor. 
c     The principal stress (or strain) in a direction i 
c     corresponding to column i of 'eigvec(i,j)' is 
c     given by 'eigval(i)'.
c
c     e.g. principal stress direction with principal stress 
c          eigval(1) is :
c                         eigvec(1,1) along x1
c                         eigvec(2,1) along x2
c                         eigvec(3,1) along x3
c 
c     The matrix b(i,j) used in the definition of beta(1..3),
c     also contains the coordinates (expressed in x1 x2 x3) 
c     of the unit vectors in the principal directions of the 
c     given tensor, but in its rows.
c ----------------------------------------------------------------------
c ----------------------------------------------------------------------
c     CALCULATION OF BETA(4) IN RANGE 0..30 OR 180..210;
c ----------------------------------------------------------------------
c ----------------------------------------------------------------------
c     calculate the deviatoric of the principal values, 
c     the length of this deviatoric,
c     and the normalised deviatoric 'ou(1..3)';
c ----------------------------------------------------------------------
      hydr=(eigval(1)+eigval(2)+eigval(3))/3.d0
      do 120 i=1,3
         odevpr(i)=eigval(i)-hydr
 120  continue
      r1=0.d0
      do 130 i=1,3
c*+1 (BVB) 8 uur om deze rotzak te killen ..., 23/5/91
c         r1=r1+eigval(i)**2
         r1=r1+odevpr(i)**2
 130   continue
      length=dsqrt(r1)
      ou(1)=odevpr(1)/length
      ou(2)=odevpr(2)/length
      ou(3)=odevpr(3)/length
c ----------------------------------------------------------------------
c     obtain 'nu(1..3)' by switching the order of 'ou(1..3)' 
c     in such a way that |nu(3)| >= |nu(1)| >= |nu(2)|,
c     since this relation must be satisfied to obtain 'beta(4)' 
c     in the range 0..30 or 180..210;
c ----------------------------------------------------------------------
      do 140 j=1,3
         absou(j)=dabs(ou(j))
 140  continue
      isw3=3
      if (absou(1).gt.absou(3)) isw3=1
      if (absou(2).gt.absou(isw3)) isw3=2
      isw2=2
      if (absou(1).lt.absou(2)) isw2=1
      if (absou(3).lt.absou(isw2)) isw2=3
      isw1=6-isw2-isw3
      nu(1)=ou(isw1)
      nu(2)=ou(isw2)
      nu(3)=ou(isw3)
      tenspr(1)=eigval(isw1)
      tenspr(2)=eigval(isw2)
      tenspr(3)=eigval(isw3)
      cos4=-nu(3)*root32
c ----------------------------------------------------------------------
c     due to rounding errors 'cos4' can be outside (-1,1),
c     and in this case 'cos4' is set to -1 or 1;
c ----------------------------------------------------------------------
      if (cos4.gt.1.d0) beta(4)=0.d0
      if (cos4.lt.-1.d0) beta(4)=dacos(-1.d0)
c ----------------------------------------------------------------------
c     for positive values of 'cos4', beta(4) lies in the 0..30 
c     range, and for negative values in the 180..210 range;
c    (remark : the intrinsic function acos always produces 
c              an angle in the 0..180 range)
c ----------------------------------------------------------------------
      if ((cos4.ge.-1.d0).and.(cos4.le.1.d0)) then
         if (cos4.gt.0.d0) then
            beta(4)=dacos(cos4)
         else
            beta(4)=pi2-dacos(cos4)
         endif
      endif
c ----------------------------------------------------------------------
c     the rotation matrix b(i,j) is derived from the 
c     matrix 'eigvec(i,j)', taking into account the switching 
c     operations and the previously given description of the 
c     content of 'eigvec(i,j)';
c     If determinant of 'b(3,3)' less than zero,
c     the signs of one row are changed.
c     (the determinant of the new 'b'matrix will become positive,
c     and the new 'b' will represent a transformation
c     matrix between two "same handed" coordinate systems);
c ----------------------------------------------------------------------
      do 150 j=1,3
         b(1,j)=eigvec(j,isw1)
         b(2,j)=eigvec(j,isw2)
         b(3,j)=eigvec(j,isw3)
 150  continue
      det= b(1,1)*b(2,2)*b(3,3)
     x    +b(1,2)*b(2,3)*b(3,1)
     x    +b(1,3)*b(2,1)*b(3,2)
     x    -b(1,3)*b(2,2)*b(3,1)
     x    -b(1,2)*b(2,1)*b(3,3)
     x    -b(1,1)*b(2,3)*b(3,2)
      if (det.lt.0.d0) then
         do 160 i=1,3
            b(3,i)=-b(3,i)
 160     continue
      endif
c ----------------------------------------------------------------------
c     CALCULATION OF BETA(1) IN RANGE 0..360;
c                    BETA(2) IN RANGE 0.. 90;
c                    BETA(3) IN RANGE 0..180;
c     with subroutine krm2eul which calculates 3 euler angles 'fi(1..3)'
c     equivalent to a rotation matrix 'b';
c ----------------------------------------------------------------------
      call krm2eul(fi,b,iw)
      do 162 i=1,3
         beta(i)=fi(i)
 162  continue
c ----------------------------------------------------------------------
c     print results;
c ----------------------------------------------------------------------
      if (iw.eq.1) then
         write(6,9999)
         write(6,9998)
         write(6,9997) ((m(i,j),j=1,3),i=1,3)
         write(6,9996)
         do 310 i=1,3
            write(6,9995) i,eigval(i)
 310      continue
         write(6,9994)
         write(6,9997) ((eigvec(i,j),j=1,3),i=1,3)
         write(6,9993)
         do 320 i=1,3
            write(6,9992) i,tenspr(i)
 320     continue
         write(6,9991)
         write(6,9997) ((b(i,j),j=1,3),i=1,3)
         write(6,9990)
         write(6,9989) (beta(i)*rtd,i=1,4)
 9999    format(//,'INFORMATION ABOUT CALCULATION OF BETA(1..4)')
 9998    format(/,'   INPUT MATRIX',/)
 9997    format(5x,3e20.10,/,5x,3e20.10,/5x,3e20.10,/)
 9996    format(/,'   RESULTS OF NAG ROUTINE',/)
 9995    format(7x,'eigenvalue(',i1,') = ',e20.10)
 9994    format(/,7x,'eigenvectors in colums of')
 9993    format(/,'   AFTER SWITCHING PRINCIPAL VALUES',/)
 9992    format(7x,'principal(',i1,')  = ',e20.10)
 9991    format(/,7x,'transformation matrix b[i,j]',
     x        /,7x,'between global sample directions ',
     x        /,7x,'and principal tensor directions',
     x        /,7x,'[with principal tensor direction unit vectors ',
     x        /,7x,' expressed in x1 x2 x3 in its rows]')
 9990    format(/,'   RESULTING BETA-VALUES',/)
 9989    format(7x,'beta(1..4)    ',4f10.5)
      endif
c ----------------------------------------------------------------------
c     end of subroutine kbetam;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   krm2eul(fi,b,iw)
c ----------------------------------------------------------------------
c     calculates    "fi(1..3)"   , 3 euler angles
c                                  with fi(1) in range   0..360
c                                       fi(2) in range   0.. 90
c                                       fi(3) in range   0..180
c
c     equivalent to
c        the given  "b(3,3)"     , 3x3 rotation matrix 
c                                  containing direction cosines;
c
c     SOME VARIABLES :
c
c     'iw'            : control switch for amount of output
c                      (1 = output; else no output);
c     'b(1..3,1..3)'  : rotation matrix between reference system 
c                       and other directions;
c     'fi(1..3)'      : 3 euler angles equivalent to 'b';
c ----------------------------------------------------------------------
      implicit none
      integer i,iw
      real*8 b(3,3),cos1,cos2,cos3,fi(3),sin1,sin2,sin3
      real*8 pi,pi12,pi2,rtd
c ----------------------------------------------------------------------
c     calculate some constants;
c ----------------------------------------------------------------------
      pi     = dacos(-1.d0)
      pi12   = 0.5d0*pi
      pi2    = 2.d0*pi
      rtd    = 180.d0/pi
c ----------------------------------------------------------------------
c     CALCULATION OF fi(2) IN RANGE 0..180;
c ----------------------------------------------------------------------
      cos2=b(3,3)
      if (cos2.gt.1.d0) cos2=1.d0
      if (cos2.lt.-1.d0) cos2=-1.d0
      fi(2)=dacos(cos2)
      sin2=dsin(fi(2))
c ----------------------------------------------------------------------
c     CALCULATION OF fi(1) IN RANGE 0..360;
c                    fi(3) IN RANGE 0..360;
c ----------------------------------------------------------------------
      if (dabs(sin2).gt.1e-5) then
         sin1=b(3,1)/sin2
         cos1=-b(3,2)/sin2
         if (cos1.gt.1.d0) cos1=1.d0
         if (cos1.lt.-1.d0) cos1=-1.d0
         if (sin1.ge.0.d0) then
            fi(1)=dacos(cos1)
         else
            fi(1)=-dacos(cos1)+pi2
         endif
         sin3=b(1,3)/sin2
         cos3=b(2,3)/sin2
         if (cos3.gt.1.d0) cos3=1.d0
         if (cos3.lt.-1.d0) cos3=-1.d0
         if (sin3.ge.0.d0) then
            fi(3)=dacos(cos3)
         else
            fi(3)=-dacos(cos3)+pi2
         endif
      else
         fi(3)=0.d0
         sin1=b(1,2)
         cos1=b(1,1)
         if (cos1.gt.1.d0) cos1=1.d0
         if (cos1.lt.-1.d0) cos1=-1.d0
         if (sin1.ge.0.d0) then
            fi(1)=dacos(cos1)
         else
            fi(1)=-dacos(cos1)+pi2
         endif
      endif
c ----------------------------------------------------------------------
c     CALCULATION OF fi(1) IN RANGE 0..360;
c                    fi(2) IN RANGE 0.. 90;
c                    fi(3) IN RANGE 0..180;
c
c     STARTING FROM THE PREVIOUSLY CALCULATED fi(1..3) SET WITH 
c                    fi(1) IN RANGE 0..360;
c                    fi(2) IN RANGE 0..180;
c                    fi(3) IN RANGE 0..360;
c ----------------------------------------------------------------------
c ----------------------------------------------------------------------
c     reduction of fi(2) range from 0..180 to 0..90,
c     without changing the ranges of fi(1) and fi(3);
c ----------------------------------------------------------------------
      if (iw.eq.1) write(6,123) (fi(i)*rtd,i=1,3)
 123  format(5x,3f10.5,5x,'    first set')
      if (fi(2).gt.pi12) then
         if (fi(1).gt.pi) then
            fi(1)=fi(1)-pi
         else
            fi(1)=fi(1)+pi
         endif
         fi(2)=pi-fi(2)
         fi(3)=pi2-fi(3)
      endif
      if (iw.eq.1) write(6,124) (fi(i)*rtd,i=1,3)
 124  format(5x,3f10.5,5x,'    after reduction for fi(2) range')
c ----------------------------------------------------------------------
c     reduction of fi(3) range from 0..360 to 0..180,
c     without changing the ranges of fi(1) and fi(2);
c ----------------------------------------------------------------------
      if (fi(3).gt.pi) fi(3)=fi(3)-pi
      if (iw.eq.1) write(6,125) (fi(i)*rtd,i=1,3)
 125  format(5x,3f10.5,5x,'    after reduction for fi(3) range')
c ----------------------------------------------------------------------
c     end of subroutine krm2eul;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kbetap(beta,length,ap,iw)
c ----------------------------------------------------------------------
c     calculates "beta(1..4)" , 4 beta angles characterising 
c                               the mode of the deviatoric part 
c                               of a tensor,
c                               with beta(1) in range   0..360
c                                    beta(2) in range   0.. 90
c                                    beta(3) in range   0..180
c                                    beta(4) in range   0.. 30
c                                                  or 180..210
c            and "length"     , "length" of the deviatoric,
c     corresponding to
c     the given  "ap(5)"      , 5-dimensional vector representation
c                               of a deviatoric tensor
c                               (e.g. deviatoric stress or 
c                               plastic strain rate tensor);
c ----------------------------------------------------------------------
      implicit none
      integer iw
      real*8 am(3,3),ap(5),beta(4),length
      call kmdv(am,ap)
      call kbetam(beta,length,am,iw)
c ----------------------------------------------------------------------
c     end of subroutine kbetap;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kbbe(b,beta1,beta2,beta3)
c ----------------------------------------------------------------------
c     if beta1, beta2, beta3 are the three Euler angles describing
c     the orientation of a coordinate system (x1p x2p x3p)
c     with respect to    a coordinate system (x1  x2  x3 ),
c
c     the calculated b(i,j) is such that
c
c     the   rows of b(i,j) =
c     coordinates of the unit vectors Uip expressed in (x1  x2  x3 ),
c
c     or equivalently,
c
c     the colums of b(i,j) =
c     coordinates of the unit vectors Uj  expressed in (x1p x2p x3p)
c
c     This means that if
c     V and Vp are column matrices describing the same vector,
c     but V  with the components  expressed in (x1  x2  x3 )
c     and Vp with the components  expressed in (x1p x2p x3p)
c     the following relations hold
c        Vp = B.V                 and   V = [B]transpose.Vp
c
c     It also means that if
c     S and Sp are (3,3) matrices describing the same tensor,
c     but S  with the components  expressed in (x1  x2  x3 )
c     and Sp with the components  expressed in (x1p x2p x3p)
c     the following relations hold
c        Sp = B.S.[B]transpose    and   S = [B]transpose.Sp.B
c ----------------------------------------------------------------------
      implicit none
      real*8 b(3,3),beta1,beta2,beta3,
     x     s1s2,s1c2,c1s2,c1c2,
     x     s1s3,s1c3,c1s3,c1c3,
     x     s2s3,s2c3,c2s3,c2c3,
     x     sss,scs,css,ccs,ssc,scc,csc,ccc,
     x     s1,c1,s2,c2,s3,c3
c ----------------------------------------------------------------------
c     calculation of some sines, cosines and product terms;
c ----------------------------------------------------------------------
      s1=sin(beta1)
      c1=cos(beta1)
      s2=sin(beta2)
      c2=cos(beta2)
      s3=sin(beta3)
      c3=cos(beta3)
      s1s2=s1*s2
      s1c2=s1*c2
      c1s2=c1*s2
      c1c2=c1*c2
      s1s3=s1*s3
      s1c3=s1*c3
      c1s3=c1*s3
      c1c3=c1*c3
      s2s3=s2*s3
      s2c3=s2*c3
      c2s3=c2*s3
      c2c3=c2*c3
      sss=s1*s2*s3
      scs=s1*c2*s3
      css=c1*s2*s3
      ccs=c1*c2*s3
      ssc=s1*s2*c3
      scc=s1*c2*c3
      csc=c1*s2*c3
      ccc=c1*c2*c3
c ----------------------------------------------------------------------
c     calculation of 'b';
c ----------------------------------------------------------------------
      b(1,1)= c1c3-scs
      b(2,1)=-c1s3-scc
      b(3,1)= s1s2
      b(1,2)= s1c3+ccs
      b(2,2)=-s1s3+ccc
      b(3,2)=-c1s2
      b(1,3)= s2s3
      b(2,3)= s2c3
      b(3,3)= c2
c ----------------------------------------------------------------------
c     end of subroutine kbbe;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
************************************************************************
************************************************************************
****
***
**
*    CONCATENATION OF TEXTURE SUBROUTINES FOR CASHFORM MODELS
**
***
****
************************************************************************
      subroutine   kinif3(pamet,maxme,nlawm,nchlst)
c ----------------------------------------------------------------------
c     initializes common needed in kf3der;
c     bvb/eh 15/01/97: only for series and 1st order derivatives; 
c     bvb/eh 20/06/97: now also for 2nd order partial derivatives 
c                      needed in case of iptial=2;
ct----------------------------------------------------------------------
ct    13.03.02: bvb/sh : modifications for ifun=-3, i.e. M^n and npoevn
ct    - npoevn declared as integer and added to common/opteao/
ct    - npoevn=pamet(8,1)
ct    - check pamet(8,i)
ct    03.05.00: syl/eh
ct    - Add the declaration of identifier i
ct----------------------------------------------------------------------
cc at present common only for one "nevn+nodd+symmetry class"
      implicit none
      integer nlawm,i
      integer mxorde,mxfe,mxdfe,mxddfe
      integer mxordo,mxfo,mxdfo,mxddfo
      parameter (mxorde=6,mxfe=210,mxdfe=126,mxddfe=70)
      parameter (mxordo=1,mxfo=5,mxdfo=1,mxddfo=1)
      integer nevn,nodd,npoevn
      integer inde(mxfe,mxorde+2),
     x   inde1(mxdfe,mxorde+1),inde2(mxdfe,mxorde+1),
     x   inde3(mxdfe,mxorde+1),inde4(mxdfe,mxorde+1),
     x   inde5(mxdfe,mxorde+1),
     x   inde11(mxddfe,mxorde),inde12(mxddfe,mxorde),
     x   inde13(mxddfe,mxorde),inde14(mxddfe,mxorde),
     x   inde15(mxddfe,mxorde),inde22(mxddfe,mxorde),
     x   inde23(mxddfe,mxorde),inde24(mxddfe,mxorde),
     x   inde25(mxddfe,mxorde),inde33(mxddfe,mxorde),
     x   inde34(mxddfe,mxorde),inde35(mxddfe,mxorde),
     x   inde44(mxddfe,mxorde),inde45(mxddfe,mxorde),
     x   inde55(mxddfe,mxorde)
      integer indo(mxfo,mxordo+2),
     x   indo1(mxdfo,mxordo+1),indo2(mxdfo,mxordo+1),
     x   indo3(mxdfo,mxordo+1),indo4(mxdfo,mxordo+1),
     x   indo5(mxdfo,mxordo+1),
     x   indo11(mxddfo,mxordo),indo12(mxddfo,mxordo),
     x   indo13(mxddfo,mxordo),indo14(mxddfo,mxordo),
     x   indo15(mxddfo,mxordo),indo22(mxddfo,mxordo),
     x   indo23(mxddfo,mxordo),indo24(mxddfo,mxordo),
     x   indo25(mxddfo,mxordo),indo33(mxddfo,mxordo),
     x   indo34(mxddfo,mxordo),indo35(mxddfo,mxordo),
     x   indo44(mxddfo,mxordo),indo45(mxddfo,mxordo),
     x   indo55(mxddfo,mxordo)
      integer ncfe,ncfe1,ncfe2,ncfe3,ncfe4,ncfe5,
     x   ncfe11,ncfe12,ncfe13,ncfe14,ncfe15,ncfe22,ncfe23,ncfe24,
     x   ncfe25,ncfe33,ncfe34,ncfe35,ncfe44,ncfe45,ncfe55,
     x   nevnd,nevndd
      integer ncfo,ncfo1,ncfo2,ncfo3,ncfo4,ncfo5,
     x   ncfo11,ncfo12,ncfo13,ncfo14,ncfo15,ncfo22,ncfo23,ncfo24,
     x   ncfo25,ncfo33,ncfo34,ncfo35,ncfo44,ncfo45,ncfo55,
     x   noddd,nodddd
      common /opteao/ nevn,nodd,npoevn,
     x     inde,inde1,inde2,inde3,inde4,inde5,
     x     inde11,inde12,inde13,inde14,inde15,inde22,inde23,inde24,
     x     inde25,inde33,inde34,inde35,inde44,inde45,inde55,
     x     ncfe,ncfe1,ncfe2,ncfe3,ncfe4,ncfe5,nevnd,
     x     ncfe11,ncfe12,ncfe13,ncfe14,ncfe15,ncfe22,ncfe23,ncfe24,
     x     ncfe25,ncfe33,ncfe34,ncfe35,ncfe44,ncfe45,ncfe55,nevndd,
     x     indo,indo1,indo2,indo3,indo4,indo5,
     x     indo11,indo12,indo13,indo14,indo15,indo22,indo23,indo24,
     x     indo25,indo33,indo34,indo35,indo44,indo45,indo55,
     x     ncfo,ncfo1,ncfo2,ncfo3,ncfo4,ncfo5,noddd,
     x     ncfo11,ncfo12,ncfo13,ncfo14,ncfo15,ncfo22,ncfo23,ncfo24,
     x     ncfo25,ncfo33,ncfo34,ncfo35,ncfo44,ncfo45,ncfo55,nodddd
c
      integer ider,impred,iptial,ired,iw,maxme,nchlst
      real*8 pamet(maxme,*),rclass
      character class
c ----------------------------------------------------------------------
c     extract required info from pamet;
c ----------------------------------------------------------------------
      nevn=pamet(1,1)
      ncfe=pamet(2,1)
      nodd=pamet(3,1)
      ncfo=pamet(4,1)
      rclass=pamet(5,1)
      iptial=pamet(6,1)
      ired=pamet(7,1)
      npoevn=pamet(8,1)
      if (nlawm.gt.1) then
         do 10 i=2,nlawm
            if ((nevn.ne.pamet(1,i)).or.
     x           (ncfe.ne.pamet(2,i)).or.
     x           (nodd.ne.pamet(3,i)).or.
     x           (ncfo.ne.pamet(4,i)).or.
     x           (rclass.ne.pamet(5,i)).or.
     x           (iptial.ne.pamet(6,i)).or.
     x           (ired.ne.pamet(7,i)).or.
     x           (npoevn.ne.pamet(8,i))) then
               write(nchlst,*) 'PROGRAM STOPPED'
               write(nchlst,*) 'met-files not of same nevn/nodd/class'
               stop
            endif
 10      continue
      endif
      ider=iptial
      if (rclass.eq.1.d0) class='b'
      if (rclass.eq.2.d0) class='c'
      if (rclass.eq.3.d0) class='d'
      if (rclass.eq.4.d0) class='e'
      if (rclass.eq.5.d0) class='f'
      impred=ired
      iw=0
c ----------------------------------------------------------------------
c     initialise common for kf3der;
c ----------------------------------------------------------------------
      if (iw.ge.1) write(nchlst,6992)
      call kinif(inde,mxfe,ncfe,nevn,ired,impred,class,nchlst,iw)
      if (nodd.ge.1) call kinif(indo,mxfo,ncfo,nodd,ired,impred,
     x     class,nchlst,iw)
      if (iw.ge.1) write(nchlst,6991)
      if (ider.ge.1) then
         if (iw.ge.1) write(nchlst,6990)
         call kinidf(inde1,mxdfe,ncfe1,1,nevn,ncfe,inde,mxfe,
     x      nchlst,iw)
         call kinidf(inde2,mxdfe,ncfe2,2,nevn,ncfe,inde,mxfe,
     x      nchlst,iw)
         call kinidf(inde3,mxdfe,ncfe3,3,nevn,ncfe,inde,mxfe,
     x      nchlst,iw)
         call kinidf(inde4,mxdfe,ncfe4,4,nevn,ncfe,inde,mxfe,
     x      nchlst,iw)
         call kinidf(inde5,mxdfe,ncfe5,5,nevn,ncfe,inde,mxfe,
     x      nchlst,iw)
         nevnd=nevn-1
         if (nodd.ge.1) then
            call kinidf(indo1,mxdfo,ncfo1,1,nodd,ncfo,indo,mxfo,
     x           nchlst,iw)
            call kinidf(indo2,mxdfo,ncfo2,2,nodd,ncfo,indo,mxfo,
     x           nchlst,iw)
            call kinidf(indo3,mxdfo,ncfo3,3,nodd,ncfo,indo,mxfo,
     x           nchlst,iw)
            call kinidf(indo4,mxdfo,ncfo4,4,nodd,ncfo,indo,mxfo,
     x           nchlst,iw)
            call kinidf(indo5,mxdfo,ncfo5,5,nodd,ncfo,indo,mxfo,
     x           nchlst,iw)
            noddd=nodd-1
         endif
         if (iw.ge.1) write(nchlst,6989)
      endif
      if (ider.ge.2) then
         if (iw.ge.1) write(nchlst,6988)
         call kinidf(inde11,mxddfe,ncfe11,1,nevnd,ncfe1,inde1,
     x      mxdfe,nchlst,iw)
         call kinidf(inde12,mxddfe,ncfe12,2,nevnd,ncfe1,inde1,
     x      mxdfe,nchlst,iw)
         call kinidf(inde13,mxddfe,ncfe13,3,nevnd,ncfe1,inde1,
     x      mxdfe,nchlst,iw)
         call kinidf(inde14,mxddfe,ncfe14,4,nevnd,ncfe1,inde1,
     x      mxdfe,nchlst,iw)
         call kinidf(inde15,mxddfe,ncfe15,5,nevnd,ncfe1,inde1,
     x      mxdfe,nchlst,iw)
         call kinidf(inde22,mxddfe,ncfe22,2,nevnd,ncfe2,inde2,
     x      mxdfe,nchlst,iw)
         call kinidf(inde23,mxddfe,ncfe23,3,nevnd,ncfe2,inde2,
     x      mxdfe,nchlst,iw)
         call kinidf(inde24,mxddfe,ncfe24,4,nevnd,ncfe2,inde2,
     x      mxdfe,nchlst,iw)
         call kinidf(inde25,mxddfe,ncfe25,5,nevnd,ncfe2,inde2,
     x      mxdfe,nchlst,iw)
         call kinidf(inde33,mxddfe,ncfe33,3,nevnd,ncfe3,inde3,
     x      mxdfe,nchlst,iw)
         call kinidf(inde34,mxddfe,ncfe34,4,nevnd,ncfe3,inde3,
     x      mxdfe,nchlst,iw)
         call kinidf(inde35,mxddfe,ncfe35,5,nevnd,ncfe3,inde3,
     x      mxdfe,nchlst,iw)
         call kinidf(inde44,mxddfe,ncfe44,4,nevnd,ncfe4,inde4,
     x      mxdfe,nchlst,iw)
         call kinidf(inde45,mxddfe,ncfe45,5,nevnd,ncfe4,inde4,
     x      mxdfe,nchlst,iw)
         call kinidf(inde55,mxddfe,ncfe55,5,nevnd,ncfe5,inde5,
     x      mxdfe,nchlst,iw)
         nevndd=nevnd-1
         if (nodd.ge.1) then
            call kinidf(indo11,mxddfo,ncfo11,1,noddd,ncfo1,indo1,
     x           mxdfo,nchlst,iw)
            call kinidf(indo12,mxddfo,ncfo12,2,noddd,ncfo1,indo1,
     x           mxdfo,nchlst,iw)
            call kinidf(indo13,mxddfo,ncfo13,3,noddd,ncfo1,indo1,
     x           mxdfo,nchlst,iw)
            call kinidf(indo14,mxddfo,ncfo14,4,noddd,ncfo1,indo1,
     x           mxdfo,nchlst,iw)
            call kinidf(indo15,mxddfo,ncfo15,5,noddd,ncfo1,indo1,
     x           mxdfo,nchlst,iw)
            call kinidf(indo22,mxddfo,ncfo22,2,noddd,ncfo2,indo2,
     x           mxdfo,nchlst,iw)
            call kinidf(indo23,mxddfo,ncfo23,3,noddd,ncfo2,indo2,
     x           mxdfo,nchlst,iw)
            call kinidf(indo24,mxddfo,ncfo24,4,noddd,ncfo2,indo2,
     x           mxdfo,nchlst,iw)
            call kinidf(indo25,mxddfo,ncfo25,5,noddd,ncfo2,indo2,
     x           mxdfo,nchlst,iw)
            call kinidf(indo33,mxddfo,ncfo33,3,noddd,ncfo3,indo3,
     x           mxdfo,nchlst,iw)
            call kinidf(indo34,mxddfo,ncfo34,4,noddd,ncfo3,indo3,
     x           mxdfo,nchlst,iw)
            call kinidf(indo35,mxddfo,ncfo35,5,noddd,ncfo3,indo3,
     x           mxdfo,nchlst,iw)
            call kinidf(indo44,mxddfo,ncfo44,4,noddd,ncfo4,indo4,
     x           mxdfo,nchlst,iw)
            call kinidf(indo45,mxddfo,ncfo45,5,noddd,ncfo4,indo4,
     x           mxdfo,nchlst,iw)
            call kinidf(indo55,mxddfo,ncfo55,5,noddd,ncfo5,indo5,
     x           mxdfo,nchlst,iw)
            nodddd=noddd-1
         endif
         if (iw.ge.1) write(nchlst,6987)
      endif
 6992 format(1x,'*** prepare integer array for using subroutine keval')
 6991 format(1x,'*** integer array prepared',
     x   /1x,'**************************')
 6990 format(1x,'*** prepare integer arrays for 1st order ',
     x   'partial derivatives')
 6989 format(1x,'*** integer arrays prepared',
     x   /1x,'***************************')
 6988 format(1x,'*** prepare integer arrays for 2nd order ',
     x   'partial derivatives')
 6987 format(1x,'*** integer arrays prepared',
     x   /1x,'***************************')
c ----------------------------------------------------------------------
c     end of subroutine kinif3;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      integer function   kipnz(class,ilst,nord)
c ----------------------------------------------------------------------
c     returns 1 if coefficient possibly non-zero, else 0;
c     (bvb 12/12/96)
c ----------------------------------------------------------------------
ct----------------------------------------------------------------------
ct    03.05.00: syl/eh
ct    - add the declaration integer kisodd, external kisodd to avoid 
ct      warnings during compilation.  The same for knjklst.
ct----------------------------------------------------------------------
      implicit none
      integer kisodd
      external kisodd
      integer knjklst
      external knjklst
      integer nord
      integer ilst(nord),iodd45,iodd53,iodd34
      character class
      kipnz=0
      if (class.eq.'b') then
         iodd45=kisodd(knjklst(4,5,ilst,nord))
         iodd53=kisodd(knjklst(5,3,ilst,nord))
         if (iodd45.ne.1.and.iodd53.ne.1) kipnz=1
      endif
      if (class.eq.'c') then
         iodd45=kisodd(knjklst(4,5,ilst,nord))
         if (iodd45.ne.1) kipnz=1
      endif
      if (class.eq.'d') then
         iodd53=kisodd(knjklst(5,3,ilst,nord))
         if (iodd53.ne.1) kipnz=1
      endif
      if (class.eq.'e') then
         iodd34=kisodd(knjklst(3,4,ilst,nord))
         if (iodd34.ne.1) kipnz=1
      endif
      if (class.eq.'f'.or.class.eq.'x') kipnz=1
c ----------------------------------------------------------------------
c     end of function kipnz;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      integer function   kisodd(i)
c ----------------------------------------------------------------------
c     returns 1 if i is odd, and 0 if not;
c     (bvb 12/12/96)
c ----------------------------------------------------------------------
      implicit none
      integer i
      kisodd=i-i/2*2
      return
      end
************************************************************************
      integer function   knjklst(j,k,ilst,nord)
c ----------------------------------------------------------------------
c     returns number of times j/k occurs in the list ilst(1..nord);
c     (bvb 12/12/96)
c ----------------------------------------------------------------------
      implicit none
      integer nord
      integer i,ilst(nord),j,k
      knjklst=0
      do 10 i=1,nord
         if (ilst(i).eq.j.or.ilst(i).eq.k) knjklst=knjklst+1
 10   continue
      return
      end
************************************************************************
      subroutine   kinif(aa,iaa,nterm,nord,ired,impred,class,
     x   nchlst,iw)
c ----------------------------------------------------------------------
c     prepare integer-array "aa" for evaluating series expansion
c     with subroutine keval;
c     (bvb 12/12/96)
ct----------------------------------------------------------------------
ct    03.05.00: syl/eh
ct    - Permute the declaration of iaa and aa(iaa,*) to avoid warnings.
ct    - Add the declaration of identifier i and k.
ct    - add the declaration integer kipnz, external kipnz to avoid 
ct      warnings during compilation.
ct----------------------------------------------------------------------
      implicit none
      integer kipnz
      external kipnz
ct----------------------------------------------------------------------
      integer maxord,nvar
      parameter (maxord=6,nvar=5)
      integer iaa,ifcf,iord,ilst(maxord),impred,ired,
     x   iterm,itermy,iw,jpnz,nchlst,nord,nterm,i,k
      integer aa(iaa,*)
      character class
c ----------------------------------------------------------------------
c     loop over all terms;
c ----------------------------------------------------------------------
      ifcf=0
      iterm=0
      do 10 i=1,nord
         ilst(i)=1
 10   continue
 1    itermy=0
      jpnz=kipnz(class,ilst,nord)
      if (jpnz.eq.1) then
         itermy=1
      else
         if (ired.eq.0.and.impred.eq.0) itermy=1
      endif
      if (ired.eq.0) then
         ifcf=ifcf+1
      else
         if (jpnz.eq.1) ifcf=ifcf+1
      endif
      if (itermy.eq.1) then
         iterm=iterm+1
         aa(iterm,1)=1
         aa(iterm,2)=ifcf
         do 20 iord=1,nord
            aa(iterm,iord+2)=ilst(iord)
 20      continue
         if (iw.ge.2) write(nchlst,6999)
     x      ifcf,(aa(iterm,i),i=1,nord+2)
 6999    format(1x,3i5,5x,8i1)
      endif
c ----------------------------------------------------------------------
c     determine indices for next coefficient;
c ----------------------------------------------------------------------
      if (ilst(nord).eq.nvar) then
         k=nord
 2       k=k-1
         if (k.eq.0) goto 3
         if (ilst(k).lt.nvar) then
            ilst(k)=ilst(k)+1
            do 30 i=k+1,nord
               ilst(i)=ilst(k)
 30         continue
            goto 1
         else
            goto 2
         endif
      else
         ilst(nord)=ilst(nord)+1
         goto 1
      endif
 3    nterm=iterm
c ----------------------------------------------------------------------
c     end of subroutine kinif;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kinidf(bb,ibb,ntermd,iq,nord,nterm,aa,iaa,nchlst,iw)
c ----------------------------------------------------------------------
c     prepares integer array "bb" to evaluates partial derivative
c     of series expansion described by "nord", "nterm" and "aa";
c     (bvb 12/12/96)
c ----------------------------------------------------------------------
ct    27.04.00: syl/eh
ct    - add the declaration of identifiers iaa, ibb and ntermd
c ----------------------------------------------------------------------
      implicit none
      integer maxord
      parameter (maxord=6)
      integer iaa,ibb
      integer aa(iaa,*),bb(ibb,*)
      integer i,ilst(maxord-1),j,k,iord,iq,iterm,itermd,ntermd,iw,
     x   nchlst,nord,nq,nterm
      if (iw.ge.2) write(nchlst,6999) iq,nord,nterm
 6999 format(1x,'subr inidf for iq=',i2,'; nord=',i2,'; nterm=',i5)
      itermd=0
      do 10 iterm=1,nterm
         nq=0
         k=0
         do 12 iord=1,nord
            k=k+1
            j=aa(iterm,iord+2)
            ilst(k)=j
            if (j.eq.iq) nq=nq+1
            if (j.eq.iq.and.nq.eq.1) k=k-1
 12      continue
         if (iw.ge.4) write(nchlst,6998)
     x      iterm,(aa(iterm,i),i=1,nord+2)
 6998       format(1x,3i5,5x,8i1)
         if (nq.gt.0) then
            itermd=itermd+1
            bb(itermd,1)=nq*aa(iterm,1)
            bb(itermd,2)=aa(iterm,2)
            do 14 i=1,nord-1
               bb(itermd,i+2)=ilst(i)
 14         continue
            if (iw.ge.3) write(nchlst,6997)
     x         itermd,(bb(itermd,i),i=1,nord+1)
 6997       format(30x,3i5,5x,8i1)
         endif
 10   continue
      ntermd=itermd
      if (iw.ge.2) write(nchlst,6996) ntermd
 6996 format(1x,'           gives ',i3,' terms')
c ----------------------------------------------------------------------
c     end of subroutine kinidf;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
************************************************************************
************************************************************************
****
***
**
*    CONCATENATION OF TEXTURE SUBROUTINES FOR CASHFORM MODELS
**
***
****
************************************************************************

* F02FTEXT

*UPTODATE F02ABFTEXT
      SUBROUTINE   KF02ABF(A, IA, N, R, V, IV, E, IFAIL)
      implicit none
C     MARK 2 RELEASE. NAG COPYRIGHT 1972
C     MARK 3 REVISED.
C     MARK 4.5 REVISED
C
C     EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIX MATRIX
C     1ST AUGUST 1971
C
      INTEGER KP01AAF, ISAVE, IFAIL, N, IA, IV
C$P 1
      DOUBLE PRECISION SRNAME
CT    ------------------------------------------------------------------
CT    EH/SYL 07.04.00
CT       - DOUBLE PRECISION XXXX IS REMOVED BECAUSE NOT USED
CT    ------------------------------------------------------------------
      DOUBLE PRECISION TOL, A(IA,N), R(N), V(IV,N), E(N), kx02adf,
     * KX02AAF
      DATA SRNAME /8H F02ABF /
      ISAVE = IFAIL
      IFAIL = 1
      TOL = kx02adf(1.0D0)
      CALL KF01AJF(N, TOL, A, IA, R, E, V, IV)
      TOL = KX02AAF(1.0D0)
      CALL KF02AMF(N, TOL, R, E, V, IV, IFAIL)
      IF (IFAIL.NE.0) IFAIL = KP01AAF(ISAVE,IFAIL,SRNAME)
      RETURN
      END
** END OF F02ABFTEXT

*UPTODATE F02AMFTEXT
      SUBROUTINE   KF02AMF(N, ACHEPS, D, E, Z, IZ, IFAIL)
C     MARK 2 RELEASE. NAG COPYRIGHT 1972
C     MARK 3 REVISED.
C     MARK 4 REVISED.
C     MARK 4.5 REVISED
C     MARK 9 REVISED. IER-326 (SEP 1981).
C
C     TQL2
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS OF A
C     TRIDIAGONAL MATRIX, T, GIVEN WITH ITS DIAGONAL ELEMENTS IN
C     THE ARRAY D(N) AND ITS SUB-DIAGONAL ELEMENTS IN THE LAST N
C     - 1 STORES OF THE ARRAY E(N), USING QL TRANSFORMATIONS. THE
C     EIGENVALUES ARE OVERWRITTEN ON THE DIAGONAL ELEMENTS IN THE
C     ARRAY D IN ASCENDING ORDER. THE EIGENVECTORS ARE FORMED IN
C     THE ARRAY Z(N,N), OVERWRITING THE ACCUMULATED
C     TRANSFORMATIONS AS SUPPLIED BY THE SUBROUTINE KF01AJF. THE
C     SUBROUTINE WILL FAIL IF ALL EIGENVALUES TAKE MORE THAN 30*N
C     ITERATIONS.
C     1ST APRIL 1972
C
      INTEGER KP01AAF, ISAVE, IFAIL, N, I, L, J, M, I1, M1, II, K, IZ
C$P 1
      DOUBLE PRECISION SRNAME
      DOUBLE PRECISION B, F, H, ACHEPS, G, P, R, C, S, D(N), E(N), Z(IZ,
     *N)
      DATA SRNAME /8H F02AMF /
      ISAVE = IFAIL
      IF (N.EQ.1) GO TO 40
      DO 20 I=2,N
         E(I-1) = E(I)
   20 CONTINUE
   40 E(N) = 0.0D0
      B = 0.0D0
      F = 0.0D0
      J = 30*N
      DO 300 L=1,N
         H = ACHEPS*(DABS(D(L))+DABS(E(L)))
         IF (B.LT.H) B = H
C     LOOK FOR SMALL SUB-DIAG ELEMENT
         DO 60 M=L,N
            IF (DABS(E(M)).LE.B) GO TO 80
   60    CONTINUE
   80    IF (M.EQ.L) GO TO 280
  100    IF (J.LE.0) GO TO 400
         J = J - 1
C     FORM SHIFT
         G = D(L)
         H = D(L+1) - G
         IF (DABS(H).GE.DABS(E(L))) GO TO 120
         P = H*0.5D0/E(L)
         R = DSQRT(P*P+1.0D0)
         H = P + R
         IF (P.LT.0.0D0) H = P - R
         D(L) = E(L)/H
         GO TO 140
  120    P = 2.0D0*E(L)/H
         R = DSQRT(P*P+1.0D0)
         D(L) = E(L)*P/(1.0D0+R)
  140    H = G - D(L)
         I1 = L + 1
         IF (I1.GT.N) GO TO 180
         DO 160 I=I1,N
            D(I) = D(I) - H
  160    CONTINUE
  180    F = F + H
C     QL TRANSFORMATION
         P = D(M)
         C = 1.0D0
         S = 0.0D0
         M1 = M - 1
         DO 260 II=L,M1
            I = M1 - II + L
            G = C*E(I)
            H = C*P
            IF (DABS(P).LT.DABS(E(I))) GO TO 200
            C = E(I)/P
            R = DSQRT(C*C+1.0D0)
            E(I+1) = S*P*R
            S = C/R
            C = 1.0D0/R
            GO TO 220
  200       C = P/E(I)
            R = DSQRT(C*C+1.0D0)
            E(I+1) = S*E(I)*R
            S = 1.0D0/R
            C = C/R
  220       P = C*D(I) - S*G
            D(I+1) = H + S*(C*G+S*D(I))
C     FORM VECTOR
            DO 240 K=1,N
               H = Z(K,I+1)
               Z(K,I+1) = S*Z(K,I) + C*H
               Z(K,I) = C*Z(K,I) - S*H
  240       CONTINUE
  260    CONTINUE
         E(L) = S*P
         D(L) = C*P
         IF (DABS(E(L)).GT.B) GO TO 100
  280    D(L) = D(L) + F
  300 CONTINUE
C     ORDER EIGENVALUES AND EIGENVECTORS
      DO 380 I=1,N
         K = I
         P = D(I)
         I1 = I + 1
         IF (I1.GT.N) GO TO 340
         DO 320 J=I1,N
            IF (D(J).GE.P) GO TO 320
            K = J
            P = D(J)
  320    CONTINUE
  340    IF (K.EQ.I) GO TO 380
         D(K) = D(I)
         D(I) = P

         DO 360 J=1,N            
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
  360    CONTINUE
  380 CONTINUE
      IFAIL = 0
      RETURN
  400 IFAIL = KP01AAF(ISAVE,1,SRNAME)
      RETURN
      END
** END OF F02AMFTEXT

*UPTODATE F01AGZTEXT
      SUBROUTINE   KF01AGZ(A, IA, N, B, C)
C     MARK 11 RELEASE. NAG COPYRIGHT 1983.
C
C     COMPUTES  C = A*B  WHERE
C     A IS A SYMMETRIC N-BY-N MATRIX,
C     WHOSE LOWER TRIANGLE IS STORED IN A.
C     C MUST BE DISTINCT FROM B.
C
C     .. SCALAR ARGUMENTS ..
      INTEGER IA, N
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION A(IA,N), B(N), C(N)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION Y
      INTEGER I, IP1, J, NM1
C     ..
C
      DO 20 I=1,N
         C(I) = 0.0D0
   20 CONTINUE
      IF (N.EQ.1) GO TO 100
      NM1 = N - 1
      DO 80 I=1,NM1
         DO 40 J=I,N
            C(J) = C(J) + A(J,I)*B(I)
   40    CONTINUE
         Y = C(I)
         IP1 = I + 1
         DO 60 J=IP1,N
            Y = Y + A(J,I)*B(J)
   60    CONTINUE
         C(I) = Y
   80 CONTINUE
  100 C(N) = C(N) + A(N,N)*B(N)
      RETURN
      END
** END OF F01AGZTEXT

*UPTODATE F01AJFTEXT
      SUBROUTINE   KF01AJF(N, ATOL, A, IA, D, E, Z, IZ)
C     MARK 2 RELEASE. NAG COPYRIGHT 1972
C     MARK 4 REVISED.
C     MARK 4.5 REVISED
C     MARK 5C REVISED
C     MARK 11 REVISED. VECTORISATION (JAN 1984).
C
C     TRED2
C     THIS SUBROUTINE REDUCES THE GIVEN LOWER TRIANGLE OF A
C     SYMMETRIC MATRIX, A, STORED IN THE ARRAY A(N,N), TO
C     TRIDIAGONAL FORM USING HOUSEHOLDERS REDUCTION. THE DIAGONAL
C     OF THE RESULT IS STORED IN THE ARRAY D(N) AND THE
C     SUB-DIAGONAL IN THE LAST N - 1 STORES OF THE ARRAY E(N)
C     (WITH THE ADDITIONAL ELEMENT E(1) = 0). THE TRANSFORMATION
C     MATRICES ARE ACCUMULATED IN THE ARRAY Z(N,N). THE ARRAY
C     A IS LEFT UNALTERED UNLESS THE ACTUAL PARAMETERS
C     CORRESPONDING TO A AND Z ARE IDENTICAL.
C     1ST AUGUST 1971
C
      INTEGER I, IA, II, IZ, J, K, L, N
      DOUBLE PRECISION ATOL, F, G, H, HH, A(IA,N), D(N), E(N), Z(IZ,N)
      DO 40 I=1,N
         DO 20 J=I,N
            Z(J,I) = A(J,I)
   20    CONTINUE
         D(I) = A(N,I)
   40 CONTINUE
      IF (N.EQ.1) GO TO 440
      DO 260 II=2,N
         I = N - II + 2
         L = I - 2
         F = D(I-1)
         G = 0.0D0
         IF (L.EQ.0) GO TO 80
         DO 60 K=1,L
            G = G + D(K)*D(K)
   60    CONTINUE
   80    H = G + F*F
         L = L + 1
C     IF G IS TOO SMALL FOR ORTHOGONALITY TO BE
C     GUARANTEED THE TRANSFORMATION IS SKIPPED
         IF (G.GT.ATOL) GO TO 100
         E(I) = F
         H = 0.0D0
         DO 85 J=1,L
            Z(J,I) = D(J)
   85    CONTINUE
         DO 90 J=1,L
            Z(I,J) = 0.0D0
            D(J) = Z(I-1,J)
   90    CONTINUE
         GO TO 240
  100    G = DSQRT(H)
         IF (F.GE.0.0D0) G = -G
         E(I) = G
         H = H - F*G
         D(I-1) = F - G
C     COPY U
         DO 110 J=1,L
            Z(J,I) = D(J)
  110    CONTINUE
C     FORM A*U
         CALL KF01AGZ(Z, IZ, L, D, E)
C     FORM P
         DO 182 J=1,L
            E(J) = E(J)/H
  182    CONTINUE
         F = 0.0D0
         DO 185 J=1,L
            F = F + E(J)*D(J)
  185    CONTINUE
C     FORM K
         HH = F/(H+H)
C     FORM Q
         DO 190 J=1,L
            E(J) = E(J) - HH*D(J)
  190    CONTINUE
C     FORM REDUCED A
         DO 220 J=1,L
            F = D(J)
            G = E(J)
            DO 200 K=J,L
               Z(K,J) = (Z(K,J) - G*D(K)) - F*E(K)
  200       CONTINUE
            D(J) = Z(L,J)
            Z(I,J) = 0.0D0
  220    CONTINUE
  240    D(I) = H
  260 CONTINUE
C     ACCUMULATION OF TRANSFORMATION MATRICES
      DO 400 I=2,N
         L = I - 1
         Z(N,L) = Z(L,L)
         Z(L,L) = 1.0D0
         H = D(I)
         IF (H.EQ.0.0D0) GO TO 360
         DO 290 K=1,L
            D(K) = 0.0D0
  290    CONTINUE
         CALL KF01CKY(Z, IZ, L, L, Z(1,I), 1, D)
         DO 310 K=1,L
            D(K) = D(K)/H
  310    CONTINUE
         DO 340 J=1,L
            DO 320 K=1,L
               Z(K,J) = Z(K,J) - Z(K,I)*D(J)
  320       CONTINUE
  340    CONTINUE
  360    DO 380 J=1,L
            Z(J,I) = 0.0D0
  380    CONTINUE
  400 CONTINUE
      DO 420 I=1,N
         D(I) = Z(N,I)
         Z(N,I) = 0.0D0
  420 CONTINUE
  440 Z(N,N) = 1.0D0
      E(1) = 0.0D0
      RETURN
      END
** END OF F01AJFTEXT

*UPTODATE F01CKYTEXT
      SUBROUTINE   KF01CKY(A, IA, M, N, B, IB, C)
C     MARK 11 RELEASE. NAG COPYRIGHT 1983.
C
C     COMPUTES  C = C +  (A**T)*B  WHERE
C     A IS RECTANGULAR M BY N.
C     C MUST BE DISTINCT FROM B.
C     THE ELEMENTS OF B MAY BE NON-CONSECUTIVE, WITH OFFSET IB.
C
C     .. SCALAR ARGUMENTS ..
      INTEGER IA, IB, M, N
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION A(IA,N), B(IB,M), C(N)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION X
      INTEGER I, J
C     ..
C
      DO 60 I=1,N
         X = C(I)
         DO 40 J=1,M
            X = X + A(J,I)*B(1,J)
   40    CONTINUE
         C(I) = X
   60 CONTINUE
      RETURN
      END
** END OF F01CKYTEXT

*UPTODATE X02ADFTEXT
      DOUBLE PRECISION FUNCTION   KX02ADF(X)
C     NAG COPYRIGHT 1975
C     MARK 4.5 RELEASE
      DOUBLE PRECISION X
C     * TOL *
C     RETURNS THE RATIO OF THE SMALLEST POSITIVE REAL FLOATING-
C     POINT NUMBER REPRESENTABLE ON THE COMPUTER TO EPS
C     FOR DEC VAX 11/780
C     kx02adf = 2.0D0**(-72)
      kx02adf = 2.0D0**(-72)
      RETURN
      END
** END OF X02ADFTEXT
************************************************************************
****
***
**
*    CONCATENATION OF TEXTURE SUBROUTINES FOR CASHFORM MODELS
**
***
****
************************************************************************
* F04FTEXT
*UPTODATE F04JDFTEXT
      SUBROUTINE   KF04JDF(M,N,A, NRA, B, TOL, SIGMA, IRANK, WORK,LWORK,
     * IFAIL)
      implicit none
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (SVDLS2)
C
C     F04JDF RETURNS THE N ELEMENT VECTOR X, OF MINIMAL
C     LENGTH, THAT MINIMIZES THE EUCLIDEAN LENGTH OF THE M
C     ELEMENT VECTOR R GIVEN BY
C
C     R = B-A*X ,
C
C     WHERE A IS AN M*N (M.LE.N) MATRIX AND B IS AN M ELEMENT
C     VECTOR. X IS OVERWRITTEN ON B.
C
C     THE SOLUTION IS OBTAINED VIA A SINGULAR VALUE
C     DECOMPOSITION (SVD) OF THE MATRIX A GIVEN BY
C
C     A = Q*(D 0)*(P**T) ,
C
C     WHERE Q AND P ARE ORTHOGONAL AND D IS A DIAGONAL MATRIX WITH
C     NON-NEGATIVE DIAGONAL ELEMENTS, THESE BEING THE SINGULAR
C     VALUES OF A.
C
C     INPUT PARAMETERS.
C
C     M     - NUMBER OF ROWS OF A. M MUST BE AT LEAST UNITY.
C
C     N     - NUMBER OF COLUMNS OF A. N MUST BE AT LEAST M.
C
C     A     - AN M*N REAL MATRIX.
C
C     NRA   - ROW DIMENSION OF A AS DECLARED IN THE CALLING PROGRAM.
C             NRA MUST BE AT LEAST M.
C
C     B     - AN N ELEMENT REAL VECTOR.
C             THE FIRST M ELEMENTS OF B MUST CONTAIN THE
C             VECTOR B OF THE LEAST SQUARES PROBLEM.
C
C
C     TOL   - A RELATIVE TOLERANCE USED TO DETERMINE THE RANK OF A.
C             TOL SHOULD BE CHOSEN AS APPROXIMATELY THE
C             LARGEST RELATIVE ERROR IN THE ELEMENTS OF A.
C             FOR EXAMPLE IF THE ELEMENTS OF A ARE CORRECT
C             TO ABOUT 4 SIGNIFICANT FIGURES THEN TOL
C             SHOULD BE CHOSEN AS ABOUT 5.0*10.0**(-4).
C
C     IFAIL - THE USUAL FAILURE PARAMETER. IF IN DOUBT SET
C             IFAIL TO ZERO BEFORE CALLING THIS ROUTINE.
C
C     OUTPUT PARAMETERS.
C
C     A     - A WILL CONTAIN THE FIRST M ROWS OF THE
C             ORTHOGONAL MATRIX P**T OF THE SVD.
C
C     B     - B WILL CONTAIN THE MINIMAL LEAST SQUARES
C             SOLUTION VECTOR X.
C
C     SIGMA - IF M IS GREATER THAN IRANK THEN SIGMA WILL CONTAIN THE
C             STANDARD ERROR GIVEN BY
C             SIGMA=L(R)/SQRT(M-IRANK), WHERE L(R) DENOTES
C             THE EUCLIDEAN LENGTH OF THE RESIDUAL VECTOR
C             R. IF M=IRANK THEN SIGMA IS RETURNED AS ZERO.
C
C     IRANK - THE RANK OF THE MATRIX A.
C
C     IFAIL - ON NORMAL RETURN IFAIL WILL BE ZERO.
C             IN THE UNLIKELY EVENT THAT THE QR-ALGORITHM
C             FAILS TO FIND THE SINGULAR VALUES IN 50*M
C             ITERATIONS THEN IFAIL IS SET TO 2.
C             IF AN INPUT PARAMETER IS INCORRECTLY SUPPLIED
C             THEN IFAIL IS SET TO UNITY.
C
C     WORKSPACE PARAMETERS.
C
C     WORK  - AN (M*M+4*M) ELEMENT VECTOR.
C             ON RETURN THE FIRST M ELEMENTS OF WORK WILL
C             CONTAIN THE SINGULAR VALUES OF A ARRANGED IN
C             DESCENDING ORDER. WORK(M+1) WILL CONTAIN THE
C             TOTAL NUMBER OF ITERATIONS TAKEN BY THE
C             QR-ALGORITHM.
C
C     LWORK - THE LENGTH OF THE VECTOR WORK.
C             LWORK MUST BE AT LEAST M*M+4*M.
C
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION SIGMA, TOL
      INTEGER IFAIL, IRANK, LWORK, M, N, NRA
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION A(NRA,N), B(N), WORK(LWORK)
C     ..
C     .. LOCAL SCALARS ..
C$P 1
      DOUBLE PRECISION SRNAME
      INTEGER IERR, K, MP1, MP2
C     .. FUNCTION REFERENCES ..
      INTEGER KF02WDY, KP01AAF
C     .. SUBROUTINE REFERENCES ..
C     F02WBF, F04JAZ
C     ..
      DATA SRNAME /8H F04JDF /
      IERR = IFAIL
      IF (IERR.EQ.0) IFAIL = 1
C
      IF (NRA.LT.M .OR. N.LT.M .OR. M.LT.1 .OR. LWORK.LT.M*(M+4))
     *GO TO 20
C
      K = LWORK - M
      MP1 = M + 1
      MP2 = MP1 + 1
C
      CALL KF02WBF(M, N, A, NRA, .TRUE., B, WORK, WORK(MP1), K,IFAIL)
C
      IF (IFAIL.NE.0) GO TO 20
C
      IRANK = KF02WDY(M,WORK,TOL)
C
      CALL KF04JAZ(M, N, IRANK, WORK, M, B, A, NRA, B, SIGMA,WORK(MP2))
C
      RETURN
C
   20 IFAIL = KP01AAF(IERR,IFAIL,SRNAME)
      RETURN
      END
** END OF F04JDFTEXT
*UPTODATE F04JAYTEXT
      SUBROUTINE   KF04JAY(N, IRANK, SV, LSV, B, PT, NRPT, X, WORK)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (SVDLSQ)
C
C     F04JAY RETURNS THE N ELEMENT VECTOR X GIVEN BY
C
C     X = P*(D**(-1))*B ,
C
C     WHERE D IS AN IRANK*IRANK NON-SINGULAR DIAGONAL MATRIX,
C     P CONTAINS THE FIRST IRANK COLUMNS OF AN N*N ORTHOGONAL
C     MATRIX AND B IS AN IRANK ELEMENT VECTOR.
C
C     THE ROUTINE MAY BE CALLED WITH IRANK=0 IN WHICH CASE X
C     IS RETURNED AS THE ZERO VECTOR.
C
C     INPUT PARAMETERS.
C
C     N     - NUMBER OF ROWS OF P. N MUST BE AT LEAST UNITY.
C
C     IRANK - ORDER OF THE MATRIX D.
C             IF IRANK=0 THEN SV, B, PT AND WORK ARE NOT REFERENCED.
C
C     SV    - AN IRANK ELEMENT VECTOR CONTAINING THE
C             DIAGONAL ELEMENTS OF D. SV MUST BE SUCH THAT
C             NO ELEMENT OF (D**(-1)*B WILL OVERFLOW.
C
C     LSV   - LSV MUST BE AT LEAST MAX(1,IRANK).
C
C     B     - AN IRANK ELEMENT VECTOR.
C
C     PT    - AN IRANK*N ELEMENT MATRIX CONTAINING THE MATRIX P**T.
C
C     NRPT  - ROW DIMENSION OF PT AS DECLARED IN THE
C             CALLING PROGRAM. NRPT MUST BE AT LEAST LSV.
C
C     OUTPUT PARAMETER.
C
C     X     - N ELEMENT VECTOR CONTAINING P*(D**(-1))*B.
C             IF IRANK=0 THEN X RETURNS THE ZERO VECTOR.
C             THE ROUTINE MAY BE CALLED WITH X=B OR WITH X=SV.
C
C     WORKSPACE PARAMETER.
C
C     WORK  - AN LSV ELEMENT VECTOR.
C             IF THE ROUTINE IS NOT CALLED WITH X=B THEN IT MAY BE
C             CALLED WITH WORK=B. SIMILARLY IF THE ROUTINE
C             IS NOT CALLED WITH X=SV THEN IT MAY BE CALLED
C             WITH WORK=SV.
C
C     .. SCALAR ARGUMENTS ..
      INTEGER IRANK, LSV, N, NRPT
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION B(LSV), PT(NRPT,N), SV(LSV), WORK(LSV), X(N)
C     ..
C     .. LOCAL SCALARS ..
      INTEGER I
C     .. SUBROUTINE REFERENCES ..
C     F02WCW
C     ..
      IF (IRANK.EQ.0) GO TO 40
C
      DO 20 I=1,IRANK
         WORK(I) = B(I)/SV(I)
   20 CONTINUE
C
      CALL KF02WCW(IRANK, N, PT, NRPT, WORK, X, X)
C
      RETURN
C
   40 DO 60 I=1,N
         X(I) = 0.0D0
   60 CONTINUE
C
      RETURN
      END
** END OF F04JAYTEXT
*UPTODATE F04JAZTEXT
      SUBROUTINE   KF04JAZ(M,N, IRANK, SV, LSV, B, PT, NRPT, X, SIGMA,
     * WORK)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (SVDLS0)
C
C     F04JAZ RETURNS THE N ELEMENT VECTOR X, OF MINIMAL
C     LENGTH, THAT MINIMIZES THE EUCLIDEAN LENGTH OF THE M
C     ELEMENT VECTOR R GIVEN BY
C
C     R = B-A*X ,
C
C     WHERE B IS AN M ELEMENT VECTOR AND A IS AN M*N MATRIX,
C     FOLLOWING A SINGULAR VALUE DECOMPOSITION (SVD) OF A
C     GIVEN BY
C
C     A = Q*D*(P**T) ,
C
C     WHERE D IS A RECTANGULAR DIAGONAL MATRIX WHOSE DIAGONAL
C     ELEMENTS CONTAIN THE SINGULAR VALUES OF A IN DESCENDING
C     ORDER.
C
C     INPUT PARAMETERS.
C
C     M     - NUMBER OF ROWS OF A. M MUST BE AT LEAST UNITY.
C
C     N     - NUMBER OF COLUMNS OF A. N MUST BE AT LEAST UNITY.
C
C     IRANK - THE RANK OF THE MATRIX A. IRANK MUST BE SUCH THAT THE
C             ELEMENTS SV(I), I=1,2,...,IRANK ARE NON-NEGLIGIBLE.
C             IRANK MUST BE AT LEAST ZERO AND MUST NOT BE
C             LARGER THAN MIN(M,N).
C             ROUTINE KF02WDY CAN BE USED TO DETERMINE RANK FOLLOWING
C             AN SVD.
C
C     SV    - AN LSV ELEMENT VECTOR CONTAINING THE POSITIVE
C             NON-NEGLIGIBLE SINGULAR VALUES OF A.
C
C     LSV   - LENGTH OF THE VECTOR SV.
C             LSV MUST BE AT LEAST MAX(1,IRANK).
C
C     B     - MUST CONTAIN THE M ELEMENT VECTOR (Q**T)*B, WHERE Q IS
C             THE LEFT-HAND ORTHOGONAL MATRIX OF THE SVD.
C
C     PT    - THE IRANK*N PART OF PT MUST CONTAIN THE FIRST
C             IRANK ROWS OF THE RIGHT-HAND ORTHOGONAL
C             MATRIX P**T OF THE SVD.
C
C     NRPT  - ROW DIMENSION OF PT AS DECLARED IN THE CALLING PROGRAM
C             NRPT MUST BE AT LEAST LSV.
C
C     OUTPUT PARAMETERS.
C
C     X     - THE N ELEMENT SOLUTION VECTOR.
C             THE ROUTINE MAY BE CALLED WITH X=B OR WITH X=SV.
C
C     SIGMA - IF M IS GREATER THAN IRANK THEN SIGMA WILL CONTAIN THE
C             STANDARD ERROR GIVEN BY
C             SIGMA=L(R)/SQRT(M-IRANK), WHERE L(R) DENOTES
C             THE EUCLIDEAN LENGTH OF THE RESIDUAL VECTOR
C             R. IF M=IRANK THEN SIGMA IS RETURNED AS ZERO.
C
C     WORKSPACE PARAMETER.
C
C     WORK  - AN LSV ELEMENT VECTOR.
C             IF THE ROUTINE IS NOT CALLED WITH X=B THEN IT MAY BE
C             CALLED WITH WORK=B. SIMILARLY IF THE ROUTINE
C             IS NOT CALLED WITH X=SV THEN IT MAY BE CALLED
C             WITH WORK=SV.
C
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION SIGMA
      INTEGER IRANK, LSV, M, N, NRPT
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION B(M), PT(NRPT,N), SV(LSV), WORK(LSV), X(N)
C     ..
C     .. LOCAL SCALARS ..
      INTEGER IRP1, MMIR
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION KF04JGW, DSQRT
C     *** IMPLEMENTATION DEPENDENT DECLARATION ***
C     DOUBLE PRECISION DBLE
C     .. SUBROUTINE REFERENCES ..
C     F04JAY
C     ..
      SIGMA = 0.0D0
      IF (IRANK.EQ.M) GO TO 20
      IRP1 = IRANK + 1
      MMIR = M - IRANK
C
      SIGMA = KF04JGW(MMIR,B(IRP1))/DSQRT(DBLE(MMIR))
C
   20 CALL KF04JAY(N, IRANK, SV, LSV, B, PT, NRPT, X, WORK)
C
      RETURN
      END
** END OF F04JAZTEXT

*UPTODATE F04JGTTEXT
      SUBROUTINE   KF04JGT(N, X, SCALE, SUMSQ, TINY, UNDFLW)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (SCLSQS)
C
C     F04JGT RETURNS THE VALUES SCL AND SUM SUCH THAT
C
C     (SCL**2)*SUM = X(1)**2+X(2)**2+...+X(N)**2+(SCALE**2)*SUMSQ .
C
C     SCL IS OVERWRITTEN ON SCALE AND SUM IS OVERWRITTEN ON SUMSQ.
C
C     THE SUPPLIED VALUE OF SUMSQ IS ASSUMED TO BE AT LEAST
C     UNITY IN WHICH CASE SUM WILL SATISFY THE BOUNDS
C
C     1.0 .LE. SUM .LE. SUMSQ+N .
C
C     ONLY ONE PASS THROUGH THE VECTOR X IS MADE.
C
C     TINY MUST BE SUCH THAT
C
C     TINY = SQRT(KX02AGF) ,
C
C     WHERE KX02AGF IS THE SMALL VALUE RETURNED FROM ROUTINE KX02AGF.
C
C     UNDFLW MUST BE THE VALUE RETURNED BY KX02DAF
C
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION SCALE, SUMSQ, TINY
      INTEGER N
      LOGICAL UNDFLW
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION X(N)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION ABSXI, Q
      INTEGER I
C     ..
      IF (UNDFLW) GO TO 60
C
      DO 40 I=1,N
         IF (X(I).EQ.0.0D0) GO TO 40
C
         ABSXI = DABS(X(I))
         IF (SCALE.GE.ABSXI) GO TO 20
C
         SUMSQ = 1.0D0 + SUMSQ*(SCALE/ABSXI)**2
         SCALE = ABSXI
         GO TO 40
C
   20    SUMSQ = SUMSQ + (ABSXI/SCALE)**2
C
   40 CONTINUE
C
      RETURN
C
   60 DO 100 I=1,N
         IF (X(I).EQ.0.0D0) GO TO 100
C
         ABSXI = DABS(X(I))
         Q = 0.0D0
         IF (SCALE.LT.ABSXI) GO TO 80
C
         IF (SCALE.GT.TINY) Q = SCALE*TINY
         IF (ABSXI.GE.Q) SUMSQ = SUMSQ + (ABSXI/SCALE)**2
         GO TO 100
C
   80    IF (ABSXI.GT.TINY) Q = ABSXI*TINY
         IF (SCALE.GE.Q) SUMSQ = 1.0D0 + SUMSQ*(SCALE/ABSXI)**2
         SCALE = ABSXI
C
  100 CONTINUE
C
      RETURN
      END
** END OF F04JGTTEXT
*UPTODATE F04JGUTEXT
      DOUBLE PRECISION FUNCTION   KF04JGU(SCALE, SUMSQ, BIG)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (TWONRM)
C
C     KF04JGU RETURNS THE VALUE
C
C     KF04JGU = SCALE*SQRT(SUMSQ) .
C
C     SCALE IS ASSUMED TO BE NON-NEGATIVE AND SUMSQ IS ASSUMED
C     TO BE AT LEAST UNITY.
C
C     BIG MUST BE GIVEN BY
C
C     BIG = 1.0/KX02AGF ,
C
C     WHERE KX02AGF IS THE SMALL REAL VALUE RETURNED FROM
C     ROUTINE KX02AGF.
C
C     KF04JGU IS USED IN CONJUNCTION WITH F04JGT BY VARIOUS
C     EUCLIDEAN NORM ROUTINES.
C
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION BIG, SCALE, SUMSQ
C     ..
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION DSQRT
C     ..
      IF (SCALE.GE.BIG/SUMSQ) GO TO 20
      KF04JGU = SCALE*DSQRT(SUMSQ)
      RETURN
C
   20 KF04JGU = BIG
      RETURN
      END
** END OF F04JGUTEXT
*UPTODATE F04JGVTEXT
      DOUBLE PRECISION FUNCTION   KF04JGV(N, X, TINY, BIG)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (VENORM)
C
C     REAL FUNCTION KF04JGV RETURNS THE VALUE OF THE EUCLIDEAN
C     LENGTH OF THE N ELEMENT VECTOR X. KF04JGV IS DEFINED AS
C
C     KF04JGV = SQRT(X(1)**2+X(2)**2+...+X(N)**2).
C
C     TINY AND BIG MUST BE GIVEN BY
C
C     TINY = SQRT(KX02AGF)   AND   BIG = 1.0/KX02AGF
C
C     WHERE KX02AGF IS THE SMALL REAL VALUE RETURNED FROM
C     ROUTINE KX02AGF.
C
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION BIG, TINY
      INTEGER N
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION X(N)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION SCALE, SUMSQ
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION KF04JGU
      LOGICAL KX02DAF
C     .. SUBROUTINE REFERENCES ..
C     F04JGT
C     ..
      SCALE = 0.0D0
      SUMSQ = 1.0D0
C
      CALL KF04JGT(N, X, SCALE, SUMSQ, TINY, KX02DAF(0.0D0))
C
      KF04JGV = KF04JGU(SCALE,SUMSQ,BIG)
C
      RETURN
      END
** END OF F04JGVTEXT
*UPTODATE F04JGWTEXT
      DOUBLE PRECISION FUNCTION   KF04JGW(N, X)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (V2NORM)
C
C     REAL FUNCTION KF04JGW RETURNS THE VALUE OF THE EUCLIDEAN
C     LENGTH OF THE N ELEMENT VECTOR X. KF04JGW IS DEFINED AS
C
C     KF04JGW = SQRT(X(1)**2+X(2)**2+...+X(N)**2).
C
C     .. SCALAR ARGUMENTS ..
      INTEGER N
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION X(N)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION BIG, SMALL, TINY
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION KF04JGV, DSQRT, KX02AGF
C     ..
      SMALL = KX02AGF(SMALL)
      BIG = 1.0D0/SMALL
      TINY = DSQRT(SMALL)
C
      KF04JGW = KF04JGV(N,X,TINY,BIG)
C
      RETURN
      END
** END OF F04JGWTEXT
* F01FTEXT
*UPTODATE F01LZFTEXT
      SUBROUTINE   KF01LZF(N, A, NRA, C, NRC, WANTB, B, WANTQ, WANTY,Y,
     * NRY, LY, WANTZ, Z, NRZ, NCZ, D, E, WORK1, WORK2, IFAIL)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (BIDIAG)
C
C     F01LZF RETURNS ALL OR PART OF THE FACTORIZATION OF THE
C     N*N UPPER TRIANGULAR MATRIX A GIVEN BY
C
C     A = Q*C*(P**T) ,
C
C     WHERE Q AND P ARE N*N ORTHOGONAL MATRICES AND C IS AN
C     N*N UPPER BIDIAGONAL MATRIX.
C
C     IF WANTB IS .TRUE. THEN B RETURNS (Q**T)*B.
C     IF WANTY IS .TRUE. THEN Y RETURNS Y*Q.
C     IF WANTZ IS .TRUE. THEN Z RETURNS (P**T)*Z.
C
C     INPUT PARAMETERS.
C
C     N     - ORDER OF THE MATRIX A.
C
C     A     - THE N*N UPPER TRIANGULAR MATRIX TO BE FACTORIZED. THE
C             STRICTLY LOWER TRIANGULAR PART OF A IS NOT REFERENCED.
C
C     NRA   - ROW DIMENSION OF A AS DECLARED IN THE CALLING PROGRAM.
C             NRA MUST BE AT LEAST N.
C
C     NRC   - ROW DIMENSION OF C AS DECLARED IN THE CALLING PROGRAM.
C             NRC MUST BE AT LEAST N.
C
C     WANTB - MUST BE .TRUE. IF (Q**T)*B IS REQUIRED.
C             IF WANTB IS .FALSE. THEN B IS NOT REFERENCED.
C
C     B     - AN N ELEMENT REAL VECTOR.
C
C     WANTQ - MUST BE .TRUE. IF DETAILS OF Q ARE TO BE
C             STORED BELOW THE BIDIAGONAL PART OF C.
C             IF WANTQ IS .FALSE. THEN THE LOWER TRIANGULAR
C             PART OF C IS NOT REFERENCED.
C
C     WANTY - MUST BE .TRUE. IF Y*Q IS REQUIRED.
C             IF WANTY IS .FALSE. THEN Y IS NOT REFERENCED.
C
C     Y     - AN LY*N REAL MATRIX.
C
C     NRY   - IF WANTY IS .TRUE. THEN NRY MUST BE THE ROW
C             DIMENSION OF Y AS DECLARED IN THE CALLING
C             PROGRAM AND MUST BE AT LEAST LY.
C
C     LY    - IF WANTY IS .TRUE. THEN LY MUST BE THE NUMBER
C             OF ROWS OF Y AND MUST BE AT LEAST 1.
C
C     WANTZ - MUST BE .TRUE. IF (P**T)*Z IS REQUIRED.
C             IF WANTZ IS .FALSE. THEN Z IS NOT REFERENCED.
C
C     Z     - AN N*NCZ REAL MATRIX.
C
C     NRZ   - IF WANTZ IS .TRUE. THEN NRZ MUST BE THE ROW
C             DIMENSION OF Z AS DECLARED IN THE CALLING
C             PROGRAM AND MUST BE AT LEAST N.
C
C     NCZ   - IF WANTZ IS .TRUE. THEN NCZ MUST BE THE
C             NUMBER OF COLUMNS OF Z AND MUST BE AT LEAST
C             1.
C
C     IFAIL - THE USUAL FAILURE PARAMETER. IF IN DOUBT SET
C             IFAIL TO ZERO BEFORE CALLING THIS ROUTINE.
C
C     OUTPUT PARAMETERS.
C
C     C     - N*N MATRIX CONTAINING THE UPPER BIDIAGONAL MATRIX B.
C             DETAILS OF P ARE STORED ABOVE THE BIDIAGONAL
C             PART OF C. UNLESS WANTQ IS .TRUE. THE
C             STRICTLY LOWER TRIANGULAR PART OF C IS NOT
C             REFERENCED.
C             THE ROUTINE MAY BE CALLED WITH C=A.
C
C     B     - IF WANTB IS .TRUE. THEN B WILL RETURN THE N ELEMENT
C             VECTOR (Q**T)*B.
C
C     Y     - IF WANTY IS .TRUE. THEN Y WILL RETURN THE
C             LY*N MATRIX Y*Q.
C
C     Z     - IF WANTZ IS .TRUE. THEN Z WILL RETURN THE N*NCZ MATRIX
C             (P**T)*Z.
C
C     D     - N ELEMENT VECTOR CONTAINING THE DIAGONAL ELEMENTS OF C
C             SUCH THAT D(I)=C(I,I), I=1,2,...,N.
C
C     E     - N ELEMENT VECTOR CONTAINING THE
C             SUPER-DIAGONAL ELEMENTS OF C SUCH THAT
C             E(I)=C(I-1,I), I=2,3,...,N. E(1) IS NOT
C             REFERENCED.
C
C     IFAIL - ON NORMAL RETURN IFAIL WILL BE ZERO.
C             IF AN INPUT PARAMETER IS INCORRECTLY SUPPLIED
C             THEN IFAIL IS SET TO UNITY. NO OTHER FAILURE
C             IS POSSIBLE.
C
C     WORKSPACE PARAMETERS.
C
C     WORK1
C     WORK2 - N ELEMENT REAL VECTORS.
C             IF WANTZ IS .FALSE. THEN WORK1 AND WORK2 ARE NOT
C             REFERENCED.
C
C     .. SCALAR ARGUMENTS ..
      INTEGER IFAIL, LY, N, NCZ, NRA, NRC, NRY, NRZ
      LOGICAL WANTB, WANTQ, WANTY, WANTZ
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION A(NRA,N), B(N), C(NRC,N), D(N), E(N), WORK1(N),
     *WORK2(N), Y(NRY,N), Z(NRZ,NCZ)
C     ..
C     .. LOCAL SCALARS ..
C$P 1
      DOUBLE PRECISION SRNAME
      DOUBLE PRECISION BIG, CS, EPS, RSQTPS, SMALL, SN, SQTEPS, T, W, X
      INTEGER I, IERR, J, JJ, JP1, K, KP1, KP2, NM2
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION KF01LZZ, DSQRT, KX02AAF, KX02AGF
      INTEGER KP01AAF
C     .. SUBROUTINE REFERENCES ..
C     F01LZW, F01LZX, F01LZY
C     ..
      DATA SRNAME /8H F01LZF /
      IERR = IFAIL
      IF (IERR.EQ.0) IFAIL = 1
C
      IF (NRA.LT.N .OR. NRC.LT.N .OR. N.LT.1) GO TO 220
      IF (WANTY .AND. (NRY.LT.LY .OR. LY.LT.1)) GO TO 220
      IF (WANTZ .AND. (NRZ.LT.N .OR. NCZ.LT.1)) GO TO 220
C
      SMALL = KX02AGF(0.0D0)
      BIG = 1.0D0/SMALL
      EPS = KX02AAF(0.0D0)
      SQTEPS = DSQRT(EPS)
      RSQTPS = 1.0D0/SQTEPS
C
      D(1) = A(1,1)
C
      DO 40 J=1,N
         DO 20 I=1,J
            C(I,J) = A(I,J)
   20    CONTINUE
   40 CONTINUE
C
      IFAIL = 0
      IF (N.EQ.1) RETURN
      IF (N.EQ.2) GO TO 200
C
C     START MAIN LOOP. K(TH) STEP PUTS ZEROS INTO K(TH) ROW OF C.
C
      NM2 = N - 2
      DO 180 K=1,NM2
         KP1 = K + 1
C
C     SET UP PLANE ROTATION P(J,J+1) TO ANNIHILATE C(K,J+1).
C     THIS ROTATION INTRODUCES AN UNWANTED ELEMENT IN C(J+1,J)
C     WHICH IS STORED IN X.
C     J GOES N-1,N-2,...,K+1.
C
         J = N
         DO 100 JJ=K,NM2
            JP1 = J
            J = J - 1
            W = C(K,JP1)
C
            T = KF01LZZ(C(K,J),W,SMALL,BIG)
C
            C(K,JP1) = T
            X = 0.0D0
C
            CALL KF01LZW(T, CS, SN, SQTEPS, RSQTPS, BIG)
C
            IF (.NOT.WANTZ) GO TO 60
            WORK1(J) = CS
            WORK2(J) = SN
C
   60       IF (T.EQ.0.0D0) GO TO 80
            C(K,J) = CS*C(K,J) + SN*W
C
C     NOW APPLY THE TRANSFORMATION P(J,J+1).
C
            CALL KF01LZY(J-K, CS, SN, C(KP1,J), C(KP1,JP1))
C
            X = SN*C(JP1,JP1)
            C(JP1,JP1) = CS*C(JP1,JP1)
C
C     NOW SET UP PLANE ROTATION Q(J,J+1)**T TO ANNIHILATE
C     X=C(J+1,J).
C
   80       T = KF01LZZ(C(J,J),X,SMALL,BIG)
C
            IF (WANTQ) C(JP1,K) = T
C
            CALL KF01LZW(T, D(J), E(J), SQTEPS, RSQTPS, BIG)
C
            C(J,J) = D(J)*C(J,J) + E(J)*X
C
            IF (WANTY) CALL KF01LZY(LY, D(J), E(J), Y(1,J), Y(1,JP1))
C
  100    CONTINUE
C
C     NOW APPLY THE TRANSFORMATIONS Q(J,J+1)**T AND FORM
C     (P(J,J+1)**T)*Z, J=N-1,N-2,...,K+1 COLUMN BY COLUMN
C
         KP2 = KP1 + 1
         DO 120 J=KP2,N
C
            CALL KF01LZX(J-K, D(K), E(K), C(KP1,J))
C
  120    CONTINUE
C
         IF (WANTB) CALL KF01LZX(N-K, D(K), E(K), B(KP1))
C
         IF (.NOT.WANTZ) GO TO 160
         DO 140 J=1,NCZ
C
            CALL KF01LZX(N-K, WORK1(K), WORK2(K), Z(KP1,J))
C
  140    CONTINUE
C
  160    D(KP1) = C(KP1,KP1)
         E(KP1) = C(K,KP1)
C
  180 CONTINUE
C
  200 D(N) = C(N,N)
      E(N) = C(N-1,N)
      RETURN
C
  220 IFAIL = KP01AAF(IERR,IFAIL,SRNAME)
      RETURN
      END
** END OF F01LZFTEXT
*UPTODATE F01LZWTEXT
      SUBROUTINE   KF01LZW(T, C, S, SQTEPS, RSQTPS, BIG)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (COSSIN)
C
C     F01LZW RETURNS THE VALUES
C
C     C = COS(THETA)   AND   S = SIN(THETA)
C
C     FOR A GIVEN VALUE OF
C
C     T = TAN(THETA) .
C
C     C IS ALWAYS NON-NEGATIVE AND S HAS THE SAME SIGN AS T.
C
C     SQTEPS, RSQTPS AND BIG MUST BE SUCH THAT
C
C     SQTEPS = SQRT(KX02AAF) , RSQTPS = 1.0/SQTEPS AND BIG =
C     1.0/KX02AGF ,
C
C     WHERE KX02AAF AND KX02AGF ARE THE NUMBERS RETURNED FROM
C     ROUTINES KX02AAF AND KX02AGF RESPECTIVELY.
C
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION BIG, C, RSQTPS, S, SQTEPS, T
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION ABST, TT
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION DSQRT
C     ..
      IF (T.NE.0.0D0) GO TO 20
      C = 1.0D0
      S = 0.0D0
      RETURN
C
   20 ABST = DABS(T)
      IF (ABST.LT.SQTEPS) GO TO 60
      IF (ABST.GT.RSQTPS) GO TO 80
C
      TT = ABST*ABST
      IF (ABST.GT.1.0D0) GO TO 40
C
      TT = 0.25D0*TT
      C = 0.5D0/DSQRT(0.25D0+TT)
      S = C*T
      RETURN
C
   40 TT = 0.25D0/TT
      S = 0.5D0/DSQRT(0.25D0+TT)
      C = S/ABST
      S = DSIGN(S,T)
      RETURN
C
   60 C = 1.0D0
      S = T
      RETURN
C
   80 C = 0.0D0
      IF (ABST.LT.BIG) C = 1.0D0/ABST
      S = DSIGN(1.0D0,T)
      RETURN
      END
** END OF F01LZWTEXT
*UPTODATE F01LZXTEXT
      SUBROUTINE   KF01LZX(N, C, S, X)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (PLROT6)
C
C     F01LZX RETURNS THE N ELEMENT VECTOR
C
C     Y = R(1,2)*R(2,3)*...*R(N-1,N)*X ,
C
C     WHERE X IS AN N ELEMENT VECTOR AND R(J-1,J) IS A PLANE
C     ROTATION FOR THE (J-1,J)-PLANE.
C
C     Y IS OVERWRITTEN ON X.
C
C     THE N ELEMENT VECTORS C AND S MUST BE SUCH THAT THE
C     NON-IDENTITY PART OF R(J-1,J) IS GIVEN BY
C
C     R(J-1,J) = (  C(J)  S(J) ) .
C                ( -S(J)  C(J) )
C
C     C(1) AND S(1) ARE NOT REFERENCED.
C
C     .. SCALAR ARGUMENTS ..
      INTEGER N
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION C(N), S(N), X(N)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION W
      INTEGER I, II, IM1
C     ..
C
C     N MUST BE AT LEAST 1. IF N=1 THEN AN IMMEDIATE RETURN TO
C     THE CALLING PROGRAM IS MADE.
C
      IF (N.EQ.1) RETURN
C
      I = N
      DO 20 II=2,N
         IM1 = I - 1
         W = X(IM1)
         X(IM1) = C(I)*W + S(I)*X(I)
         X(I) = C(I)*X(I) - S(I)*W
         I = IM1
   20 CONTINUE
C
      RETURN
      END
** END OF F01LZXTEXT
*UPTODATE F01LZYTEXT
      SUBROUTINE   KF01LZY(N, C, S, X, Y)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (PLROT8)
C
C     F01LZY FORMS THE N*2 MATRIX
C
C     Z = ( X  Y )*( C  -S ) ,
C                  ( S   C )
C
C     WHERE X AND Y ARE N ELEMENT VECTORS, C=COS(THETA) AND
C     S=SIN(THETA).
C
C     THE FIRST COLUMN OF Z IS OVERWRITTEN ON X AND THE SECOND
C     COLUMN OF Z IS OVERWRITTEN ON Y.
C
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION C, S
      INTEGER N
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION X(N), Y(N)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION W
      INTEGER I
C     ..
C
C     N MUST BE AT LEAST 1.
C
      DO 20 I=1,N
         W = X(I)
         X(I) = C*W + S*Y(I)
         Y(I) = C*Y(I) - S*W
   20 CONTINUE
C
      RETURN
      END
** END OF F01LZYTEXT
*UPTODATE F01LZZTEXT
      DOUBLE PRECISION FUNCTION   KF01LZZ(A, B, SMALL, BIG)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (TANGNT)
C
C     KF01LZZ RETURNS THE VALUE
C
C     KF01LZZ = B/A .
C
C     SMALL AND BIG MUST BE SUCH THAT
C
C     SMALL = KX02AGF     AND     BIG = 1.0/SMALL ,
C
C     WHERE KX02AGF IS THE SMALL NUMBER RETURNED FROM ROUTINE
C     KX02AGF.
C
C     IF B/A IS LESS THAN SMALL THEN KF01LZZ IS RETURNED AS
C     ZERO AND IF B/A IS GREATER THAN BIG THEN KF01LZZ IS
C     RETURNED AS SIGN(BIG,B).
C
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION A, B, BIG, SMALL
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION ABSA, ABSB, X
C     ..
      KF01LZZ = 0.0D0
      IF (B.EQ.0.0D0) RETURN
C
      ABSA = DABS(A)
      ABSB = DABS(B)
      X = 0.0D0
      IF (ABSA.GE.1.0D0) X = ABSA*SMALL
C
      IF (ABSB.LT.X) RETURN
C
      X = 0.0D0
      IF (ABSB.GE.1.0D0) X = ABSB*SMALL
C
      IF (ABSA.LE.X) GO TO 20
C
      KF01LZZ = B/A
      RETURN
C
   20 KF01LZZ = DSIGN(BIG,B)
      RETURN
      END
** END OF F01LZZTEXT
*UPTODATE F01QAWTEXT
      SUBROUTINE   KF01QAW(N, X, XMUL, Y, UNDFLW)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (ROWOP2)
C
C     F01QAW RETURNS THE N ELEMENT VECTOR Z GIVEN BY
C
C     Z = X - XMUL*Y ,
C
C     WHERE XMUL IS A REAL VALUE AND X AND Y ARE N ELEMENT
C     VECTORS.
C
C     Z IS OVERWRITTEN ON X.
C
C     N MUST BE AT LEAST 1.
C
C     UNDFLW MUST BE THE VALUE RETURNED BY KX02DAF
C
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION XMUL
      INTEGER N
      LOGICAL UNDFLW
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION X(N), Y(N)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION AMUL, W
      INTEGER I
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION KX02AGF
C     ..
      IF (XMUL.EQ.0.0D0) RETURN
C
      IF (UNDFLW) GO TO 60
C
   20 DO 40 I=1,N
         X(I) = X(I) - XMUL*Y(I)
   40 CONTINUE
C
      RETURN
C
   60 AMUL = DABS(XMUL)
      IF (AMUL.GE.1.0D0) GO TO 20
      W = KX02AGF(0.0D0)/AMUL
C
      DO 80 I=1,N
         IF (DABS(Y(I)).LT.W) GO TO 80
         X(I) = X(I) - XMUL*Y(I)
   80 CONTINUE
C
      RETURN
      END
** END OF F01QAWTEXT
*UPTODATE F01QAXTEXT
      DOUBLE PRECISION FUNCTION   KF01QAX(NR, N, V, PLUS, X, Y, UNDFLW)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (DOTPRD)
C
C     KF01QAX RETURNS THE VALUE
C
C     KF01QAX = ( V + (X**T)*Y , WHEN PLUS = .TRUE.
C              (
C              ( V - (X**T)*Y , WHEN PLUS = .FALSE. ,
C
C     WHERE V IS A REAL VALUE AND X AND Y ARE N ELEMENT VECTORS.
C
C     IF N IS LESS THAN UNITY THEN KF01QAX IS RETURNED AS V.
C
C     NR MUST BE AT LEAST MAX(1,N).
C
C     UNDFLW MUST BE THE VALUE RETURNED BY KX02DAF
C
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION V
      INTEGER N, NR
      LOGICAL PLUS, UNDFLW
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION X(NR), Y(NR)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION ABSXI, SMALL, SUM
      INTEGER I
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION KX02AGF
C     ..
      SUM = V
C
      IF (N.LT.1) GO TO 80
C
      IF (UNDFLW) GO TO 100
C
      IF (PLUS) GO TO 40
C
      DO 20 I=1,N
         SUM = SUM - X(I)*Y(I)
   20 CONTINUE
C
      KF01QAX = SUM
C
      RETURN
C
   40 DO 60 I=1,N
         SUM = SUM + X(I)*Y(I)
   60 CONTINUE
C
   80 KF01QAX = SUM
C
      RETURN
C
  100 SMALL = KX02AGF(0.0D0)
C
      IF (PLUS) GO TO 160
C
      DO 140 I=1,N
         ABSXI = DABS(X(I))
         IF (ABSXI.GE.1.0D0 .OR. ABSXI.EQ.0.0D0) GO TO 120
         IF (DABS(Y(I)).LT.SMALL/ABSXI) GO TO 140
  120    SUM = SUM - X(I)*Y(I)
  140 CONTINUE
C
      KF01QAX = SUM
C
      RETURN
C
  160 DO 200 I=1,N
         ABSXI = DABS(X(I))
         IF (ABSXI.GE.1.0D0 .OR. ABSXI.EQ.0.0D0) GO TO 180
         IF (DABS(Y(I)).LT.SMALL/ABSXI) GO TO 200
  180    SUM = SUM + X(I)*Y(I)
  200 CONTINUE
C
      KF01QAX = SUM
C
      RETURN
      END
** END OF F01QAXTEXT

*UPTODATE F01QBFTEXT
      SUBROUTINE   KF01QBF(M, N, A, NRA, C, NRC, WORK, IFAIL)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (GIVNUQ)
C
C     SUBROUTINE KF01QBF RETURNS, IN C, THE GIVENS UQ
C     FACTORIZATION OF THE M*N (M.LE.N) MATRIX A. THAT IS, A
C     IS FACTORIZED AS
C
C         A = (U O)Q , M.LT.N,          A = UQ , M.EQ.N,
C
C     WHERE U IS AN M*M UPPER TRIANGULAR MATRIX AND Q IS AN
C     N*N ORTHOGONAL MATRIX.
C
C     INPUT PARAMETERS.
C
C     M     - NUMBER OF ROWS OF A. M MUST BE AT LEAST UNITY.
C
C     N     - NUMBER OF COLUMNS OF A. N MUST BE AT LEAST M.
C
C     A     - THE M*N MATRIX TO BE FACTORIZED.
C
C     NRA   - ROW DIMENSION OF A AS DECLARED IN THE CALLING PROGRAM.
C             NRA MUST BE AT LEAST M.
C
C     NRC   - ROW DIMENSION OF C AS DECLARED IN THE CALLING PROGRAM.
C             NRC MUST BE AT LEAST M
C
C     IFAIL - THE USUAL FAILURE PARAMETER. IF IN DOUBT SET
C             IFAIL TO ZERO BEFORE CALLING THIS ROUTINE.
C
C     OUTPUT PARAMETERS.
C
C     C     - AN M*N MATRIX CONTAINING DETAILS OF THE UQ
C             FACTORIZATION. U IS RETURNED IN THE LEFT HAND
C             UPPER TRIANGULAR PART OF C. DETAILS OF Q ARE
C             STORED IN THE REMAINING PART OF C SUCH THAT
C             C(K,J) CONTAINS THE TANGENT FOR THE PLANE
C             ROTATION,  R(K,J), IN THE K,J PLANE THAT
C             ANNHILATES THE K,J ELEMENT OF A.
C             THE ROUTINE MAY BE CALLED WITH C=A.
C
C     IFAIL - ON NORMAL RETURN IFAIL WILL BE ZERO.
C             IF AN INPUT PARAMETER IS INCORRECTLY SUPPLIED
C             THEN IFAIL IS SET TO UNITY. NO OTHER FAILURE
C             IS POSSIBLE.
C
C     WORKSPACE PARAMETER.
C
C     WORK  - AN M ELEMENT VECTOR.
C
C     .. SCALAR ARGUMENTS ..
      INTEGER IFAIL, M, N, NRA, NRC
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION A(NRA,N), C(NRC,N), WORK(M)
C     ..
C     .. LOCAL SCALARS ..
C$P 1
      DOUBLE PRECISION SRNAME
      DOUBLE PRECISION BIG, CKK, CS, RSQTPS, SMALL, SN, SQTEPS, T, Z
      INTEGER I, IERR, J, JF, JJ, JL, K, KK, KM1
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION KF01LZZ, DSQRT, KX02AAF, KX02AGF
      INTEGER KP01AAF
C     .. SUBROUTINE REFERENCES ..
C     F01LZW, F01LZY
C     ..
      DATA SRNAME /8H F01QBF /
      IERR = IFAIL
      IF (IERR.EQ.0) IFAIL = 1
C
      IF (NRA.LT.M .OR. NRC.LT.M .OR. M.LT.1 .OR. N.LT.M) GO TO 220
C
      SMALL = KX02AGF(0.0D0)
      BIG = 1.0D0/SMALL
      SQTEPS = DSQRT(KX02AAF(0.0D0))
      RSQTPS = 1.0D0/SQTEPS
C
      DO 40 J=1,N
         DO 20 I=1,M
            C(I,J) = A(I,J)
   20    CONTINUE
   40 CONTINUE
C
      IFAIL = 0
      K = M
      DO 200 KK=1,M
C
         DO 60 I=1,K
            WORK(I) = C(I,K)
   60    CONTINUE
         CKK = WORK(K)
C
         JL = 1
         KM1 = K - 1
         IF (K.EQ.1) GO TO 140
         JF = KM1
   80    J = JF
         DO 120 JJ=JL,JF
            Z = C(K,J)
C
            T = KF01LZZ(CKK,Z,SMALL,BIG)
C
            C(K,J) = T
C
C     C(K,J) NOW CONTAINS THE TANGENT FOR THE PLANE ROTATION
C     R(K,J). IF THE TANGENT IS ZERO THEN WE CAN SKIP THE
C     TRANSFORMATION, OTHERWISE WE COMPUTE CS=COSINE AND
C     SN=SINE AND PERFORM THE TRANSFORMATION R(K,J).
C
            IF (T.EQ.0.0D0) GO TO 100
C
            CALL KF01LZW(T, CS, SN, SQTEPS, RSQTPS, BIG)
C
            CKK = CS*CKK + SN*Z
C
            IF (K.GT.1) CALL KF01LZY(KM1, CS, SN, WORK, C(1,J))
C
  100       J = J - 1
  120    CONTINUE
C
  140    IF (JL.NE.1 .OR. M.EQ.N) GO TO 160
         JF = N
         JL = M + 1
         GO TO 80
C
C     THE RETURN TO STATEMENT NUMBER 80 IS TO PERFORM THE
C     TRANSFORMATIONS FOR COLUMNS M+1,M+2,...,N.
C
  160    WORK(K) = CKK
         DO 180 I=1,K
            C(I,K) = WORK(I)
  180    CONTINUE
C
         K = KM1
  200 CONTINUE
C
      RETURN
C
  220 IFAIL = KP01AAF(IERR,IFAIL,SRNAME)
      RETURN
      END
** END OF F01QBFTEXT
* F02FTEXT
*UPTODATE F02SZFTEXT
      SUBROUTINE   KF02SZF(N,D,E,SV, WANTB, B, WANTY, Y, NRY, LY,WANTZ,
     * Z, NRZ, NCZ, WORK1, WORK2, WORK3, IFAIL)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     MARK 9 REVISED. IER-328 (SEP 1981).
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (SVDBID)
C
C     F02SZF RETURNS PART OR ALL OF THE SINGULAR VALUE
C     DECOMPOSITION OF THE N*N UPPER BIDIAGONAL MATRIX A. THAT
C     IS, A IS FACTORIZED AS
C
C     A = Q*DIAG(SV)*(P**T) ,
C
C     WHERE Q AND P ARE N*N ORTHOGONAL MATRICES AND DIAG(SV)
C     IS AN N*N DIAGONAL MATRIX WITH NON-NEGATIVE DIAGONAL
C     ELEMENTS SV(1),SV(2),..., SV(N), THESE BEING THE
C     SINGULAR VALUES OF A.
C
C     IF WANTB IS .TRUE. THEN B RETURNS (Q**T)*B.
C     IF WANTY IS .TRUE. THEN Y RETURNS Y*Q.
C     IF WANTZ IS .TRUE. THEN Z RETURNS (P**T)*Z.
C
C     INPUT PARAMETERS.
C
C     N     - THE ORDER OF THE MATRIX. MUST BE AT LEAST 1.
C
C     D     - N ELEMENT VECTOR SUCH THAT D(I)=A(I,I), I=1,2,...,N.
C             D IS UNALTERED UNLESS ROUTINE IS CALLED WITH SV=D.
C
C     E     - N ELEMENT VECTOR SUCH THAT E(I)=A(I-1,I), I=2,3,...,N.
C             E(1) IS NOT REFERENCED.
C             E IS UNALTERED UNLESS ROUTINE IS CALLED WITH WORK1=E.
C
C     WANTB - MUST BE .TRUE. IF (Q**T)*B IS REQUIRED.
C             IF WANTB IS .FALSE. THEN B IS NOT REFERENCED.
C
C     B     - AN N ELEMENT REAL VECTOR.
C
C     WANTY - MUST BE .TRUE. IF Y*Q IS REQUIRED.
C             IF WANTY IS .FALSE. THEN Y IS NOT REFERENCED.
C
C     Y     - AN LY*N REAL MATRIX.
C
C     NRY   - IF WANTY IS .TRUE. THEN NRY MUST BE THE ROW
C             DIMENSION OF Y AS DECLARED IN THE CALLING
C             PROGRAM AND MUST BE AT LEAST LY.
C
C     LY    - IF WANTY IS .TRUE. THEN LY MUST BE THE NUMBER
C             OF ROWS OF Y AND MUST BE AT LEAST 1.
C
C     WANTZ - MUST BE .TRUE. IF (P**T)*Z IS REQUIRED.
C             IF WANTZ IS .FALSE. THEN Z IS NOT REFERENCED.
C
C     Z     - AN N*NCZ REAL MATRIX.
C
C     NRZ   - IF WANTZ IS .TRUE. THEN NRZ MUST BE THE ROW
C             DIMENSION OF Z AS DECLARED IN THE CALLING
C             PROGRAM AND MUST BE AT LEAST N.
C
C     NCZ   - IF WANTZ IS .TRUE. THEN NCZ MUST BE THE
C             NUMBER OF COLUMNS OF Z AND MUST BE AT LEAST
C             1.
C
C     IFAIL - THE USUAL FAILURE PARAMETER. IF IN DOUBT SET
C             IFAIL TO ZERO BEFORE CALLING F02SZF.
C
C     OUTPUT PARAMETERS.
C
C     SV    - N ELEMENT VECTOR CONTAINING THE SINGULAR
C             VALUES OF A. THEY ARE ORDERED SO THAT
C             SV(1).GE.SV(2).GE. ... .GE.SV(N). THE ROUTINE
C             MAY BE CALLED WITH SV=D.
C
C     B     - IF WANTB IS .TRUE. THEN B WILL RETURN THE N
C             ELEMENT VECTOR (Q**T)*B.
C
C     Y     - IF WANTY IS .TRUE. THEN Y WILL RETURN THE
C             LY*N MATRIX Y*Q.
C
C     Z     - IF WANTZ IS .TRUE. THEN Z WILL RETURN THE N*NCZ MATRIX
C             (P**T)*Z.
C
C     IFAIL - ON NORMAL RETURN IFAIL WILL BE ZERO.
C             IN THE UNLIKELY EVENT THAT THE QR-ALGORITHM
C             FAILS TO FIND THE SINGULAR VALUES IN 50*N
C             ITERATIONS THEN IFAIL WILL BE 2 OR MORE AND
C             SUCH THAT SV(1),SV(2),..,SV(IFAIL-1) MAY NOT
C             HAVE BEEN FOUND. SEE WORK1 BELOW. THIS
C             FAILURE IS NOT LIKELY TO OCCUR.
C             IF AN INPUT PARAMETER IS INCORRECTLY SUPPLIED
C             THEN IFAIL IS SET TO UNITY.
C
C     WORKSPACE PARAMETERS.
C
C     WORK1 - AN N ELEMENT VECTOR. IF E IS NOT REQUIRED ON
C             RETURN THEN THE ROUTINE MAY BE CALLED WITH
C             WORK1=E. WORK1(1) RETURNS THE TOTAL NUMBER OF
C             ITERATIONS TAKEN BY THE  QR-ALGORITHM. IF
C             IFAIL IS POSITIVE ON RETURN THEN THE MATRIX A
C             IS GIVEN  BY A=Q*C*(P**T) , WHERE C IS THE
C             UPPER BIDIAGONAL MATRIX WITH SV AS ITS
C             DIAGONAL AND WORK1 AS ITS SUPER-DIAGONAL.
C
C     WORK2
C     WORK3 - N ELEMENT VECTORS. IF WANTZ IS .FALSE. THEN WORK2 AND
C             WORK3 ARE NOT REFERENCED.
C
C     .. SCALAR ARGUMENTS ..
      INTEGER IFAIL, LY, N, NCZ, NRY, NRZ
      LOGICAL WANTB, WANTY, WANTZ
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION B(N), D(N), E(N), SV(N), WORK1(N), WORK2(N),
     * WORK3(N),Y(NRY,N), Z(NRZ,NCZ)
C     ..
C     .. LOCAL SCALARS ..
C$P 1
      DOUBLE PRECISION SRNAME
      DOUBLE PRECISION ANORM, BIG, C, DK, DKM1, DL, EK, EKM1, EPS, F, G,
     *RSQTPS, S, SHIFT, SMALL, SQTEPS, SVI, T, X
      INTEGER I, IERR, ITER, J, JJ, K, KK, L, LL, LM1, LP1, MAXIT
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION KF01LZZ, DSQRT, KX02AAF, KX02AGF
      INTEGER KP01AAF
C     .. SUBROUTINE REFERENCES ..
C     F01LZW, F01LZY, F02SZZ
C     ..
      DATA SRNAME /8H F02SZF /
      IERR = IFAIL
      IF (IERR.EQ.0) IFAIL = 1
C
      IF (N.LT.1) GO TO 500
      IF (WANTY .AND. (NRY.LT.LY .OR. LY.LT.1)) GO TO 500
      IF (WANTZ .AND. (NRZ.LT.N .OR. NCZ.LT.1)) GO TO 500
C
      SMALL = KX02AGF(0.0D0)
      BIG = 1.0D0/SMALL
      EPS = KX02AAF(0.0D0)
      SQTEPS = DSQRT(EPS)
      RSQTPS = 1.0D0/SQTEPS
C
      ITER = 0
      K = N
      SV(1) = D(1)
      ANORM = DABS(D(1))
      IF (N.EQ.1) GO TO 280
C
      DO 20 I=2,N
         SV(I) = D(I)
         WORK1(I) = E(I)
         ANORM = DMAX1(ANORM,DABS(D(I)),DABS(E(I)))
   20 CONTINUE
C
      MAXIT = 50*N
      EPS = EPS*ANORM
C
C     MAXIT IS THE MAXIMUM NUMBER OF ITERATIONS ALLOWED.
C     EPS WILL BE USED TO TEST FOR NEGLIGIBLE ELEMENTS.
C     START MAIN LOOP. ONE SINGULAR VALUE IS FOUND FOR EACH
C     VALUE OF K. K GOES IN OPPOSITE DIRECTION TO KK.
C
      DO 260 KK=2,N
C
C     NOW TEST FOR SPLITTING. L GOES IN OPPOSITE DIRECTION TO LL.
C
   40    L = K
         DO 60 LL=2,K
            IF (DABS(WORK1(L)).LE.EPS) GO TO 240
            L = L - 1
            IF (DABS(SV(L)).LT.EPS) GO TO 180
   60    CONTINUE
C
   80    IF (ITER.EQ.MAXIT) GO TO 280
C
C     MAXIT QR-STEPS WITHOUT CONVERGENCE. FAILURE.
C
         ITER = ITER + 1
C
C     NOW DETERMINE SHIFT.
C
         LP1 = L + 1
         DL = SV(L)
         DKM1 = SV(K-1)
         DK = SV(K)
         EKM1 = 0.0D0
         IF (K.NE.2) EKM1 = WORK1(K-1)
         EK = WORK1(K)
         F = (DKM1-DK)*(DKM1+DK) + (EKM1-EK)*(EKM1+EK)
         F = F/(2.0D0*EK*DKM1)
         G = DABS(F)
         IF (G.LE.RSQTPS) G = DSQRT(1.0D0+F**2)
         IF (F.LT.0.0D0) G = -G
C
         SHIFT = EK*(EK-DKM1/(F+G))
         F = (DL-DK)*(DL+DK) - SHIFT
         X = DL*WORK1(LP1)
C
C     NOW PERFORM THE QR-STEP AND CHASE ZEROS.
C
         DO 140 I=LP1,K
C
            T = KF01LZZ(F,X,SMALL,BIG)
C
            CALL KF01LZW(T, C, S, SQTEPS, RSQTPS, BIG)
C
            IF (I.GT.LP1) WORK1(I-1) = C*F + S*X
            F = C*SV(I-1) + S*WORK1(I)
            WORK1(I) = C*WORK1(I) - S*SV(I-1)
            X = S*SV(I)
            SVI = C*SV(I)
C
            IF (.NOT.WANTZ) GO TO 100
            WORK2(I) = C
            WORK3(I) = S
C
  100       T = KF01LZZ(F,X,SMALL,BIG)
C
            CALL KF01LZW(T, C, S, SQTEPS, RSQTPS, BIG)
C
            IF (WANTY) CALL KF01LZY(LY, C, S, Y(1,I-1), Y(1,I))
C
            IF (.NOT.WANTB) GO TO 120
            T = B(I)
            B(I) = C*T - S*B(I-1)
            B(I-1) = C*B(I-1) + S*T
C
  120       SV(I-1) = C*F + S*X
            F = C*WORK1(I) + S*SVI
            SV(I) = C*SVI - S*WORK1(I)
C
            IF (I.EQ.K) GO TO 140
            X = S*WORK1(I+1)
            WORK1(I+1) = C*WORK1(I+1)
C
  140    CONTINUE
C
         WORK1(K) = F
         IF (.NOT.WANTZ) GO TO 40
         DO 160 J=1,NCZ
C
            CALL KF02SZZ(K-L+1, WORK2(L), WORK3(L), Z(L,J))
C
  160    CONTINUE
         GO TO 40
C
C     COME TO NEXT PIECE IF SV(L-1) IS NEGLIGIBLE. FORCE A SPLIT.
C
  180    LM1 = L
         L = L + 1
         X = WORK1(L)
         WORK1(L) = 0.0D0
         DO 220 I=L,K
C
            T = KF01LZZ(SV(I),X,SMALL,BIG)
C
            CALL KF01LZW(T, C, S, SQTEPS, RSQTPS, BIG)
C
            IF (WANTY) CALL KF01LZY(LY, C, -S, Y(1,LM1), Y(1,I))
C
            IF (.NOT.WANTB) GO TO 200
            T = B(I)
            B(I) = C*T + S*B(LM1)
            B(LM1) = C*B(LM1) - S*T
C
  200       SV(I) = C*SV(I) + S*X
            IF (I.EQ.K) GO TO 220
            X = -S*WORK1(I+1)
            WORK1(I+1) = C*WORK1(I+1)
C
  220    CONTINUE
C
C     IF WE COME HERE WITH L=K THEN A SINGULAR VALUE HAS BEEN
C     FOUND.
C
  240    IF (L.LT.K) GO TO 80
C
         K = K - 1
  260 CONTINUE
C
  280 IFAIL = K - 1
      WORK1(1) = ITER
C
C     NOW MAKE SINGULAR VALUES NON-NEGATIVE.
C     K WILL BE 1 UNLESS FAILURE HAS OCCURED.
C
      DO 320 J=K,N
         IF (SV(J).GE.0.0D0) GO TO 320
C
         SV(J) = -SV(J)
C
         IF (WANTB) B(J) = -B(J)
         IF (.NOT.WANTY) GO TO 320
         DO 300 I=1,LY
            Y(I,J) = -Y(I,J)
  300    CONTINUE
C
  320 CONTINUE
C
C     NOW SORT THE SINGULAR VALUES INTO DESCENDING ORDER.
C
ct----------------------------------------------------------------------
ct   03.05.00: syl/eh
ct   - Add initialisation of jj to avoid warnings.  No effect on results
ct     since jj is defined each time wantz is true and used only when
ct     wantz is true.
ct----------------------------------------------------------------------
      JJ=0
      IF (WANTZ) JJ = 0
      DO 400 J=K,N
         S = 0.0D0
         L = J
C
         DO 340 I=J,N
            IF (SV(I).LE.S) GO TO 340
            S = SV(I)
            L = I
  340    CONTINUE
C
         IF (S.EQ.0.0D0) GO TO 420
         IF (WANTZ) WORK2(J) = L
         IF (L.EQ.J) GO TO 400
         IF (WANTZ) JJ = J
C
         SV(L) = SV(J)
         SV(J) = S
         IF (.NOT.WANTY) GO TO 380
C
         DO 360 I=1,LY
            T = Y(I,J)
            Y(I,J) = Y(I,L)
            Y(I,L) = T
  360    CONTINUE
C
  380    IF (.NOT.WANTB) GO TO 400
         T = B(J)
         B(J) = B(L)
         B(L) = T
C
  400 CONTINUE
C
  420 IF (.NOT.WANTZ) GO TO 480
      IF (JJ.EQ.0) GO TO 480
      DO 460 I=1,NCZ
         DO 440 J=K,JJ
            L = WORK2(J)
            IF (J.EQ.L) GO TO 440
            T = Z(J,I)
            Z(J,I) = Z(L,I)
            Z(L,I) = T
  440    CONTINUE
  460 CONTINUE
C
  480 IF (IFAIL.EQ.0) RETURN
C
      IFAIL = IFAIL + 1
  500 IFAIL = KP01AAF(IERR,IFAIL,SRNAME)
      RETURN
      END
** END OF F02SZFTEXT
*UPTODATE F02SZZTEXT
      SUBROUTINE   KF02SZZ(N, C, S, X)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (PLRT10)
C
C     F02SZZ RETURNS THE N ELEMENT VECTOR
C
C     Y = R(N-1,N)*R(N-2,N-1)*...*R(1,2)*X ,
C
C     WHERE X IS AN N ELEMENT VECTOR AND R(J-1,J) IS A PLANE
C     ROTATION FOR THE (J-1,J)-PLANE.
C
C     Y IS OVERWRITTEN ON X.
C
C     THE N ELEMENT VECTORS C AND S MUST BE SUCH THAT THE
C     NON-IDENTITY PART OF R(J-1,J) IS GIVEN BY
C
C     R(J-1,J) = (  C(J)  S(J) ) .
C                ( -S(J)  C(J) )
C
C     C(1) AND S(1) ARE NOT REFERENCED.
C
C     .. SCALAR ARGUMENTS ..
      INTEGER N
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION C(N), S(N), X(N)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION W
      INTEGER I
C     ..
C
C     N MUST BE AT LEAST 1. IF N=1 THEN AN IMMEDIATE RETURN TO
C     THE CALLING PROGRAM IS MADE.
C
      IF (N.EQ.1) RETURN
C
      DO 20 I=2,N
         W = X(I-1)
         X(I-1) = C(I)*W + S(I)*X(I)
         X(I) = C(I)*X(I) - S(I)*W
   20 CONTINUE
C
      RETURN
      END
** END OF F02SZZTEXT
*UPTODATE F02WAYTEXT
      SUBROUTINE   KF02WAY(N, C, NRC, PT, NRPT)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (BIGVPT)
C
C     F02WAY RETURNS THE N*N ORTHOGONAL MATRIX P**T FOR THE
C     FACTORIZATION OF ROUTINE KF01LZF.
C
C     DETAILS OF P MUST BE SUPPLIED IN THE N*N MATRIX C AS
C     RETURNED FROM ROUTINE KF01LZF.
C
C     NRC AND NRPT MUST BE THE ROW DIMENSIONS OF C AND PT
C     RESPECTIVELY AS DECLARED IN THE CALLING PROGRAM AND MUST
C     EACH BE AT LEAST N.
C
C     THE ROUTINE MAY BE CALLED WITH PT=C.
C
C     .. SCALAR ARGUMENTS ..
      INTEGER N, NRC, NRPT
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION C(NRC,N), PT(NRPT,N)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION BIG, CS, RSQTPS, SN, SQTEPS, T
      INTEGER I, J, K, KK, KM1, KP1
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION DSQRT, KX02AAF, KX02AGF
C     .. SUBROUTINE REFERENCES ..
C     F01LZW, F01LZY
C     ..
      BIG = 1.0D0/KX02AGF(0.0D0)
      SQTEPS = DSQRT(KX02AAF(0.0D0))
      RSQTPS = 1.0D0/SQTEPS
C
      PT(N,N) = 1.0D0
      IF (N.EQ.1) RETURN
C
      PT(N-1,N) = 0.0D0
      PT(N,N-1) = 0.0D0
      PT(N-1,N-1) = 1.0D0
      IF (N.EQ.2) RETURN
C
      K = N
      DO 60 KK=3,N
         KP1 = K
         K = K - 1
         KM1 = K - 1
         PT(KM1,K) = 0.0D0
C
         DO 20 J=KP1,N
            T = C(KM1,J)
            PT(KM1,J) = 0.0D0
            IF (T.EQ.0.0D0) GO TO 20
C
            CALL KF01LZW(-T, CS, SN, SQTEPS, RSQTPS, BIG)
C
            CALL KF01LZY(N-KM1, CS, SN, PT(K,J-1), PT(K,J))
C
   20    CONTINUE
C
         PT(KM1,KM1) = 1.0D0
         DO 40 I=K,N
            PT(I,KM1) = 0.0D0
   40    CONTINUE
C
   60 CONTINUE
C
      RETURN
      END
** END OF F02WAYTEXT
*UPTODATE F02WBFTEXT
      SUBROUTINE   KF02WBF(M,N,A, NRA, WANTB, B, SV, WORK, LWORK,IFAIL)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (SVDGN2)
C
C     F02WBF RETURNS PART OF THE SINGULAR VALUE DECOMPOSITION
C     OF THE M*N (M.LE.N) MATRIX A GIVEN BY
C
C     A = Q*(D 0)*(P**T) ,
C
C     WHERE Q AND P ARE ORTHOGONAL MATRICES AND D IS AN M*M
C     DIAGONAL MATRIX WITH NON-NEGATIVE DIAGONAL ELEMENTS,
C     THESE BEING THE SINGULAR VALUES OF A.
C
C     THE DIAGONAL ELEMENTS OF D AND THE FIRST M ROWS OF P**T
C     ARE RETURNED. IF WANTB IS .TRUE. THEN (Q**T)*B IS ALSO
C     RETURNED.
C
C     INPUT PARAMETERS.
C
C     M     - NUMBER OF ROWS OF A. M MUST BE AT LEAST UNITY.
C
C     N     - NUMBER OF COLUMNS OF A. N MUST BE AT LEAST M.
C
C     A     - THE M*N MATRIX TO BE FACTORIZED.
C
C     NRA   - ROW DIMENSION OF A AS DECLARED IN THE CALLING PROGRAM
C             NRA MUST BE AT LEAST M.
C
C     WANTB - MUST BE .TRUE. IF (Q**T)*B IS REQUIRED.
C             IF WANTB IS .FALSE. THEN B IS NOT REFERENCED.
C
C     B     - AN M ELEMENT VECTOR.
C
C     IFAIL - THE USUAL FAILURE PARAMETER. IF IN DOUBT SET
C             IFAIL TO ZERO BEFORE CALLING F02WBF.
C
C     OUTPUT PARAMETERS.
C
C     A     - A WILL CONTAIN THE FIRST M ROWS OF P**T.
C
C     B     - IF WANTB IS .TRUE. THEN B IS OVERWRITTEN BY
C             THE M ELEMENT VECTOR (Q**T)*B.
C
C     SV    - M ELEMENT VECTOR CONTAINING THE SINGULAR
C             VALUES OF A. THEY ARE ORDERED SO THAT
C             SV(1).GE.SV(2).GE. ... .GE.SV(M).GE.0.
C
C     IFAIL - ON NORMAL RETURN IFAIL WILL BE ZERO.
C             IN THE UNLIKELY EVENT THAT THE QR-ALGORITHM
C             FAILS TO FIND THE SINGULAR VALUES IN 50*M
C             ITERATIONS THEN IFAIL IS SET TO 2.
C             IF AN INPUT PARAMETER IS INCORRECTLY SUPPLIED
C             THEN IFAIL IS SET TO UNITY.
C
C     WORKSPACE PARAMETERS.
C
C     WORK  - AN (M*M+3*M) ELEMENT VECTOR.
C             WORK(1) RETURNS THE TOTAL NUMBER OF ITERATIONS TAKEN
C             BY THE QR-ALGORITHM.
C
C     LWORK - THE LENGTH OF THE VECTOR WORK.
C             LWORK MUST BE AT LEAST M*M+3*M.
C
C     .. SCALAR ARGUMENTS ..
      INTEGER IFAIL, LWORK, M, N, NRA
      LOGICAL WANTB
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION A(NRA,N), B(M), SV(M), WORK(LWORK)
C     ..
C     .. LOCAL SCALARS ..
C$P 1
      DOUBLE PRECISION SRNAME
      INTEGER IERR, J, K1, K2, K3
C     .. FUNCTION REFERENCES ..
      INTEGER KP01AAF
C     .. SUBROUTINE REFERENCES ..
C     F01LZF, F01QBF, F02SZF, F02WAY, F02WBY, F02WBZ
C     ..
      DATA SRNAME /8H F02WBF /
      IERR = IFAIL
      IF (IERR.EQ.0) IFAIL = 1
C
      IF (NRA.LT.M .OR. N.LT.M .OR. LWORK.LT.M*(M+3) .OR. M.LT.1)
     *GO TO 40
C
      K1 = M + 1
      K2 = K1 + M
      K3 = K2 + M
C
      CALL KF01QBF(M, N, A, NRA, A, NRA, WORK, IFAIL)
C
      CALL KF01LZF(M, A, NRA, WORK(K3), M, WANTB, B, .FALSE.,.FALSE.,
     * WORK, 1, 1, .FALSE., WORK, 1, 1, SV, WORK, WORK,WORK, IFAIL)
C
      CALL KF02WAY(M, WORK(K3), M, WORK(K3), M)
C
      IFAIL = 1
      CALL KF02SZF(M,SV,WORK, SV, WANTB, B, .FALSE., WORK, 1, 1,.TRUE.,
     * WORK(K3), M, M, WORK, WORK(K1), WORK(K2), IFAIL)
C
      CALL KF02WBZ(M, N, A, NRA, A, NRA, WORK(K1))
C
      DO 20 J=1,N
C
         CALL KF02WBY(M, M, WORK(K3), M, A(1,J), A(1,J), WORK(K1))
C
   20 CONTINUE
C
      IF (IFAIL.EQ.0) RETURN
C
      IFAIL = 2
   40 IFAIL = KP01AAF(IERR,IFAIL,SRNAME)
      RETURN
      END
** END OF F02WBFTEXT
*UPTODATE F02WBYTEXT
      SUBROUTINE   KF02WBY(M, N, A, NRA, X, Y, WORK)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (AXMULT)
C
C     F02WBY RETURNS THE M ELEMENT VECTOR Y GIVEN BY
C
C     Y = A*X ,
C
C     WHERE A IS AN M*N MATRIX AND X IS AN N ELEMENT VECTOR.
C
C     NRA MUST BE THE ROW DIMENSION OF A AS DECLARED IN THE
C     CALLING PROGRAM AND MUST BE AT LEAST M.
C
C     THE M ELEMENT VECTOR WORK IS REQUIRED FOR INTERNAL WORKSPACE.
C
C     THE ROUTINE MAY BE CALLED EITHER WITH Y=X OR WITH
C     WORK=Y, BUT NOT BOTH.
C
C     .. SCALAR ARGUMENTS ..
      INTEGER M, N, NRA
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION A(NRA,N), WORK(M), X(N), Y(M)
C     ..
C     .. LOCAL SCALARS ..
      INTEGER I, J
      LOGICAL UNDFLW
C     .. FUNCTION REFERENCES ..
      LOGICAL KX02DAF
C     .. SUBROUTINE REFERENCES ..
C     F01QAW
C     ..
      UNDFLW = KX02DAF(0.0D0)
C
      DO 20 I=1,M
         WORK(I) = 0.0D0
   20 CONTINUE
C
      DO 40 J=1,N
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     THE CALL TO F01QAW CAN BE REPLACED BY THE FOLLOWING IN-LINE
C     CODE, PROVIDED THAT NO PRECAUTIONS AGAINST UNDERFLOW
C     ARE REQUIRED
C
C        XJ = X(J)
C        DO 30 I=1,M
C           WORK(I) = WORK(I) + XJ*A(I,J)
C  30    CONTINUE
C
         CALL KF01QAW(M, WORK, -X(J), A(1,J), UNDFLW)
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
   40 CONTINUE
C
      DO 60 I=1,M
         Y(I) = WORK(I)
   60 CONTINUE
C
      RETURN
      END
** END OF F02WBYTEXT
*UPTODATE F02WBZTEXT
      SUBROUTINE   KF02WBZ(M, N, C, NRC, PT, NRPT, WORK)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (GIVNPT)
C
C     F02WBZ RETURNS THE FIRST M ROWS OF THE N*N ORTHOGONAL
C     MATRIX Q FOR THE FACTORIZATION OF ROUTINE KF01QBF
C
C     M MUST NOT BE LARGER THAN N
C
C     DETAILS OF Q MUST BE SUPPLIED IN THE M*N MATRIX C AS
C     RETURNED FROM ROUTINE KF01QBF.
C
C     Q IS RETURNED IN THE M*N MATRIX PT.
C
C     THE ROUTINE MAY BE CALLED WITH PT=C.
C
C     NRC AND NRPT MUST BE THE ROW DIMENSIONS OF C AND PT
C     RESPECTIVELY AS DECLARED IN THE CALLING PROGRAM AND MUST
C     EACH BE AT LEAST M.
C
C     THE M ELEMENT VECTOR WORK IS REQUIRED FOR INTERNAL WORKSPACE.
C
C     .. SCALAR ARGUMENTS ..
      INTEGER M, N, NRC, NRPT
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION C(NRC,N), PT(NRPT,N), WORK(M)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION BIG, CS, RSQTPS, SN, SQTEPS
      INTEGER I, J, JF, JL, K, KM1, MP1
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION DSQRT, KX02AAF, KX02AGF
C     .. SUBROUTINE REFERENCES ..
C     F01LZW, F01LZY
C     ..
      BIG = 1.0D0/KX02AGF(0.0D0)
      SQTEPS = DSQRT(KX02AAF(0.0D0))
      RSQTPS = 1.0D0/SQTEPS
C
ct----------------------------------------------------------------------
ct   03.05.00: syl/eh
ct   - Add initialisation of km1 to avoid warnings.  No effect on result
ct     since km1 is defined each time k.ne.1 and used only when
ct     k.ne.1.
ct----------------------------------------------------------------------
      KM1=0
C
      MP1 = M + 1
      DO 160 K=1,M
         IF (K.EQ.1) GO TO 40
         KM1 = K - 1
C
         DO 20 I=1,KM1
            WORK(I) = 0.0D0
   20    CONTINUE
C
   40    WORK(K) = 1.0D0
         JF = MP1
         IF (M.EQ.N) GO TO 100
         JL = N
C
   60    DO 80 J=JF,JL
C
            CALL KF01LZW(-C(K,J), CS, SN, SQTEPS, RSQTPS, BIG)
C
            PT(K,J) = 0.0D0
C
            CALL KF01LZY(K, CS, SN, WORK, PT(1,J))
C
   80    CONTINUE
C
  100    IF (JF.EQ.1 .OR. K.EQ.1) GO TO 120
         JF = 1
         JL = KM1
         GO TO 60
C
  120    DO 140 I=1,K
            PT(I,K) = WORK(I)
  140    CONTINUE
C
  160 CONTINUE
C
      RETURN
      END
** END OF F02WBZTEXT
*UPTODATE F02WCWTEXT
      SUBROUTINE   KF02WCW(M, N, A, NRA, X, Y, WORK)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (ATXMUL)
C
C     F02WCW RETURNS THE N ELEMENT VECTOR Y GIVEN BY
C
C     Y = (A**T)*X ,
C
C     WHERE A IS AN M*N MATRIX AND X IS AN M ELEMENT VECTOR.
C
C     NRA MUST BE THE ROW DIMENSION OF A AS DECLARED IN THE
C     CALLING PROGRAM AND MUST BE AT LEAST M.
C
C     THE N ELEMENT VECTOR WORK IS REQUIRED FOR INTERNAL WORKSPACE.
C
C     THE ROUTINE MAY BE CALLED WITH Y=X OR WITH WORK=Y BUT
C     NOT BOTH
C
C     .. SCALAR ARGUMENTS ..
      INTEGER M, N, NRA
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION A(NRA,N), WORK(N), X(M), Y(N)
C     ..
C     .. LOCAL SCALARS ..
      INTEGER J
      LOGICAL UNDFLW
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION KF01QAX
      LOGICAL KX02DAF
C     ..
      UNDFLW = KX02DAF(0.0D0)
C
      DO 20 J=1,N
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     THE CALL TO KF01QAX CAN BE REPLACED BY THE FOLLOWING IN-LINE
C     CODE, PROVIDED THAT NO PRECAUTIONS AGAINST UNDERFLOW
C     ARE REQUIRED
C
C        D = 0.0E0
C        DO 10 I=1,M
C           D = D + A(I,J)*X(I)
C  10    CONTINUE
C        WORK(J) = D
C
C     IN THIS CASE THE DECLARATION
C
C     REAL KF01QAX
C
C     MUST ALSO BE REMOVED.
C
         WORK(J) = KF01QAX(M,M,0.0D0,.TRUE.,A(1,J),X,UNDFLW)
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
   20 CONTINUE
C
      DO 40 J=1,N
         Y(J) = WORK(J)
   40 CONTINUE
C
      RETURN
      END
** END OF F02WCWTEXT
*UPTODATE F02WDYTEXT
      INTEGER FUNCTION   KF02WDY(N, SV, TOL)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (IRKSVD)
C
C     KF02WDY RETURNS THE RANK OF AN M*K MATRIX A FOLLOWING A
C     SINGULAR VALUE DECOMPOSITION OF A.
C
C     THE N=MIN(M,K) SINGULAR VALUES OF A MUST BE IN
C     DESCENDING ORDER IN THE N ELEMENT VECTOR SV. THEN KF02WDY
C     RETURNS THE LARGEST INTEGER SUCH THAT
C
C     SV(KF02WDY) .GT. TOL*SV(1) .
C
C     IF SV(1)=0 THEN KF02WDY IS RETURNED AS ZERO.
C
C     IF TOL.LT.EPS OR TOL.GE.1 THEN THE VALUE EPS IS USED IN
C     PLACE OF TOL, WHERE EPS IS THE SMALLEST REAL FOR WHICH
C     1.0+EPS.GT.1.0 ON THE MACHINE. FOR MOST PROBLEMS THIS IS
C     UNREASONABLY SMALL AND TOL SHOULD BE CHOSEN TO
C     APPROXIMATE THE RELATIVE ERRORS IN THE ELEMENTS OF A.
C
C     IF INSTEAD SINGULAR VALUES BELOW SOME VALUE DELTA ARE TO
C     BE REGARDED AS ZERO THEN SUPPLY TOL AS DELTA/SV(1).
C
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION TOL
      INTEGER N
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION SV(N)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION DELTA, TL
      INTEGER I, IR
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION KX02AAF
C     ..
      TL = TOL
      DELTA = KX02AAF(0.0D0)
      IF (TL.LT.DELTA .OR. TL.GE.1.0D0) TL = DELTA
C
      IR = 0
      DELTA = TL*SV(1)
C
      DO 20 I=1,N
         IF (SV(I).LE.DELTA) GO TO 40
         IR = I
   20 CONTINUE
C
   40 KF02WDY = IR
      RETURN
      END
** END OF F02WDYTEXT

*UPTODATE X02AGFTEXT
      DOUBLE PRECISION FUNCTION   KX02AGF(X)
C     MARK 8 RELEASE. NAG COPYRIGHT 1980.
C
C     RETURNS THE SMALLEST POSITIVE FLOATING-POINT NUMBER  R
C     EXACTLY REPRESENTABLE ON THE COMPUTER SUCH THAT  -R, 1.0/R,
C     AND -1.0/R CAN ALL BE COMPUTED WITHOUT OVERFLOW OR UNDERFLOW.
C     ON MANY MACHINES THE CORRECT VALUE CAN BE DERIVED FROM THOSE
C     OF KX02AAF, X02ABF AND X02ACF AS FOLLOWS
C
C     IF (X02ABF(X)*X02ACF(X).GE.1.0) KX02AGF = X02ABF(X)
C     IF (X02ABF(X)*X02ACF(X).LT.1.0)
C    *                            KX02AGF = (1.0+KX02AAF(X))/X02ACF(X)
C
C     THE CORRECT VALUE SHOULD BE DEFINED AS A CONSTANT,
C     POSSIBLY IN SOME BINARY, OCTAL OR HEXADECIMAL REPRESENTATION,
C     AND INSERTED INTO THE ASSIGNMENT STATEMENT BELOW.
C
C     X IS A DUMMY ARGUMENT
C
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION X
C     ..
      KX02AGF =(1.0D0 + 2.0D0**(-55))/((2.0D0**126 - 2.0D0**70) * 2.0D0)
      RETURN
      END
** END OF X02AGFTEXT
*UPTODATE X02DAFTEXT
      LOGICAL FUNCTION   KX02DAF(X)
C     MARK 8 RELEASE. NAG COPYRIGHT 1980.
C
C     RETURNS .FALSE. IF THE SYSTEM SETS UNDERFLOWING QUANTITIES
C     TO ZERO, WITHOUT ANY ERROR INDICATION OR UNDESIRABLE WARNING
C     OR SYSTEM OVERHEAD.
C     RETURNS .TRUE. OTHERWISE, IN WHICH CASE CERTAIN LIBRARY
C     ROUTINES WILL TAKE SPECIAL PRECAUTIONS TO AVOID UNDERFLOW
C     (USUALLY AT SOME COST IN EFFICIENCY).
C
C     X IS A DUMMY ARGUMENT
C
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION X
C     ..
      KX02DAF = .FALSE.
      RETURN
      END
** END OF X02DAFTEXT

************************************************************************
****
***
**
*    CONCATENATION OF TEXTURE SUBROUTINES FOR CASHFORM MODELS
**
***
****
************************************************************************
      SUBROUTINE   KX04AAF(I,NERR)
      implicit none
C     MARK 7 RELEASE. NAG COPYRIGHT 1978
C     MARK 7C REVISED IER-190 (MAY 1979)
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     IF I = 0, SETS NERR TO CURRENT ERROR MESSAGE UNIT NUMBER
C     (STORED IN NERR1).
C     IF I = 1, CHANGES CURRENT ERROR MESSAGE UNIT NUMBER TO
C     VALUE SPECIFIED BY NERR.
C
C     .. Scalar Arguments ..
      INTEGER           I, NERR
C     .. Local Scalars ..
      INTEGER           NERR1
C     .. Save statement ..
      SAVE              NERR1
C     .. Data statements ..
c     DATA              NERR1/6/
      integer          out6,out9,outopt
      common /knchout/ out6,out9,outopt

C     .. Executable Statements ..
      NERR1=OUT6
      IF (I.EQ.0) NERR = NERR1
      IF (I.EQ.1) NERR1 = NERR
      RETURN
      END
************************************************************************
****
***
**
*    CONCATENATION OF TEXTURE SUBROUTINES FOR CASHFORM MODELS
**
***
****
************************************************************************
************************************************************************
****
***
**
*    CONCATENATION OF TEXTURE SUBROUTINES FOR CASHFORM MODELS
**
***
****
************************************************************************
* P01FTEXT
*UPTODATE P01AAFTEXT
      INTEGER FUNCTION   KP01AAF(IFAIL, ERROR, SRNAME)
      implicit none
C     MARK 1 RELEASE.  NAG COPYRIGHT 1971
C     MARK 3 REVISED
C     MARK 4A REVISED, IER-45
C     MARK 4.5 REVISED
C     MARK 7 REVISED (DEC 1978)
C     MARK 11 REVISED (FEB 1984)
C     RETURNS THE VALUE OF ERROR OR TERMINATES THE PROGRAM.
      INTEGER ERROR, IFAIL, NOUT
C$P 1
      DOUBLE PRECISION SRNAME
C     TEST IF NO ERROR DETECTED
      IF (ERROR.EQ.0) GO TO 20
C     DETERMINE OUTPUT UNIT FOR MESSAGE
      CALL KX04AAF (0,NOUT)
C     TEST FOR SOFT FAILURE
      IF (MOD(IFAIL,10).EQ.1) GO TO 10
C     HARD FAILURE
      WRITE (NOUT,99999) SRNAME, ERROR
C     ******************** IMPLEMENTATION NOTE ********************
C     THE FOLLOWING STOP STATEMENT MAY BE REPLACED BY A CALL TO AN
C     IMPLEMENTATION-DEPENDENT ROUTINE TO DISPLAY A MESSAGE AND/OR
C     ABORT THE PROGRAM.
C     *************************************************************
c
c      STOP
c
c  also considered to be soft failure ( 19/4/89 );
c  STOP skipped and  goto 20 added
c   
      go to 20
C     SOFT FAIL
C     TEST IF ERROR MESSAGES SUPPRESSED
   10 IF (MOD(IFAIL/10,10).EQ.0) GO TO 20
      WRITE (NOUT,99999) SRNAME, ERROR
   20 KP01AAF = ERROR
      RETURN
99999 FORMAT (1H0, 38HERROR DETECTED BY NAG LIBRARY ROUTINE , A8,
     * 11H - IFAIL = , I5//)
      END
** END OF P01AAFTEXT
************************************************************************
****
***
**
*    CONCATENATION OF TEXTURE SUBROUTINES FOR CASHFORM MODELS
**
***
****
************************************************************************

* X02FTEXT
*UPTODATE X02AAFTEXT
      DOUBLE PRECISION FUNCTION   KX02AAF(X)
      implicit none
C     NAG COPYRIGHT 1975
C     MARK 4.5 RELEASE
      DOUBLE PRECISION X
C     * EPS *
C     RETURNS THE VALUE EPS WHERE EPS IS THE SMALLEST
C     POSITIVE
C     NUMBER SUCH THAT 1.0 + EPS > 1.0
C     THE X PARAMETER IS NOT USED
C     FOR DEC VAX 11/780
C     KX02AAF = 2.0**(-56)  -  (IN THEORY)
C     KX02AAF = 2.0**(-55)  -  (MORE PRACTICAL VALUE)
      KX02AAF = 2.0D0**(-55)
      RETURN
      END
** END OF X02AAFTEXT
ct    Modifications eh/syl for cashform:
ct       10.04.00
ct       - Adding .0d0 for all double precisions
c ----------------------------------------------------------------------
C
c     set of subroutines for implementation into FE routines of JAW;
c     yield locus described by means of series expansion fitted to 1/s;
c     routines in this file should be linked to lagapre
c             
c     BVB & EH 20/6/97
c
************************************************************************
************************************************************************
      subroutine   KREAD2(PAMET,MPAME,NTINP,NT6,NTPRI)
c ----------------------------------------------------------------------
c     reads input file with series expansion fitted to 1/s;
c ----------------------------------------------------------------------
ct----------------------------------------------------------------------
ct    03.05.00: syl/eh
ct    - permute the declaration of mpame and pamet(mpame)
ct    - add the declaration of maxf, nchlst, ired, mpamec
ct    - initialise rclass to -1.  No effect since redefined after.
ct    - adding implicit none
ct    - add definition 'nodd=ncfodd' similar to 'nevn=ncfevn'.  This
ct      is an important correction.
ct    13.03.02: bvb/sh : modifications for ifun=-3, i.e. M^n and npoevn
ct    - modifications from rfieao bvb 07/08/01
ct    - npoevn added to parameters + declaration as integer
ct    - pamet(8)=npoevn
ct    - MPAMEC = 7+ncfe+ncfo   ->  MPAMEC = 8+ncfe+ncfo  
ct    - pamet(7+...) -> pamet(8+...)
ct    - all calls to KF3DER and KYLP: s(8),s(8+...)  ->  s(9),s(9...)
ct  
ct----------------------------------------------------------------------
      implicit none
      integer maxf,nchlst,ired,mpamec
      integer mxorde,mxfe,mxdfe
      parameter (mxorde=6,mxfe=210,mxdfe=126)
      integer mxordo,mxfo,mxdfo
      parameter (mxordo=6,mxfo=210,mxdfo=126)
      integer mpame,ntinp,nt6,ntpri
      integer i,ibausch,impred,iw,iptial,
     x     nevn,ncfe,ncfevn,ncfo,ncfodd,nodd,npoevn
      real*8 cfevn(mxfe),cfodd(mxfo),pamet(mpame),rclass
      character class,tt1*40,tt2*40
c
      do i=1,mpame
         pamet(i)=0.d0
      enddo
c ----------------------------------------------------------------------
c     at present impred=1;
c ----------------------------------------------------------------------
      impred=1
      iw=1
c ----------------------------------------------------------------------
c     read eao-file;
c ----------------------------------------------------------------------
      if (iw.ge.1) write(NTPRI,6998) 
      call krfieao(cfevn,maxf,cfodd,maxf,ibausch,iptial,
     x   nevn,nodd,ncfevn,ncfodd,npoevn,class,tt1,tt2,NTINP,NT6,iw)
      if (iw.ge.1) write(NTPRI,6997)
c ----------------------------------------------------------------------
c     determine symmetry class;
c ----------------------------------------------------------------------
      if (iw.ge.1) write(NTPRI,6996)
      if (class.eq.'x') call ksymcla(class,nevn,cfevn,maxf,nchlst,1)
cc      if (class.eq.'x') call ksymcla(class,nevn,cfevn,maxf,nchlst,iw)
      if (iw.ge.1) write(NTPRI,6995) class
      rclass=-1.0d0
      if (class.eq.'b') rclass=1.0D0
      if (class.eq.'c') rclass=2.0D0
      if (class.eq.'d') rclass=3.0D0
      if (class.eq.'e') rclass=4.0D0
      if (class.eq.'f') rclass=5.0D0
c ----------------------------------------------------------------------
c     impose reduction of coeff. array if useful + asked for;
c ----------------------------------------------------------------------
      ired=0
      ncfe=ncfevn
      nodd=ncfodd
      if (impred.eq.1) then
         if (iw.ge.1) write(NTPRI,6994)
         call kreduce(cfevn,mxfe,ncfe,class,nevn,cfevn,mxfe,NT6,iw)
         if (iw.ge.1) write(NTPRI,6993) ncfevn,ncfe
         if (ibausch.eq.1) then
            call kreduce(cfodd,mxfo,ncfo,class,nodd,cfodd,mxfo,NT6,iw)
            if (iw.ge.1) write(NTPRI,6993) ncfodd,ncfo
         endif
         ired=1
      endif
c ----------------------------------------------------------------------
c     print reduced coeff.;
c ----------------------------------------------------------------------
      write(NTPRI,*) tt1
      write(NTPRI,*) tt2
      write(NTPRI,*) ncfe,' reduced even order coefficients'
      do 100 i=1,ncfe
         write(NTPRI,7999) cfevn(i)
 100  continue
 7999 format(d15.8)
      write(NTPRI,*) ncfo,' reduced odd order coefficients'
      if (ibausch.eq.1) then
         do 200 i=1,ncfo
            write(NTPRI,7999) cfodd(i)
 200     continue
      endif
c ----------------------------------------------------------------------
c     store info of MET-file in pamet;
c ----------------------------------------------------------------------
      pamet(1)=nevn
      pamet(2)=ncfe
      pamet(3)=nodd
      pamet(4)=ncfo
      pamet(5)=rclass
      pamet(6)=iptial
      pamet(7)=ired
      pamet(8)=npoevn
C
C     CHECK SPACE RESERVED FOR PAMET
C
      MPAMEC = 8+ncfe+ncfo
      IF (MPAMEC.gt.MPAME)THEN
         WRITE(NT6,*)' IN READ2 FOR BERT VAN BAEL DATA'
         WRITE(NT6,*)MPAME,' IS THE SPACE BOOKED WHEN ',MPAMEC,
     .               ' IS THE SPACE USED'
         stop
      END IF
      do 10 i=1,ncfe
         pamet(8+i)=cfevn(i)
 10   continue
      do 20 i=1,ncfo
         pamet(8+ncfe+i)=cfodd(i)
 20   continue
c ----------------------------------------------------------------------
c     formats;
c ----------------------------------------------------------------------
 6998 format(1x,'*** start reading epo-file')
 6997 format(1x,'*** reading epo-file finished',
     x   /1x,'******************************')
 6996 format(1x,'*** determine symm. class')
 6995 format(1x,'*** symmetry class is "',a1,'"',
     x   /1x,'**************************')
 6994 format(1x,'*** reduce array to possibly non zero coeff.')
 6993 format(1x,'*** reduction from ',i3,' to ',i3,' coeff.',
     x   /1x,'*************************************')
c ----------------------------------------------------------------------
c     end of subroutine kread2;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   krfieao(cfevn,maxfe,cfodd,maxfo,ibausch,iptial,
     x   nevn,nodd,ncfevn,ncfodd,npoevn,class,tt1,tt2,nch,nchlst,iw)
c ----------------------------------------------------------------------
c     reads epo-file with Even And Odd coefficients;
c     (bvb,eh 10/1/97)
c ----------------------------------------------------------------------
ct    27.04.00: syl/eh
ct    - correct the loop do 30: previously it was do 30 i=ncfodd, now
ct      it is do 30 i=1,ncfodd
ct    - add the declaration of identifers maxfe and maxfo
ct    13.03.02: bvb/sh : modifications for ifun=-3, i.e. M^n and npoevn
ct    - modifications from rfieao bvb 07/08/01
c ----------------------------------------------------------------------

      implicit none
      integer i,ibausch,ifun,iptial,iw,
     x   nch,nchlst,nevn,ncfevn,ncfodd,nodd,npoevn
      integer maxfe,maxfo
      DOUBLE PRECISION cfevn(maxfe),cfodd(maxfo)
      character class,tt1*40,tt2*40
c ----------------------------------------------------------------------
c     read epo-file;
c ----------------------------------------------------------------------
      class='x'
      read(nch,5999) tt1
      read(nch,5999) tt2
      read(nch,*) ifun
      if (abs(ifun).ne.3) then
         write(nchlst,998) ifun
 998     format('ifun should be 3/-3 for EAO-files but it is',i5)
         stop
      endif
      read(nch,*) iptial
      read(nch,*) ibausch
      read(nch,*) nevn,ncfevn
      npoevn=1
      if (ifun.eq.-3) npoevn=nevn
      if (ibausch.eq.1) then
         read(nch,*) nodd,ncfodd
      else
         nodd=0
         ncfodd=0
      endif
      do 20 i=1,ncfevn
         read(nch,*) cfevn(i)
 20   continue
      if (ibausch.eq.1) then
         do 30 i=1,ncfodd
            read(nch,*) cfodd(i)
 30      continue
      endif
 5999 format(a40)
c ----------------------------------------------------------------------
c     print info if asked for;
c ----------------------------------------------------------------------
      if (iw.ge.1) then
         write(nchlst,6999) tt1,tt2,iptial,ibausch,nevn,ncfevn,npoevn,
     x      nodd,ncfodd,class
 6999 format(2(/1x,a40),
     x   /1x,'iptial  = ',i3,/1x,'ibausch = ',i3,
     x   /1x,'nevn    = ',i3,';   ncfevn  = ',i3,';   npoevn  = ',i3,
     x   /1x,'nodd    = ',i3,';   ncfodd   = ',i3,
     x   /1x,'class   = ',a3)
      endif
c ----------------------------------------------------------------------
c     end of subroutine krfieao;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   ksymcla(class,nord,fcf,maxf,nchlst,iw)
c ----------------------------------------------------------------------
c     determines symmetry class of series expansion;
c     (bvb 12/12/96)
c ----------------------------------------------------------------------
ct    12.01.09: bvb/skye
ct      data symmin/0.1d-02/ in place of symmin/1.d-10/
ct    syl/eh: 27.04.00
ct    - add the declaration of identifiers iodd45,iodd34,iodd53,i,k
ct    syl/eh: 03.05.00
ct    add integer/external declaration for kisodd and knjklst
ct    13.03.02: bvb/sh 
ct    - iw + NTPRI
c ----------------------------------------------------------------------
      implicit none
      integer kisodd,NTPRI
      external kisodd
      integer knjklst
      external knjklst
      integer maxord,nvar
      integer iodd45,iodd34,iodd53,i,k
      parameter (maxord=6,nvar=5)
      integer ic,id,ie,ifcf,ilst(maxord),iw,maxf,nchlst,nord
      DOUBLE PRECISION cmax,dmax,emax,fcf(maxf),symmin
      character class
c     data symmin/0.1d-02/   ! Imposes Symmetry Class 'b' (orthorhombic)
      data symmin/1d-10/     ! Imposes Symmetry Class 'f' (triclinic)
      NTPRI=123
      cmax=0.d0
      dmax=0.d0
      emax=0.d0
      ic=1
      id=1
      ie=1
c ----------------------------------------------------------------------
c     loop over all terms;
c ----------------------------------------------------------------------
      ifcf=0
      do 10 i=1,nord
         ilst(i)=1
 10   continue
 1    ifcf=ifcf+1
      iodd45=kisodd(knjklst(4,5,ilst,nord))
      iodd53=kisodd(knjklst(5,3,ilst,nord))
      iodd34=kisodd(knjklst(3,4,ilst,nord))
c     bug found 12.01.09 bvb/skye: cmax,dmax,emax must be absolute values  
      if (iodd45.eq.1.and.abs(fcf(ifcf)).gt.symmin) then
         if (abs(fcf(ifcf)).gt.cmax) cmax=abs(fcf(ifcf))
         ic=0
      endif
      if (iodd53.eq.1.and.abs(fcf(ifcf)).gt.symmin) then
         if (abs(fcf(ifcf)).gt.dmax) dmax=abs(fcf(ifcf))
         id=0
      endif
      if (iodd34.eq.1.and.abs(fcf(ifcf)).gt.symmin) then
         if (abs(fcf(ifcf)).gt.emax) emax=abs(fcf(ifcf))     
         ie=0                                                    
      endif
c ----------------------------------------------------------------------
c     determine indices for next coefficient;
c ----------------------------------------------------------------------
      if (ilst(nord).eq.nvar) then
         k=nord
 2       k=k-1
         if (k.eq.0) goto 3
         if (ilst(k).lt.nvar) then
            ilst(k)=ilst(k)+1
            do 30 i=k+1,nord
               ilst(i)=ilst(k)
 30         continue
            goto 1
         else
            goto 2
         endif
      else
         ilst(nord)=ilst(nord)+1
         goto 1
      endif
 3    class='f'
      if (ic.eq.1) class='c'
      if (id.eq.1) class='d'
      if (ie.eq.1) class='e'
      if (ic.eq.1.and.id.eq.1.and.ie.eq.1) class='b'
      if (iw.ge.1) write(NTPRI,6999) cmax,dmax,emax
      if (iw.ge.1) write(NTPRI,'(a,e15.6)') 'symmin= ',symmin
 6999 format('SUBROUTINE ksymcla',/,'cmax =',e15.6,';  dmax =',e15.6,
     x    '; emax =',e15.6)   
c ----------------------------------------------------------------------
c     end of subroutine ksymcla;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kreduce(fcf,maxf,nfcf,class,nord,
     x   fcfor,maxfor,nchlst,iw)
c ----------------------------------------------------------------------
c     reduction of array with all series expansion coefficients
c     to array with only "possibly non zero" coefficients;
c     (bvb 12/12/96)
c ----------------------------------------------------------------------
ct    27.04.00: syl/eh
ct    - add the declaration of identifier k
ct    03.05.00: syl/eh
ct    - add integer/external declaration for kipnz
c ----------------------------------------------------------------------
      implicit none
      integer kipnz
      external kipnz
      integer maxord,k
      parameter (maxord=6)
      integer i,ifcf,ifcfor,ilst(maxord),iw,
     x   maxf,maxfor,nchlst,nfcf,nfcfor,nord,nvar
      real*8 fcf(maxf),fcfor(maxfor)
      character class
      data nvar/5/
c ----------------------------------------------------------------------
c     loop over all terms;
c ----------------------------------------------------------------------
      ifcfor=0
      ifcf=0
      do 10 i=1,nord
         ilst(i)=1
 10   continue
 1    ifcfor=ifcfor+1
      if (kipnz(class,ilst,nord).eq.1) then
         ifcf=ifcf+1
         fcf(ifcf)=fcfor(ifcfor)
      endif
c ----------------------------------------------------------------------
c     determine indices for next coefficient;
c ----------------------------------------------------------------------
      if (ilst(nord).eq.nvar) then
         k=nord
 2       k=k-1
         if (k.eq.0) goto 3
         if (ilst(k).lt.nvar) then
            ilst(k)=ilst(k)+1
            do 30 i=k+1,nord
               ilst(i)=ilst(k)
 30         continue
            goto 1
         else
            goto 2
         endif
      else
         ilst(nord)=ilst(nord)+1
         goto 1
      endif
 3    nfcfor=ifcfor
      nfcf=ifcf
c ----------------------------------------------------------------------
c     end of subroutine kreduce;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      SUBROUTINE KRDLINES(funit,nlines)
C     
      INTEGER funit, i, nlines
      CHARACTER subline*120
C
      DO i = 1, nlines
        READ(funit,'(A)') subline
      END DO
C     
      RETURN  
      END 
************************************************************************
      SUBROUTINE KGETZFACET (zval)
C
      INTEGER NSTEPS
      DOUBLE PRECISION qut, zval, tmp
      DOUBLE PRECISION Uut(5), Fc, DFc(5), DDFc(5,5), aij(3,3)
      INTEGER nchlst, IERR, iptial, IDER, NVAR, IOFF, IFUN, ITRY3, IW
C
      DOUBLE PRECISION    AONSET(5),BEU(4),BEA0(4),BEA(4),PHIYLP(5)
      DOUBLE PRECISION    ELAFRA,SCALS,POT,COSANG,ANG,SOV5(5)
      DOUBLE PRECISION    TAU, UNILEN
C
      CHARACTER*100 KFACETID1, KFACETID2
      INTEGER KTYPEXPR, KTYPFACET, KSTGFACET, KCTRi, KCTRj
      INTEGER KNUMFACET, KORDFACET
      DOUBLE PRECISION KLAMFACET(2500), KSNSFACET(2500,5)
      COMMON /KFACETPAR1/ KFACETID1, KFACETID2
      COMMON /KFACETPAR2/ KTYPFACET, KSTGFACET, KNUMFACET,
     &                    KORDFACET, KTYPEXPR
      COMMON /KFACETPAR3/ KLAMFACET
      COMMON /KFACETPAR4/ KSNSFACET
C
C     Assuming axis-1 is tensile axis: deviatoric stress sigut:
C
      Uut(1) = sqrt(0.75D0)
      Uut(2) = 0.5D0
      Uut(3) = 0.0D0
      Uut(4) = 0.0D0
      Uut(5) = 0.0D0
C
      IF (KTYPFACET==1) THEN
        IDER=1
        CALL KFACETPOT(Fc,DFc,DDFc,Uut,IDER,nchlst,0)
        SCALS = 1.D0/Fc
C        CALL KNORML5D(DFc,AONSET,tmp)
C        CALL KDOTV(tmp,Uut,AONSET)
C        POT = SCALS*tmp
      END IF 
      IF (KTYPFACET==2) THEN
        CALL Kbetap(beu,tmp,Uut,0)
        bea0(1) = beu(1)
        bea0(2) = beu(2)
        bea0(3) = beu(3)
        bea0(4) = beu(4) 
        TAU = 1.D0
        UNILEN = 1.D0
        IOFF = 0
        ITRY3 = 0
        IFUN = 30
        IW = 0
        CALL KYLPFacet(ELAFRA,SCALS,POT,AONSET,BEA,ANG,COSANG,NSTEPS,
     x     SOV5,TAU,UNILEN,BEU,BEA0,IOFF,ITRY3,IFUN,IERR,0,PHIYLP,
     x     0.2D0,200)
c       Ensure that double precision numbers are entered properly.
c       (tau=1.D0,unilen=1.D0,ang1ok=0.1D0)  SKYE + BVB, 17/12/2008.        
      END IF
C      CALL KVEC5D2MAT(AONSET,aij)
C      qut = -aij(2,2) / aij(1,1)
C      zval = POT*dsqrt(1.D0 + qut*qut + (1.D0-qut)*(1.D0-qut) )
C      WRITE(ISCR,'(A,F8.5)') '   qut..... = ', qut
C      WRITE(ISCR,'(A,F8.5)') '   POT..... = ', POT
      zval = dsqrt(3.D0/2.D0)*SCALS       ! Simplified & better approach
C     WRITE(ISCR,'(A,F8.5)') '   zval.... = ', zval
c ----------------------------------------------------------------------
c     end of subroutine KGETZFACET
c ----------------------------------------------------------------------
      RETURN
      END 
************************************************************************
      SUBROUTINE KGETZSEP (zval, S, npame)
C
      INTEGER npame, NCFE, NSTEPS
      DOUBLE PRECISION qut, zval, tmp, S(npame)
      DOUBLE PRECISION Uut(5), Fc, DFc(5), DDFc(5,5), aij(3,3)
      INTEGER nchlst, IERR, iptial, NVAR, IDER, IOFF, IFUN, ITRY3, IW
C
      DOUBLE PRECISION    AONSET(5),BEU(4),BEA0(4),BEA(4),PHIYLP(5)
      DOUBLE PRECISION    ELAFRA,SCALS,POT,COSANG,ANG,SOV5(5)
      DOUBLE PRECISION    TAU, UNILEN
C
C     Assuming axis-1 is tensile axis: deviatoric stress sigut:
C
      Uut(1) = sqrt(0.75D0)
      Uut(2) = 0.5D0
      Uut(3) = 0.0D0
      Uut(4) = 0.0D0
      Uut(5) = 0.0D0
C
      iptial = S(6)
      NCFE= S(2)
      IF (iptial==1) THEN
        NVAR=5
        IDER=1 
        CALL KF3DER(Fc,DFc,DDFc,Uut,S(9),S(9+NCFE),
     x     NVAR,IDER,NCHLST,0)
        SCALS = 1.D0/Fc
C        CALL KNORML5D (DFc,AONSET,tmp)
C        CALL KDOTV(tmp,Uut,AONSET)
C        POT = SCALS*tmp
      END IF    
      IF (iptial==2) THEN
        CALL Kbetap(beu,tmp,Uut,0)
        bea0(1) = beu(1)
        bea0(2) = beu(2)
        bea0(3) = beu(3)
        bea0(4) = beu(4) 
        TAU = 1.D0
        UNILEN = 1.D0
        IOFF = 0
        ITRY3 = 0
        IFUN = 3
        IW = 0
        CALL KYLP(ELAFRA,SCALS,POT,AONSET,BEA,ANG,COSANG,NSTEPS,
     &           SOV5,TAU,UNILEN,BEU,BEA0,S(9),S(9+NCFE),
     &           IOFF,ITRY3,IFUN,IERR,IW,PHIYLP)
      END IF
C      CALL KVEC5D2MAT(AONSET,aij)
C      qut = -aij(2,2) / aij(1,1)
C      zval = POT*dsqrt(1.D0 + qut*qut + (1.D0-qut)*(1.D0-qut) )
C      WRITE(ISCR,'(A,F8.5)') '   qut..... = ', qut
C      WRITE(ISCR,'(A,F8.5)') '   POT..... = ', POT
      zval = dsqrt(3.D0/2.D0)*SCALS       ! Simplified & better approach
C     WRITE(ISCR,'(A,F8.5)') '   zval.... = ', zval
c ----------------------------------------------------------------------
c     end of subroutine KGETZSEP
c ----------------------------------------------------------------------
      RETURN
      END 
************************************************************************
      subroutine   KYLPFacet(hg,scalsg,fg,avg,beag,ang1,cos1,nsteps,
     x     sov,tau,unilen,beui,bea0,ioff,itry3in,ifun,ierr,iw,svg,
     x     ang1ok,maxstp)
c ----------------------------------------------------------------------
c     calculation of yield locus point
c     for a given stress direction by an iterative procedure;
c
c     iteration loop [3]: cutting fish-tails by using different starting
c                         guesses 'bea0()';
c
c     iteration loop [2]: mean normal by improving 'avmean()';
c
c     -> 'ang2ok' : 0.2 degrees (convergence if angle between 'avmean()'
c                               'avmean()' and 'avmid()' smaller than 
c                               'ang2ok');
c     -> 'mxmean' :             (maximum number of iterations);
c     -> 'rel2ok' : 0.0001      (minimum relative improvement);
c
c     iteration loop [1]: intersection with yield locus by improving
c                         'avg()';
c
c     --> 'ang1ok' : 0.2 degrees (convergence if angle between 'uiv()'
c                                and 'uvg()' smaller than 'ang1ok');
c     --> 'maxstp' : 20          (maximum number of iterations);
c     --> 'rel1ok' : 0.0001      (minimum relative improvement);
c
c     iteration loop [4]: hardening for hyperplane approximation of
c                         yield locus corresponding to 'avg()';
c
c     --> 'maxtau' :             (maximum number of iterations);
c     --> 'rel4ok' : 0.0001      (minimum relative improvement);
c
c     SOME VARIABLES :
c
c     ioff      : if ioff=0, no offset stress;
c                 if ioff=1, offset stress  inside yield locus;
c                 if ioff=2, offset stress outside yield locus;
c     ifun      : if ifun=1, "fcf" series expansion;
c                 if ifun=2, "gen" series expansion;
c                 if ifun=3, "eao" series expansion;
c     sov(1..5) : offset stress;
c ----------------------------------------------------------------------
      implicit none
      integer maxvar
      parameter (maxvar=5)
      integer i,iagain,ibest1(3),ibesttry,iconv1(3),iconv2(3),iconv4(3),
     x     icvg1,icvg2,icvg4,ider,ierr,ifun,ilocal,imean,imeans(3),ioff,
cc     x     iprin,
     x     iscale,istep,isteps(3),itau,itaus(3),itry,itry3,itry3in,
     x     itrymx,iw,iw1,iw2,j,maxstp,nout,nsteps,nvar
c  17/08/04
      integer nbeco
      real*8 am0p(3,3),ammean(3,3),ang1,ang1ok,ang1old,ang2,ang2ok,
     x     ang2old,av0(maxvar),av0p(maxvar),avg(maxvar),avmean(maxvar),
     x     rot,rotmax,fac,
cc     x     amdu(3,3),beuidu(4),fdu,sdu,
     x     avx(5),beax(4),du,
cc     x     appx,facapp,
cc     x     appxx,avxx(5),beacon(4),beaxx(4),rotxx,
     x     avmid(maxvar),bea0(4),beaco(4),beag(4),beui(4),
     x     besta1(3),
     x     bestav(3,5),bestbe(3,4),bestc1(3),besth(3),bestf(3),bests(3),
     x     bestr1(3),bestr4(3),bestt(3),bstacm(3,6),bstsca(3),cos1,
     x     cos1ok,cos2,cos2ok,
     x     davg(5,4),ddavg(5,10),ddfbeg(10),ddfg(maxvar,maxvar),
     x     ddhbeg(10),dfbeg(4),dfg(maxvar),dhbeg(4),draph,
     x     dsic(6),dsim(3,3),dtau,eg,evg(maxvar),fg,fgor,
     x     hfac(4),
     x     hg,hgco,hgold,hmax,hmin,hsi,lasta2(3),lastc2(3),lastr2(3),
     x     psnile,psnimi,r,r1,raph,rel1,rel1ok,rel2,rel2ok,rel4,rel4ok,
     x     scalsg,sg,sic(6),sig,sivavg,sivg(maxvar),sovavg,
     x     sov(maxvar),svg(maxvar),tau,uivavg,uiv(maxvar),uivg(maxvar),
     x     unilen,uvg(maxvar)
      real*8 dtr,pi,rtd,root23
      character wicvg1*7,wicvg2*7,wicvg4*7,wimean*7,wistep*7,witau*7,
     x     wloop1*44,wloop2*44,wloop4*52,wrel1*10,wrel2*10,wrel4*10,
     x     wstars*15
      data wstars/'***************'/
      integer ifish,maxtau,mxmean
      common /parylp/ ifish,maxtau,mxmean
      real*8 ac0p(6),acmean(6),delas(6,6),ep,ep0,epfact,epi,er,
     x     fmean,scale,tmp
      common /wrkylp/ ac0p,acmean,delas,ep,ep0,epfact,epi,er,
     x     fmean,scale,tmp
c      parameter (ang1ok=0.0002d0,rel1ok=0.000001d0)
c      parameter (ang1ok=0.3d0,rel1ok=0.000001d0)
      parameter (rel1ok=0.000001d0)
      parameter (ang2ok=0.2d0,rel2ok=0.0001d0)
      parameter (rel4ok=0.0001d0)
c     parameter (maxstp=2000)
      parameter (rotmax=3.d0)
c     data nout/9/
      integer          out6,out9,outopt
      common /knchout/ out6,out9,outopt
c ----------------------------------------------------------------------
c     bvb 11/08/04: initialise iconv1()
c ----------------------------------------------------------------------
      iconv1(1)=-1
      iconv1(2)=-1
      iconv1(3)=-1
      isteps(1)= 0
      isteps(2)= 0
      isteps(3)= 0
c ----------------------------------------------------------------------
c     calculate some constants;
c ----------------------------------------------------------------------
      nout=out9
      pi=acos(-1.d0)
      rtd=180.d0/pi
      dtr=pi/180.d0
      root23=sqrt(2.d0/3.d0)
      cos1ok=cos(ang1ok*dtr)
      cos2ok=cos(ang2ok*dtr)
      if (scale.eq.1.d0) iscale=0
      if (ifish.eq.0) itry3=0
c ----------------------------------------------------------------------
c     'nvar=5' is assumed in various subroutines called from ylp;
c ----------------------------------------------------------------------
      nvar=5
c ----------------------------------------------------------------------
c     set sov(1..5)=0. if no offset-stress;
c     set eg=1.
c     calculate 'av0(1..5)'    corresponding to given 'bea0(1..4)';
c     calculate 'av0p(1..5)'   corresponding to given 'ac0p(1..6)';
c               'avmean(1..5)' corresponding to given 'acmean(1..6)';
c               'uiv(1..5)'    corresponding to given 'beui(1..4)';
c     initialize 'iagain';
c     initialize 'itry3' and 'itrymx';
c ----------------------------------------------------------------------
      if (ioff.eq.0) then
         do 10 i=1,nvar
            sov(i)=0.d0
 10      continue
      endif
      eg=1.d0
cc      c2=1.d0
      call kapbea(av0,bea0,0)
      if (ioff.eq.2.and.mxmean.gt.0) then
         call kmsnc(am0p,ac0p)
         call kvdm(av0p,am0p)
         call kmsnc(ammean,acmean)
         call kvdm(avmean,ammean)
      else
         call kapbea(uiv,beui,0)
      endif
      iagain=0
      itry3=itry3in
      if (itry3.eq.0) itrymx=1
      if (itry3.eq.-1) itrymx=2
      if (itry3.eq.1) itrymx=3
 9    if (iw.ge.1) then
         write(nout,6999)
         write(nout,6998) ioff
         if (ioff.ne.0) write(nout,6997) (sov(i),i=1,nvar)
         write(nout,6996) mxmean
         if (ioff.eq.2.and.mxmean.gt.0) then
            write(nout,6995) (av0p(i),i=1,nvar)
            write(nout,6994) (avmean(i),i=1,nvar)
         else
            write(nout,6993) unilen
            write(nout,6992) (beui(i)*rtd,i=1,4)
            write(nout,6991) (uiv(i),i=1,nvar)
         endif
         write(nout,6990) maxtau
         write(nout,6989) tau
         write(nout,6988) (bea0(i)*rtd,i=1,4)
         write(nout,6955) (av0(i),i=1,nvar)
         write(nout,6987) itry3
         write(nout,6986) ifun
         write(nout,6950) mxmean,1.0d0-cos2ok,ang2ok,rel2ok,maxstp,
     x        1.d0-cos1ok,ang1ok,rel1ok,maxtau,rel4ok
         write(nout,6949) 
         write(nout,6940) wstars,wstars,wstars,wstars,wstars,
     x        wstars,wstars,wstars,wstars,wstars
      endif
c ----------------------------------------------------------------------
c     start of iteration loop [3];
c     cutting fish-tails by using different starting guesses 'bea0()';
c ----------------------------------------------------------------------
      nsteps=0
      itry=0
 3    itry=itry+1
      ilocal=0
      if (itry3.eq.-1) then
         if (itry.eq.1) bea0(4)=bea0(4)-60.d0*dtr
         if (itry.eq.2) bea0(4)=bea0(4)+120.d0*dtr
      endif
      if (itry3.eq.1) then
         if (itry.eq.2) bea0(4)=bea0(4)-60.d0*dtr
         if (itry.eq.3) bea0(4)=bea0(4)+120.d0*dtr
      endif
      do 30 i=1,4
         beag(i)=bea0(i)
 30   continue
      if (iw.ge.3) then
         write(nout,6985) itry
         write(nout,6988) (bea0(i)*rtd,i=1,4)
      endif
c ----------------------------------------------------------------------
c     start of iteration loop [2];
c     mean normal by improving 'avmean()';
c ----------------------------------------------------------------------
      icvg2=-1
      imean=0
 2    imean=imean+1
      if (ioff.eq.2.and.mxmean.gt.0) then
         do 200 i=1,6
            r=0.d0
            do 202 j=1,6
               r=r+delas(i,j)*acmean(j)
 202        continue
            sic(i)=-r
 200     continue
         call kdhc(dsic,hsi,sic)
         call kmssc(dsim,dsic)
         call kbetam(beui,r,dsim,0)
         call kapbea(uiv,beui,0)
         scale=1.d0
         if (iscale.eq.1) scale=unilen/r
         ider=0
         iw1=0
         if (iw.ge.5) iw1=1
c         if (ifun.eq.1) call kf1der(fmean,dfg,ddfg,avmean,ider,iw1)
c         if (ifun.eq.2) call kf2der(fmean,dfg,ddfg,avmean,nvar,ider,iw1)
c         if (ifun.eq.3) call kf3der(fmean,dfg,ddfg,avmean,nvar,ider,
c     x      nout,iw1)
c        if (ifun.eq.5) call kf4der(fmean,dfg,ddfg,avmean,ider,nout,iw1)
c         epfact=fmean*scale
         if (iw.ge.10) then
            write(nout,6984) imean
            write(nout,6994) (avmean(i),i=1,nvar)
            write(nout,6983) fmean
            write(nout,6993) unilen
            write(nout,6992) (beui(i)*rtd,i=1,4)
            write(nout,6991) (uiv(i),i=1,nvar)
         endif
         do 204 i=1,6
            bstacm(itry,i)=acmean(i)
 204     continue
         bstsca(itry)=scale
      endif
c ----------------------------------------------------------------------
c     start of iteration loop [1];
c     intersection with yield locus by improving 'avg()';
c ----------------------------------------------------------------------
      icvg1=-1
      istep=0
 1    istep=istep+1
      nsteps=nsteps+1
c ----------------------------------------------------------------------
c     calculate 'avg(1..5)' corresponding to 'beag(1..4)';
c     calculate distortion factors in 4-d space for 'beag(1..4)';
c ----------------------------------------------------------------------
      call kapbea(avg,beag,0)
      call kdistor(hfac,beag)
c ----------------------------------------------------------------------
c     reverse the sign of this plastic strain rate mode
c     if at wrong side of yield locus;
c ----------------------------------------------------------------------
      call kdotv(uivavg,uiv,avg)
      if ((ioff.ne.2.and.uivavg.lt.0.d0).or.
     +     (ioff.eq.2.and.uivavg.gt.0.d0)) then
         if (beag(4).lt.pi) then
            beag(4)=beag(4)+pi
         else
            beag(4)=beag(4)-pi
         endif
         do 40 i=1,nvar
            avg(i)=-avg(i)
 40      continue
         uivavg=-uivavg
       endif
c ----------------------------------------------------------------------
c     calculate 'evg(1..5)' from 'avg(1..5)' and 'eg'; 
c ----------------------------------------------------------------------
       do 50 i=1,nvar
          evg(i)=avg(i)*eg
 50    continue
c ----------------------------------------------------------------------
c     calculate 'fg'       : value of series expansion for 'avg(1..5)';
c               'dfg(..)'  : first  order partial derivatives of series
c                            expansion;
c               'ddfg(..)' : second order partial derivatives of series
c                            expansion;
c ----------------------------------------------------------------------
      ider=2
      iw1=0
      if (iw.ge.5) iw1=1
c     if (ifun.eq.1) call kf1der(fg,dfg,ddfg,avg,ider,iw1)
c     if (ifun.eq.2) call kf2der(fg,dfg,ddfg,avg,nvar,ider,iw1)
c     if (ifun.eq.3) call kf3der(fg,dfg,ddfg,avg,nvar,ider,nout,iw1)
c     if (ifun.eq.5) call kf4der(fg,dfg,ddfg,avg,ider,nout,iw1)
      if (ifun.eq.30) call KFacetPOT(fg,dfg,ddfg,avg,ider,nout,iw1)
c ----------------------------------------------------------------------
c     iteration loop [4];
c     Newton-Raphson iteration to solve 'hg' and 'tau' from non-linear
c     relation 'sovavg+hg*sivavg=tau(hg)*fg';
c ----------------------------------------------------------------------
      sivavg=unilen*uivavg
      call kdotv(sovavg,sov,avg)
      itau=0
      if (ioff.eq.2.and.maxtau.gt.0) then
         hg=0.d0
         icvg4=-1
 4       itau=itau+1
         epi=epfact*hg
         ep=ep0+epi
c        call ystres(ep,er,tmp,tau,dtau)
         dtau=epfact*dtau
         raph=sovavg+hg*sivavg-tau*fg
         draph=sivavg-dtau*fg
         hgco=-raph/draph
         if (itau.gt.1) rel4=hgco/hgold
         hg=hg+hgco
         hgold=hg
         if (itau.gt.1.and.abs(rel4).lt.rel4ok) icvg4=2
         if (itau.eq.maxtau) icvg4=0
         if (iw.ge.10) then
            wrel4=''
            wicvg4=''
            write(witau,1999) itau
            if (itau.gt.1) write(wrel4,1998) rel4
            if (icvg4.ge.0) write(wicvg4,1999) icvg4
            write(wloop4,1997) witau,hg,tau,wrel4,wicvg4
         endif
         if (icvg4.ge.0) goto 49
         if (iw.ge.10) write(nout,6948) wloop4
         goto 4
      else
         hg=(tau*fg-sovavg)/sivavg
ccc    WRITE(nout,*) 'hg,fg,sivavg,uivavg'
ccc    WRITE(nout,*) hg,fg,sivavg,uivavg
cc         hg=(c1*fg**c2-sovavg)/sivavg
         if (iw.ge.10) then
            wrel4=''
            wicvg4=''
            witau=''
            write(wloop4,1997) witau,hg,tau,wrel4,wicvg4
         endif
      endif
 49   epi=epfact*hg
      ep=ep0+epi
      psnile=scale*hg
      psnimi=root23*psnile
c ----------------------------------------------------------------------
c     store original value of 'fg' in 'fgor' and modify value of 'fg'
c     to take into account offset stress;
c ----------------------------------------------------------------------
      fgor=fg
      fg=hg*uivavg
c ----------------------------------------------------------------------
c     calculate yield stress 'svg(1..5)' corresponding to 'avg(1..5)';
c               stress mode  'uvg(1..5)' corresponding to 'svg(1..5)';
c ----------------------------------------------------------------------
cc      iw1=0
cc      if (iw.ge.5) iw1=1
cc      if (ifun.eq.1) then
cc        call spap(svg,scf,nfoder,avg,iw1)
cc        do 51 i=1,nvar
cc           svg(i)=tau*svg(i)
cc 51     continue
cc        call upsmsp(uvg,sg,svg)
cc      endif
      r=0.d0
      do 52 i=1,nvar
         r=r+dfg(i)*avg(i)
 52   continue
      r=fgor-r
      do 54 i=1,nvar
cc         svg(i)=c1*(dfg(i)+avg(i)*r)
         svg(i)=tau*(dfg(i)+avg(i)*r)
 54   continue
      call kupsmsp(uvg,sg,svg)
c ----------------------------------------------------------------------
c     calculate increment  'sivg(1..5)' from 'sov(1..5)' to 'svg(1..5)';
c               stress mode 'uivg(1..5)' corresponding to 'sivg(1..5)';
c ----------------------------------------------------------------------
      do 60 i=1,nvar
         sivg(i)=svg(i)-sov(i)
 60   continue
      call kupsmsp(uivg,sig,sivg)
c ----------------------------------------------------------------------
c     calculate scalar product 'cos1' of 'uiv(1..5)' and 'uivg(1..5)',
c     i.e. the cosine of the angle between these two unit vectors;
c     also calculate this angle itself, 'ang1', and the relative 
c     change of 'ang1' with respect to its previous value 'ang1old';
c ----------------------------------------------------------------------
      call kdotv(cos1,uiv,uivg)
      if (cos1.lt.-1.d0) cos1=-1.d0
      if (cos1.gt.1.d0) cos1=1.d0
      ang1=acos(cos1)*rtd
      if (istep.gt.1) rel1=(ang1-ang1old)/ang1old
      ang1old=ang1
      if (iw.ge.3) then
         write(nout,6982) istep
         write(nout,6978) (beag(i)*rtd,i=1,4)
         write(nout,6900) (hfac(i),i=1,4)
         write(nout,6979) (avg(i),i=1,nvar)
         write(nout,6980) eg
         write(nout,6981) (evg(i),i=1,nvar)
         write(nout,6977) fgor
         write(nout,6976) hg
         write(nout,6975) (svg(i),i=1,nvar)
         write(nout,6974) sg
         write(nout,6973) (uvg(i),i=1,nvar)
         write(nout,6972) (sivg(i),i=1,nvar)
         write(nout,6971) sig
         write(nout,6970) (uivg(i),i=1,nvar)
         write(nout,6870) ang1
      endif
c ----------------------------------------------------------------------
c     keep track of best solution of iteration loop [1];
c ----------------------------------------------------------------------
      if (istep.eq.1.or.cos1.gt.bestc1(itry)) then
         ibest1(itry)=istep
         bests(itry)=sg
         bestf(itry)=fgor
         do 70 i=1,nvar
            bestav(itry,i)=avg(i)
 70      continue
         do 80 i=1,4
            bestbe(itry,i)=beag(i)
 80      continue
         bestc1(itry)=cos1
         besta1(itry)=ang1
         bestr1(itry)=rel1
         itaus(itry)=itau
         besth(itry)=hg
         bestt(itry)=tau
         bestr4(itry)=rel4
         iconv4(itry)=icvg4
      endif
c ----------------------------------------------------------------------
c     stop iteration loop [1] if convergence criterion is satisfied,
c     or if relative improvement is too small,
c     or if maximum number of iteration steps 'maxstp' reached;
c ----------------------------------------------------------------------
      if (istep.eq.maxstp) icvg1=0
      if (istep.gt.1.and.abs(rel1).lt.rel1ok) then
         icvg1=2
cc         if (ilocal.eq.4) icvg1=2
cc         if (ilocal.eq.2) ilocal=3
cc         if (ilocal.eq.0) ilocal=1
      endif
      if (cos1.gt.cos1ok) icvg1=1
      if (iw.ge.10) then
         wrel1=''
         wicvg1=''
         write(wistep,1999) istep
         if (istep.gt.1) write(wrel1,1998) rel1
         if (icvg1.ge.0) write(wicvg1,1999) icvg1
         write(wloop1,1996) wistep,1.d0-cos1,ang1,wrel1,wicvg1
         write(nout,6969) wloop1,wloop4
      endif
      if (icvg1.ge.0) goto 19
cc      if (ilocal.eq.1) then
cc         beuidu(1)=beag(1)+pi/18
cc         beuidu(2)=beag(2)+pi/18
cc         beuidu(3)=beag(3)+pi/18
cc         beuidu(4)=beag(4)
cc     iprin=1
cc         call psylp(sdu,fdu,amdu,beag,beuidu,ifun,iprin,iw)
cc         if (iw.ge.3) write(nout,6947) ilocal
cc         ilocal=2
cc         goto 1
cc      endif
cc      if (ilocal.eq.3) then
cc         goto 1
cc      endif
c ----------------------------------------------------------------------
c     modify partial derivatives 'dfg(..)' and 'ddfg(..)' to take into 
c     account offset stress 'sov(1..5)';
c ----------------------------------------------------------------------
      r1=1.d0/unilen
cc      r2=c1*c2*fgor**(c2-1.d0)
cc      r3=(c2-1.d0)/fgor
      do 90 i=1,nvar
         do 92 j=i,nvar
cc            r=r2*(r3*dfg(i)*dfg(j)+ddfg(i,j))*r1
            r=tau*ddfg(i,j)*r1
            ddfg(i,j)=r
            ddfg(j,i)=r
 92      continue
 90   continue
      do 100 i=1,nvar
cc         dfg(i)=(r2*dfg(i)-sov(i))*r1
         dfg(i)=(tau*dfg(i)-sov(i))*r1
 100  continue
c ----------------------------------------------------------------------
c     calculate new guess 'beag(1..4)';
c ----------------------------------------------------------------------
c+6  17/08/04 
      nbeco=4
      if (nbeco.eq.1) then
         call knew1(beaco,fg,dfg,ddfg,avg,beag,uiv,nout,iw)
         fac=1.d0
      endif
      if (nbeco.eq.4) then
         iw1=0
         if (iw.ge.5) iw1=1
         call kdapb(davg,ddavg,beag,iw1)
         iw1=0
         iw2=0
         if (iw.ge.5) then
            iw1=1
            iw2=1
         endif
         call kdfb(dfbeg,ddfbeg,davg,ddavg,dfg,ddfg,iw1,iw2)
         iw1=0
         iw2=0
         if (iw.ge.4) then
            iw1=1
            iw2=1
         endif
         call kddisb(dhbeg,ddhbeg,fg,dfbeg,ddfbeg,avg,davg,ddavg,uiv,
     x        iw1,iw2)
         iw1=0
         if (iw.ge.4) iw1=1
cc test 5 Jan 1994
         if (iw.ge.3.and.ilocal.gt.0) iw1=1
         call knew(beaco,dhbeg,ddhbeg,iw1)
cc start test 9/8/99
c        WRITE(6,*) 'AFTER NEW '
         do 111 i=1,4
            beax(i)=beag(i)+beaco(i)
 111     continue
         call kapbea(avx,beax,0)
         call kdotv(du,avx,avg)
         if (du.gt.1.d0) du=1.d0
         if (du.lt.-1.d0) du=-1.d0
         rot=dacos(du)
c      WRITE(6,*) 'rot= ',rot
ccc approximation of angle between avg() and avx()
cc      rot=dsqrt((hfac(1)*abs(beaco(1)))**2+
cc     x   (hfac(2)*abs(beaco(2)))**2+
cc     x   (hfac(3)*abs(beaco(3)))**2+
cc     x   (hfac(4)*abs(beaco(4)))**2)
c (28/02/2007: SKYE+BVB rot=0 problem solved by new 2 lines
c        fac=min(1.d0,rotmax*dtr/rot)
         fac=1.d0
         if (rot.gt.rotmax*dtr) fac=rotmax*dtr/rot
         r=0.d0
         do 112 i=1,4
            r=r+beaco(i)*dhbeg(i)
 112     continue
         if (r.gt.0.d0) fac=-fac
         if (iw.ge.3) write(nout,6901) (beaco(i)*rtd,i=1,4),rot*rtd,
     x      fac,hg+fac*r
cc 10/8/99: line search if correction points to higher hg
c        WRITE(6,*) 'BEFORE LINE '
         if (r.gt.0.d0) 
     x      call kline(fac,avg,beag,beaco,rot,rotmax,uiv,ifun,
     x      iw,nout)
cc      call normal(beacon,beaco,4)
cc      do 112 i=1,4
cc         beacon(i)=beacon(i)*dtr
cc 112  continue
cc      do 114 i=1,4
cc         beaxx(i)=beag(i)+beacon(i)
cc 114  continue
cc      call apbea(avxx,beaxx,0)
cc      call dotv(du,avxx,avg)
cc      rotxx=dacos(du)
cc      appxx=dsqrt((hfac(1)*abs(beacon(1)))**2+
cc     x   (hfac(2)*abs(beacon(2)))**2+
cc     x   (hfac(3)*abs(beacon(3)))**2+
cc     x   (hfac(4)*abs(beacon(4)))**2)
cc      if (iw.ge.3) write(nout,6902) 
cc     x   (beacon(i)*rtd,i=1,4),(beaxx(i)*rtd,i=1,4),rotxx*rtd,appxx*rtd
cc stop test 9/8/99
c+1  17/08/04 
      endif
      do 110 i=1,4
         beag(i)=beag(i)+fac*beaco(i)
 110  continue
 17   if (iw.ge.3) write(nout,6968) (fac*beaco(i)*rtd,i=1,4)
c     WRITE(6,*) 'GOTO 1'
      goto 1
c ----------------------------------------------------------------------
c     end of iteration loop [1];
c ----------------------------------------------------------------------
 19   isteps(itry)=istep
      iconv1(itry)=icvg1
      wloop2=''
      if (ioff.eq.2.and.mxmean.gt.0) then
         istep=ibest1(itry)
         do 210 i=1,nvar
            avg(i)=bestav(itry,i)
 210     continue
         call kdotv(r,av0p,avg)
         r=1.d0/sqrt(2.d0+2.d0*r)
         do 220 i=1,nvar
            avmid(i)=(av0p(i)+avg(i))*r
 220     continue
         call kdotv(cos2,avmean,avmid)
         if (cos2.lt.-1.d0) cos2=-1.d0
         if (cos2.gt.1.d0) cos2=1.d0
         ang2=acos(cos2)*rtd
         if (imean.gt.1) rel2=(ang2-ang2old)/ang2old
         ang2old=ang2
         if (iw.ge.3) then
            write(nout,6984) imean
            write(nout,6995) (av0p(i),i=1,nvar)
            write(nout,6979) (avg(i),i=1,nvar)
            write(nout,6967) (avmid(i),i=1,nvar)
            write(nout,6994) (avmean(i),i=1,nvar)
         endif
c ----------------------------------------------------------------------
c     stop iteration loop [2] if convergence criterion is satisfied,
c     or if relative improvement is too small,
c     or if maximum number of iteration steps 'maxstp' reached;
c ----------------------------------------------------------------------
         if (imean.eq.mxmean) icvg2=0
         if (imean.gt.1.and.abs(rel2).lt.rel2ok) icvg2=2
         if (cos2.gt.cos2ok) icvg2=1
         if (iw.ge.10) then
            wrel2=''
            wicvg2=''
            write(wimean,1999) imean
            if (imean.gt.1) write(wrel2,1998) rel2
            if (icvg2.ge.0) write(wicvg2,1999) icvg2
            write(wloop2,1996) wimean,1.0-cos2,ang2,wrel2,wicvg2
            wloop1=''
            write(wloop1,1995) istep
            write(nout,6966) wloop2,wloop1
         endif
         if (icvg2.ge.0) goto 29
         do 230 i=1,5
            avmean(i)=avmid(i)
 230     continue
         call kmdv(ammean,avmean)
         call kcsnm(acmean,ammean)
         goto 2
      endif
c ----------------------------------------------------------------------
c     end of iteration loop [2];
c ----------------------------------------------------------------------
 29   imeans(itry)=imean
      iconv2(itry)=icvg2
      lastc2(itry)=cos2
      lasta2(itry)=ang2
      lastr2(itry)=rel2
      if (iw.ge.10) then
         i=isteps(itry)
         istep=ibest1(itry)
         cos1=bestc1(itry)
         ang1=besta1(itry)
         rel1=bestr1(itry)
         icvg1=iconv1(itry)
         wrel1=''
         write(wistep,1995) istep
         if (i.gt.1) write(wrel1,1998) rel1
         write(wicvg1,1999) icvg1
         write(wloop1,1996) wistep,1-cos1,ang1,wrel1,wicvg1
         itau=itaus(itry)
         hg=besth(itry)
         tau=bestt(itry)
         rel4=bestr4(itry)
         icvg4=iconv4(itry)
         wrel4=''
         wicvg4=''
         if (itau.gt.1) write(wrel4,1998) rel4
         if (maxtau.gt.0) then
            write(witau,1999) itau
            write(wicvg4,1999) icvg4
         endif
         write(wloop4,1997) witau,hg,tau,wrel4,wicvg4
         write(nout,6959) itry,wloop2,wloop1,wloop4
         write(nout,6940) wstars,wstars,wstars,wstars,wstars,
     x        wstars,wstars,wstars,wstars,wstars
      endif
c ----------------------------------------------------------------------
c     stop iteration loop [3] if 1 starting guess is to be considered 
c     and convergence is reached with it,
c     or if maximum number of starting guesses is reached;
c     remark : if only one starting guess to be considered (itry3=0) 
c     and no convergence obtained with this starting guess, override 
c     original value of "itry3" and try with two other starting guesses;
c ----------------------------------------------------------------------
      if (itry3.eq.0) then
ccc+1 17/08/04       
ccc         if (iconv1(1).eq.1.or.iconv1(1).eq.2) then
ccc 17/01/2007 skye+bvb restore overriding if no convergence
         if (iconv1(1).eq.1) then
            goto 39
         else
            itry3=1
            itrymx=3
         endif
      endif
      if (itry.eq.itrymx) goto 39
      goto 3
c ----------------------------------------------------------------------
c     end of iteration loop [3];
c ----------------------------------------------------------------------
c ----------------------------------------------------------------------
c     determine error level "ierr";
c     "ierr=0" : at least one iteration procedure leads to a
c                solution satisfying the convergence criterion;
c     "ierr=1" : none of the 3 iteration procedures leads to a 
c                solution satisfying the convergence criterion,
c                but at least one of them leads to a stable solution;
c     "ierr=2" : none of the 3 iteration procedures leads to a
c                solution satisfying the convergence criterion
c                or a stable solution;
c ----------------------------------------------------------------------
 39   ierr=1
      if (iconv1(1).eq.0.and.iconv1(2).eq.0.and.iconv1(3).eq.0) ierr=2
      if (iconv1(1).eq.1.or.iconv1(2).eq.1.or.iconv1(3).eq.1) ierr=0
c ----------------------------------------------------------------------
c     if "ierr=2" : re-run the iteration procedures once, 
c     this time printing intermediate results;
c ----------------------------------------------------------------------
      if (ierr.eq.2.and.iagain.eq.0) then
         iagain=1
cc iw=0 tijdelijk om te veel output te vermijden
cc         iw=0
cc         bea0(4)=bea0(4)-60.d0*dtr
cc         goto 9
      endif
c ----------------------------------------------------------------------
c     if "ierr=0" : only the solutions satisfying the convergence 
c     criterion will be considered further on;
c ----------------------------------------------------------------------
      if (ierr.eq.0) then
         if (iconv1(1).eq.2) iconv1(1)=0
         if (iconv1(2).eq.2) iconv1(2)=0
         if (iconv1(3).eq.2) iconv1(3)=0
      endif
c-1 (23/3/99)
c      if (ierr.eq.0.or.ierr.eq.1) then
c bvb   
c-1 (24/12/08) skye+bvb
c      if (ierr.eq.0.or.ierr.eq.1) then
c ----------------------------------------------------------------------
c     make a choice between the iteration runs in order to use the 
c     corresponding results as final results of this subroutine;
c ----------------------------------------------------------------------
         if (itry3.eq.0) then
            ibesttry=1
         else
c ----------------------------------------------------------------------
c     in case of 2/3 starting guesses (itry3=-1/1), the one leading to 
c     the stress point closest to the stress origin is chosen in order 
c     to deal with fish-tail-situations;
c ----------------------------------------------------------------------
            ibesttry=0
            if (ioff.ne.2) then
               hmin=9999999.
               do 120 i=1,itrymx
cc test 5 Jan 1994
cc                  if (iconv1(i).ne.0.
cc     x                 and.besth(i).gt.0.d0.and.besth(i).lt.hmin) then
                  if (besth(i).gt.0.d0.and.besth(i).lt.hmin) then
                     hmin=besth(i)
                     ibesttry=i
                  endif
 120           continue
            endif
            if (ioff.eq.2) then
               hmax=0.d0
               do 130 i=1,itrymx
cc test 5 Jan 1994
cc                  if (iconv1(i).ne.0.
cc     x                 and.besth(i).gt.0.d0.and.besth(i).gt.hmax) then
                  if (besth(i).gt.0.d0.and.besth(i).gt.hmax) then
                     hmax=besth(i)
                     ibesttry=i
                  endif
 130           continue
            endif
         endif
c ----------------------------------------------------------------------
c     the results of the best iteration solution
c     are taken to be the final results of this subroutine;
c ----------------------------------------------------------------------
         hg=besth(ibesttry)
         sg=bests(ibesttry)
         scalsg=sg/tau
         fg=bestf(ibesttry)
         do 140 i=1,nvar
            avg(i)=bestav(ibesttry,i)
 140     continue
         do 150 i=1,4
            beag(i)=bestbe(ibesttry,i)
 150     continue
         cos1=bestc1(ibesttry)
         ang1=besta1(ibesttry)
         rel1=bestr1(ibesttry)
         do 152 i=1,6
            acmean(i)=bstacm(ibesttry,i)
 152     continue
         scale=bstsca(ibesttry)
c-1 (23/3/99)
c      endif
c bvb 
c-1 (24/12/08) skye+bvb
c      endif  
      if (iw.ge.1) then
         write(nout,6954) 
cc         do 154 itry=1,1+itry3*2
         do 154 itry=1,itrymx
            write(nout,6953) itry,besth(itry),bests(itry),bestf(itry),
     x           (bestav(itry,i),i=1,nvar),(bestbe(itry,i)*rtd,i=1,4),
     x            isteps(itry)
 154     continue
         write(nout,6952) ibesttry,ierr
         write(nout,6939) 
      endif
 6999 format(/,'***start of subroutine KYLPFacet***')
 6998 format(5x,'ioff        =',i15)
 6997 format(5x,'sov(i)      =',5e15.6)
 6996 format(5x,'mxmean      =',i15)
 6995 format(5x,'av0p(i)     =',5e15.6)
 6994 format(5x,'avmean(i)   =',5e15.6)
 6993 format(5x,'unilen      =',e15.6)
 6992 format(5x,'beui(i)     =',4e15.6)
 6991 format(5x,'uiv(i)      =',5e15.6)
 6990 format(5x,'maxtau      =',i15)
 6989 format(5x,'tau         =',e15.6)
 6988 format(5x,'bea0(i)     =',4e15.6)
 6955 format(5x,'av0(i)      =',5e15.6)
 6987 format(5x,'itry3       =',i15)
 6986 format(5x,'ifun        =',i15)
 6950 format(/8x,'mxmean',2x,'1-cos2ok',4x,'ang2ok',4x,'rel2ok',
     x     8x,'maxstp',2x,'1-cos1ok',4x,'ang1ok',4x,'rel1ok',
     x     8x,'maxtau',32x,'rel4ok',
     x     /7x,i7,2e10.2,f10.5,7x,i7,2e10.2,f10.5,
     x     7x,i7,28x,f10.5)
 6949 format(/3x,'itry',2x,'imean',4x,'1-cos2',6x,'ang2',6x,'rel2',2x,
     x     'icvg2',2x,'istep',4x,'1-cos1',6x,'ang1',6x,'rel1',2x,
     x     'icvg1',3x,'itau',12x,'hg',11x,'tau',6x,'rel4',2x,'icvg4')
 6985 format(5x,'**itry**      =',i4)
 6984 format(5x,'**imean**     =',i4)
 6983 format(5x,'fmean         =',e15.6)
 6948 format(95x,a52)
 6982 format(5x,'**istep**     =',i4)
 6978 format(5x,'beag(i)     =',4e15.6)
 6900 format(5x,'hfac(i)     =',4e15.6)
 6979 format(5x,'avg(i)      =',5e15.6)
 6980 format(5x,'eg          =',e15.6)
 6981 format(5x,'evg(i)      =',5e15.6)
 6977 format(5x,'fg          =',e15.6)
 6976 format(5x,'hg          =',e15.6)
 6975 format(5x,'svg(i)      =',5e15.6)
 6974 format(5x,'sg          =',e15.6)
 6973 format(5x,'uvg(i)      =',5e15.6)
 6972 format(5x,'sivg(i)     =',5e15.6)
 6971 format(5x,'sig         =',e15.6)
 6970 format(5x,'uivg(i)     =',5e15.6)
 6870 format(5x,'ang1        =',e15.6)
 6969 format(51x,a44,a52)
 6947 format(1x,'ilocal=',i1,' : beag(2)+pi/18 and psylp')
 6901 format(5x,'beaco(i)    =',4e15.6,
     x   /5x,'rot         =',e15.6,
     x   /5x,'fac         =',e15.6,
     x   /5x,'hg approx.  =',e15.6)
cc 6902 format(5x,'beacon(i)   =',4e15.6,
cc     x   /5x,'beaxx(i)    =',4e15.6,
cc     x   /5x,'rotxx       =',e15.6,
cc     x   /5x,'appxx       =',e15.6)
 6968 format(5x,'beaco(i)    =',4e15.6)
 6967 format(5x,'avmid(i)    =',5e15.6)
 6966 format(7x,a44,a44)
 6964 format(i7)
 6959 format(i7,a44,a44,a52)
 6954 format(/3x,'itry',1x,'besth(itry)',4x,'bests(itry)',1x,
     x     'bestf(itry)',2x,'bestav(itry,1..5)',35x,
     x     'bestbe(itry,1..4)',10x,'isteps(itry)')
 6953 format(i7,f12.6,e15.6,f12.6,5f10.6,4f12.6,i8)
 6952 format(5x,'ibesttry  = ',i20,
     x     /5x,'ierr      = ',i20,/)
 6940 format(3x,9a15,a9)
 6939 format('***end of subroutine KYLPFacet***'/)
 1999 format(i7)
 1998 format(f10.5)
 1997 format(a7,2e14.5,a10,a7)
 1996 format(a7,2e10.2,a10,a7)
 1995 format(i5)
c ----------------------------------------------------------------------
c     end of subroutine KYLPFacet;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine KFacetPOT(f,df,ddf,ap,ider,nchlst,iw)
c ----------------------------------------------------------------------
c     calculate value of potential function, first order derivatives and
c     second order derivatives for given strain rate mode;
c     f =    function value
c     df =   first order derivatives
c     ddf =  second order derivatives
c     ap =   5-d plastic strain rate mode
c     ider = switch that controls what is calculated
c            0: f, 1: f+df, 2: f+df+ddf 
c ----------------------------------------------------------------------
      integer ider, iw, nchlst
      real*8 ap(5),f,df(5),ddf(5,5)

      CHARACTER*100 KFACETID1, KFACETID2
      INTEGER KTYPEXPR, KTYPFACET, KSTGFACET, KCTRi, KCTRj
      INTEGER KNUMFACET, KORDFACET
      DOUBLE PRECISION KLAMFACET(2500), KSNSFACET(2500,5)
      COMMON /KFACETPAR1/ FFACETID1, KFACETID2
      COMMON /KFACETPAR2/ KTYPFACET, KSTGFACET, KNUMFACET,
     &                    KORDFACET, KTYPEXPR
      COMMON /KFACETPAR3/ KLAMFACET
      COMMON /KFACETPAR4/ KSNSFACET
c ----------------------------------------------------------------------
c     calling the routine to calculate function values & derivatives
c ----------------------------------------------------------------------
      IF (iw.ge.1) 
     x  WRITE(nchlst,*) 'WITHIN FacetPOT: KordFacet= ',KordFacet
      call KFacetDERS(KORDFACET,KNUMFACET,ap,KLAMFACET,KSNSFACET,IDER,
     x                f,df,ddf,nchlst,iw)
c ----------------------------------------------------------------------
c     end of subroutine KFacetPOT
c ----------------------------------------------------------------------
      return
      end
************************************************************************ 
      subroutine KFacetDERS (Nfun,modes,A,lamvals,svecs,ider,F,dF,ddF,
     x   nchlst,iw)
c
c     This routine evaluates the function for a given mode.
c
      implicit none
c
c     Description of variables: 
c
c        Nfun              - Order of the function 
c        modes             - Number of plastic strain rate modes 
c        A()               - Plastic strain or strain rate mode 
c        lamvals()         - Coefficents (lambdas) of the function
c        svecs()           - Stress vectors in 5D space for all modes
c        F, g              - Variables to store the computed value of 
c                            the functions F & g respectively. 
c                            Note= F  = g^(1/6). 
c        dF, dg            - First order partial derivatives
c        ddF, ddg          - Second order partial derivatives
c        tmpdg,tmpddg      - Temporary variables
c        termI, termII     - Temporary variables
c        wt                - Weighting factor: For a given stain mode Ap, 
c                            if (Sp.Ap < 0), then wt = 0.0 else wt = 1.0 
c        pwork             - plastic work (Sp.Ap) 
c        w, wn2, wn1, wn   - To store pwork, pwork^(n-2), pwork^(n-1) &
c                            pwork^n respectively
c        i, p, q           - Integer to handle I/O operation
c 
      integer modes, i, p, q, Nfun, ider, iw, nchlst
      real*8 A(5), pwork, lamvals(2500), svecs(2500,5)
      real*8 g, dg(5), ddg(5,5) 
      real*8 F, dF(5), termI, termII, ddF(5,5)
      real*8 tmpdg, tmpddg, wt(2500), wtd
      real*8 w(2500), wn2(2500), wn1(2500), wn(2500) 
C
      do i = 1, modes
        pwork =(A(1)*svecs(i,1) + A(2)*svecs(i,2) + A(3)*svecs(i,3)
     &         + A(4)*svecs(i,4) + A(5)*svecs(i,5))
        if (pwork.LT.0.0) then
          wt(i) = 0.0
        else
          wt(i) = 1.0
        end if
        w(i) = pwork
        wn(i) = pwork**(Nfun)
        if (ider.EQ.1) wn1(i) = pwork**(Nfun-1)
        if (ider.EQ.2) then
          wn1(i) = pwork**(Nfun-1)
          wn2(i) = pwork**(Nfun-2)
        end if
      end do
c
c     Calculation of 'g' & 'F' :: g = sum-over-i[lambdas(i)*(w(i)**Nfun)]; 
c                                 F = g^(1/Nfun);
c
      g = 0.0
      do i = 1, modes
        g = g + lamvals(i)*wt(i)*wn(i)
      end do
c     IF (iw.ge.1) WRITE(nchlst,*) 'g= , Nfun= ', g, Nfun
      F = g**(1.0/Nfun)      
c     IF (iw.ge.1) WRITE(nchlst,*) 'AFTER DIVISION'
c
c     Calculation of first order partial derivatives 'dg' & stresses 'dF' 
c
      if (ider.EQ.1) then
        do p = 1, 5
          tmpdg = 0.0
          do i = 1, modes
            wtd=wt(i)
          tmpdg = tmpdg + lamvals(i)*wtd*svecs(i,p)*wn1(i)
cc        if (iw.ge.5) then
cc           if (i.eq.1) write(nchlst,*) 
cc     x   '   p    i lamvals(i)        wtd    svecs(i,p) wn1(i) tmpdg' 
cc           write(nchlst,'(2i5,5f10.4)') 
cc     x            p, i, lamvals(i), wtd, svecs(i,p), wn1(i), tmpdg
cc            endif
          end do
          dg(p) = Nfun*tmpdg
c
c 15/12/2006: SKYE+BVB: don't forget to use 1.0 instead of 1, or else...
c
c         dF(p) = (1.0/Nfun)*( dg(p) / g**((Nfun-1)/Nfun) )
c
          dF(p) = (1.0/Nfun)*g**(1.0/Nfun-1)*dg(p)
cc      if (iw.ge.4) then
cc         write(nchlst,*) '  dg(p)   g   g**(1/n-1)   dF(p)'
cc         write(nchlst,'(4f10.4)') dg(p),g,g**(1/Nfun-1),dF(p)
cc         write(nchlst,*) 'dg(p)               ', dg(p)
cc         write(nchlst,*) ' g                  ', g
cc         write(nchlst,*) '1.0/Nfun-1          ', 1.0/Nfun-1
cc         write(nchlst,*) 'g**(1.0/Nfun-1)     ', g**(1.0/Nfun-1)
cc         write(nchlst,*) '(Nfun-1.0)/Nfun       ', (Nfun-1.0)/Nfun
cc      write(nchlst,*) '1.0/g**((Nfun-1.0)/Nfun)', 1.0/g**((Nfun-1.0)/Nfun)
cc      endif
        end do
      end if
c
c     Calculation of second order partial derivative 'ddg' & 'ddF'
c
      if (ider.EQ.2) then
        do p = 1, 5
          tmpdg = 0.0
          do i = 1, modes
            tmpdg = tmpdg + lamvals(i)*wt(i)*svecs(i,p)*wn1(i)
          end do
          dg(p) = Nfun*tmpdg
          dF(p) = (1.0/Nfun)*( dg(p) / g**((Nfun-1.0)/Nfun) )
        end do
c
       do p = 1, 5
        do q = 1, 5
         tmpddg = 0.0
         do i = 1, modes
          tmpddg = tmpddg + 
     &       lamvals(i)*wt(i)*svecs(i,p)*svecs(i,q)*wn2(i)
         end do
         ddg(p,q) = Nfun*(Nfun-1.0)*tmpddg
        end do
       end do
c
c 19/04/2009: SKYE+BVB: Oops! 1 was instead of 1.0...
c
       do p = 1, 5
        do q = 1, 5
         termI = ddg(p,q)*( g**((1.0-Nfun)/Nfun) ) 
       termII = dg(p)*((1.0-Nfun)/Nfun)*dg(q)*(g**((1.0-2.0*Nfun)/Nfun))
         ddF(p,q) = (1.0/Nfun)*(termI + termII)
        end do
       end do  
      end if
c ----------------------------------------------------------------------
c     end of subroutine KFacetDERS
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      SUBROUTINE KNORML5D(Dp,Ap,normDp)
c
c     Objective: To normalise the 5D vector with its norm 
c                i.e., to produce so called strain rate mode or stress mode
c
      IMPLICIT NONE
c
      INTEGER i                        ! I/O handler
      REAL*8 Dp(5),Ap(5)               ! Vector in 5D form &its normalised form
      REAL*8 sumsq, normDp             ! Norm of Dp
c
      sumsq = 0.D0
      DO i = 1, 5
        sumsq = sumsq + Dp(i)*Dp(i)
      END DO
c     
      normDp = dsqrt(sumsq)
      IF (normDp /= 0.0) THEN 
        DO i = 1, 5
          Ap(i) = Dp(i) / normDp
        END DO
      ELSE
        PRINT '(A)', 'DATA ERROR: Normal of vector is found to be zero'
        STOP
      END IF
c ----------------------------------------------------------------------
c     end of subroutine KNORML5D
c ----------------------------------------------------------------------
      RETURN
      END
************************************************************************
      subroutine   kdistor(hfac,bea)
c ----------------------------------------------------------------------
c     calculates distortion factors in 4-d angular space;
c ----------------------------------------------------------------------
      real*8 bea(4),c2,c4,c3star,hfac(4),s2,s4
      s4=sin(bea(4))
      c4=cos(bea(4))
      s2=sin(bea(2))
      c2=cos(bea(2))
      c3star=cos(2.d0*bea(3))
      hfac(1)=sqrt(3.d0*s2*s2*c4*c4+(1.d0+3.d0*c2*c2)*s4*s4
     x     +2.d0*sqrt(3.d0)*c3star*s2*s2*c4*s4)
      hfac(2)=sqrt(3.d0*c4*c4+s4*s4-2.d0*sqrt(3.d0)*c3star*c4*s4)
      hfac(3)=sqrt(4.d0*s4*s4)
      hfac(4)=1.d0
c ----------------------------------------------------------------------
c     end of subroutine kdistor;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   knew1(beaco,f,df,ddf,ap,bea,up,nout,iw)
c ----------------------------------------------------------------------
c     17/08/04
c     calculates correction to beta angles with only variation of beta4;
c
c     INPUT:
c     f            current value of potential 
c     df(5)        current first order derivatives of f to ap
c     ddf(5,5)     current second order derivatives of f to apaq
c     ap(5)        current strain rate mode (5d vector)
c     bea(1..4)    current strain rate mode (beta angles)
c     up(1..5)     deviatoric stress direction of interest
c     nout         unit for printing output
c     iw           switch for printing output (0=none)
c
c
c     OUTPUT:
c     beaco(1..4)  corrections to beta angles
c
c
c     LOCAL VARIABLES:
c     b(3,3)       transformation matrix for bea(1..3)
c     apr(3)       diagonal of current principal strain rate mode tensor
c     aij(5)       current strain rate mode tensor (11 22 23 31 12)
c     ap(5)        current strain rate mode (5d vector)
c     dapr(3)       first order derivatives of apr() wrt bea(4)
c     ddapr(3)     second order derivatives of apr() wrt bea(4)bea(4)
c     daij1(5)      first order derivatives of aij() wrt bea(4)
c     ddaij1(5)    second order derivatives of aij() wrt bea(4)bea(4)
c     dap1(5)       first order derivatives of ap()  wrt bea(4)
c     ddap1(5)     second order derivatives of ap()  wrt bea(4)bea(4)
c     dfbe1         first order derivative  of f     wrt bea(4)
c     ddfbe1       second order derivative  of f     wrt bea(4)bea(4)
c     dhbe1         first order derivative  of h     wrt bea(4)
c     ddhbe1       second order derivative  of h     wrt bea(4)bea(4)
c
c ----------------------------------------------------------------------
      integer iw,nout
      real*8 ap(5),bea(4),beaco(4),f,df(5),ddf(5,5),up(5)
      integer i,ij,j,k,p,q,m
      real*8 b(3,3),b1,b2,beta1,beta2,beta3,beta4,
     x     s1s2,c1s2,
     x     s1s3,s1c3,c1s3,c1c3,
     x     s2s3,s2c3,
     x     scs,ccs,scc,ccc,
     x     s1,c1,s2,c2,s3,c3      
      real*8 apr(3),dapr(3),ddapr(3)
      real*8 aij(5),daij1(5),ddaij1(5),sum
      real*8 dap1(5),ddap1(5)
      real*8 dfbe1,ddfbe1
      real*8 ddhbe1,dhbe1,upap,upapi,updap1,sum1
      real*8 rotmax,tol
      real*8 pi,root2,root23,root2i,root32
      parameter (tol=5.0d-3)
      parameter (rotmax=3.d0)
c ----------------------------------------------------------------------
c     calculate some constants;
c ----------------------------------------------------------------------
      pi=dacos(-1.d0)
      root2 =dsqrt(2.d0)
      root2i=0.5d0*root2
      root23=dsqrt(2.d0/3.d0)
      root32=dsqrt(1.5d0)
c ----------------------------------------------------------------------
c     calculate 'b'; 
c     [based on subroutine dbb]
c ----------------------------------------------------------------------
      beta1=bea(1)
      beta2=bea(2)
      beta3=bea(3)
      s1=dsin(beta1)
      c1=dcos(beta1)
      s2=dsin(beta2)
      c2=dcos(beta2)
      s3=dsin(beta3)
      c3=dcos(beta3)
      s1s2=s1*s2
      c1s2=c1*s2
      s1s3=s1*s3
      s1c3=s1*c3
      c1s3=c1*s3
      c1c3=c1*c3
      s2s3=s2*s3
      s2c3=s2*c3
      scs= s1*c2*s3
      ccs= c1*c2*s3
      scc= s1*c2*c3
      ccc= c1*c2*c3
      b(1,1)= c1c3-scs
      b(2,1)=-c1s3-scc
      b(3,1)= s1s2
      b(1,2)= s1c3+ccs
      b(2,2)=-s1s3+ccc
      b(3,2)=-c1s2
      b(1,3)= s2s3
      b(2,3)= s2c3
      b(3,3)= c2
      if (iw.ge.5) then
         write(nout,1999)
         do 100 j=1,3
         do 100 i=1,3
            write(nout,1998) i,j,b(i,j)
 100      continue
      endif
 1999 format(//1x,' b(i,j)       : transformation matrix ',
     x     /21x,'between reference system of tensor a',
     x     /21x,'and     principal directions of a',
     x     /21x,'  a(i,j) = b(k,i)*b(k,j)*apr(k)    i,j,k = 1..3'/)
 1998 format(1x,'   b(',i1,',',i1,')     =',e15.6)
c ----------------------------------------------------------------------
c     calculate 'apr','dapr','ddapr' for a given 'beta4';
c     [based on subroutine daprb]
c ----------------------------------------------------------------------
      beta4=bea(4)
      b1=beta4-pi/3.d0
      b2=beta4+pi/3.d0
      apr(1)=   root23*cos(b1)
      apr(2)=   root23*cos(b2)
      apr(3)=  -root23*cos(beta4)
      dapr(1)= -root23*sin(b1)
      dapr(2)= -root23*sin(b2)
      dapr(3)=  root23*sin(beta4)
      ddapr(1)=-apr(1)
      ddapr(2)=-apr(2)
      ddapr(3)=-apr(3)
      if (iw.ge.5) then
         write(nout,2999)
         do 200 k=1,3
            write(nout,2998) k,apr(k)
 200     continue
         write(nout,2997)
         do 210 k=1,3
            write(nout,2996) k,dapr(k)
 210     continue
         write(nout,2995)
         do 220 k=1,3
            write(nout,2994) k,ddapr(k)
 220     continue
      endif
 2999 format(//1x,' apr(k)       : diagonal of principal ',
     x     'strain rate mode tensor')
 2998 format(1x,'   apr(',i1,')      =',e15.6)
 2997 format(//1x,' dapr(k)      : 1st order partial derivatives',
     x     /21x,'of apr(k)   k=1..3',
     x     /21x,'to beta(4)'/)
 2996 format(1x,'   dapr(',i1,')     =',e15.6)
 2995 format(//1x,' ddapr(k)     : 2nd order partial derivatives',
     x     /21x,'of apr(k)   k=1..3',
     x     /21x,'to beta(4)'/)
 2994 format(1x,'   ddapr(',i1,')    =',e15.6)
c ----------------------------------------------------------------------
c     calculate 'aij','daij4','ddaij4';
c     [based on subroutine daijb]
c ----------------------------------------------------------------------
      do 310 ij=1,5
         if (ij.eq.1) then
            i=1
            j=1
         endif
         if (ij.eq.2) then
            i=2
            j=2
         endif
         if (ij.eq.3) then
            i=2
            j=3
         endif
         if (ij.eq.4) then
            i=1
            j=3
         endif
         if (ij.eq.5) then
            i=1
            j=2
         endif
         sum=0.d0
         do 320 k=1,3
            sum=sum+b(k,i)*b(k,j)*apr(k)
 320     continue
         aij(ij)=sum
         sum=0.d0
         do 334 k=1,3
            sum=sum+b(k,i)*b(k,j)*dapr(k)
 334     continue
         daij1(ij)=sum
         sum=0.d0
         do 348 k=1,3
            sum=sum+b(k,i)*b(k,j)*ddapr(k)
 348     continue
         ddaij1(ij)=sum
 310  continue
      if (iw.ge.5) then
         write(nout,3999)
         do 350 ij=1,5
            write(nout,3998) ij,aij(ij)
 350     continue
         write(nout,3997)
         do 362 ij=1,5
            write(nout,3996) ij,daij1(ij)
 362     continue
         write(nout,3995)
         write(nout,3994)
         do 372 ij=1,5
            write(nout,3993) ij,ddaij1(ij)
 372     continue
      endif
 3999 format(//1x,' aij(ij)      : elements of strain rate mode',
     x     ' tensor',
     x     /21x,'ij = 1..5',
     x     /21x,'i,j= (1,1),(2,2),(2,3),(1,3),(1,2)'/)
 3998 format(1x,'   aij(',i1,')    =',e15.6)
 3997 format(//1x,' daij1(ij)   : 1st order partial derivatives',
     x     /21x,'of aij(ij)    ij = 1..5',
     x     /21x,'        i,j = (1,1),(2,2),(2,3),(1,3),(1,2)',
     x     /21x,'to beta(4)'/)
 3996 format(1x,'   daij1(',i1,')  =',e15.6)
 3995 format(1x)
 3994 format(//1x,' ddaij1(ij)  : 2nd order partial derivatives',
     x     /21x,'of aij(ij)    ij = 1..5',
     x     /21x,'        i,j = (1,1),(2,2),(2,3),(1,3),(1,2)',
     x     /21x,'to beta(4)beta(4)'/)
 3993 format(1x,'   ddaij1(',i1,') =',e15.6)
 3992 format(1x)
c ----------------------------------------------------------------------
c     calculate 'ap','dap1' and 'ddap1';
c     [based on subroutine dapbx]
c ----------------------------------------------------------------------
ccc      WRITE(nout,*) 'ap passed to new1'
ccc      WRITE(nout,*) (ap(p),p=1,5)
ccc      CALL APBEA(ap,bea,0)
ccc      WRITE(nout,*) 'ap obtained with apbea'
ccc      WRITE(nout,*) (ap(p),p=1,5)
      ap(1)= root2i*(aij(1)-aij(2))
      ap(2)= root32*(aij(1)+aij(2))
      ap(3)= root2 * aij(3)
      ap(4)= root2 * aij(4)
      ap(5)= root2 * aij(5)
ccc      WRITE(nout,*) 'ap calculated in new1'
ccc      WRITE(nout,*) (ap(p),p=1,5)
      dap1(1)= root2i*(daij1(1)-daij1(2))
      dap1(2)= root32*(daij1(1)+daij1(2))
      dap1(3)= root2 * daij1(3)
      dap1(4)= root2 * daij1(4)
      dap1(5)= root2 * daij1(5)
      ddap1(1)= root2i*(ddaij1(1)-ddaij1(2))
      ddap1(2)= root32*(ddaij1(1)+ddaij1(2))
      ddap1(3)= root2 * ddaij1(3)
      ddap1(4)= root2 * ddaij1(4)
      ddap1(5)= root2 * ddaij1(5)
      if (iw.ge.5) then
         do 450 p=1,5
            write(nout,4998) p,ap(p)
 450     continue
         do 462 p=1,5
            write(nout,4996) p,dap1(p)
 462     continue
         do 472 p=1,5
            write(nout,4993) p,ddap1(p)
 472     continue
      endif
 4998 format(1x,'   ap(',i1,')       =',e15.6)
 4996 format(1x,'   dap1(',i1,')     =',e15.6)
 4993 format(1x,'   ddap1(',i1,')    =',e15.6)
c ----------------------------------------------------------------------
c     calculate 'dfbe1' and 'ddfbe1';
c     [based on subroutine dfb]
c ----------------------------------------------------------------------
      sum=0.d0
      do 512 p=1,5
         sum=sum+df(p)*dap1(p)
 512  continue
      dfbe1=sum
      sum=0.d0
      do 524 p=1,5
      do 524 q=p,5
         m=1
         if (p.eq.q) m=0
         sum=sum+ddf(p,q)*(dap1(q)*dap1(p)+m*dap1(p)*dap1(q))
 524  continue
      do 528 p=1,5
         sum=sum+df(p)*ddap1(p)
 528  continue
      ddfbe1=sum
      if (iw.ge.4) then
         write(nout,5999)
         write(nout,5998) dfbe1
         write(nout,5997)
         write(nout,5996) ddfbe1
      endif
 5999 format(//1x,' dfbe1      : 1st order partial derivative',
     x     /21x,'of f',
     x     /21x,'to beta4'/)
 5998 format(1x,'   dfbe1     =',e15.6)
 5997 format(//1x,' ddfbe1     : 2nd order partial derivative',
     x     /21x,'of f',
     x     /21x,'to beta(4)beta(4)'/)
 5996 format(1x,'   ddfbe1    =',e15.6)
c ----------------------------------------------------------------------
c     calculate 'dhbe1' and 'ddhbe1';
c     [based on subroutine ddisb]
c ----------------------------------------------------------------------
      call kdotv(upap,up,ap)
      upapi=1.d0/upap
      sum1=0.d0
      do 622 p=1,5
         sum1=sum1+up(p)*dap1(p)
 622  continue
      dhbe1=(dfbe1-f*sum1*upapi)*upapi
      updap1=sum1
      sum1=0.d0
      do 634 p=1,5
         sum1=sum1+up(p)*ddap1(p)
 634  continue
      ddhbe1=(ddfbe1-
     x     (2*dfbe1*updap1+f*sum1)*upapi+
     x     2*f*updap1**2*upapi**2)*upapi
      if (iw.ge.4) then
         write(nout,6999)
         write(nout,6998) dhbe1
         write(nout,6997)
         write(nout,6996) ddhbe1
      endif
 6999 format(//1x,' dhbe1      : 1st order partial derivative',
     x     /21x,'of h',
     x     /21x,'to beta(4)'/)
 6998 format(1x,'   dhbe1     =',e15.3)
 6997 format(//1x,' ddhbe1     : 2nd order partial derivative',
     x     /21x,'of h',
     x     /21x,'to beta(4)beta(4)'/)
 6996 format(1x,'   ddhbe1    =',e15.6)
c ----------------------------------------------------------------------
c     calculate 'beaco';
c     [based on subroutine new]
c ----------------------------------------------------------------------
      beaco(1)=0.d0
      beaco(2)=0.d0
      beaco(3)=0.d0
      beaco(4)=0.d0
      if (dabs(ddhbe1).lt.tol) then
         if (dabs(dhbe1).gt.tol) beaco(4)=-dsign(rotmax*pi/180.d0,dhbe1)
      else
         beaco(4)=-dhbe1/ddhbe1
      endif
      if (iw.ge.3) then
         write(nout,7999)
         write(nout,7998) beaco(4)*180.d0/pi
      endif
 7999 format(//1x,' beaco(4)    : correction to beta(4)')
 7998 format(1x,'   beaco(4)  =',e15.3)
c ----------------------------------------------------------------------
c     end of subroutine knew1;
c ----------------------------------------------------------------------
      return
      end
************************************************************************
      subroutine   kline(facmin,avg,beag,beaco,rot,rotmax,uiv,ifun,
     x                   iw,nout)
c ----------------------------------------------------------------------
c     line search in direction beaco();
c ----------------------------------------------------------------------
      implicit none
      integer maxvar,nvar
      parameter (maxvar=5,nvar=5)
      integer i,ider,ifun,ilin,iw,iw1,nlin,nout
      real*8 avx(5),avg(5),beaco(4),beag(4),beax(4),ddfx(maxvar,maxvar),
     x    dfx(maxvar),dtr,du,fac,facmin,fx,hx,hxmin,rot,rotmax,rotx,rtd,
     x    uiv(maxvar),uivavx
cc      parameter (nlin=5)
c ----------------------------------------------------------------------
c     constants;
c ----------------------------------------------------------------------
      rtd=180.d0/dacos(-1.d0)
      dtr=1.d0/rtd
c ----------------------------------------------------------------------
c     loop over 2*nlin+1 points;
c ----------------------------------------------------------------------
      if (iw.ge.3) then
         write(nout,6999) 
       write(nout,6998) (uiv(i),i=1,nvar)
       write(nout,6997)
      endif
      nlin=2*rotmax+1
      hxmin=99999.d0
      do 116 ilin=1,2*nlin+1
c (28/02/2007: SKYE+BVB rot=0 problem solved by removing rot-factor
c i.e. corrections between -1 and -1/nlin times proposed beaco(i) 
c      fac=(ilin-1.d0-nlin)/nlin*(rotmax*dtr/rot)
       fac=(ilin-1.d0-nlin)/nlin
c        WRITE(nout,*) 'ilin=, fac= ', ilin, fac
         do 118 i=1,4
            beax(i)=beag(i)+beaco(i)*fac
 118     continue        
         call kapbea(avx,beax,0)
         call kdotv(uivavx,uiv,avx)
c        WRITE(nout,*) 'avx= ', (avx(i),i=1,5)   
c        WRITE(nout,*) 'uivavx= ', uivavx
ccc ----------------------------------------------------------------------
ccc     reverse the sign of this plastic strain rate mode
ccc     if at wrong side of yield locus;
ccc ----------------------------------------------------------------------
cc         if (uivavx.lt.0.d0) then
cc            if (beax(4).lt.pi) then
cc               beax(4)=beax(4)+pi
cc            else
cc               beax(4)=beax(4)-pi
cc            endif
cc            do 40 i=1,nvar
cc               avx(i)=-avx(i)
cc 40         continue
cc            uivavx=-uivavx
cc         endif
c ----------------------------------------------------------------------
c     calculate 'fg'       : value of series expansion for 'avg(1..5)';
c ----------------------------------------------------------------------
         ider=0
         iw1=0
         if (iw.ge.5) iw1=1
c        if (ifun.eq.1) call kf1der(fx,dfx,ddfx,avx,ider,iw1)
c        if (ifun.eq.2) call kf2der(fx,dfx,ddfx,avx,nvar,ider,iw1)
c        if (ifun.eq.3) call kf3der(fx,dfx,ddfx,avx,nvar,ider,nout,iw1)
c        if (ifun.eq.5) call kf4der(fx,dfx,ddfx,avx,ider,nout,iw1)
c        WRITE(nout,*) 'BEFORE FacetPOT: nordFacet: ', nordFacet
         if (ifun.eq.30) call KFacetPOT(fx,dfx,ddfx,avx,ider,nout,iw1)
         hx=fx/uivavx
c        WRITE(nout,*) 'hx= ', hx
       if (hx.lt.hxmin) then
          hxmin=hx
          facmin=fac
       endif
       if (iw.ge.3) then
            call kdotv(du,avx,avg)
          if (du.gt.1.d0) du=1.d0
            if (du.lt.-1.d0) du=-1.d0
            rotx=dacos(du)
          write(nout,6996) ilin,hx,fx,rotx*rtd,uivavx,(avx(i),i=1,5)
       endif
 116  continue    
 6999 format('***** subroutine line *****')
 6998 format('uiv(1..5) =',5e15.5)
 6997 format(1x,'ilin',13x,'hx',13x,'fx',11x,'rotx',9x,'uivavx',
     x   3x,'avx(1..5)')
 6996 format(i5,9e15.5)
c ----------------------------------------------------------------------
c     end of subroutine kline;
c ----------------------------------------------------------------------
      return
      end
C***********************************************************************
      SUBROUTINE KVEC5D2MAT(ap, aij)
C      
C     Objective: To transform a tensor from 5D vector notation to matrix form
C
      IMPLICIT NONE
C
      REAL*8 aij(3,3)                  ! Matrix representation of plastic 
C                                        strain mode or deviatoric stress tensor 
      REAL*8 ap(5)                     ! Vector notation of plastic strain mode
C                                        or deviatoric stress tensor
      REAL*8 rt2i, rt23, rt6i          ! 1/root(2), root(2/3) and 1/root(6) 
C
      rt2i = 1.D0/dsqrt(2.D0)
      rt23 = dsqrt(2.D0/3.D0)
      rt6i = 1.D0/dsqrt(6.D0)    
C  
C     Conversion from 5D vector form to matrix form
C
      aij(1,1)= rt2i*ap(1) + rt6i*ap(2)
      aij(2,2)=-rt2i*ap(1) + rt6i*ap(2)
      aij(3,3)=-rt23*ap(2)
      aij(2,3)= rt2i*ap(3)
      aij(3,1)= rt2i*ap(4)
      aij(1,2)= rt2i*ap(5)
      aij(3,2)= aij(2,3)
      aij(1,3)= aij(3,1)
      aij(2,1)= aij(1,2)
c ----------------------------------------------------------------------
c     end of subroutine KVEC5D2MAT
c ----------------------------------------------------------------------
      RETURN
      END 
C***********************************************************************
