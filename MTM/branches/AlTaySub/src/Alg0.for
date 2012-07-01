      FUNCTION FTAU(GAMMA)
#ifdef ALTAY_SUBROUTINE
      use altayConfig
#endif
C      Double Precision FTAU 
      implicit double precision (a-h,o-z)
      double precision GAMMA
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1
      SAVE
C     check wether model parameters must be read:
      if (GAMMA.gt.-1000.0) goto 1
#ifdef ALTAY_SUBROUTINE
      TIII1 = acnf%hardening%TIII1
      TIIIS = acnf%hardening%TIIIS
      TIVS  = acnf%hardening%TIVS
      THIII1= acnf%hardening%THIII1
      THT   = acnf%hardening%THT
#else
C     Read the parameters of the work hardening model:
      read (KLEC,99) TIII1,TIIIS,TIVS
      read (KLEC,99) THIII1,THT
  99  format (3f10.0)
      if(NLIST.eq.1) then
      write (IMP,100) TIII1,TIIIS,TIVS,THIII1,THT
	end if
 100  format(' Work hardening model = DOUBLE VOCE-model',/,
     2 ' TAU-III-1=  ',f20.8,/,
     1 ' TAU-III-S = ',F20.8,/,
     4 ' TAU-IV-S =  ',f20.8,/,
     3 ' THETA-III-1=',f20.8,/,
     3 ' THETA-T=    ',f20.8)
      if (TIIIS.gt.TIII1.and.THIII1.gt.THT) goto 3
      if(NLIST.eq.1) then
	write (IMP,101)
	end if
 101  format (' ALG0 - FTAU - reading data - TAU-III-S must be'
     1 ,' larger than TAU-III-1',
     2 /, '     also, THETA-III-1 must be larger than THETA-T')
      stop
#endif
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
	if(NLIST.eq.1) then
      write (IMP,102) GAMMAT,TAUT,THIV,TIV0
	end if
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
      COMMON /IGLIJS/ FK1(2,96),M11,CC(2,96)
      COMMON/TLR1/ N,M,N1,NGL,NTW,NC,LC,B1(3,96),B(5,5),
     1B2(6,96),G(96),DI1(5)
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1
      COMMON /ES1/ IMP3
      COMMON /NRSTEP/ nrstep
C
C     LEC= data set with slip systems
C     KLEC= data set with parameters
C     IMP= printer
C     IMP1=output-file with successive "current situations"
C     IMP2=output-file with successive "responses to imposed strain"
C     IMP3=output-file with twinning information
C     IDISK1= work file
C     NDAT1= input texture file
C
      DATA LEC,KLEC,IDISK1,IMP,NDAT1/4,5,12,3,9/
      data IMP1,IMP2 /7,8/
      data IMP3/11/
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
C      DATA NUNGL /0/
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      DATA N,NC,LC    /5,1,300/
      data nrstep/0/
      END
