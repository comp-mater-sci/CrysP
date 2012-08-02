      module altayHard
      implicit none
      
      
      integer,parameter :: hard_none = 0, hard_voce = 1, hard_pebp =11
      
      !> Configuration parameters of Voce hardening law. Some 'reasonable' defaults are used.
      double precision                          :: TIII1  = 1.486     
      double precision                          :: TIIIS  = 2.476     
      double precision                          :: TIVS   = 8.357 
      double precision                          :: THIII1 = 2.75      
      double precision                          :: THT    = 0.55

      !> Pre-calculated parameters of Voce hardening law.
      double precision,private  :: GAMMAT = 0.0, THIII = 0.0, ETA = 0.0,
     x                             TAUT =  0.0, THIV =  0.0,TIV0 = 0.0
                  
      contains
      
      
      subroutine readHardParams(inunit,KOST,info)
#ifdef ALTAY_SUBROUTINE
      use altayConfig
#endif
      use KOST1x
      implicit double precision (a-h,o-z),integer(i-n)
      integer,intent(in)      :: inunit
      integer,intent(in)      :: KOST
      integer,intent(out)     :: info
      !
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1
      !
      info = -1
      !
      
      !!! FIXME !!! <- simple workaround 
      NLIST = 0  ! this variable must not be set here!
      !!! FIXME !!!
      
      
      select case(KOST)
      !
      case(0,1)
            ! Just for non-hardening and isotropic, Voce-type hardening
#ifdef ALTAY_SUBROUTINE
      TIII1 = acnf%hardening%TIII1
      TIIIS = acnf%hardening%TIIIS
      TIVS  = acnf%hardening%TIVS
      THIII1= acnf%hardening%THIII1
      THT   = acnf%hardening%THT
#else
C     Read the parameters of the work hardening model:
      read (inunit,99,iostat=info) TIII1,TIIIS,TIVS
      if (info /= 0) return
      read (inunit,99,iostat=info) THIII1,THT
      if (info /= 0) return
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
    3 continue
      info = 0
      call precalculateVoceParams()
      if(NLIST.eq.1) then
      write (IMP,102) GAMMAT,TAUT,THIV,TIV0
	end if
 102  format (' GAMMA-T, TAU-T, THETA-IV-0, TAU-IV-0',/,4d15.5)
      !
      case(11)
            info = InitModuleKOST1x(inunit,KOST,LEC)
      !
      case default
            ! Unsupported hardening model is requested
            info = -1
      !      
      end select
      !
      end subroutine
      
      subroutine precalculateVoceParams()
      implicit none
      ! All the variables are inherited by host association.
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
      end subroutine
      
      
      double precision function FTAU(GAMMA,KOST)
      implicit none
      double precision,intent(in)   :: GAMMA
      integer,intent(in)            :: KOST
      !
      select case(KOST)
      case(hard_none,hard_PEBP)
            FTAU = 1.0
      case(hard_Voce)
C     Implementation of the VOCE-model
            if (GAMMA.le.GAMMAT) then
              FTAU=TIIIS-(TIIIS-TIII1)*EXP(-THIII*GAMMA/TIIIS)
            else
              FTAU=TIVS-(TIVS-TIV0)*EXP(-THIV*GAMMA/TIVS)
            endif
      case default
            FTAU = 1.0
      end select
      end function
      
      end module
      
      
      BLOCK DATA
      implicit double precision (a-h,o-z)
      COMMON /IGLIJS/ FK1(2,96),M11,CC(2,96)
      COMMON/TLR1/ N,M,N1,NGL,NTW,NC,LC,B1(3,96),B(5,5),
     1B2(6,96),G(96),DI1(5)
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1
      COMMON /ES1/ IMP3
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
      END
