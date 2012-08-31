
!> Implementation of Voce hardening law
module hardVoce
implicit none

      !> Configuration parameters of Voce hardening law. 
      !> Some 'reasonable' defaults are used.
      type :: VoceConfig
            double precision  :: TIII1  = 1.486     
            double precision  :: TIIIS  = 2.476     
            double precision  :: TIVS   = 8.357 
            double precision  :: THIII1 = 2.75      
            double precision  :: THT    = 0.55
      end type


      !> Pre-calculated parameters of Voce hardening law.
      type :: VoceParams
            double precision  :: GAMMAT = 0.0
            double precision  :: THIII  = 0.0
            double precision  :: ETA    = 0.0
            double precision  :: TAUT   = 0.0
            double precision  :: THIV   = 0.0
            double precision  :: TIV0   = 0.0
      end type

      ! Two instances of the model parameters:
      type(VoceConfig),save      :: voceCnf
      
      type(VoceParams),save      :: vocePar
      
contains

      subroutine readVoceConfig(inunit,c,info)
      use IOConfig
      implicit none
      integer,intent(in)                  :: inunit
      type(VoceConfig),intent(out)        :: c
      integer,intent(out)                 :: info
      !
            info = -1
            ! Read the parameters of the work hardening model:
            read (inunit,99,iostat=info) c%TIII1,c%TIIIS,c%TIVS
            if (info /= 0) return
            read (inunit,99,iostat=info) c%THIII1,c%THT
            if (info /= 0) return
  99        format (3f10.0)
            if(NLIST.eq.1) write (IMP,100) c%TIII1,c%TIIIS,c%TIVS,c%THIII1,c%THT
 100        format(' Work hardening model = DOUBLE VOCE-model',/, &
                   ' TAU-III-1=  ',f20.8,/, &
                   ' TAU-III-S = ',F20.8,/, &
                   ' TAU-IV-S =  ',f20.8,/, &
                   ' THETA-III-1=',f20.8,/, &
                   ' THETA-T=    ',f20.8)
            info = 0
      !
      end subroutine



      subroutine precalculateVoceParams(c,p,info)
      use IOConfig
      implicit none
      type(VoceConfig),intent(in)         :: c
      type(VoceParams),intent(out)        :: p
      integer,intent(out)                 :: info
      ! Calculation of transition-gamma
      info = -1
      ! Check validity of inputs:
            if (.not.(c%TIIIS.gt.c%TIII1.and.c%THIII1.gt.c%THT)) then
#ifdef ALTAY_SUBROUTINE
                  return
#else
                  if(NLIST.eq.1) write (IMP,101)
       101  format (' ALG0 - FTAU - reading data - TAU-III-S must be larger than TAU-III-1',/, &
                    'also, THETA-III-1 must be larger than THETA-T')
                  stop
#endif
            endif
            p%THIII=c%THIII1/(1.0-c%TIII1/c%TIIIS)
            if ((abs(p%THIII) < epsilon(0.D0)) .or. (abs(c%TIIIS) < epsilon(0.D0))) return      
            p%ETA=c%THT/p%THIII
            p%GAMMAT=-c%TIIIS*LOG(p%ETA*c%TIIIS/(c%TIIIS-c%TIII1))/p%THIII
            ! Calculation of transition TAU
            p%TAUT=c%TIIIS-(c%TIIIS-c%TIII1)*exp(-p%THIII*p%GAMMAT/c%TIIIS)
            ! Calculation of theta-IV-0
            p%THIV=c%THT/(1.0-p%TAUT/c%TIVS)
            ! Calculation of TAU-IV-0
            p%TIV0=c%TIVS+(p%TAUT-c%TIVS)*exp(p%THIV*p%GAMMAT/c%TIVS)
            info = 0
            !      
            if(NLIST.eq.1) write (IMP,102) p%GAMMAT,p%TAUT,p%THIV,p%TIV0
       102  format (' GAMMA-T, TAU-T, THETA-IV-0, TAU-IV-0',/,4d15.5)
      end subroutine

      
      double precision function hardVoceFtau(GAMMA) result(FTAU)
      implicit none
      double precision,intent(in)         :: GAMMA
      ! Implementation of the VOCE-model
      if (GAMMA.le.vocePar%GAMMAT) then
            FTAU=voceCnf%TIIIS-(voceCnf%TIIIS-voceCnf%TIII1)*EXP(-vocePar%THIII*GAMMA/voceCnf%TIIIS)
      else
            FTAU=voceCnf%TIVS-(voceCnf%TIVS-vocePar%TIV0)*EXP(-vocePar%THIV*GAMMA/voceCnf%TIVS)
      endif
      end function
      
end module