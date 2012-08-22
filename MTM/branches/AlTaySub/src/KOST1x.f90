!
! $Id$
!
      MODULE KOST1x
!     v1.0 by P. Eyckens, MTM, KU Leuven, 17 July 2012.
!     v1.1 by P. Eyckens, MTM, and J. Gawad, CS, KULeuven, 2 August 2012.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     KOST= 11
!     --------
!     The Bart Peeters hardening model as described in:
!     PhD B. Peeters, MTM, 2002, paragraph 3.2.2: 'Mesoscopic model'
!     This implementation differs only in a few details:

!     (1) The sqrt() [square root] in Eq. (3.15), is replaced in this implementation
!      with tanh() [tangent hyperbolic]. This replacement was also found
!      in the original source code by B. Peeters.

!     (2) Eq. (3.17) (evolution equation of RHO) is integrated here explicitly,
!      while in the PhD, it is mentioned that a Runge-Kutta method is used.
!      Differences in results (in LST-, CUR-, RES-files) between both methods 
!      are only marginal. Explicit integration requires less operations and is
!      more accurate.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IMPLICIT NONE
      PRIVATE 

      TYPE :: PAR11 
            !PUBLIC components
            double precision :: b,G,alfa,f,tau0
            double precision :: I,R,Iwd,Rwd,Rncg,beta1,beta2
            double precision :: Iwp,Rwp,Rrev,R2
            double precision :: RHOcbSAT,RHOwdSAT,RHOwpSAT
            double precision :: RHOcbMIN,RHOwdMIN,RHOwpMIN
            double precision :: RHOwpLOW
      END TYPE PAR11
      
      TYPE :: CBBtype
            !PUBLIC components
            double precision :: RHOwd = 0.D0
            double precision :: RHOwp = 0.D0
            double precision :: RHOwdHOM = 0.D0 
            double precision :: accGAMMA_new = 0.D0
            double precision :: RHOwd_ini = 0.D0    
      END TYPE CBBtype

      TYPE :: StatVar
      !PUBLIC components
            double precision                    :: RHOcb = 0.D0
            TYPE(CBBtype), DIMENSION(6)         :: CBB 
            integer, DIMENSION(2)               :: ActiveCBB = 0
            double precision, DIMENSION(2,24)   :: CRSS = 0.D0
      END TYPE StatVar

      INTERFACE InitModuleKOST1x !Generic Interface
        MODULE PROCEDURE Init_file,Init_PAR11
       !MODULE PROCEDURE Init_PAR12 !for future variant with different/more parameters
      END INTERFACE
                  
      PUBLIC                                                             &
      !procedures:  
            InitModuleKOST1x,                                            &
            GetInitStatVar,                                              &
            MakeInc,                                                     &
			ReadSVfile,         &
			WriteSVfile,        &
      !derived types:
            PAR11,                                                       &
            StatVar,            &
			CBBtype

      !> \name Exit codes from KOST1x subroutines and functions:
      !>@{
      integer,PARAMETER,PUBLIC :: KS_OK = 0           !< OK
      integer,PARAMETER,PUBLIC :: KS_Error = -1       !< General error (not covered by any specific error code).
      integer,PARAMETER,PUBLIC :: KS_ErrBadDims = -2  !< At least one parameter out of boundaries
      integer,PARAMETER,PUBLIC :: KS_ErrBadValue = -5 !< At least one input parameter has unacceptable value
      integer,PARAMETER,PUBLIC :: KS_ErrIO = -15      !< Error during an IO operation
      !>@}
            
      !Remaining declarations all PRIVATE:
      TYPE(PAR11), SAVE :: P
      logical, SAVE :: InitOK=.FALSE.
      integer, SAVE :: iKOST=0
      integer, PRIVATE :: i !running index
      double precision, SAVE :: alfa_G_b 
      double precision, SAVE, DIMENSION(24,6)::     eff = 0.,         &
                                          effslashb = 0.,   &
                                          alfa_G_b_eff = 0.,&
                                          alfa_G_b_ABSeff = 0.

      double precision, PARAMETER :: MINfrac= 2.0E-3
      double precision, PARAMETER :: LOWfrac=10.0E-3

      !bDirSS(s,1:3): normalized burgers vector on slip system s
      double precision, PARAMETER :: p3= 0.5773502 !1.0/sqrt(3.0)
      double precision, PARAMETER :: n3=-0.5773502 !1.0/sqrt(3.0)
      double precision, SAVE, DIMENSION(24,3)::bDirSS    
      DATA (bDirSS( 1: 3,i),i=1,3) /3*p3,3*p3,3*p3/ !s.s. 1 to 3
      DATA (bDirSS( 4: 6,i),i=1,3) /3*n3,3*n3,3*p3/ !s.s. 4 to 6
      DATA (bDirSS( 7: 9,i),i=1,3) /3*n3,3*p3,3*p3/ !..
      DATA (bDirSS(10:12,i),i=1,3) /3*p3,3*n3,3*p3/ !..
      DATA (bDirSS(13:15,i),i=1,3) /3*p3,3*p3,3*p3/ !..
      DATA (bDirSS(16:18,i),i=1,3) /3*n3,3*n3,3*p3/ !..
      DATA (bDirSS(19:21,i),i=1,3) /3*n3,3*p3,3*p3/ !..
      DATA (bDirSS(22:24,i),i=1,3) /3*p3,3*n3,3*p3/ !s.s. 21 to 24

      !CBBnormal(i,1:3): normalized vector normal to CBB i
      double precision, PARAMETER :: p2= 0.7071068 !1.0/sqrt(2.0)
      double precision, PARAMETER :: n2=-0.7071068 !1.0/sqrt(2.0)
      double precision, SAVE, DIMENSION(6,3)::CBBnormal  
      DATA (CBBnormal(1,i),i=1,3) /0.,p2,n2/ !CBBs on (01-1)-plane
      DATA (CBBnormal(2,i),i=1,3) /n2,0.,p2/ !CBBs on (-101)-plane
      DATA (CBBnormal(3,i),i=1,3) /p2,n2,0./ !CBBs on (1-10)-plane
      DATA (CBBnormal(4,i),i=1,3) /0.,n2,n2/ !CBBs on (0-1-1)-plane
      DATA (CBBnormal(5,i),i=1,3) /p2,0.,p2/ !CBBs on (101)-plane
      DATA (CBBnormal(6,i),i=1,3) /n2,n2,0./ !CBBs on (-1-10)-plane



      CONTAINS

      !CONTAINed by MODULE KOST1x:
      integer FUNCTION Init_PAR11(P11try,KOSTtry,LEC) result(iError)
      !Before EXITing, this function REWINDs the PRE-file but does not CLOSE it.
      !This procedure returns an error code (iError):  
      !      0 : no error
      !     -5 : incorrect value of KOSTtry for the inputted parameter-type
      !     -2 : incorrect PRE-file
	  !     -3 : (at least one) parameter out of boundaries
      TYPE(PAR11),INTENT(IN) :: P11try 
      integer    ,INTENT(IN) :: KOSTtry !proposed value of KOST
      integer    ,INTENT(IN) :: LEC !unit number of PRE-file

      character(LEN=64) :: line1
      integer           :: s,i
      
      InitOK=.FALSE.
            
      !Check KOSTtry
      if (KOSTtry /= 11) then
          iError=-5
          return 
      end if
          iKOST=KOSTtry !=11; iKOST: PRIVATE to this module.
/*
      !Check PRE-file
      rewind (unit=LEC)
      read (LEC,FMT='(A)') line1
      rewind (unit=LEC)
      if ( (index(line1,'BCC') == 0) .or. (index(line1,'{BP}') == 0)) then
        iError=-2
        return 
      end if
*/
      !Check and save the parameters in P (private to this module)
      if(P11try%b    >  0.    .AND. P11try%b    <= 1.e-8    .AND.& ! [m]
         P11try%G    >= 10.e3 .AND. P11try%G    <= 500.e3   .AND.& ! [MPa]
	     P11try%alfa >  0.    .AND. P11try%alfa <= 5.       .AND.& ! [/]
         P11try%f    >= 0.    .AND. P11try%f    <= 1.       .AND.& ! [/]
         P11try%tau0 >= 0.    .AND. P11try%tau0 <= 1.e4     .AND.& ! [MPa]
         P11try%I    >= 0.    .AND. P11try%I    <= 10.      .AND.& ! [/]
         P11try%Iwd  >= 0.    .AND. P11try%Iwd  <= 10.      .AND.& ! [/]
         P11try%Iwp  >= 0.    .AND. P11try%Iwp  <= 10.      .AND.& ! [/]
         P11try%R    >  0.    .AND. P11try%R    <= 1.e-6    .AND.& ! [m]
         P11try%Rwd  >  0.    .AND. P11try%Rwd  <= 1.e-6    .AND.& ! [m]
         P11try%Rncg >  0.    .AND. P11try%Rncg <= 1.e-6    .AND.& ! [m]
         P11try%Rwp  >  0.    .AND. P11try%Rwp  <= 1.e-6    .AND.& ! [m]
         P11try%Rrev >  0.    .AND. P11try%Rrev <= 1.e-6    .AND.& ! [m]
         P11try%R2   >  0.    .AND. P11try%R2   <= 1.e-6    .AND.& ! [m]
         P11try%beta1>= 0.    .AND. P11try%beta1<= 100.     .AND.& ! [/]
         P11try%beta2>= 0.    .AND. P11try%beta2<= 100.          & ! [/]
	      )then
		    P=P11try
		  else
            iError=-3
            return 
	  end if

      !Calculate dependent hardening parameters
      P%RHOcbSAT=P%I  * P%I  /( P%R  * P%R  )
      P%RHOwdSAT=P%Iwd* P%Iwd/( P%Rwd* P%Rwd)
      P%RHOwpSAT=(sqrt((P%Iwp/P%Rwp)**4 +                                &
                 4.*(P%Iwp*P%Iwd/(P%Rwp*P%Rwd))**2) +                    &
                 (P%Iwp/P%Rwp)**2)/2.  

      P%RHOcbMIN=  MINfrac * P%RHOcbSAT
      P%RHOwdMIN=  MINfrac * P%RHOwdSAT  
      P%RHOwpMIN=  MINfrac * P%RHOwpSAT   

      P%RHOwpLOW=  LOWfrac * P%RHOwpSAT

      !Calculate "Wall-effectivity"-matrices
      do s=1,24 
        do i=1,6
          eff(s,i)=DOT_PRODUCT( bDirSS(s,:) , CBBnormal(i,:) )  
        end do
      end do
      effslashb       = eff / P%b  
      alfa_G_b= P%alfa* P%G * P%b 
      alfa_G_b_eff    = alfa_G_b * eff       
      alfa_G_b_ABSeff = ABS(alfa_G_b_eff)  

      !If control passes here, initialization is done without errors
      InitOK=.TRUE. !PRIVATE to this module
      iError=0      !OUT

      END FUNCTION Init_PAR11



      !CONTAINed by MODULE KOST1x:
      integer FUNCTION Init_file(inunit,KOST,LEC) result(info)
      implicit none
      integer,intent(in)      :: inunit
      integer,intent(in)      :: KOST     !< Id of the model version.
      integer,intent(in)      :: LEC      
      !
      TYPE(PAR11) :: PARtry
      !
      info = KS_Error
      select case(KOST)
      case(11)
            ! Read parameters of PE-BP hardening model
            if (ReadPar11(inunit,PARtry) == 0) then
                  info = Init_PAR11(PARtry,KOST,LEC)
            endif
      case default
            info = KS_ErrBadValue !Unsupported value of KOST
      end select
      !
      end FUNCTION Init_file


      
      !CONTAINed by MODULE KOST1x:
      integer FUNCTION ReadPar11(inunit,Pf)
      implicit none
      integer,intent(in)      :: inunit   !< IO unit number
      TYPE(PAR11),INTENT(OUT) :: Pf       !< Parameters to be read from a formatted file.
      !
      read(inunit,fmt=100,err=666,end=666) Pf%b
      read(inunit,fmt=100,err=666,end=666) Pf%G
      read(inunit,fmt=100,err=666,end=666) Pf%alfa
      read(inunit,fmt=100,err=666,end=666) Pf%f
      read(inunit,fmt=100,err=666,end=666) Pf%tau0
      read(inunit,fmt=100,err=666,end=666) Pf%I
      read(inunit,fmt=100,err=666,end=666) Pf%R
      read(inunit,fmt=100,err=666,end=666) Pf%Iwd
      read(inunit,fmt=100,err=666,end=666) Pf%Rwd
      read(inunit,fmt=100,err=666,end=666) Pf%Rncg
      read(inunit,fmt=100,err=666,end=666) Pf%beta1
      read(inunit,fmt=100,err=666,end=666) Pf%beta2
      read(inunit,fmt=100,err=666,end=666) Pf%Iwp
      read(inunit,fmt=100,err=666,end=666) Pf%Rwp
      read(inunit,fmt=100,err=666,end=666) Pf%Rrev
      read(inunit,fmt=100,err=666,end=666) Pf%R2
100   format(F12.5)
      ! Do extra validation tests here to check the contents of the structure P
      ! ...
      ReadPar11 = 0
      return
      !
666   ReadPar11 = -4 !Error in reading from file    
      !
      end FUNCTION ReadPar11
      
      

      !CONTAINed by MODULE KOST1x:
      SUBROUTINE GetInitStatVar(SV0,iError)
      !This procedure returns:
      ! state variables for an annealed & undeformed substructure (SV0)
      ! an error code (iError):  
      !      0 , no error
      !     -10, in case this module is not correctly initialized

      TYPE(StatVar),INTENT(OUT) :: SV0
      integer,      INTENT(OUT) :: iError

      if(.NOT.InitOK) then
            iError=-10
            return
      end if
      iError=0

      SV0%RHOcb               = P%RHOcbMIN
      SV0%CBB(:)%RHOwd        = P%RHOwdMIN
      SV0%CBB(:)%RHOwp        = P%RHOwpMIN
      SV0%CBB(:)%RHOwdHOM     = P%RHOwdMIN
      SV0%CBB(:)%accGAMMA_new = 0.
      SV0%CBB(:)%RHOwd_ini    = P%RHOwdMIN
      SV0%ActiveCBB(:)        = 0
      SV0%CRSS(:,:)           = P%tau0

      END SUBROUTINE GetInitStatVar



      !CONTAINed by MODULE KOST1x:
      SUBROUTINE MakeInc(SVa,sliprate,deltaT,SVb,iError)
      !This procedure requires as input:
      ! state variable at beginning of increment (SVa)
      ! the slip rates, assumed constant throughout the increment (sliprate)
      ! the time increment (deltaT)
      !This procedure returns:
      ! state variables at end of the increment (SVb)
      ! an error code (iError):  
      !      0 , no error
      !     -10, in case this module is not correctly initialized
      TYPE(StatVar),INTENT(IN)       :: SVa
      double precision,INTENT(IN), DIMENSION(24) :: sliprate
      double precision,INTENT(IN)                      :: deltaT
      TYPE(StatVar),INTENT(OUT)      :: SVb !OUT
      integer,INTENT(OUT)            :: iError

      !local variable declarations
      double precision :: SUMabsGamDot=0.,GAMMAdot_new=0.,RHObausch=0.  
      double precision :: SUMabsGam   =0.,GAMMA_new   =0.
      double precision,    DIMENSION(6) :: GAMMAdot=0.,GAMMA=0. 
      integer, DIMENSION(6) :: r 
      integer :: j 
      double precision :: fl,wd

      if(.NOT.InitOK) then
            SVb=SVa
            iError=-10
            return
      end if
      iError=0

      !! Calc. quantities of slip rates and slips
      !! Identify currently generated and non-currently generated walls
      !!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ! 'Gam'   ~ small-caps gamma: for a slip system
      ! 'GAMMA' ~ large-caps GAMMA: for a wall

      
      SUMabsGamDot=sum(abs(sliprate))
      SUMabsGam=SUMabsGamDot*deltaT
      !
      if (SUMabsGam < epsilon(0.D0)) then
            ! No slip rate in the current grain => no deformation, no update of the state
            SVb=SVa
            ! Issue error only on negative time increment.
            if (deltaT < 0.D0) iError = -5
            return
      endif

      GAMMAdot=F_GAMMAdot(sliprate) 
      GAMMA=GAMMAdot*deltaT

      r= sort110planes(GAMMAdot) !sort the walls in r
      SVb%ActiveCBB(1)=r(1)
      SVb%ActiveCBB(2)=r(2)

      GAMMAdot_new=GAMMAdot(r(1))+GAMMAdot(r(2))
      GAMMA_new=GAMMAdot_new*deltaT

      !! Update dislocation densities
      !!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      RHObausch=0. !init.
      do j=1,2 !Loop over 2 currently generated walls
        !RHOwd
        SVb%CBB(r(j))%RHOwd= F_KocksMeck(SVa%CBB(r(j))%RHOwd             &
                                         ,GAMMA(r(j)),P%Iwd,P%Rwd)  
        SVb%CBB(r(j))%RHOwdHOM= SVb%CBB(r(j))%RHOwd
        !RHOwp
        call UPD_cur_wp(r(j),SVa%CBB(r(j))%RHOwp,                        & !in
                             SVb%CBB(r(j))%RHOwp,RHObausch) !out
      end do
      
      do j=3,6 !Loop over 4 non-currently generated walls
        !RHOwd
        call UPD_ncg_wd(r(j),SVa,                                        & !in
                             SVb ) !out
        !RHOwp
        call UPD_ncg_wp(SVa%CBB(r(j))%RHOwp,                             & !in
                        SVb%CBB(r(j))%RHOwp ) !out
      end do

      !RHOcb
      call UPD_cb(RHObausch,SUMabsGam,SVa%RHOcb,                         & !in
                                      SVb%RHOcb ) !out

      !! Calculate Critical Resolved Shear Stresses
      !!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SVb%CRSS= F_CRSS(SVb)



      CONTAINS

      !CONTAINed by SUBROUTINE MakeInc:
      FUNCTION F_GAMMAdot(sr) 
      ! Calculate the total slip rates on each of the six (110)-planes
      double precision, DIMENSION(24), INTENT(IN)  :: sr !Slip Rate
      double precision, DIMENSION( 6)              :: F_GAMMAdot !OUT
      
      F_GAMMAdot(1)= abs(sr( 1))+abs(sr( 7))!(01-1)-plane
      F_GAMMAdot(2)= abs(sr( 2))+abs(sr(11))!(-101)-plane
      F_GAMMAdot(3)= abs(sr( 3))+abs(sr( 6))!(1-10)-plane
      F_GAMMAdot(4)= abs(sr( 4))+abs(sr(10))!(0-1-1)-plane
      F_GAMMAdot(5)= abs(sr( 5))+abs(sr( 8))!(101)-plane
      F_GAMMAdot(6)= abs(sr( 9))+abs(sr(12))!(-1-10)-plane

      END FUNCTION F_GAMMAdot



      !CONTAINed by SUBROUTINE MakeInc:
      FUNCTION sort110planes(PlaneSlip)
      !For the six total PlaneSlips on the (110)-planes:
      !The plane of the largest PlaneSlip is identified by sort110planes(1).
      !The plane of 2nd-largest PlaneSlip is identified by sort110planes(2).
      !The remaining 4 planes are identified by            sort110planes(3:6). 
      ! (note: the 4 remaining planes are not ordered from higher to lower total PlaneSlip!)
      double precision,    DIMENSION(6), INTENT(IN)  :: PlaneSlip
      integer, DIMENSION(6)              :: sort110planes !OUT

      !declaration of local variables
      integer r(6), i

      if (PlaneSlip(1).GE.PlaneSlip(2)) then 
        r(1)=1
        r(2)=2
      else
        r(1)=2
        r(2)=1
      end if

      do i=3,6
        if      (PlaneSlip(i).GT.PlaneSlip(r(1))) then
          r(i)=r(2)
          r(2)=r(1)
          r(1)=i
        else if (PlaneSlip(i).GT.PlaneSlip(r(2))) then
          r(i)=r(2)
          r(2)=i
        else
          r(i)=i
        end if
      end do

      sort110planes = r

      END FUNCTION sort110planes    



      !CONTAINed by SUBROUTINE MakeInc:
      double precision FUNCTION F_KocksMeck(RHO_a,delta_g,II,RR) !PE27062012-2
      !Returns RHO_b, the value of RHO at the end of an interval (a,b) 
      ! for the following differential equation:
      !
      ! d(RHO)    1
      ! ------ = --- * ( II*sqrt(RHO) - RR*RHO )
      !  d(g)    P%b
      !
      ! The value of 'P%b', the size of burgers vector, is inherited.
      !
      ! To calc. RHO_b, following inputs are required: 
      !   -> RHO_a, the value of RHO at the start of the interval (a,b)
      !   -> delta_g = g_b - g_a, the increment in g during the interval (a,b)
      double precision ,INTENT(IN):: RHO_a,delta_g,II,RR

!     local variable declarations
      double precision x

      x=exp(-0.5*RR*delta_g/P%b)
      x=II/RR*(1.-x)+sqrt(RHO_a)*x
      F_KocksMeck=x*x

      END FUNCTION F_KocksMeck



      !CONTAINed by SUBROUTINE MakeInc:
      SUBROUTINE UPD_cur_wp(rdr,RHOwp_a,RHOwp_b,RHObausch)
      integer, INTENT(IN):: rdr
      double precision ,INTENT(IN)   :: RHOwp_a 
      double precision ,INTENT(OUT)  :: RHOwp_b 
      double precision, INTENT(INOUT):: RHObausch

      !inherited variables:
      !P%Iwd, P%Rwd, P%b 
      !glidedir, wall
      !effslashb, sliprate       
      !P%RHOwpSAT, P%RHOwpLOW 
      !fl, wd

      !local variable declarations:
      double precision :: wpFLUX !wp-flux on the wall 'rdr' 

      logical :: FLUXreversal,wpLOW

      wpFLUX=DOT_PRODUCT( effslashb(:,rdr) , sliprate(:) )  

      FLUXreversal= wpFLUX*RHOwp_a .LT. 0.0
      wpLOW= abs(RHOwp_a) .LE. P%RHOwpLOW

      if ( FLUXreversal .and. .NOT.(wpLOW) ) then
        ! |RHOwp| gets smaller, following analytic time integration
        RHOwp_b=RHOwp_a*exp(-P%Rrev*abs(wpFLUX)*deltaT) !wpFLUX is a rate!
        RHObausch=RHObausch+abs(RHOwp_a)    
      else
        ! |RHOwp| gets larger, following numeric time integration (4th order Runge-Kutta)
        fl=wpFLUX                       !to be used by RungeKutta->dwp_dt
        wd=SVb%CBB(rdr)%RHOwdHOM  !to be used by RungeKutta->dwp_dt
        if ( FLUXreversal ) then
          !In this case, it must also be that: wpLOW=.TRUE.
          !AFTER change of its sign, RHOwp will build up again.
          RHOwp_b = RungeKutta(-RHOwp_a)    
        else
          RHOwp_b = RungeKutta( RHOwp_a) 
        end if
        !RHObausch=RHObausch : No contribution to RHObausch
      end if

      END SUBROUTINE UPD_cur_wp                                          



      !CONTAINed by SUBROUTINE MakeInc:
      FUNCTION RungeKutta(wpini)
      !This function returns the 4th order Runge-Kutta approximation
      !of the differential equation given by 
      !
      !                             d(wp)/dt = F(wp)
      ! 
      ! The dif. eq. is implemented as another function: function dwp_dt(wp).
      !
      ! This function returns:
      ! RungeKutta = wpini + (K1+2*K2+2*K3+K4)/6
      !    in which     K1= deltaT * F(wpini     )
      !                       K2= deltaT * F(wpini+K1/2)
      !                       K3= deltaT * F(wpini+K2/2)
      !                       K4= deltaT * F(wpini+K3  )
      !    with wpini : the value of wp at the start of the increment
      !         deltaT: the time of the increment
      double precision, INTENT(IN) :: wpini
      double precision                RungeKutta   !OUT

      !local variable declarations:
      double precision, DIMENSION(4) :: K

      K(1)=deltaT*dwp_dt(wpini        )
      K(2)=deltaT*dwp_dt(wpini+K(1)/2.)
      K(3)=deltaT*dwp_dt(wpini+K(2)/2.)
      K(4)=deltaT*dwp_dt(wpini+K(3)   )
      RungeKutta=wpini+(K(1)+2.*K(2)+2.*K(3)+K(4))/6.

      END FUNCTION RungeKutta



      !CONTAINed by SUBROUTINE MakeInc:
      FUNCTION dwp_dt(wp)
      double precision, INTENT(IN) :: wp
      double precision             :: dwp_dt !OUT

      !inherited variables:
      !fl, wd
      !P%Iwp, P%Rwp

      dwp_dt=(P%Iwp*sqrt(wd+abs(wp))*fl/abs(fl) - P%Rwp*wp) * abs(fl)

      END FUNCTION dwp_dt



      !CONTAINed by SUBROUTINE MakeInc:
      SUBROUTINE UPD_ncg_wp(RHOwp_a,RHOwp_b) 
      double precision, INTENT(IN)  :: RHOwp_a 
      double precision, INTENT(OUT) :: RHOwp_b 

      !inherited variables:
      !P%Rncg, GAMMAdot_new, P%b, P%RHOwpMIN 

      if (abs(RHOwp_a) .GT. P%RHOwpMIN) then
        RHOwp_b= RHOwp_a*exp(-P%Rncg*GAMMA_new/P%b)
      else
        if (RHOwp_a .GE. 0.0) then 
          RHOwp_b=  P%RHOwpMIN
        else
          RHOwp_b= -P%RHOwpMIN
        end if
      end if

      END SUBROUTINE UPD_ncg_wp



      !CONTAINed by SUBROUTINE MakeInc:
      SUBROUTINE UPD_ncg_wd(rdr,SV_a,SV_b)
      integer,INTENT(IN )       :: rdr
      TYPE(StatVar), INTENT(IN) :: SV_a
      TYPE(StatVar), INTENT(INOUT):: SV_b

      !inherited variables:
      !P%b, P%Rncg, P%beta1, P%beta2, P%RHOwdMIN  
      !SVa%ActiveCBB 
      !GAMMAdot_new

!     local variable declarations
      double precision RHOwdLOC 
      double precision RHOwdHOM,accGAMMA_new,RHOwd_ini 
      double precision RHOwd 

      RHOwdHOM     = SV_a%CBB(rdr)%RHOwdHOM 
      accGAMMA_new = SV_a%CBB(rdr)%accGAMMA_new
      RHOwd_ini    = SV_a%CBB(rdr)%RHOwd_ini

      if (RHOwdHOM.GT.P%RHOwdMIN) then
       !if the wall was NOT active in prev. inc.
        if (rdr .NE. SVa%ActiveCBB(1) .AND.                              &
            rdr .NE. SVa%ActiveCBB(2)      ) then 
         !accGAMMA_new=[accGAMMA_new]_inc(i-1) + [GAMMA_new]_inc(i) 
            accGAMMA_new=accGAMMA_new+GAMMA_new
        else !the wall was active in prev. inc.
          accGAMMA_new=GAMMA_new
          RHOwd_ini=RHOwdHOM
        end if

        RHOwdLOC=-tanh( P%beta1*accGAMMA_new)*                           &
                   exp(-P%beta1*accGAMMA_new)*RHOwd_ini*P%beta2
        RHOwdHOM=RHOwdHOM*exp(-P%Rncg*GAMMA_new/P%b)
        RHOwd=RHOwdHOM+RHOwdLOC
        if (RHOwd .LT. P%RHOwdMIN)  RHOwd=P%RHOwdMIN
      else
        RHOwdHOM=P%RHOwdMIN
        RHOwd   =P%RHOwdMIN
      end if

      SV_b%CBB(rdr)%RHOwd           = RHOwd 
      SV_b%CBB(rdr)%RHOwdHOM     = RHOwdHOM 
      SV_b%CBB(rdr)%accGAMMA_new = accGAMMA_new
      SV_b%CBB(rdr)%RHOwd_ini    = RHOwd_ini

      END SUBROUTINE UPD_ncg_wd



      !CONTAINed by SUBROUTINE MakeInc:
      SUBROUTINE UPD_cb(RHObausch,SUMabsGam,RHO_a,RHO_b) 
      double precision, INTENT(IN)  :: RHObausch,SUMabsGam
      double precision, INTENT(IN)  :: RHO_a 
      double precision, INTENT(OUT) :: RHO_b 

      !inherited variables:
      !P%I, P%R, P%R2, P%b, P%RHOwpSAT 

!     local variable declarations
      double precision Reffective

      if(RHObausch .GT. 0.0) then
        Reffective=P%R + P%R2*RHObausch/(2.*P%RHOwpSAT)  
        if (P%I*sqrt(RHO_a) - Reffective*RHO_a .LE. 0.0) then
          RHO_b=RHO_a !Keep as is. 
        else
            RHO_b= F_KocksMeck(RHO_a,SUMabsGam,P%I,Reffective)
        end if
      else !RHObausch .EQ. 0.0
        RHO_b= F_KocksMeck(  RHO_a,SUMabsGam,P%I,P%R       )  
      end if

      END SUBROUTINE UPD_cb



      !CONTAINed by SUBROUTINE MakeInc:
      FUNCTION F_CRSS(SV_b) 
      TYPE(StatVar), INTENT(IN) :: SV_b 
      double precision, DIMENSION(2,24):: F_CRSS !OUT

!     P%tau0,P%f  ->inherited
!     alfa_G_b ->inherited
!     alfa_G_b_eff,alfa_G_b_ABSeff ->inherited

      !local variables declarations
      double precision tau_CB,CRSS_0_CB
      double precision,DIMENSION(2,24)::tau_CBB
      integer j,s,i
      double precision signfac
      double precision,DIMENSION(6)::wpcontr,wdcontr
 
       !CRSS within cells & CBs
      tau_CB=alfa_G_b*sqrt(SV_b%RHOcb) 
      
      !contributions from tau_0 and CBs to CRSS
      CRSS_0_CB=P%tau0 +  P%f*tau_CB

      !Calc. CRSS for each slip system s, for the sense of slip j
      do j=1,2 
      signfac=3.0-2.0*j ! 1 for j=1 ; -1 for j=2
        do s=1,24 
          !wp- and wd-contributions from all CBBs i
          do i=1,6
              wpcontr(i)=sqrt(abs(SV_b%CBB(i)%RHOwp)) *                  &
                       signfac * alfa_G_b_eff(s,i) *                     &
                               SV_b%CBB(i)%RHOwp / abs(SV_b%CBB(i)%RHOwp) 
            if (wpcontr(i) .LT. 0.0) wpcontr(i)=0.0
            wdcontr(i)=sqrt(SV_b%CBB(i)%RHOwd)*alfa_G_b_ABSeff(s,i)
          end do
          !CRSS within CBB = wp- and wd-contributions for all 6 walls
          tau_CBB(j,s)=sum(wpcontr)+sum(wdcontr) 
          !C.R.S.S. for the "two-phase composite"
          F_CRSS(j,s)= CRSS_0_CB + (1.0-P%f)*tau_CBB(j,s) 
        end do
      end do

      END FUNCTION F_CRSS

      END SUBROUTINE MakeInc



      !CONTAINed by MODULE KOST1x:
      integer FUNCTION ReadSVfile(unit,SV) result(iError)
      integer,      INTENT(IN)  :: unit
      TYPE(StatVar),INTENT(OUT) :: SV

      !local variables declarations
      integer :: i,j

      read(unit,fmt=101,err=666,end=666) SV%RHOcb
      do i=1,6 !one line per WALL
        read(unit,fmt=102,err=666,end=666)SV%CBB(i)%RHOwd,        &
                                          SV%CBB(i)%RHOwp,        &
                                          SV%CBB(i)%RHOwdHOM,     &
                                          SV%CBB(i)%accGAMMA_new, &
                                          SV%CBB(i)%RHOwd_ini    
      end do
      read(unit,fmt=103,err=666,end=666) SV%ActiveCBB(1),SV%ActiveCBB(2)
      do i=1,2 !first line for positive sense, 2nd line for negative sense
        read(unit,fmt=104,err=666,end=666)(SV%CRSS(i,j),j=1,24)
      end do
      iError = 0
      return
101   format(   E15.8 )
102   format( 5(E15.8))
103   format( 2(I5   ))
104   format(24(E15.8))
      !
666   iError = -4 !Error in reading from file    
      !
      END FUNCTION ReadSVfile



      !CONTAINed by MODULE KOST1x:
      integer FUNCTION WriteSVfile(unit,SV) result(iError)
      integer,      INTENT(IN)  :: unit
      TYPE(StatVar),INTENT(IN)  :: SV

      !local variables declarations
      integer :: i,j

      write(unit,fmt=101,err=666) SV%RHOcb
      do i=1,6 !one line per WALL
            write(unit,fmt=102,err=666)SV%CBB(i)%RHOwd,        &
                                    SV%CBB(i)%RHOwp,        &
                                    SV%CBB(i)%RHOwdHOM,     &
                                    SV%CBB(i)%accGAMMA_new, &
                                    SV%CBB(i)%RHOwd_ini    
      end do
      write(unit,fmt=103,err=666) SV%ActiveCBB(1),SV%ActiveCBB(2)
      do i=1,2 !first line for positive sense, 2nd line for negative sense
            write(unit,fmt=104,err=666)(SV%CRSS(i,j),j=1,24)
      end do
      iError = 0
      return
	  !
101   format(   E15.8 )
102   format( 5(E15.8))
103   format( 2(I5   ))
104   format(24(E15.8))
      !
666   iError = -4 !Error in reading from file    
      !
      END FUNCTION WriteSVfile

      END MODULE KOST1x
