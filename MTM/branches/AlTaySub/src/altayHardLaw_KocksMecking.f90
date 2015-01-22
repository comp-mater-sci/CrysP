!
      module altayHardLaw_KocksMecking
!     v1.0 by P. Eyckens, MTM, KU Leuven, 22 Jan 2015.

      implicit none
      private 

      type :: Par 
          !PUBLIC components
          double precision :: b,G,alfa,tau0
          double precision :: I,R
          double precision :: rho_ann
          double precision :: rho_sat
      end type Par
      
      type :: StatVar
      !PUBLIC components
          double precision                    :: rho  = 0.D0
          double precision, DIMENSION(2,12)   :: crss = 0.D0  !max. 12 slip systems supported
      end type StatVar

      type :: StateDerivedVars
          !> Dislocation density in grain; unit: m^(-2)
          double precision :: rho = 0.D0
          !> Saturation fraction of dislocation density, in percentage (%)
          double precision :: SatFracRho = 0.D0
      end type 
      
      interface operator(+)
          module procedure  StateDerivedVar_plus
      end interface
      
      interface operator(*)
          module procedure  StateDerivedVar_times
      end interface
      
      interface InitModuleAltayHardLaw_KocksMecking !Generic Interface
          module procedure InitFromFile,InitFromType
      end interface
                  
      public                    &
      !procedures:  
          InitModuleAltayHardLaw_KocksMecking,   &
          ReadPar,            &
          GetInitStatVar,     &
          MakeInc,            &
          WriteHeadSVfile,    &
          ReadHeadSVfile,     &          
          WriteSVfile,        &
          ReadSVfile,         & 
          GetStateDerivedVar, &
          WriteSDV,           &
      !operators
          operator(+),        &
          operator(*),        &
      !derived types:
          Par,                &
          StatVar,            &
          StateDerivedVars

      !> \name Exit codes from altayHardLaw_KocksMecking subroutines and functions:
      !>@{
      integer,parameter,public :: i_OK = 0           !< OK
      integer,parameter,public :: i_Error = -1       !< General error (not covered by any specific error code).
      integer,parameter,public :: i_ErrBadDims = -2  !< At least one parameter out of boundaries
      integer,parameter,public :: i_ErrBadValue = -5 !< At least one input parameter has unacceptable value
      integer,parameter,public :: i_ErrOutOfRange = -6 !< At least one input parameter has a value outside acceptable range
      integer,parameter,public :: i_ErrIO = -15      !< Error during an IO operation
      integer,parameter,public :: i_ErrNss = -16     !< Unsupported number of slip systems proposed. Supported values are: 12      
      integer,parameter,public :: i_ErrUninitialized = -50 !< Call to module procedures without proper initialization of the module
      !>@}
            
      !Remaining declarations all private:
      type(Par), save  :: P !unit system: MPa; nm(nanometer)
      logical, save    :: InitOK=.FALSE.
      integer, save    :: Nss !Number of slip systems.
      integer, private :: i !running index
      
      double precision, parameter :: TENpow6 = 1.D6      
      
      contains
       
      integer function InitFromFile(inunit,LEC) result(info)
      implicit none
      integer,intent(in)      :: inunit
      integer,intent(in)      :: LEC      
      !
      type(Par) :: ParInp
      !
      info = i_Error
      if (ReadPar(inunit,ParInp) == 0) then
          info = InitFromType(ParInp,LEC)
      endif
      !
      end function InitFromFile
            
      
      integer function InitFromType(P_in,LEC) result(info)
      type(Par),intent(in) :: P_in    !proposed parameter set
      integer  ,intent(in) :: LEC !unit number of PRE-file 
      
      !declaration of local variables:
      character(LEN=128) :: line1
      integer            :: s,i,Idum=0,Nsstry=0
      
      InitOK=.FALSE.
  
      !Check PRE-file: Is number of slip systems (Nss) supported?
      rewind (unit=LEC)
      read (LEC,FMT='(A)') line1
      read (LEC,FMT='(8I4)') Idum, Nsstry, Idum, Idum, Idum, Idum, Idum, Idum
      rewind (unit=LEC)
      select case (Nsstry)
      case (12) !supported number of slip systems
          Nss=Nsstry
      case default !unsupported number of slip systems specified  in LEC
          info = i_ErrNss
          return 
      end select
  
      !Check the input parameters                              ! Units of input parameters:
      if(P_in%b    >  0.    .AND. P_in%b    <= 1.e-8    .AND.& ! [m]
         P_in%G    >= 10.e3 .AND. P_in%G    <= 500.e3   .AND.& ! [MPa]
         P_in%alfa >  0.    .AND. P_in%alfa <= 5.       .AND.& ! [/]
         P_in%tau0 >= 0.    .AND. P_in%tau0 <= 1.e4     .AND.& ! [MPa]
         P_in%I    >= 0.    .AND. P_in%I    <= 10.      .AND.& ! [/]
         P_in%R    >  0.    .AND. P_in%R    <= 1.e-6    .AND.& ! [m]
         P_in%rho_ann >  0. .AND. P_in%rho_ann < P_in%I**2/P_in%R**2 & ! [m^(-2)]  !! i.e. rho_ann < saturation stress
      )then
          !save the parameters in P (private to this module)          
          P=P_in
          !change of units if different (units of P are: MPa; nm(nanometer))
          P%b         = P%b       * TENpow6       ![m] -> [nm]
          P%R         = P%R       * TENpow6       ![m] -> [nm]
          P%rho_ann   = P%rho_ann * TENpow6**(-2) ![m^(-2)] -> [nm^(-2)] 
      else
          info = i_ErrOutOfRange
          return 
      end if

      !Calculate dependent parameters
      P%rho_sat= P%I**2 / P%R**2

      !If control passes here, initialization is done without errors
      InitOK=.TRUE. !private to this module
      info=i_OK     !OUT

      end function InitFromType
   
      
      integer function ReadPar(inunit,p) result (info)
      implicit none
      integer,intent(in)    :: inunit   !< IO unit number
      type(Par),intent(out) :: p       !< parameters to be read from a formatted file.
      !
      read(inunit,fmt=100,err=999,end=999) p%b
      read(inunit,fmt=100,err=999,end=999) p%G
      read(inunit,fmt=100,err=999,end=999) p%alfa
      read(inunit,fmt=100,err=999,end=999) p%tau0
      read(inunit,fmt=100,err=999,end=999) p%I
      read(inunit,fmt=100,err=999,end=999) p%R
      read(inunit,fmt=100,err=999,end=999) p%rho_ann      
100   format(F12.5)
      !
      info = i_OK
      return
      !
999   info = i_ErrIO !Error in reading from file    
      !
      end function ReadPar 

 
      subroutine GetInitStatVar(SV0,info)
      !This procedure returns:
      ! state variables for an annealed & undeformed substructure (SV0)
      ! an error code (info):  
      !      i_OK , no error
      !      i_ErrUninitialized, in case this module is not correctly initialized

      type(StatVar),intent(out) :: SV0
      integer,      intent(out) :: info

      if(.NOT.InitOK) then
          info = i_ErrUninitialized
          return
      end if
      info=i_OK

      SV0%rho  = P%rho_ann
      SV0%crss = F_crss(SV0)

      end subroutine GetInitStatVar

       
      subroutine MakeInc(SVa,sliprate,deltaT,SVb,info)
      !This procedure requires as input:
      ! state variable at beginning of increment (SVa)
      ! the slip rates, assumed constant throughout the increment (sliprate)
      ! the time increment (deltaT)
      !This procedure returns:
      ! state variables at end of the increment (SVb)
      ! an error code (info):  
      !   *  i_OK , no error
	  !   *  i_ErrBadValue, if negative deltaT is provided
      !   *  i_ErrUninitialized, in case this module is not correctly initialized
      type(StatVar),intent(in)                      :: SVa
      double precision,intent(in), DIMENSION(12)    :: sliprate
      double precision,intent(in)                   :: deltaT
      type(StatVar),intent(out)                     :: SVb !OUT
      integer,intent(out)                           :: info

      !local variable declarations
      double precision :: gamma   =0.

      if(.NOT.InitOK) then
          SVb=SVa
          info = i_ErrUninitialized
          return
      end if
      info = i_OK

      !! Calculate accumulated slip during this inc over all slip systems
      gamma= sum(abs(sliprate(1:Nss))) * deltaT
      !
      if (gamma < epsilon(0.D0)) then
          ! No slip rate in the current grain => no deformation, no update of the state
          SVb=SVa
          ! Issue error only on negative time increment.
          if (deltaT < 0.D0) info = i_ErrBadValue
          return
      endif

      !! Update dislocation density
      SVb%rho= F_KocksMeck(SVa%rho,gamma,P%I,P%R) 
      
      !! Calculate Critical Resolved Shear Stresses
      SVb%crss= F_crss(SVb)



      contains


      !CONTAINed by subroutine MakeInc:
      double precision function F_KocksMeck(rho_a,delta_g,II,RR) 
      !Returns rho_b, the value of rho at the end of an interval (a,b) 
      ! for the following differential equation:
      !
      ! d(rho)    1
      ! ------ = --- * ( II*sqrt(rho) - RR*rho )
      !  d(g)    P%b
      !
      ! The value of 'P%b', the size of burgers vector, is inherited.
      !
      ! To calc. rho_b, following inputs are required: 
      !   -> rho_a, the value of rho at the start of the interval (a,b)
      !   -> delta_g = g_b - g_a, the increment in g during the interval (a,b)
      double precision ,intent(in):: rho_a,delta_g,II,RR

!     local variable declarations
      double precision x

      x=exp(-0.5D0*RR*delta_g/P%b)
      x=II/RR*(1.D0-x)+sqrt(rho_a)*x
      F_KocksMeck=x*x

      end function F_KocksMeck

      end subroutine MakeInc
 
       
      function F_crss(SV) 
      type(StatVar), intent(in) :: SV 
      double precision, DIMENSION(2,12):: F_crss !OUT 

!     P%tau0  ->inherited
!     P%alfa, P%G, P%b ->inherited

      !local variables declarations
      double precision :: crss_Tay
      integer :: j, s
      
      !Slip systems not allowed to become active retain initialization value of -1.0
      F_crss=-1.D0
            
      !Taylor equation
      crss_Tay= P%tau0 + P%alfa*P%G*P%b*sqrt(SV%rho)

      !Calc. crss for each slip system s, for each sense of slip j
      do j=1,2 
          do s=1,Nss 
              F_crss(j,s)= crss_Tay  
          end do
      end do

      end function F_crss

       
      !> Perform an IO formatted read operation on StatVar 
      !>
      !> \Param dummy if true, the function performs a fake read operation of by simply skipping the same number of lines
      !> as the ReadSVfile would normally read. The resulting SV becomes initialized to default values.
      integer function ReadSVfile(unit,SV,dummy) result(info)
      integer,      intent(in)  :: unit
      type(StatVar),intent(out) :: SV
      logical,optional,intent(in)   :: dummy

      !local variables declarations
      integer :: i,j
      logical :: is_dummy
      character(len=5)             :: tmpstr
      !
      is_dummy = .false.
      if (present(dummy)) is_dummy = dummy
      if (is_dummy) then
          do i=1,10
              read(unit,fmt=100,err=999,end=999) tmpstr
          enddo
      else            
          read(unit,fmt=101,err=999,end=999) SV%rho
          do i=1,2 !first line for positive sense, 2nd line for negative sense
              read(unit,fmt=104,err=999,end=999)(SV%crss(i,j),j=1,12)
          end do
      endif
      info = i_OK
      return
100   format(A5)
101   format(   E15.8 )
104   format(12(E15.8,1X))
      !
999   info = i_ErrIO !Error in reading from file    
      !
      end function ReadSVfile

        
      integer function WriteSVfile(unit,SV) result(info)
      integer,      intent(in)  :: unit
      type(StatVar),intent(in)  :: SV

      !local variables declarations
      integer :: i,j

      write(unit,fmt=101,err=999) SV%rho
      do i=1,2 !first line for positive sense, 2nd line for negative sense
          write(unit,fmt=104,err=999)(SV%crss(i,j),j=1,12)
      end do
      info = i_OK
      return
      !
101   format(   E15.8 )
104   format(12(E15.8,1X))
      !
999   info = i_ErrIO !Error in reading from file    
      !
      end function WriteSVfile

       
      integer function WriteHeadSVfile(unit) result(info)
      integer,intent(in)  :: unit
      !      
      write(unit,fmt=100,err=999)"#-------------------------------------------------------------------------------------------"
      write(unit,fmt=100,err=999)"# Disl.dens. [micrometer^(-2)]: [1]rho                                                      "
      write(unit,fmt=100,err=999)"# crss+sense [MPa]            : [1]crss(1+) [2]crss(2+) ...  [11]crss(11+) [12]crss(12+)    "
      write(unit,fmt=100,err=999)"# crss-sense [MPa]            : [1]crss(1-) [2]crss(2-) ...  [11]crss(11-) [12]crss(12-)    "      
      write(unit,fmt=100,err=999)"#-------------------------------------------------------------------------------------------"
      info = i_OK
      return
      !
100   format(A92)
101   format(A26,L1)
999   info = i_ErrIO !Error in writing to file
      !
      end function WriteHeadSVfile
    
      
      integer function ReadHeadSVfile(unit) result(info)
      integer,intent(in)  :: unit
      
      !local variables declarations
      integer ::  i
      character :: tmp
      
      do i=1,15
          read(unit,fmt=100,err=999) tmp !read 15 lines
      end do
      
      info = i_OK
      return
      !
100   format(A76)
999   info = i_ErrIO !Error in reading from file
      !
      end function ReadHeadSVfile
       
       
      subroutine GetStateDerivedVar(SV,SDV,info)
      type(StatVar),   intent(in)  :: SV
      !> An object of type StateDerivedVars, which contains state-derived variables calculated from SV
      type(StateDerivedVars), intent(out) :: SDV
      !> Exit code:  
      !> - i_OK , no error
      !> - i_ErrUninitialized, in case this module is not correctly initialized
      integer,         intent(out) :: info

      info= i_Error !init
      if(.NOT.InitOK) then
          info = i_ErrUninitialized
          return
      end if
      
      SDV%rho = SV%rho * TENpow6**2 !unit conversion [nm^(-2)] -> [m^(-2)]
      SDV%SatFracRho = 100. * (SV%rho-P%rho_ann) / (P%rho_sat-P%rho_ann) !unit: %
         
      info=i_OK
      
      end subroutine GetStateDerivedVar
    
      
      !> Calculate component-wise sum of two StateDerivedVars objects 
      elemental function StateDerivedVar_plus(first,second) result(res)
      type(StateDerivedVars),intent(in) :: first,second
      type(StateDerivedVars) :: res
      !
      res%rho = first%rho + second%rho
      res%SatFracRho = first%SatFracRho + second%SatFracRho
      !
      end function StateDerivedVar_plus
    
      
      !> Multiply all components of SDV by the scalar
      elemental function StateDerivedVar_times(SDV,scalar) result(res)
      type(StateDerivedVars),intent(in) :: SDV
      double precision,intent(in)       :: scalar
      type(StateDerivedVars) :: res
      !
      res%rho = scalar * SDV%rho
      res%SatFracRho = scalar * SDV%SatFracRho
      !
      end function StateDerivedVar_times

      
      !> Output state-derived variables (SDV) or/and a header line.
      integer function writeSDV(unit,SDV,header) result(info)
      integer,intent(in)                :: unit
      logical,intent(in),optional       :: header
      type(StateDerivedVars),intent(in),optional :: SDV
      !
      integer :: ierr
      !
      info = i_ErrIO
      if (present(header)) then
          if (header) write(unit,fmt=100,iostat=ierr)
          if (ierr /= 0) return
      endif
      if (present(SDV)) then
          write(unit,fmt=101,iostat=ierr) SDV
          if (ierr /= 0) return
      endif  
      info = i_OK
      ! 
      100 format(T3,'rho[m^(-2)]',T19,'SatFracRho[%]')
      101 format(E15.7,1X,F15.2)
      !
      end function
    
      end module altayHardLaw_KocksMecking
