!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     KOST=11 & PRE-file contains 24 (110)+(112)[111] slip systems;
!     -----------------------------------------------------
!     The original 'Bart Peeters hardening model', as described in:
!     PhD B. Peeters, MTM, 2002, paragraph 3.2.2: 'Mesoscopic model'
!     This implementation differs only in a few details:

!     (1) The sqrt() [square root] in Eq. (3.15), is replaced in this implementation
!      with tanh() [tangent hyperbolic]. This replacement was also found
!      in the original source code by B. Peeters.

!     (2) Eq. (3.17) (evolution equation of RHO) is integrated here analyticaly,
!      while in the PhD, it is mentioned that a Runge-Kutta method is used.
!      Differences in results (in LST-, CUR-, RES-files) between both methods
!      are only marginal. Explicit integration requires less operations and is
!      more accurate.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module altayHardLaw_DSH
    use altay_definitions
    use altay_io
    use altay_log
    
    implicit none

    !> BP model parameters including saturation and minimum values for state dependent dislocation densities
    type :: PAR
        real(dp) :: b, G, alfa, f, tau0
        real(dp) :: I, R, Iwd, Rwd, Rncg, beta1, beta2
        real(dp) :: Iwp, Rwp, Rrev, R2
        real(dp) :: RHOcbSAT, RHOwdSAT, RHOwpSAT
        real(dp) :: RHOcbMIN, RHOwdMIN, RHOwpMIN
        real(dp) :: RHOwpLOW
    end type
        
    real(dp), parameter :: MINfrac= 2.0D-3
    real(dp), parameter :: LOWfrac=10.0D-3
    real(dp), parameter :: TENpow6 = 1.D6
    real(dp), parameter :: p2= 1.D0/sqrt(2.D0)
    real(dp), parameter :: n2= -p2
    real(dp), parameter :: p3= 1.D0/sqrt(3.D0)
    real(dp), parameter :: n3= -p3
    real(dp), parameter :: p6= 1.D0/sqrt(6.D0)
    real(dp), parameter :: n6= -p6
    real(dp), parameter :: pd6= 2.D0/sqrt(6.D0)
    real(dp), parameter :: nd6= -pd6
    real(dp), parameter :: n5_42=-0.771516749810

    integer, save, public :: iKOST = 0

    type(PAR), save                         :: P    !unit system: MPa; micrometer
    logical, save                           ::  InitOK = .FALSE.
    integer, save                           ::  Nss !Nr. of slip systems. 12: (110)[111] - 1 family, 24: (110)+(112)[111] - 2 families
    integer                                 ::  i 
    real(dp), save                  ::  alfa_G_b
    real(dp), save, dimension(24,6) ::  eff             = 0.D0, &
                                                effslashb       = 0.D0, &
                                                alfa_G_b_eff    = 0.D0, &
                                                alfa_G_b_ABSeff = 0.D0
    !EdgeDir(s,1:3): normalized movement vector of EDGE disl. on slip system s (== normalized burgers vector of slip system s)
    real(dp), save, dimension(24,3) ::  EdgeDir

    data (EdgeDir( 1: 3,i),i=1,3) /3*p3,3*p3,3*p3/ !s.s. 1 to 3
    data (EdgeDir( 4: 6,i),i=1,3) /3*n3,3*n3,3*p3/ !s.s. 4 to 6
    data (EdgeDir( 7: 9,i),i=1,3) /3*n3,3*p3,3*p3/ !..
    data (EdgeDir(10:12,i),i=1,3) /3*p3,3*n3,3*p3/ !..
    data (EdgeDir(13:15,i),i=1,3) /3*p3,3*p3,3*p3/ !..
    data (EdgeDir(16:18,i),i=1,3) /3*n3,3*n3,3*p3/ !..
    data (EdgeDir(19:21,i),i=1,3) /3*n3,3*p3,3*p3/ !..
    data (EdgeDir(22:24,i),i=1,3) /3*p3,3*n3,3*p3/ !s.s. 21 to 24

    !ScrewDir(s,1:3): normalized movement vector of SCREW disl. on slip system s
    !  If NormSS(s,:) denotes slip plane normal vector and x the cross product, then:
    !      ScrewDir(s,:) = EdgeDir(s,:) x NormSS(s,:)
    real(dp), save, dimension(24,3) ::  ScrewDir
    data (ScrewDir(01,i),i=1,3) /nd6,p6,p6/ !s.s. 01
    data (ScrewDir(02,i),i=1,3) /p6,nd6,p6/ !s.s. 02
    data (ScrewDir(03,i),i=1,3) /p6,p6,nd6/ !s.s. 03
    data (ScrewDir(04,i),i=1,3) /pd6,n6,p6/ !s.s. 04
    data (ScrewDir(05,i),i=1,3) /n6,pd6,p6/ !s.s. 05
    data (ScrewDir(06,i),i=1,3) /n6,n6,nd6/ !s.s. 06
    data (ScrewDir(07,i),i=1,3) /nd6,n6,n6/ !s.s. 07
    data (ScrewDir(08,i),i=1,3) /p6,pd6,n6/ !s.s. 08
    data (ScrewDir(09,i),i=1,3) /p6,n6,pd6/ !s.s. 09
    data (ScrewDir(10,i),i=1,3) /pd6,p6,n6/ !s.s. 10
    data (ScrewDir(11,i),i=1,3) /n6,nd6,n6/ !s.s. 11
    data (ScrewDir(12,i),i=1,3) /n6,p6,pd6/ !s.s. 12
    data (ScrewDir(13,i),i=1,3) /0.,p2,n2/ !s.s. 13
    data (ScrewDir(14,i),i=1,3) /n2,0.,p2/ !s.s. 14
    data (ScrewDir(15,i),i=1,3) /p2,n2,0./ !s.s. 15
    data (ScrewDir(16,i),i=1,3) /0.,n2,n2/ !s.s. 16
    data (ScrewDir(17,i),i=1,3) /p2,0.,p2/ !s.s. 17
    data (ScrewDir(18,i),i=1,3) /n2,p2,0./ !s.s. 18
    data (ScrewDir(19,i),i=1,3) /0.,n2,p2/ !s.s. 19
    data (ScrewDir(20,i),i=1,3) /n2,0.,n2/ !s.s. 20
    data (ScrewDir(21,i),i=1,3) /p2,p2,0./ !s.s. 21
    data (ScrewDir(22,i),i=1,3) /0.,p2,p2/ !s.s. 22
    data (ScrewDir(23,i),i=1,3) /p2,0.,n2/ !s.s. 23
    data (ScrewDir(24,i),i=1,3) /n2,n2,0./ !s.s. 24

    !NormDir(s,1:3): normalized slip plane normal vector of slip system s
    real(dp), save, dimension(24,3) ::  NormDir
    data (NormDir(01,i),i=1,3) /0.,p2,n2/ !s.s. 01
    data (NormDir(02,i),i=1,3) /n2,0.,p2/ !s.s. 02
    data (NormDir(03,i),i=1,3) /p2,n2,0./ !s.s. 03
    data (NormDir(04,i),i=1,3) /0.,n2,n2/ !s.s. 04
    data (NormDir(05,i),i=1,3) /p2,0.,p2/ !s.s. 05
    data (NormDir(06,i),i=1,3) /n2,p2,0./ !s.s. 06
    data (NormDir(07,i),i=1,3) /0.,p2,n2/ !s.s. 07
    data (NormDir(08,i),i=1,3) /p2,0.,p2/ !s.s. 08
    data (NormDir(09,i),i=1,3) /n2,n2,0./ !s.s. 09
    data (NormDir(10,i),i=1,3) /0.,n2,n2/ !s.s. 10
    data (NormDir(11,i),i=1,3) /n2,0.,p2/ !s.s. 11
    data (NormDir(12,i),i=1,3) /p2,p2,0./ !s.s. 12
    data (NormDir(13,i),i=1,3) /pd6,n6,n6/ !s.s. 13
    data (NormDir(14,i),i=1,3) /n6,pd6,n6/ !s.s. 14
    data (NormDir(15,i),i=1,3) /n6,n6,pd6/ !s.s. 15
    data (NormDir(16,i),i=1,3) /nd6,p6,n6/ !s.s. 16
    data (NormDir(17,i),i=1,3) /p6,nd6,n6/ !s.s. 17
    data (NormDir(18,i),i=1,3) /p6,p6,pd6/ !s.s. 18
    data (NormDir(19,i),i=1,3) /nd6,n6,n6/ !s.s. 19
    data (NormDir(20,i),i=1,3) /p6,pd6,n6/ !s.s. 20
    data (NormDir(21,i),i=1,3) /p6,n6,pd6/ !s.s. 21
    data (NormDir(22,i),i=1,3) /pd6,p6,n6/ !s.s. 22
    data (NormDir(23,i),i=1,3) /n6,nd6,n6/ !s.s. 23
    data (NormDir(24,i),i=1,3) /n6,p6,pd6/ !s.s. 24

    !CBBnormal(i,1:3): normalized vector normal to CBB i
    real(dp), save, dimension(6,3)  ::  CBBnormal
    data (CBBnormal(1,i),i=1,3) /0.,p2,n2/ !CBBs on (01-1)-plane
    data (CBBnormal(2,i),i=1,3) /n2,0.,p2/ !CBBs on (-101)-plane
    data (CBBnormal(3,i),i=1,3) /p2,n2,0./ !CBBs on (1-10)-plane
    data (CBBnormal(4,i),i=1,3) /0.,n2,n2/ !CBBs on (0-1-1)-plane
    data (CBBnormal(5,i),i=1,3) /p2,0.,p2/ !CBBs on (101)-plane
    data (CBBnormal(6,i),i=1,3) /n2,n2,0./ !CBBs on (-1-10)-plane
    
    interface operator(+)
        module procedure  StateDerivedVar_plus
    end interface

    interface operator(*)
        module procedure  StateDerivedVar_times
    end interface

    interface InitModuleAltayHardLaw_DSH !Generic Interface
        module procedure Init_file,Init_PAR
    end interface

    private
    public  ::  InitModuleAltayHardLaw_DSH, &
                ReadPar,                    &
                GetInitStatVar,             &
                MakeInc,                    &
                WriteHeadSVfile,            &
                ReadHeadSVfile,             &
                WriteSVfile,                &
                ReadSVfile,                 &
                GetStateDerivedVar,         &
                WriteSDV,                   &
                operator(+),                &
                operator(*),                &
                PAR,                        &
                StatVar,                    &
                CBBtype,                    &
                StateDerivedVars

contains

    !> Initialization of altayHardLaw_DSH.
    integer function Init_PAR(Ptry,KOSTtry,LEC) result(iError)
        type(PAR), intent(in)   :: Ptry     !proposed parameter set
        integer, intent(in)     :: KOSTtry  !proposed value of KOST
        integer, intent(in)     :: LEC      !unit number of PRE-file
        character(len=128)      :: line1
        integer                 :: s,i,Idum = 0, Nsstry = 0

        InitOK=.FALSE.

        !Check KOSTtry
        select case (KOSTtry)
            case (11, 12, 13) !supported
                iKOST = KOSTtry 
            case default !unsupported
                iError = VEF_BADVAL
                return
        end select

        !Check PRE-file #1: Does 1st comment line contain strings 'BCC' and '{BP}'?
        rewind (unit=LEC)
        read (LEC,FMT='(A)') line1 !line1
        if ( (index(line1,'BCC') == 0) .or. (index(line1,'{BP}') == 0)) then
            iError = VEF_IO
            return
        end if !File is OK.

        !Check PRE-file #2: Is number of slip systems (Nss) supported?
        read (LEC,FMT='(8I4)') Idum, Nsstry, Idum, Idum, Idum, Idum, Idum, Idum
        rewind (unit=LEC)
        select case (Nsstry)
            case (12, 24) !supported number of slip systems
                Nss=Nsstry
            case default !unsupported number of slip systems specified  in LEC
                iError = VEF_NSS
                return
        end select

        !Check the input parameters                              ! Units of input parameters:
        if(Ptry%b    >  0.    .AND. Ptry%b    <= 1.e-8    .AND.& ! [m]
           Ptry%G    >= 10.e3 .AND. Ptry%G    <= 500.e3   .AND.& ! [MPa]
           Ptry%alfa >  0.    .AND. Ptry%alfa <= 5.       .AND.& ! [/]
           Ptry%f    >= 0.    .AND. Ptry%f    <= 1.       .AND.& ! [/]
           Ptry%tau0 >= 0.    .AND. Ptry%tau0 <= 1.e4     .AND.& ! [MPa]
           Ptry%I    >= 0.    .AND. Ptry%I    <= 10.      .AND.& ! [/]
           Ptry%Iwd  >= 0.    .AND. Ptry%Iwd  <= 10.      .AND.& ! [/]
           Ptry%Iwp  >= 0.    .AND. Ptry%Iwp  <= 10.      .AND.& ! [/]
           Ptry%R    >  0.    .AND. Ptry%R    <= 1.e-6    .AND.& ! [m]
           Ptry%Rwd  >  0.    .AND. Ptry%Rwd  <= 1.e-6    .AND.& ! [m]
           Ptry%Rncg >  0.    .AND. Ptry%Rncg <= 1.e-6    .AND.& ! [m]
           Ptry%Rwp  >  0.    .AND. Ptry%Rwp  <= 1.e-6    .AND.& ! [m]
           Ptry%Rrev >  0.    .AND. Ptry%Rrev <= 1.e-6    .AND.& ! [m]
           Ptry%R2   >  0.    .AND. Ptry%R2   <= 1.e-6    .AND.& ! [m]
           Ptry%beta1 >= 0.   .AND. Ptry%beta1 <= 100.     .AND.& ! [/]
           Ptry%beta2 >= 0.   .AND. Ptry%beta2 <= 100.          ) then
            !Save the parameters in P (private to this module)
            P=Ptry
            !change of units if different in Ptry from P (units of P are: MPa; micrometer)
            P%b    = P%b    * TENpow6 ![m] -> [micrometer]
            P%R    = P%R    * TENpow6 ![m] -> [micrometer]
            P%Rwd  = P%Rwd  * TENpow6 ![m] -> [micrometer]
            P%Rncg = P%Rncg * TENpow6 ![m] -> [micrometer]
            P%Rwp  = P%Rwp  * TENpow6 ![m] -> [micrometer]
            P%Rrev = P%Rrev * TENpow6 ![m] -> [micrometer]
            P%R2   = P%R2   * TENpow6 ![m] -> [micrometer]
        else
              iError = VEF_OutOfRange
              return
        end if

        !Calculate dependent hardening parameters
        P%RHOcbSAT=P%I  * P%I  / ( P%R  * P%R  )
        P%RHOwdSAT=P%Iwd * P%Iwd / ( P%Rwd* P%Rwd)
        P%RHOwpSAT=(sqrt((P%Iwp / P%Rwp)**4 +               &
                   4.D0 * (P%Iwp * P%Iwd / (P%Rwp * P%Rwd))**2) +  &
                   (P%Iwp / P%Rwp)**2) / 2.D0

        P%RHOcbMIN = MINfrac * P%RHOcbSAT
        P%RHOwdMIN = MINfrac * P%RHOwdSAT
        P%RHOwpMIN = MINfrac * P%RHOwpSAT
        P%RHOwpLOW = LOWfrac * P%RHOwpSAT

        !Calculate "Wall-effectivity"-matrices
        select case(iKOST)
            case(13) ! "LoopSlip": introduced in v.1.11; invokable through KOST=13
                do s=1,24
                    do i=1,6
                        eff(s,i)=DOT_PRODUCT( NormDir(s,:) , CBBnormal(i,:) )
                        if (abs(eff(s,i)) >= 0.99999D0) then !treat as "1" or "-1"
                            eff(s,i)=0.0D0
                        else
                            eff(s,i)=sqrt(1.0D0-(eff(s,i))**2)
                        endif
                    end do
                end do
            case(12) !"ScrewSlip": introduced in v.1.9; invokable through KOST=12 in v.1.10
                do s=1,24
                    do i=1,6
                        eff(s,i) = DOT_PRODUCT( ScrewDir(s,:) , CBBnormal(i,:) )
                    end do
                end do
            case(11) ! according to PhD Peeters
                do s=1,24
                  do i=1,6
                    eff(s,i) = DOT_PRODUCT( EdgeDir(s,:) , CBBnormal(i,:) )
                  end do
                end do
        end select

        effslashb       = eff / P%b
        alfa_G_b        = P%alfa * P%G * P%b
        alfa_G_b_eff    = alfa_G_b * eff
        alfa_G_b_ABSeff = ABS(alfa_G_b_eff)

        !If control passes here, initialization is done without errors
        InitOK = .TRUE. !private to this module
        iError = VEF_OK  !OUT
    end function 

    integer function Init_file(inunit,KOST,LEC) result(info)
        integer,intent(in)      :: inunit   !< number of
        integer,intent(in)      :: KOST     !< Id of the model version.
        integer,intent(in)      :: LEC
        type(PAR) :: PARtry
        
        info = VEF_Error
        select case(KOST)
            case(11,12,13)
                ! Supported value of KOST
                ! Read parameters of PE-BP hardening model
                if (ReadPar(inunit,KOST,PARtry) == 0) then
                    info = Init_PAR(PARtry,KOST,LEC)
                endif
            case default
                info = VEF_BADVAL !Unsupported value of KOST
        end select
    end function 

    integer function ReadPar(inunit, KOST, Pf)
        integer, intent(in)     :: inunit   !< IO unit number
        integer, intent(in)     :: KOST     !< model identifier
        type(PAR), intent(out)  :: Pf       !< Parameters to be read from a formatted file.
        
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
100     format(F12.5)
        
        ReadPar = VEF_OK
        return
        
666     ReadPar = VEF_IO !Error in reading from file
    end function ReadPar

    !>Determine state variables for an annealed & undeformed substructure for single grain (SV0)
    subroutine GetInitStatVar(SV0,iError)
        type(StatVar), intent(out)  :: SV0
        integer, intent(out)        :: iError

        if(.not. InitOK) then
              iError = VEF_Uninitialized
              return
        end if
        iError = VEF_OK

        SV0%RHOcb               = P%RHOcbMIN
        SV0%CBB(:)%RHOwd        = P%RHOwdMIN
        SV0%CBB(:)%RHOwp        = 0.
        SV0%CBB(:)%RHOwdHOM     = P%RHOwdMIN
        SV0%CBB(:)%accGAMMA_new = 0.
        SV0%CBB(:)%RHOwd_ini    = P%RHOwdMIN
        SV0%ActiveCBB(:)        = 0
        SV0%CRSS                = F_CRSS(SV0)
    end subroutine

    subroutine MakeInc(SVa, sliprate, deltaT, SVb, iError)
        type(StatVar), intent(in)                     :: SVa      !<State variable at beginning of increment
        real(dp), intent(in), dimension(24)   :: sliprate !<Assumed constant throughout the increment
        real(dp), intent(in)                  :: deltaT   !<Time increment
        type(StatVar), intent(out)                    :: SVb      !<State variables at end of the increment
        integer, intent(out)                          :: iError

        logical                         :: FLUXreversal, wpLOW
        integer                         :: j
        integer, dimension(6)           :: r
        real(dp)                ::  fl, wd, RHOwp_a, wpFLUX, RHOwdLOC, RHOwdHOM, accGAMMA_new, RHOwd_ini, RHOwd, rEffective,    &
                                            SUMabsGamDot    = 0.,                                                                       &
                                            GAMMAdot_new    = 0.,                                                                       &
                                            RHObausch       = 0.,                                                                       &
                                            SUMabsGam       = 0.,                                                                       &
                                            GAMMA_new       = 0.
        real(dp), dimension(6)  ::  GAMMAdot        = 0.,                                                                       &
                                            GAMMA           = 0.

        if(.not. InitOK) then
              SVb = SVa
              iError = VEF_Uninitialized
              return
        end if
        iError = VEF_OK

        !>Calculate quantities of slip rates and slips
        !>Identify currently generated and non-currently generated walls
        SUMabsGamDot = sum(abs(sliprate(1:Nss)))
        SUMabsGam = SUMabsGamDot * deltaT
        
        if (SUMabsGam < epsilon(0.D0)) then
            ! No slip rate in the current grain => no deformation, no update of the state
            SVb = SVa
            if (deltaT < 0.D0) iError = VEF_BADVAL
            return
        end if

        !F_GAMMADOT
        GAMMAdot(1) = abs(slipRate(1)) + abs(slipRate(7))  !(01-1)-plane
        GAMMAdot(2) = abs(slipRate(2)) + abs(slipRate(11)) !(-101)-plane
        GAMMAdot(3) = abs(slipRate(3)) + abs(slipRate(6))  !(1-10)-plane
        GAMMAdot(4) = abs(slipRate(4)) + abs(slipRate(10)) !(0-1-1)-plane
        GAMMAdot(5) = abs(slipRate(5)) + abs(slipRate(8))  !(101)-plane
        GAMMAdot(6) = abs(slipRate(9)) + abs(slipRate(12)) !(-1-10)-plane
        GAMMA = GAMMAdot * deltaT

        !sort the walls in r
        if (GAMMAdot(1) > GAMMAdot(2)) then
            r(1) = 1
            r(2) = 2
        else
            r(1) = 2
            r(2) = 1
        end if

        do i=3,6
            if (GAMMAdot(i) > GAMMAdot(r(1))) then
                r(i) = r(2)
                r(2) = r(1)
                r(1) = i
            else if (GAMMAdot(i) > GAMMAdot(r(2))) then
                r(i) = r(2)
                r(2) = i
            else
                r(i) = i
            end if
        end do

        SVb%ActiveCBB(1) = r(1)
        SVb%ActiveCBB(2) = r(2)

        GAMMAdot_new = GAMMAdot(r(1)) + GAMMAdot(r(2))
        GAMMA_new = GAMMAdot_new * deltaT

        !Update dislocation densities
        RHObausch=0. 
        do j=1,2 !Loop over 2 currently generated walls
            !RHOwd
            SVb%CBB(r(j))%RHOwd = F_KocksMeck(SVa%CBB(r(j))%RHOwd, GAMMA(r(j)),P%Iwd,P%Rwd)
            SVb%CBB(r(j))%RHOwdHOM = SVb%CBB(r(j))%RHOwd
            !RHOwp
            RHOwp_a = SVa%CBB(r(j))%RHOwp
            wpFLUX = dot_product(effslashb(:,r(j)), sliprate(:))
            FLUXreversal = wpFLUX * RHOwp_a < 0.0
            wpLOW = abs(RHOwp_a) <= P%RHOwpLOW

            if (FLUXreversal .and. .not. (wpLOW) ) then
                ! |RHOwp| gets smaller, following analytic time integration
                SVb%CBB(r(j))%RHOwp = RHOwp_a * exp(-P%Rrev * abs(wpFLUX) * deltaT) !wpFLUX is a rate!
                RHObausch = RHObausch + abs(RHOwp_a)
            else
                ! |RHOwp| gets larger, following numeric time integration (4th order Runge-Kutta)
                fl = wpFLUX                 !to be used by RungeKutta->dwp_dt
                wd = SVb%CBB(r(j))%RHOwdHOM  !to be used by RungeKutta->dwp_dt
                if (FLUXreversal) then
                    !In this case, it must also be that: wpLOW=.TRUE.
                    !AFTER change of its sign, RHOwp will build up again.
                    SVb%CBB(r(j))%RHOwp = rungeKutta(-RHOwp_a, deltaT, fl, wd)
                else
                    SVb%CBB(r(j))%RHOwp = rungeKutta(RHOwp_a, deltaT, fl, wd)
                end if
            end if
        end do

        do j=3,6 !Loop over 4 non-currently generated walls
            !RHOwd
            RHOwdHOM     = SVa%CBB(r(j))%RHOwdHOM
            accGAMMA_new = SVa%CBB(r(j))%accGAMMA_new
            RHOwd_ini    = SVa%CBB(r(j))%RHOwd_ini

            if (RHOwdHOM > P%RHOwdMIN) then
                !if the wall was NOT active in prev. inc.
                if (r(j) /= SVa%ActiveCBB(1) .and. r(j) /= SVa%ActiveCBB(2)) then
                    accGAMMA_new = accGAMMA_new + GAMMA_new
                else !the wall was active in prev. inc.
                    accGAMMA_new = GAMMA_new
                    RHOwd_ini = RHOwdHOM
                end if

                RHOwdLOC    = -tanh(P%beta1 * accGAMMA_new) * exp(-P%beta1 * accGAMMA_new) * RHOwd_ini * P%beta2
                RHOwdHOM    = RHOwdHOM * exp(-P%Rncg * GAMMA_new / P%b)
                RHOwd       = RHOwdHOM + RHOwdLOC
                if (RHOwd < P%RHOwdMIN) RHOwd = P%RHOwdMIN
            else
                RHOwdHOM    = P%RHOwdMIN
                RHOwd       = P%RHOwdMIN
            end if

            SVb%CBB(r(j))%RHOwd         = RHOwd
            SVb%CBB(r(j))%RHOwdHOM      = RHOwdHOM
            SVb%CBB(r(j))%accGAMMA_new  = accGAMMA_new
            SVb%CBB(r(j))%RHOwd_ini     = RHOwd_ini

            !RHOwp
            RHOwp_a = SVa%CBB(r(j))%RHOwp
        
            if (abs(RHOwp_a) > P%RHOwpMIN) then
                SVb%CBB(r(j))%RHOwp = RHOwp_a * exp(-P%Rncg * GAMMA_new / P%b)
            else
                if (RHOwp_a >= 0.0) then
                    SVb%CBB(r(j))%RHOwp = P%RHOwpMIN
                else
                    SVb%CBB(r(j))%RHOwp = -P%RHOwpMIN
                end if
            end if
        end do

        !RHOcb
        if(RHObausch > 0.0) then
            Reffective = P%R + P%R2 * RHObausch / (2.D0 * P%RHOwpSAT)
            if (P%I * sqrt(SVa%RHOcb) - Reffective * SVa%RHOcb <= 0.0) then 
                !Heaviside bracket, keep as is.
                SVb%RHOcb = SVa%RHOcb
            else
                SVb%RHOcb = F_KocksMeck(SVa%RHOcb, SUMabsGam, P%I, Reffective)
            end if
        else 
            SVb%RHOcb = F_KocksMeck(SVa%RHOcb, SUMabsGam, P%I, P%R)
        end if

        !! Calculate Critical Resolved Shear Stresses
        SVb%CRSS = F_CRSS(SVb)
    end subroutine 

    !>Returns the value of RHO at the end of an interval (a,b)  for the following differential equation: d(RHO)/d(g) = ( II*sqrt(RHO) - RR*RHO ) / P%b
    !>The value of 'P%b', the size of burgers vector, is inherited.
    !>@param RHO_a: the value of RHO at the start of the interval (a,b)
    !>@param delta_g: the increment in g during the interval (a,b)
    pure real(dp) function F_KocksMeck(RHO_a, delta_g, II, RR) result(kock)
        real(dp), intent(in)    :: RHO_a, delta_g,II, RR
        real(dp) x
    
        x = exp(-0.5D0 * RR * delta_g / P%b)
        x = II / RR * (1.D0 - x) + sqrt(RHO_a) * x
        kock = x**2
    end function 

    !> 4th order Runge-Kutta approximation of the differential equation given by d(wp)/dt = F(wp).
    !> @param wpini initial state of wp
    pure real(dp) function rungeKutta(wpini, deltaT, fl, wd) result(runge_kutta)
        real(dp), intent(in)    :: wpini, deltaT, fl, wd
        real(dp), dimension(4)  :: k

        k(1) = deltaT * dwp_dt(wpini, fl, wd)
        k(2) = deltaT * dwp_dt(wpini + k(1) / 2.D0, fl, wd)
        k(3) = deltaT * dwp_dt(wpini + K(2) / 2.D0, fl, wd)
        k(4) = deltaT * dwp_dt(wpini + K(3), fl, wd)
        
        runge_kutta = wpini + (k(1) + 2.D0 * k(2) + 2.D0 * k(3) + k(4)) / 6.D0
    end function 

    pure real(dp) function dwp_dt(wp, fl, wd) result(derivative)
        real(dp), intent(in)    :: wp, fl, wd

        derivative = (sign(1.D0, fl) * P%Iwp * sqrt(wd + abs(wp)) - P%Rwp * wp) * abs(fl)
    end function

    pure function F_CRSS(SV) result(CRSS) 
        type(StatVar), intent(in)           :: SV
        integer                             :: j, s, i
        real(dp)                    :: signfac, tau_CB, CRSS_0_CB
        real(dp), dimension(6)      :: wpcontr, wdcontr
        real(dp), dimension(2,24)   :: CRSS, tau_CBB

        !Slip systems not allowed to become active retain initialization value of -1.0
        CRSS = -1.D0
        !CRSS within cells & CBs
        tau_CB = alfa_G_b * sqrt(SV%RHOcb)
        !contributions from tau_0 and CBs to CRSS
        CRSS_0_CB = P%tau0 + (1.D0 - P%f) * tau_CB

        !Calc. CRSS for each slip system s, for the sense of slip j
        do j = 1,2
            signfac = 3.D0 - 2.D0 * dble(j) ! 1 for j=1 ; -1 for j=2
            do s = 1,Nss
                !wp- and wd-contributions from all CBBs i
                do i = 1,6
                    wpcontr(i) = sqrt(abs(SV%CBB(i)%RHOwp)) * signfac * alfa_G_b_eff(s,i) * sign(1.D0, SV%CBB(i)%RHOwp)
                    if (wpcontr(i) < 0.0) wpcontr(i) = 0.0 ! Heaviside bracket
                    wdcontr(i) = sqrt(SV%CBB(i)%RHOwd) * alfa_G_b_ABSeff(s,i)
                end do
                !CRSS within CBB = wp- and wd-contributions for all 6 walls
                tau_CBB(j,s) = sum(wpcontr) + sum(wdcontr)
                !C.R.S.S. for the "two-phase composite"
                CRSS(j,s) = CRSS_0_CB + P%f * tau_CBB(j,s)
            end do
        end do
    end function 

    subroutine GetStateDerivedVar(SV, SDV, iError)
        type(StatVar),          intent(in)  :: SV
        type(StateDerivedVars), intent(out) :: SDV
        integer,                intent(out) :: iError

        iError = VEF_Error !init
        if(.not. InitOK) then
              iError = VEF_Uninitialized
              return
        end if

        !Note: Number of CBBs is 6 (currently hard-coded)
        SDV%rho_CBs     = SV%RHOcb                        * TENpow6**2 !unit conversion micrometer^(-2) -> m^(-2)
        SDV%rho_CBBs    = sum(    SV%CBB(:)%RHOwd ) /6.D0 * TENpow6**2 !unit conversion micrometer^(-2) -> m^(-2)
        SDV%rho_polCBBs = sum(abs(SV%CBB(:)%RHOwp)) /6.D0 * TENpow6**2 !unit conversion micrometer^(-2) -> m^(-2)
        SDV%rho_avg     = (1.D0-P%f) * SDV%rho_CBs + P%f * (SDV%rho_CBBs + SDV%rho_PolCBBs)

        iError = VEF_OK
    end subroutine 

    !> Calculate component-wise sum of two StateDerivedVars objects
    pure elemental type(StateDerivedVars) function StateDerivedVar_plus(first,second) result(res)
        type(StateDerivedVars), intent(in)  :: first, second
        
        res%rho_CBs     = first%rho_CBs     + second%rho_CBs
        res%rho_CBBs    = first%rho_CBBs    + second%rho_CBBs
        res%rho_polCBBs = first%rho_polCBBs + second%rho_polCBBs
        res%rho_avg     = first%rho_avg     + second%rho_avg
    end function 

    !> Multiply all components of SDV by the scalar
    pure elemental type(StateDerivedVars) function StateDerivedVar_times(SDV, scalar) result(res)
        real(dp),       intent(in) :: scalar
        type(StateDerivedVars), intent(in) :: SDV
        
        res%rho_CBs     = scalar * SDV%rho_CBs
        res%rho_CBBs    = scalar * SDV%rho_CBBs
        res%rho_polCBBs = scalar * SDV%rho_polCBBs
        res%rho_avg     = scalar * SDV%rho_avg
    end function StateDerivedVar_times

end module
