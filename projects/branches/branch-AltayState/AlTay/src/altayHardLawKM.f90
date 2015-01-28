! $Id$
    
!     v1.0 by P. Eyckens, MTM, KU Leuven, 22 Jan 2015.
!     v2.0 by J. Gawad, CS, KU Leuven, 27 Jan 2015

!> Hardening law: KocksMecking dislocation-based isotropic hardening.
!>
!> The prefix added to the components of this module is `KM`, which
!> stands for Kocks-Mecking.
module altayHardLaw_KM
use altayMaterial
implicit none
private

    ! Fundamental interface of the module. The host/user code should use these
    ! functions for operations on the data types defined in this module.
    public                          &
        KMConfigParameters_init,    &
        KMConfigParameters_read,    &
        KMStateVariables_init,      &
        KMStateVariables_update,    &
        KMStateVariables_getCRSS,   &
        KMStateDerivedVariables_calculate, &
        KMStateDerivedVariables_homogenize


    ! Supplementary interface of the module. The host/user code may use these
    ! convenience function.
    public                          &
        KMStateVariables_read,      &
        KMStateVariables_write,     &
        KMStateDerivedVariables_write


    !> Configuration parameters of the Kocks-Mecking hardening law.
    type,public :: KMConfigParameters
        double precision :: b = 1.D-10  !< Burgers vector
        double precision :: G = 1.D0    !< Shear modulus
        double precision :: alfa = 1.D0 !< Proportionality factor alpha
        double precision :: tau0 = 1.D0 !< Reference shear stress
        double precision :: I = 0.D0
        double precision :: R = 0.D0
        double precision :: rho_ann = 0.D0
        double precision :: rho_sat = 0.D0
    end type


    !> State variables to be stored per single crystal.
    type,public :: KMStateVariables
        double precision                            :: rho  = 0.D0
    end type


    !> State-derived variables per single crystal.
    type,public :: KMStateDerivedVariables
        !> Dislocation density in grain; unit: m^(-2)
        double precision :: rho = 0.D0
        !> Saturation fraction of dislocation density, in percentage (%)
        double precision :: SatFracRho = 0.D0
    end type


    !> Initialization procedure of the KMConfigParameters type.
    interface KMConfigParameters_init
        module procedure KMConfigParameters_initFromFile, &
                         KMConfigParameters_initFromType
    end interface


    interface operator(+)
        module procedure  StateDerivedVar_plus
    end interface
      
    interface operator(*)
        module procedure StateDerivedVar_times_scalar1, StateDerivedVar_times_scalar2
    end interface


    integer,parameter :: KM_nslipsystems = 12

    double precision,parameter :: TENpow6 = 1.D6


contains

    !==========================================================================
    !
    ! State variables
    !
    !==========================================================================
    
    !> Initialize config parameters from a configuration file.
    integer function KMConfigParameters_initFromFile(this, inunit) result(info)
    implicit none
    type(KMConfigParameters),intent(inout)    :: this
    integer,intent(in)                      :: inunit
    !
        info = KMConfigParameters_Read(this, inunit)
        if (info == criSuccess) info = KMConfigParameters_initFromType(this)
    !
    end function
    
    !> Initialize config parameters from a pre-configured KMConfigParameters object.
    integer function KMConfigParameters_initFromType(this) result(info)
    implicit none
    type(KMConfigParameters),intent(inout)    :: this    !
    integer            :: s,i,Idum=0,Nsstry=0
    !
    !Check the input parameters                               ! Units of input parameters:
        if (this%b    >  0.    .AND. this%b    <= 1.e-8    .AND.& ! [m]
            this%G    >= 10.e3 .AND. this%G    <= 500.e3   .AND.& ! [MPa]
            this%alfa >  0.    .AND. this%alfa <= 5.       .AND.& ! [/]
            this%tau0 >= 0.    .AND. this%tau0 <= 1.e4     .AND.& ! [MPa]
            this%I    >= 0.    .AND. this%I    <= 10.      .AND.& ! [/]
            this%R    >  0.    .AND. this%R    <= 1.e-6    .AND.& ! [m]
            this%rho_ann >  0. .AND. this%rho_ann < this%I**2/this%R**2 & ! [m^(-2)]  !! i.e. rho_ann < saturation stress
        ) then
            !change of units if different (units of this are: MPa; nm(nanometer))
            this%b         = this%b       * TENpow6       ![m] -> [nm]
            this%R         = this%R       * TENpow6       ![m] -> [nm]
            this%rho_ann   = this%rho_ann * TENpow6**(-2) ![m^(-2)] -> [nm^(-2)] 
            !Calculate dependent parameters
            this%rho_sat= this%I**2 / this%R**2
            !
            info = criSuccess
        else
            info = criErr_BadArgs
        endif
    !
    end function
   
    !> Read independent components of KMConfigParameters from the IO
    !> 
    !> \note This is a reference procedure. The client code may use a different
    !>       format.
    integer function KMConfigParameters_read(this, inunit) result (info)
    implicit none
    type(KMConfigParameters),intent(out)    :: this    !< parameters to be read from a formatted file.
    integer,intent(in)                      :: inunit   !< IO unit number
    !
        read(inunit,fmt=100,err=999,end=999) this%b
        read(inunit,fmt=100,err=999,end=999) this%G
        read(inunit,fmt=100,err=999,end=999) this%alfa
        read(inunit,fmt=100,err=999,end=999) this%tau0
        read(inunit,fmt=100,err=999,end=999) this%I
        read(inunit,fmt=100,err=999,end=999) this%R
        read(inunit,fmt=100,err=999,end=999) this%rho_ann      
        100   format(F12.5)
        !
        info = criSuccess
        return
        !
        999   info = criErr_IORead !Error in reading from file
    !
    end function


    !> Initialize state variables from configuration object
    subroutine KMStateVariables_init(this, params, info)
    implicit none
    type(KMStateVariables),intent(out)  :: this
    type(KMConfigParameters),intent(in) :: params
    integer,intent(out)                 :: info
    !
        this%rho  = params%rho_ann
        info = criSuccess
    !
    end subroutine

    
    !> Calculate CRSS from state variables and configuration object
    subroutine KMStateVariables_getCRSS(this, params, crss, info)
    implicit none
    type(KMStateVariables),intent(in)   :: this
    type(KMConfigParameters),intent(in) :: params
    type(CRSSData),intent(inout)        :: crss
    integer,intent(out)                 :: info
    !
    double precision :: crss_Tay
    !
        if (CRSSData_size(crss) >= KM_nslipsystems) then
            crss_Tay= params%tau0 + params%alfa * params%G * params%b * sqrt(this%rho)
            ! Note: Slip systems not allowed to become active should get value of -1.0
            crss%crss = crss_Tay
            info = criSuccess
            !
        else
            info = criErr_BadArgs
        endif
    !
    end subroutine
    
    !> Calculate new state variables.
    !>
    !> The procedure requires as input:
    !> - old state variables, i.e. at the beginning of the increment (previous)
    !> - the slip rates, assumed constant throughout the increment (sliprate)
    !> - the time increment (deltaT)
    subroutine KMStateVariables_update(this, previous, params, sliprate, deltaT, info)
    implicit none
    type(KMStateVariables),intent(out)          :: this !< State variables to be updated (new)
    type(KMStateVariables),intent(in)           :: previous
    type(KMConfigParameters),intent(in)         :: params
    double precision,dimension(:),intent(in)    :: sliprate
    double precision,intent(in)                 :: deltaT
    integer,intent(out)                         :: info
    !
    double precision :: gamma   =0.
    !
        info = criErr_BadArgs
        ! Check whether size of sliprate and number of slipsystem  conform.
        ! In this module the check may be omitted.
        if (size(sliprate) < KM_nslipsystems) return
        !Calculate accumulated slip during this inc over all slip systems
        gamma = sum(abs(sliprate)) * deltaT
        !
        if (gamma < epsilon(0.D0)) then
            ! Negligeable slip rate in the current grain => no deformation, no update of the state
            this = previous
        else
            ! Update dislocation density
            this%rho = F_KocksMeck(previous%rho, gamma, params%I, params%R, params%b) 
        endif
        info = criSuccess
        return
    !
    contains


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
        double precision function F_KocksMeck(rho_a, delta_g, II, RR, b) 
        double precision,intent(in) :: rho_a,delta_g,II,RR, b
        !
        double precision :: x
        !
            x=exp(-0.5D0*RR*delta_g/b)
            x=II/RR*(1.D0-x)+sqrt(rho_a)*x
            F_KocksMeck=x*x
        !
        end function F_KocksMeck
    !
    end subroutine
    
    !> Calculate homogenized SDV
    pure subroutine KMStateDerivedVariables_homogenize(this, values, weights, info)
    implicit none
    type(KMStateDerivedVariables),intent(out)               :: this
    !> Input SDV
    type(KMStateDerivedVariables),dimension(:),intent(in)   :: values
    !> weights (must be of the same size as the `values`
    double precision,dimension(:),intent(in)                :: weights  
    integer,intent(out)                                     :: info
    !
    integer :: i
    double precision :: iws ! reciprocal of the sum of weights
    !
        info = criErr_BadDims
        iws = sum(weights)
        if ((size(values) /= size(weights)) .or. (iws < epsilon(0.0))) return
        ! Do the homogenization: weighted averaging
        iws = 1.D0 / iws
        do i = 1, size(values)
            !DIR$ INLINE
            this = this + weights(i) * values(i)
        enddo
        this = iws * this
        !
        info = criSuccess
    !
    end subroutine
    
    
    ! TODO: consider if the read/write should operate on single instances or 
    !       on arrays of objects.
    
    !> Perform formatted IO read operation on KMStateVariables object. 
    !>
    !> \Param `skip` if true, the function performs a fake read operation by simply
    !> skipping the same number of lines 
    !> as the function would normally read. The resulting SV becomes initialized to default values.
    integer function KMStateVariables_read(this, unit, skip, header, value) result(info)
    integer,intent(in)                      :: unit
    type(KMStateVariables),intent(out)      :: this
    logical,optional,intent(in)             :: skip
    logical,intent(in),optional             :: header !< Process the header. Default: .false.
    logical,intent(in),optional             :: value  !< Process the value. Default: .true.
    !
    integer :: i,j
    logical :: skip_, value_
    character(len=5)             :: tmpstr
    !
        if (present(header)) then
            if (header) then
                do i=1,3 
                    read(unit,fmt=100,err=999) tmpstr
                enddo
            endif
        endif
        !
        value_ = .true.
        if (present(value)) value_ = value
        !
        if (value_) then
            skip_ = .false.
            if (present(skip)) skip_ = skip
            if (skip_) then
                read(unit,fmt=100,err=999,end=999) tmpstr
            else            
                read(unit,fmt=201,err=999,end=999) this%rho
            endif
        endif
        info = criSuccess
        return
    100 format(A5)
    201 format(E15.8 )
    !
    ! Error handler:
    999 info = criErr_IORead
    !
    end function


    integer function KMStateVariables_write(this, unit, header, value) result(info)
    type(KMStateVariables),intent(in)   :: this
    integer,intent(in)                  :: unit
    logical,intent(in),optional         :: header !< Process the header. Default: .false.
    logical,intent(in),optional         :: value  !< Process the value. Default: .true.
    !
    logical :: value_
    !
        info = criErr_IOWrite
        if (present(header)) then
            if (header) then
                write(unit,fmt=101,err=999)
                write(unit,fmt=100,err=999)"Disl.dens. [micrometer^(-2)]: [1]rho"
                write(unit,fmt=101,err=999)
            endif
        endif
        !
        value_ = .true.
        if (present(value)) value_ = value
        if (value_) write(unit,fmt=201,err=999) this%rho
        !
        info = criSuccess
        return
    !
    100 format('#',1X, A68)
    101 format('#',69('-'))
    201 format(E15.8 )
        !
    999 info = criErr_IOWrite !Error in reading from file    
    !
    end function


    !==========================================================================
    !
    ! State-Derived Variables
    !
    !==========================================================================


    !> Output state-derived variables (SDV) or/and a header line.
    integer function KMStateDerivedVariables_write(this,unit,header,value) result(info)
    type(KMStateDerivedVariables),intent(in)    :: this
    integer,intent(in)                          :: unit
    logical,intent(in),optional                 :: header
    logical,intent(in),optional                 :: value
    !
    integer :: ierr
    !
        info = criErr_IOWrite
        if (present(header)) then
            if (header) then
                write(unit,fmt=100,iostat=ierr)
                if (ierr /= 0) return
            endif
        endif
        if (present(value)) then
            if (value) then
                write(unit,fmt=101,iostat=ierr) this
                if (ierr /= 0) return
            endif
        endif  
        info = criSuccess
        ! 
        100 format(T3,'rho[m^(-2)]',T19,'SatFracRho[%]')
        101 format(E15.7,1X,F15.2)
    !
    end function



    !> Calculate state-derived variables
    elemental subroutine KMStateDerivedVariables_calculate(this,state,params,info)
    type(KMStateDerivedVariables),intent(out)   :: this
    type(KMStateVariables),intent(in)           :: state
    type(KMConfigParameters),intent(in)         :: params
    integer,         intent(out) :: info !< Exit code
    !
        this%rho = state%rho * TENpow6**2 !unit conversion [nm^(-2)] -> [m^(-2)]
        this%SatFracRho = 100. * (state%rho-params%rho_ann) / (params%rho_sat-params%rho_ann) !unit: %
        info = criSuccess
    !
    end subroutine
    

    !
    ! Auxilliary operators that simplify notation of manipulating SDV objects
    !


    !> Calculate component-wise sum of two SDV objects 
    elemental function StateDerivedVar_plus(first,second) result(res)
    type(KMStateDerivedVariables),intent(in) :: first,second
    type(KMStateDerivedVariables) :: res
    !
        res%rho = first%rho + second%rho
        res%SatFracRho = first%SatFracRho + second%SatFracRho
    !
    end function
    
    
    !> Multiply all components of SDV by the scalar
    elemental function StateDerivedVar_times_scalar1(scalar, second) result(res)
    type(KMStateDerivedVariables),intent(in) :: second
    double precision,intent(in)       :: scalar
    type(KMStateDerivedVariables) :: res
    !
        res%rho = scalar * second%rho
        res%SatFracRho = scalar * second%SatFracRho
    !
    end function
    
    
    !> Multiply all components of SDV by the scalar
    elemental function StateDerivedVar_times_scalar2(first, scalar) result(res)
    type(KMStateDerivedVariables),intent(in) :: first
    double precision,intent(in)       :: scalar
    type(KMStateDerivedVariables) :: res
    !
        res = StateDerivedVar_times_scalar1(scalar, first)
    !
    end function


end module
