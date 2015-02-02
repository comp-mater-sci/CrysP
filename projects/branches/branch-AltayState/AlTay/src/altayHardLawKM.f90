! $Id$
    
!     v1.0 by P. Eyckens, MTM, KU Leuven, 22 Jan 2015.
!     v2.0 by J. Gawad, CS, KU Leuven, 27 Jan 2015
!     v2.1 by J. Gawad, CS, KU Leuven, 1 Feb 2015
!
!> Hardening law: KocksMecking dislocation-based isotropic hardening.
!>
!> The prefix added to the components of this module is `KM`, which
!> stands for Kocks-Mecking.
module altayHardLaw_KM
use criErrcodes
use altayCRSSTypes
implicit none
private

    ! Fundamental interface of the module. The host/user code should use these
    ! functions for operations on the data types defined in this module.
    public                          &
        KMConfig_read,              &
        KMParameters_init,          &
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
    type,public :: KMConfig
        double precision :: b = 1.D-10  !< Burgers vector
        double precision :: G = 1.D0    !< Shear modulus
        double precision :: alfa = 1.D0 !< Proportionality factor alpha
        double precision :: tau0 = 1.D0 !< Reference shear stress
        double precision :: I = 0.D0
        double precision :: R = 0.D0
        double precision :: rho_ann = 0.D0
    end type

    !> Parameters of the Kocks-Mecking hardening law.
    type,public :: KMParameters
        type(KMConfig)   :: base
        double precision :: rho_sat = 0.D0
    end type

    !> State variables to be stored per single crystal.
    type,public :: KMStateVariables
        !> Dislocation density in grain; unit: m^(-2)
        double precision :: rho  = 0.D0
    end type


    !> State-derived variables per single crystal.
    type,public :: KMStateDerivedVariables
        !> Dislocation density in grain; unit: m^(-2)
        double precision :: rho = 0.D0
        !> Saturation fraction of dislocation density, in percentage (%)
        double precision :: SatFracRho = 0.D0
    end type


    !> Initialization procedure of the KMParameters type.
    interface KMParameters_init
        module procedure KMParameters_initFromFile, &
                         KMParameters_initFromConfig
    end interface


    integer,parameter :: KM_nslipsystems = 12

    double precision,parameter :: TENpow6 = 1.D6


contains

    !==========================================================================
    !
    ! Parameters and configuration
    !
    !==========================================================================
    
    
    !> Initialize config parameters from a configuration file.
    integer function KMParameters_initFromFile(this, inunit) result(info)
    implicit none
    type(KMParameters),intent(out)  :: this
    integer,intent(in)              :: inunit
    !
    type(KMConfig) :: config
    !
        info = KMConfig_read(config, inunit)
        if (info == criSuccess) info = KMParameters_initFromConfig(this, config)
    !
    end function
    
    !> Initialize config parameters from a KMConfig object.
    integer function KMParameters_initFromConfig(this, config) result(info)
    implicit none
    type(KMParameters),intent(out)    :: this
    type(KMConfig),intent(in)               :: config
    !
    !Check the input parameters                               ! Units of input parameters:
        if (config%b    >  0.    .AND. config%b    <= 1.e-8    .AND.& ! [m]
            config%G    >= 10.e3 .AND. config%G    <= 500.e3   .AND.& ! [MPa]
            config%alfa >  0.    .AND. config%alfa <= 5.       .AND.& ! [/]
            config%tau0 >= 0.    .AND. config%tau0 <= 1.e4     .AND.& ! [MPa]
            config%I    >= 0.    .AND. config%I    <= 10.      .AND.& ! [/]
            config%R    >  0.    .AND. config%R    <= 1.e-6    .AND.& ! [m]
            config%rho_ann >  0. .AND. config%rho_ann < config%I**2/config%R**2 & ! [m^(-2)]  !! i.e. rho_ann < saturation stress
        ) then
            this%base = config
            !change of units if different (units of this are: MPa; nm(nanometer))
            this%base%b         = config%b       * TENpow6       ![m] -> [nm]
            this%base%R         = config%R       * TENpow6       ![m] -> [nm]
            this%base%rho_ann   = config%rho_ann * TENpow6**(-2) ![m^(-2)] -> [nm^(-2)] 
            !Calculate dependent parameters
            this%rho_sat= this%base%I**2 / this%base%R**2
            !
            info = criSuccess
        else
            info = criErr_BadArgs
        endif
    !
    end function
   
    !> Read independent components of KMParameters from the IO
    !> 
    !> \note This is a reference procedure. The client code may use a different
    !>       format.
    integer function KMConfig_read(this, inunit) result (info)
    implicit none
    type(KMConfig),intent(out)  :: this    !< parameters to be read from a formatted file.
    integer,intent(in)          :: inunit   !< IO unit number
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

    !==========================================================================
    !
    ! State variables
    !
    !==========================================================================


    !> Initialize state variables from configuration object
    subroutine KMStateVariables_init(this, params, info)
    implicit none
    type(KMStateVariables),intent(out)  :: this
    type(KMParameters),intent(in)       :: params
    integer,intent(out)                 :: info
    !
        this%rho  = params%base%rho_ann
        info = criSuccess
    !
    end subroutine

    
    !> Calculate CRSS from state variables and configuration object
    subroutine KMStateVariables_getCRSS(this, params, crss, info)
    implicit none
    type(KMStateVariables),intent(in)   :: this
    type(KMParameters),intent(in)           :: params
    type(CRSSData),intent(inout)        :: crss
    integer,intent(out)                 :: info
    !
    double precision :: crss_Tay
    !
        if (CRSSData_size(crss) >= KM_nslipsystems) then
            crss_Tay= params%base%tau0 + params%base%alfa * params%base%G * params%base%b * sqrt(this%rho)
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
    type(KMParameters),intent(in)         :: params
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
            this%rho = F_KocksMeck(previous%rho, gamma, params%base%I, &
                                   params%base%R, params%base%b) 
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
    
    !> Calculate homogenized state-derived variables
    pure subroutine KMStateDerivedVariables_homogenize(this, values, weights, info)
    implicit none
    type(KMStateDerivedVariables),intent(out)               :: this
    !> Input state-derived viables
    type(KMStateDerivedVariables),dimension(:),intent(in)   :: values
    !> weights (must be of the same size as the `values`
    double precision,dimension(:),intent(in)                :: weights  
    integer,intent(out)                                     :: info
    !
    double precision :: iws ! reciprocal of the sum of weights
    !
        info = criErr_BadDims
        iws = sum(weights)
        if ((size(values) /= size(weights)) .or. (iws < epsilon(0.0))) return
        ! Do the homogenization: weighted averaging
        iws = 1.D0 / iws
        this%rho = iws * dot_product(weights, values(:)%rho)
        this%SatFracRho = iws * dot_product(weights, values(:)%SatFracRho)
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
    integer :: i
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
    type(KMParameters),intent(in)               :: params
    integer,intent(out)                         :: info !< Exit code
    !
        this%rho = state%rho * TENpow6**2 !unit conversion [nm^(-2)] -> [m^(-2)]
        this%SatFracRho = 100. * (state%rho-params%base%rho_ann) / (params%rho_sat-params%base%rho_ann) !unit: %
        info = criSuccess
    !
    end subroutine

end module
