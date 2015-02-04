! $Id$
    
!     v1.0 by P. Eyckens, MTM, KU Leuven, 22 Jan 2015.
!     v2.0 by J. Gawad, CS, KU Leuven, 27 Jan 2015
!     v2.1 by J. Gawad, CS, KU Leuven, 1 Feb 2015
!     v2.2 by P. Eyckens, MTM, KU Leuven, 4 Feb 2015
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
    !> Units: MPa, s, m (meter)
    type,public :: KMConfig
        double precision :: b = 0.D0      !< Magnitude of Burgers vector
        double precision :: G = 0.D0      !< Shear modulus
        double precision :: alfa = 0.D0   !< Dislocation interaction parameter
        double precision :: tau0 = 0.D0   !< Lattice friction stress
        double precision :: I = 0.D0      !< Immobilization coefficient
        double precision :: R = 0.D0      !< Recovery coefficient
        double precision :: rho_ann = 0.D0!< Annealed state dislocation density 
    end type

    !> Parameters of the Kocks-Mecking hardening law.
    !> Units: MPa, s, micrometer
    type,public :: KMParameters
        double precision :: b = 0.D0      !< Magnitude of Burgers vector
        double precision :: alfaGb = 0.D0 !< alfa*G*b
        double precision :: tau0 = 0.D0   !< Lattice friction stress
        double precision :: I = 0.D0      !< Immobilization coefficient
        double precision :: R = 0.D0      !< Recovery coefficient        
        double precision :: rho_ann = 0.D0!< Annealed state dislocation density        
        double precision :: rho_sat = 0.D0!< Saturation dislocation density
    end type

    !> State variables of the crystal.
    type,public :: KMStateVariables
        !> Dislocation density in grain; unit: micrometer^(-2)
        double precision :: rho  = 0.D0
    end type

    !> State-derived variables of the crystal.
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
    
    
    !> Initialize KMParameters object from a configuration file.
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
    
    !> Initialize KMParameters object from a KMConfig object.
    integer function KMParameters_initFromConfig(this, config) result(info)
    implicit none
    type(KMParameters),intent(out)    :: this
    type(KMConfig),intent(in)         :: config
    !
    !Check the input parameters                               ! Units of input parameters:
        if (config%b    >  0.    .AND. config%b    <= 1.e-8    .AND.& ! [m]
            config%G    >= 10.e3 .AND. config%G    <= 500.e3   .AND.& ! [MPa]
            config%alfa >  0.    .AND. config%alfa <= 5.       .AND.& ! [/]
            config%tau0 >= 0.    .AND. config%tau0 <= 1.e4     .AND.& ! [MPa]
            config%I    >= 0.    .AND. config%I    <= 10.      .AND.& ! [/]
            config%R    >  0.    .AND. config%R    <= 1.e-6    .AND.& ! [m]
            config%rho_ann >  0. .AND. config%rho_ann < config%I**2/config%R**2 & ! [m^(-2)]  
                                      !config%rho_ann < saturation stress
        ) then
            this%b = config%b * TENpow6 ![m] -> [micrometer]
            this%alfaGb = config%alfa * config%G * config%b * TENpow6 ![MPa.m] -> [MPa.micrometer]
            this%tau0 = config%tau0 ![MPa] -> [MPa]
            this%I = config%I ![/] -> [/]
            this%R = config%R * TENpow6 ![m] -> [micrometer]
            this%rho_ann = config%rho_ann * TENpow6**(-2) ![m^(-2)] -> [micrometer^(-2)] 
            this%rho_sat = this%I**2 / this%R**2
            !
            info = criSuccess
        else
            info = criErr_BadArgs
        endif
    !
    end function
   
    !> Read components of KMConfig from the IO
    integer function KMConfig_read(this, inunit) result (info)
    implicit none
    type(KMConfig),intent(out)  :: this     !< Configuration parameters to be read from a formatted file.
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


    !> Initialize state variables from KMParameters object
    subroutine KMStateVariables_init(this, params, info)
    implicit none
    type(KMStateVariables),intent(out)  :: this
    type(KMParameters),intent(in)       :: params
    integer,intent(out)                 :: info
    !
        this%rho = params%rho_ann
        info = criSuccess
    !
    end subroutine

    
    !> Calculate CRSS from state variables and configuration object
    subroutine KMStateVariables_getCRSS(this, params, crss, info)
    implicit none
    type(KMStateVariables),intent(in)   :: this
    type(KMParameters),intent(in)       :: params
    type(CRSSData),intent(inout)        :: crss
    integer,intent(out)                 :: info
    !
    double precision :: crss_Tay
    !
        if (CRSSData_size(crss) >= KM_nslipsystems) then
            !> Taylor equation
            crss_Tay= params%tau0 + params%alfaGb * sqrt(this%rho)
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
    type(KMParameters),intent(in)               :: params
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
            this%rho = F_KocksMeck(previous%rho, gamma, params%I, &
                                   params%R, params%b) 
        endif
        info = criSuccess
        return
    !
    contains


        !> Kocks-Mecking time integration function.
        !> It returns rho_b, the value of rho at the end of an interval (a,b) 
        !> for the following differential equation:
        !>
        !> d(rho)   1
        !> ------ = - * ( II*sqrt(rho) - RR*rho )
        !>  d(g)    b
        !>
        double precision function F_KocksMeck(rho_a, delta_g, II, RR, b) 
        !> The value of rho at the start of the interval (a,b)
        double precision,intent(in) :: rho_a
        !> delta_g = g_b - g_a, the increment in g during the interval (a,b)
        double precision,intent(in) :: delta_g
        double precision,intent(in) :: II, RR, b
        double precision :: x
        !
            x=exp(-0.5D0*RR*delta_g/b)
            x=II/RR*(1.D0-x)+sqrt(rho_a)*x
            F_KocksMeck=x*x
        !
        end function F_KocksMeck
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


    !> Calculate state-derived variables
    elemental subroutine KMStateDerivedVariables_calculate(this,state,params,info)
    type(KMStateDerivedVariables),intent(out)   :: this
    type(KMStateVariables),intent(in)           :: state
    type(KMParameters),intent(in)               :: params
    integer,intent(out)                         :: info !< Exit code
    !
        this%rho = state%rho * TENpow6**2 !unit conversion [micrometer^(-2)] -> [m^(-2)]
        this%SatFracRho = 100. * (state%rho-params%rho_ann) / (params%rho_sat-params%rho_ann) !unit: %
        info = criSuccess
    !
    end subroutine


    !> Output state-derived variables or/and a header line.
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
        100 format(T3,'rho[m^(-2)]',T17,'SatFracRho[%]')
        101 format(E13.4,1X,F15.2)
    !
    end function

        
    !> Homogenize state-derived variables
    pure subroutine KMStateDerivedVariables_homogenize(this, values, weights, info)
    implicit none
    type(KMStateDerivedVariables),intent(out)               :: this
    !> Input state-derived viables
    type(KMStateDerivedVariables),dimension(:),intent(in)   :: values
    !> weights (must be of the same size as the `values`)
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

    
end module
