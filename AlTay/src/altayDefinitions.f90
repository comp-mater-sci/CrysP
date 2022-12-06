!>Global definitions used in multiple modules within AlTay
module altay_definitions
    implicit none

    !>Standard real(dp)
    integer, parameter :: dp = selected_real_kind(15,307)

    !>\name Exit codes from altayHardLaw_DSH subroutines and functions:
    !>@{
    integer, parameter :: KS_OK                 = 0     !< OK
    integer, parameter :: KS_Error              = -1    !< General error (not covered by any specific error code).
    integer, parameter :: KS_ErrBadDims         = -2    !< At least one parameter out of boundaries
    integer, parameter :: KS_ErrBadValue        = -5    !< At least one input parameter has unacceptable value
    integer, parameter :: KS_ErrOutOfRange      = -6    !< At least one input parameter has a value outside acceptable range
    integer, parameter :: KS_ErrIO              = -15   !< Error during an IO operation
    integer, parameter :: KS_ErrNss             = -16   !< Unsupported number of slip systems proposed. Supported values are: 12, 24
    integer, parameter :: KS_ErrUninitialized   = -50   !< Call to module procedures without proper initialization of the module
    !>@}

    type :: StateDerivedVars
        real(dp) :: rho_CBs     = 0.D0 !<Dislocation density of cell boundaries; unit: m^(-2)
        real(dp) :: rho_CBBs    = 0.D0 !<Dislocation density of cell block boundaries; unit: m^(-2)
        real(dp) :: rho_polCBBs = 0.D0 !<Dislocation density of polarized dislocations at cell block boundaries; unit: m^(-2)
        real(dp) :: rho_avg     = 0.D0 !<Average dislocation density; unit: m^(-2)
    end type
        
    type :: CBBtype
        real(dp) :: RHOwd           = 0.D0
        real(dp) :: RHOwp           = 0.D0
        real(dp) :: RHOwdHOM        = 0.D0
        real(dp) :: accGAMMA_new    = 0.D0
        real(dp) :: RHOwd_ini       = 0.D0
    end type

    !> State variables for single grain
    type :: StatVar
        real(dp)                    :: RHOcb      = 0.D0
        type(CBBtype), dimension(6)         :: CBB
        integer, dimension(2)               :: ActiveCBB  = 0
        real(dp), dimension(2,24)   :: CRSS       = 0.D0 !Up to 24 slip systems supported
    end type StatVar


end module 
