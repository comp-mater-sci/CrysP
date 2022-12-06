!> Provides common data types and constants to be used by various hardening laws.
module altayHardTypes
    use altay_definitions, only: dp

      !> Representation of Critical Resolved Shear Stresses
      !>
      !> Shape: [2,96]:
      !> The first index is for direction of slip system: positive and negative
      !> (in that order).
      !> The second index is sequence number of pre-defined deformation systems
      !> (either slips or twinnings).
      type :: CRSS
          real(dp),dimension(2,96)      :: crss = 1.D0
      end type

      !> Unspecified or not unimplemented hardening law.
      integer,parameter :: hard_invalid = -1

      !> Material with no hardening of slip systems
      !> hard_none results in setting CRSS == 1.0 for all slip systems (independent of the inputted "crss_ratios"):
      integer,parameter :: hard_none =          0

      !Following identifiers invoke module altayHardLaw_Simple for the reference-crss "RefTau".
      ! CRSS for each individual slip system is multiplied with inputted "crss_ratios".
      ! Note that "RefTau" is work-equivalent to the total slip rate ONLY IF all "crss_ratios" == 1.
      integer,parameter :: hard_voce =          1, &
                           hard_swiftK =        2, &   !MB: Swift law with K-factor :: TAU = K * (gamma0+GAMMA)**n
                           hard_swiftS =        3      !MB: Swift law with initial crsS :: TAU = crss0 * (1.+GAMMA/gammaA0)**n

      !Following identifiers invoke module altayHardLaw_DSH, resulting in generally different CRSS for the slip systems.
      ! The reference-crss "RefTau" is arbitrarily set to 1.
      integer,parameter :: hard_BP =           11, &
                           hard_PEBPscrew =    12, &
                           hard_PEBPloop =     13
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
        type(CBBtype), dimension(6) :: CBB
        integer, dimension(2)       :: ActiveCBB  = 0
        real(dp), dimension(2,24)   :: CRSS       = 0.D0 !Up to 24 slip systems supported
    end type StatVar

    !> BP model parameters including saturation and minimum values for state dependent dislocation densities
    type :: PAR
        real(dp) :: b, G, alfa, f, tau0
        real(dp) :: I, R, Iwd, Rwd, Rncg, beta1, beta2
        real(dp) :: Iwp, Rwp, Rrev, R2
        real(dp) :: RHOcbSAT, RHOwdSAT, RHOwpSAT
        real(dp) :: RHOcbMIN, RHOwdMIN, RHOwpMIN
        real(dp) :: RHOwpLOW
    end type

end module
