!> Provides common data types and constants to be used by various hardening laws.
module hardening_types
    use altay_definitions

    implicit none
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
end module hardening_types
