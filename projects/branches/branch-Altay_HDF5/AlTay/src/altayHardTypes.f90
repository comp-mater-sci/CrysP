!> Provides common data types and constants to be used by various hardening laws.
module altayHardTypes
      
    !> Unspecified or not unimplemented hardening law.
    integer,parameter :: hard_invalid = -1
      
    !> Material with no hardening of slip systems
    !> hard_none results in setting CRSS == 1.0 for all slip systems (independent of the inputted "crss_ratios"):
    integer,parameter :: hard_none =          0
      
    !Following identifiers invoke module altayHardLaw_Simple for the reference-crss "RefTau".
    ! CRSS for each individual slip system is multiplied with inputted "crss_ratios". 
    ! Note that "RefTau" is work-equivalent to the total slip rate ONLY IF all "crss_ratios" == 1.
    integer,parameter :: hard_voce =          1, &
                         hard_swiftK =        2, &
                         hard_swiftS =        3

    !> Kocks-Mecking hardening model. It requires calculations of CRSS on
    !> individial slip systems.
    integer,parameter :: hard_KM = 5
#ifdef PEBP_ENABLED
    !Following identifiers invoke module altayHardLaw_DSH, resulting in generally different CRSS for the slip systems.
    ! The reference-crss "RefTau" is arbitrarily set to 1.
    integer,parameter :: hard_BP =           11, &
                         hard_PEBPscrew =    12, &
                         hard_PEBPloop =     13
#endif
end module