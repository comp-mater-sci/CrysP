!> Provides common data types and constants to be used by various hardening laws.
module hardening_types
    use definitions

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

end module hardening_types
