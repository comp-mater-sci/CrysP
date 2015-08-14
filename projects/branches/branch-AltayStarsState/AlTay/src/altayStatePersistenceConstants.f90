!
! $Id$
!

module altayStatePersistenceConstants
implicit none

    !> \name Named constants for access modes
    !>@{
    integer,parameter :: StatePersistence_Read = 1   !< Read-only (loadState available)
    integer,parameter :: StatePersistence_Write = 2  !< Write-only (savaState available)
    integer,parameter :: StatePersistence_Append = 3 !< Contribute (loadState + savaState)
    !>@}
    integer,parameter,private :: n_access_modes = 3
    integer,dimension(n_access_modes),parameter :: StatePersistence_AccessModes = &
                                                            [StatePersistence_Read, &
                                                             StatePersistence_Write, &
                                                              StatePersistence_Append ]
end module
