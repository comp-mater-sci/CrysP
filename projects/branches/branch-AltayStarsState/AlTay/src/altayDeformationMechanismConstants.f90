! $Id$
!
!> Named constants and types for deformation mechanisms
module altayDeformationMechanismConstants
implicit none

    !>@{ \name Public identifiers for initialization of DeformationMechanismData object
    integer,parameter :: DM_none = -1 !< Invalid deformation mechanism
    !> FCC (111)<110>, through altayDeformationMechanismData_preconfigured 
    !> objects
    integer,parameter :: DM_fcc12 = 1

    !> BCC (110)<111> + (112)<111>, through altayDeformationMechanismData_preconfigured 
    !> objects
    integer,parameter :: DM_bcc24 =   2 
    
    !> BCC (110)<111> + (112)<111> + (123)<111>, through 
    !> altayDeformationMechanismData_preconfigured objects
    integer,parameter :: DM_bcc48 = 3 
    
    integer,parameter :: DM_user =99 !< DM_user (currently not exploited).
    
    !> Initialization from file of PRE file format
    integer,parameter :: DM_format_pre = 101

    !> Initialization from file of PRE file format
    integer,parameter :: DM_format_dat = 102
    !>@}

end module
