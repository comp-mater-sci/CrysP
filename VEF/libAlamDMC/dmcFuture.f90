!> Forward compatibility with libaltay
module dmcFuture
implicit none

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


end module
