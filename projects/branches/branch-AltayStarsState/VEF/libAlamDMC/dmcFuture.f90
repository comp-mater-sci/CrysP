! $Id: dmcFuture.f90 2851 2017-03-07 14:07:47Z jgawad $
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2016-03-17
!>    $Revision: 2851 $
!>    $Date: 2017-03-07 15:07:47 +0100 (Tue, 07 Mar 2017) $
!>
!>    History of modifications: (see svn log)
!
!
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