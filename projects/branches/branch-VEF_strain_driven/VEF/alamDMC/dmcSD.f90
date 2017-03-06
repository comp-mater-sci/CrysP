!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2017-02-13
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

!> Base class for modules implementing strain-(rate) driven simulations
module dmcSD
use criRange
use criErrcodes
use dmcBasicModule
use dmcStrainDrivenStep
implicit none


    !> Configuration related to strain(-rate) driven simulations.
    !> Currently a placeholder.
    type :: StrainDrivenSolverConfig
        ! Empty
    end type

    !> Wrapper around a polymorphic pointer to StrainDrivenStep objects
    type :: PtrStrainDrivenStep
        class(StrainDrivenStep),pointer :: step
    end type
    

    !> Base class for strain-(rate) driven simulations modules
    type,extends(BasicModule) :: SDModule

        !> Optional solver settings
        class(StrainDrivenSolverConfig),pointer :: solver_config => null()

        !> Container for steps 
        type(PtrStrainDrivenStep),dimension(:),allocatable :: steps
        
    contains
        !>@{ \name Interface methods of AbstractModule
        
        procedure,pass(this) :: printConfig => SDModule_printConfig
        
        procedure,pass(this) :: readConfig => SDModule_readConfig
        
        !>@}
        
    end type


contains


    !> Print configuration to IO unit
    integer function SDModule_printConfig(this,outunit) result(info)
    implicit none
    class(SDModule),intent(in)      :: this
    integer,intent(in)              :: outunit !< IO unit for output
    !
        info = this%BasicModule%printConfig(outunit)
        if (associated(this%solver_config)) then
            ! print solver config
            continue
        endif
    !
    end function


    !> Read configuration from IO unit
    integer function SDModule_readConfig(this,cnfunit) result(info)
    implicit none
    class(SDModule),intent(inout)   :: this
    integer,intent(in)              :: cnfunit !< IO input unit
    !
    logical :: default_solver_config
    !
        info = this%BasicModule%readConfig(cnfunit)
        ! Read "solver config flag" that belongs to the global section
        ! as it is done in the stressDrivenModule.
        if (.not. readValue(cnfunit, default_solver_config)) return
        !
        ! For the time being, only default solver config is accepted.
        if (.not. default_solver_config) then
            write(display_unit, fmt=900) 'This module does not allow non-default solver settings'
            info = criErr_BadArgs
        endif
        !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    !
    end function

end module
