!> Base class for modules implementing strain-(rate) driven simulations (such as ADPModule)
module dmcDeformationDrivenModule
use criRange
use criErrcodes
use criUncomment, only: readValue
use dmcUtils, only: display_unit
use dmcBasicModule
use dmcStrainDrivenStep
implicit none

    public :: StrainDrivenSolverConfig, DeformationDrivenModule
    private

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
    type,extends(BasicModule) :: DeformationDrivenModule

        !> Optional solver settings
        class(StrainDrivenSolverConfig),pointer :: solver_config => null()

        !> Container for steps
        type(PtrStrainDrivenStep),dimension(:),allocatable :: steps

    contains !MB: type-bound procedures
        !>@{ \name Interface methods of AbstractModule

        procedure,pass(this) :: printConfig => DeformationDrivenModule_printConfig

        procedure,pass(this) :: readConfig => DeformationDrivenModule_readConfig

        !>@}

    end type


contains


    !> Print configuration to IO unit
    integer function DeformationDrivenModule_printConfig(this,outunit) result(info)
    class(DeformationDrivenModule),intent(in)      :: this
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
    integer function DeformationDrivenModule_readConfig(this,cnfunit) result(info)
    class(DeformationDrivenModule),intent(inout)   :: this
    integer,intent(in)              :: cnfunit !< IO input unit
    !
    logical :: default_solver_config
        !
        ! read output and AlTay configuration sections
        info = this%BasicModule%readConfig(cnfunit)
        !
        ! Read "solver config flag" that belongs to the global section
        ! as it is done in the stressDrivenModule.
        if (.not. readValue(cnfunit, default_solver_config)) return
        ! For the time being, only default solver configuration is accepted for this module.
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
