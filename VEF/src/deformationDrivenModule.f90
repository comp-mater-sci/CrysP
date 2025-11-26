!> Base class for modules implementing strain-(rate) driven simulations (such as ADPModule)
module dmcDeformationDrivenModule
use criRange
use base_defs
use criUncomment, only: readValue
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

    contains
        procedure,pass(this) :: readConfig => DeformationDrivenModule_readConfig
    end type

contains

    !> Read configuration from IO unit
    integer function DeformationDrivenModule_readConfig(this,cnfunit) result(info)
        class(DeformationDrivenModule),intent(inout)   :: this
        integer,intent(in)              :: cnfunit !< IO input unit
        !
        logical :: default_solver_config
        !
        ! read output and AlTay configuration sections
        info = this%BasicModule%readConfig(cnfunit)
    end function
end module
