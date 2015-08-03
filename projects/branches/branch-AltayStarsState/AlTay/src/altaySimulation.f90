!
! $Id$
!
module altaySimulation
use criErrcodes
use altayConfig
use altayMaterial
use altayState
use altayStatePersistence
use altayStatePersistenceUtils

implicit none



    !> Abstraction for functional objects
    !> that apply certain processing and possibly modify the state.
    type,abstract :: SimulationStep
    contains
        procedure(SimulationStep_run_interface),pass(this),deferred :: run
    end type


    abstract interface
    
        integer function SimulationStep_run_interface(this, config, material, state) result(info)
            import :: SimulationStep, SimulationConfig, altayStateData, MaterialData
            class(SimulationStep),intent(inout)     :: this
            class(SimulationConfig),intent(in)      :: config
            type(MaterialData),intent(inout)        :: material
            class(altayStateData),intent(inout)     :: state
        end function
        
    end interface

    type :: PtrSimulationStep
        class(SimulationStep),pointer    :: ptr => null()
    end type


    !type :: SimulationStepFactory
    !    procedure,pass(this)    :: create
    !end type
    

    !> InitializationStep sets up state variables. It allows (expects) the state variables
    !> to be in uninitialized state.
    type,extends(SimulationStep) :: InitializationStep
        
        class(StatePersistenceScheme), pointer :: storage => null()

    contains
        procedure,pass(this) :: run => InitializationStep_run
    end type
    

    
    !> 
    type,extends(SimulationStep) :: AnalysisStep
        
    contains
        procedure,pass(this) :: run => AnalysisStep_run

    end type

    
    type,extends(SimulationStep) :: OutputStep
        
        class(StatePersistenceScheme), pointer :: storage => null()
    contains
        procedure,pass(this) :: run => OutputStep_run

    end type
    
    
    

    type :: Simulation
        
        type(SimulationConfig)              :: config
        
        type(altayStateData)                :: state
        
        type(MaterialData)                  :: material
        
        ! Storage for persistent state variables
        class(StatePersistenceScheme), pointer :: input_storage, output_storage

        
        type(PtrSimulationStep),dimension(:),allocatable    :: steps
        
    contains
    
        procedure,pass(this) :: initialize => Simulation_initialize
        
        ! procedure,pass(this) :: createStep => Simulation_createSteps
        
        procedure,pass(this) :: isReady => Simulation_isReady

        !procedure,pass(this) :: setSteps => Simulation_setSteps

        procedure,pass(this) :: runSteps => Simulation_runSteps
        
    end type

    
    type :: StepFactory
    contains
        procedure,pass(this)    :: createStep => StepFactory_createStep
    end type

contains


    integer function Simulation_initialize(this, config, step_factory) result(info)
    implicit none
    class(Simulation),intent(inout)                 :: this
    class(SimulationConfig),intent(in)              :: config
    class(StepFactory),intent(inout),target,optional:: step_factory
    ! Initialize state variables:
    !   - get state data from the storage, OR
    !   - get part of the state from storage, generate other
    !   - make all assemblies from the state variables
    ! Initialize runtime components:
    !   - output request filters 
    !
    class(StepFactory),pointer :: factory
    integer :: i, j
    !
        info = criErr_BadArgs
        if (present(step_factory)) then
            factory => step_factory
        else
            allocate(StepFactory :: factory)
        endif
        !
        this%config = config
        !
        ! Initialize data persistency schemes
        
        !
        ! Initialize the material
        call initialize(this%material, config%material, info)
        !
        ! Initialize the steps if provided in the configuration
        if (allocated(config%steps)) then
            allocate(this%steps(size(config%steps)))
            j = 0
            do i = lbound(config%steps,dim=1), ubound(config%steps,dim=1)
                if (check(config%steps(i))) then
                    j =j + 1
                    this%steps(j)%ptr => factory%createStep(config%steps(i), config, info)
                    if (info /= criSuccess) exit
                endif
            enddo
        endif
    end function
    
    
    !> Create a new step object from StepConfig.
    !> 
    !> To determine which type of step needs to be created,
    !> the members of the StepConfig object are examined in the order of
    !> declaration in the StepConfig type.
    !>
    !> New types of steps can be added by overriding/extending this 
    !> method in user-defined descendant of StepFactory class.
    function StepFactory_createStep(this, step_config, config, info) result(step)
    implicit none
    class(StepFactory),intent(inout)        :: this
    class(SimulationStep),pointer           :: step
    class(StepConfig),intent(in)            :: step_config
    class(SimulationConfig),intent(in)      :: config
    integer,intent(out)             :: info
    !
        nullify(step)
        info = criErr_BadArgs
        if (allocated(step_config%assembly_step)) then
            return
        endif
        !
        if (allocated(step_config%initialization_step)) then
            allocate(InitializationStep :: step)
            ! info = step%initialize(step_config%initialization_step)
            return
        endif
        !
        if (allocated(step_config%analysis_step)) then
            allocate(AnalysisStep :: step)
            return
        endif
        !
        if (allocated(step_config%output_step)) then
            return
        endif
    !
    end function
    
    integer function Simulation_runSteps(this) result(info)
    implicit none
    class(Simulation),intent(inout)     :: this
    !
    integer :: i
    !
        info = criErr_BadArgs
        if (.not. this%isReady()) return
        
        do i = 1, size(this%steps)
            associate(step => this%steps(i)%ptr)
                info = step%run(this%config, this%material, this%state)
            end associate
        enddo
    !
    end function


    logical function Simulation_isReady(this)
    implicit none
    class(Simulation),intent(in)     :: this
    !
        Simulation_isReady = .false.
    !
    end function


    integer function InitializationStep_run(this, config, material, state) result(info)
    implicit none
    class(InitializationStep),intent(inout)     :: this
    class(SimulationConfig),intent(in)          :: config
    type(MaterialData),intent(inout)            :: material
    class(altayStateData),intent(inout)         :: state
    !
        info = altayStateData_init(state)

        ! call initialize(material, config, info)
        
    !
    end function

    
    integer function AnalysisStep_run(this, config, material, state) result(info)
    implicit none
    class(AnalysisStep),intent(inout)           :: this
    class(SimulationConfig),intent(in)          :: config
    type(MaterialData),intent(inout)            :: material
    class(altayStateData),intent(inout)         :: state
    !
        info = criError
    !
    end function


            
    integer function OutputStep_run(this, config, material, state) result(info)
    implicit none
    class(OutputStep),intent(inout)             :: this
    class(SimulationConfig),intent(in)          :: config
    type(MaterialData),intent(inout)            :: material
    class(altayStateData),intent(inout)         :: state
    !
        info = criError
    !
    end function


        
end module
