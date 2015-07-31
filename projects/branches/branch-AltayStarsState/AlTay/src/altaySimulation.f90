!
! $Id$
!
module altaySimulation
use criErrcodes
use altayConfig
use altayMaterialTypes
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
            import :: SimulationStep, StepConfig, altayStateData, MaterialData
            class(SimulationStep),intent(inout)     :: this
            class(StepConfig),intent(in)            :: config
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
        
        !procedure,pass(this) :: createStep => Simulation_createStep
        
        procedure,pass(this) :: isReady => Simulation_isReady

        !procedure,pass(this) :: setSteps => Simulation_setSteps

        procedure,pass(this) :: runSteps => Simulation_runSteps
        
    end type


contains


    integer function Simulation_initialize(this,config) result(info)
    implicit none
    class(Simulation),intent(inout)         :: this
    class(SimulationConfig),intent(in)      :: config
    ! Initialize state variables:
    !   - get state data from the storage, OR
    !   - get part of the state from storage, generate other
    !   - make all assemblies from the state variables
    ! Initialize runtime components:
    !   - output request filters 
    !
        this%config = config
        info = criError
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
                ! info = step%run(this%config, this%material, this%state)
            end associate
        enddo
            
    ! for step in steps:
        ! prepare the step data + state variables
        ! run calculations
        ! save state variables to storage (if requested)
        ! put SV and SDV through the output request filters
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
    class(StepConfig),intent(in)                :: config
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
    class(StepConfig),intent(in)                :: config
    type(MaterialData),intent(inout)            :: material
    class(altayStateData),intent(inout)         :: state
    !
        info = criError
    !
    end function


            
    integer function OutputStep_run(this, config, material, state) result(info)
    implicit none
    class(OutputStep),intent(inout)             :: this
    class(StepConfig),intent(in)                :: config
    type(MaterialData),intent(inout)            :: material
    class(altayStateData),intent(inout)         :: state
    !
        info = criError
    !
    end function


        
end module
