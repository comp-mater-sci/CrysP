!
! $Id$
!
module altaySimulation
use criErrcodes
use criAlgorithm
use altayConfig
use altayMaterial
use altayState
use altayStatePersistence
use altayStatePersistenceUtils
use altayMacroKinematic
#ifdef USE_ALTAYSIMUL
use altaySimul
#endif
implicit none



    !> Abstraction for functional objects
    !> that apply certain processing and possibly modify the state.
    type,abstract :: SimulationStep
        character(len=CNF_stepname_maxlen) :: name = ''
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
        !> \todo consider converting into a pointer
        type(InitializationStepConfig)          :: config
        
    contains
        procedure,pass(this)    :: initialize => InitializationStep_initialize
        procedure,pass(this)    :: run => InitializationStep_run
    end type
    


    
    !> 
    type,extends(SimulationStep) :: AnalysisStep
        !> \todo consider converting into a pointer
        type(AnalysisStepConfig)            :: config
        
        type(DeformationRate)               :: macro_deformation_rate
    contains
        procedure,pass(this)    :: initialize => AnalysisStep_initialize
        procedure,pass(this)    :: run => AnalysisStep_run

    end type

    
    type,extends(SimulationStep) :: OutputStep
        
        class(StatePersistenceScheme), pointer :: storage => null()
    contains
        procedure,pass(this)    :: initialize => OutputStep_initialize
        procedure,pass(this)    :: run => OutputStep_run

    end type
    
    
    

    type :: Simulation
        
        type(SimulationConfig)              :: config
        
        type(altayStateData)                :: state
        
        type(MaterialData)                  :: material
        
        ! Storage for persistent state variables
        class(StatePersistenceScheme), pointer :: output_storage
        
        !> Simulation steps. If allocated, the shape is [0:nsteps-1]
        type(PtrSimulationStep),dimension(:),allocatable    :: steps
        
        logical                             :: is_initialized = .false.
    
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
    class(Simulation),target,intent(inout)                 :: this
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
    integer :: i, j, ierr
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
        ! Initialize the material
        call initialize(this%material, this%config%material, info)
        if (info /= criSuccess) return
        !
        ! Pre-initialize the state
        info = altayStateData_init(this%state)
        if (info /= criSuccess) return
        this%state%old%material => this%material
        this%state%new%material => this%material

        !
        ! Initialize data persistency schemes
        ! info = criErr_BadArgs
#ifdef FIXME_ENABLE
        this%output_storage => statePersistenceFactory(this%config%state_output%odf_config, &
                                                       as_input=.false.)
#endif
        if ((info /= criSuccess) .or. .not. associated(this%output_storage)) return
        !
        ! Initialize the steps if provided in the configuration
        if (allocated(this%config%steps)) then
            info = criErr_MemAlloc
            allocate(this%steps(0:size(this%config%steps)-1), stat=ierr)
            if (ierr /= 0) return
            j = 0 ! index over steps
            ! Loop over step configuration
            do i = lbound(this%config%steps,dim=1), ubound(this%config%steps,dim=1)
                associate(cnf => this%config%steps(i), step => this%steps(j))
                    ! Call the factory
                    step%ptr => factory%createStep(cnf, this%config, j, this%output_storage, info)
                    if ((info /= criSuccess) .or. .not. associated(step%ptr)) exit

                end associate
                j = j + 1
            enddo

        endif
        if (info == criSuccess) this%is_initialized = .true.
    !
    end function
    
#define DYNAMIC_CAST(_T,_P,_CP) select type(_P);class is(_T); _CP => _P;class default; nullify(_CP);endselect
    
    !> Create a new step object from StepConfig.
    !> 
    !> To determine which type of step needs to be created,
    !> the members of the StepConfig object are examined in the order of
    !> declaration in the StepConfig type.
    !>
    !> New types of steps can be added by overriding/extending this 
    !> method in user-defined descendant of StepFactory class.
    !> \todo redesign the process of making steps. The implementation in 
    !>       StepFactory_createStep is awfully complicated and redundant.
    function StepFactory_createStep(this, step_config, config, step_id, storage, info) result(step)
    implicit none
    class(StepFactory),intent(inout)        :: this
    class(SimulationStep),pointer           :: step
    class(StepConfig),intent(in)            :: step_config
    class(SimulationConfig),intent(in)      :: config
    integer,intent(in)                      :: step_id
    class(StatePersistenceScheme),intent(in),pointer :: storage
    integer,intent(out)                     :: info
    !
    integer :: ierr
    !
        nullify(step)
        info = criErr_BadArgs
        ! Verify the assumptions on step_config
        if (.not. check(step_config)) return
        !
        ! OK, there is only one allocated component.
        ! So, only one of the conditional blocks will match.
        !
        if (allocated(step_config%assembly_step)) then
            return
        endif
        !
        if (allocated(step_config%initialization_step)) then
            block
                type(InitializationStep),pointer :: ptr_step
                nullify(ptr_step)
                allocate(InitializationStep :: ptr_step, stat=ierr)
                if ((ierr == 0) .and. associated(ptr_step)) &
                    info = ptr_step%initialize(step_config%initialization_step)
                step => ptr_step
            end block
        endif
        !
        if (allocated(step_config%analysis_step)) then
            block
                type(AnalysisStep),pointer :: ptr_step
                nullify(ptr_step)
                allocate(AnalysisStep :: ptr_step, stat=ierr)
                if ((ierr == 0) .and. associated(ptr_step)) &
                    info = ptr_step%initialize(step_config%analysis_step)
                step => ptr_step
            end block
        endif
        !
        if (allocated(step_config%output_step)) then
            block
                type(OutputStep),pointer :: ptr_step
                nullify(ptr_step)
                allocate(OutputStep :: ptr_step, stat=ierr)
                if ((ierr == 0) .and. associated(ptr_step)) &
                    info = ptr_step%initialize(step_config%output_step, storage)
                step => ptr_step
            end block
        endif
        ! Finalize initialization:
        if (associated(step)) then
            ! - name the step
            if (step_config%incremental_name) then
                step%name = trim(step_config%name) // adjustl(tostring(step_id, 3)) 
            else
                step%name = trim(step_config%name) 
            endif
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
        ! Main loop over the steps
        do i = lbound(this%steps, dim=1), ubound(this%steps, dim=1)
            associate(step => this%steps(i)%ptr)
                info = step%run(this%config, this%material, this%state)
            end associate
            if (info /= 0) exit
        enddo
    !
    end function


    logical function Simulation_isReady(this)
    implicit none
    class(Simulation),intent(in)     :: this
    !
        Simulation_isReady = this%is_initialized .and. allocated(this%steps)
    !
    end function


    integer function InitializationStep_initialize(this, config) result(info)
    implicit none
    class(InitializationStep),intent(inout)     :: this
    class(InitializationStepConfig),intent(in)  :: config
    !
        this%config = config
        info = criSuccess
    !
    end function


    
    integer function InitializationStep_run(this, config, material, state) result(info)
    implicit none
    class(InitializationStep),intent(inout)     :: this
    class(SimulationConfig),intent(in)          :: config
    type(MaterialData),intent(inout)            :: material
    class(altayStateData),intent(inout)         :: state
    !
    class(StatePersistenceScheme), pointer  :: storage
#ifdef USE_ALTAYSIMUL
    ! Use old SIMUL
    type(altayConfigData)   :: old_config
#endif
    !
        !> \fixme The configuration should not deduce anything from the odf representation.
#ifdef FIXME_ENABLE
        storage => statePersistenceFactory(this%config%input%odf_config, as_input=.true.)
#endif
        if (associated(storage)) then
            call storage%loadState(state%old, info)
            deallocate(storage)
        endif
        if (info /= criSuccess) return
        info = altayStateData_assemble(state)
        !
#ifdef USE_ALTAYSIMUL
        call openStandardOutputFiles(config%jobtitle, info)
        ! Translate to old config...
        old_config = translateConfig(config)
        ! Make initalization call
        CALL SIMUL(old_config, state, material, 0, 0, 1)
#endif
    !
    end function


    !> Set up analysis step
    integer function AnalysisStep_initialize(this, config) result(info)
    implicit none
    class(AnalysisStep),intent(inout)           :: this
    class(AnalysisStepConfig),intent(in)        :: config
    !
        this%config = config
        call Set_DeformationRate(this%config%input,this%macro_deformation_rate)
        info = criSuccess
    !
    end function


    
    integer function AnalysisStep_run(this, config, material, state) result(info)
    implicit none
    class(AnalysisStep),intent(inout)           :: this
    class(SimulationConfig),intent(in)          :: config
    type(MaterialData),intent(inout)            :: material
    class(altayStateData),intent(inout)         :: state
    !
#ifdef USE_ALTAYSIMUL
    ! Use old SIMUL
    type(altayConfigData)   :: old_config
    !
        ! Translate to old config...
        old_config = translateConfig(config)
        !
        CALL SIMUL(old_config, state, material, this%config%nincrements, 1, 1, this%macro_deformation_rate)
        info = criSuccess
#else
        ! Main loop over the clusters
#endif
    !
    end function


    !> Set up output step
    integer function OutputStep_initialize(this, config, storage) result(info)
    implicit none
    class(OutputStep),intent(inout)           :: this
    class(OutputStepConfig),intent(in)        :: config
    class(StatePersistenceScheme),intent(in),pointer :: storage
    !
        this%storage => storage
        info = criSuccess
    !
    end function
            
    integer function OutputStep_run(this, config, material, state) result(info)
    implicit none
    class(OutputStep),intent(inout)             :: this
    class(SimulationConfig),intent(in)          :: config
    type(MaterialData),intent(inout)            :: material
    class(altayStateData),intent(inout)         :: state
    !
        info = criSuccess
        if (associated(this%storage)) call this%storage%saveState(state%new, info)
    !
    end function


        
end module
