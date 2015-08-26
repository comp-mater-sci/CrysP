!
! $Id$
!

!> Generic configuration reader
module altayConfigReader
use criErrcodes
use altayConfig
use altayIOContext
implicit none

    !> Abstract config reader
    type,abstract :: ConfigReader
        
    contains
        procedure(ConfigReader_read_interface),pass(this),deferred :: read
        
    end type

    abstract interface
    
        integer function ConfigReader_read_interface(this, config)
        import :: ConfigReader, SimulationConfig
        class(ConfigReader),intent(inout)       :: this
        class(SimulationConfig),intent(out)      :: config
        end function
    
    end interface

    !> Reader of configuration files given in fixed-position text format. 
    type,extends(ConfigReader) :: altayInputConfigReader
        type(RawFileContext)        :: context
    contains
    
#ifdef TESTING_ENABLED
        ! procedure :: read => altayInputConfigReader_read
        ! procedure :: read => altayInputConfigReader_fakeread_dual
        procedure :: read => altayInputConfigReader_fakeread_single
#else
        !> \fixme Use actual read in place of fakeread
        procedure :: read => altayInputConfigReader_read
#endif
        procedure :: setInput
    end type

    

    
    
contains
    
    
    integer function altayInputConfigReader_read(this, config) result(info)
    class(altayInputConfigReader),intent(inout)     :: this
    class(SimulationConfig),intent(out)              :: config
    !
        info = criError
    !
    end function
    

    
    integer function setInput(this, fname) result(info)
    implicit none
    class(altayInputConfigReader),intent(inout)     :: this
    character(len=*),intent(in)     :: fname
    !
        info = criErr_IOOpen
        this%context = RawFileContext(fname, 'rf')
        if (.not. this%context%isOpen()) return
    !
    end function
    
    
        
    integer function altayInputConfigReader_fakeread_single(this, config) result(info)
    class(altayInputConfigReader),intent(inout)     :: this
    class(SimulationConfig),intent(out)             :: config
    !
    type(AnalysisStepConfig) :: analysis_step
    type(OutputStepConfig) :: output_step
    integer,parameter :: max_step = 10
    integer :: i
        
        config%jobtitle = 'T612V4'
        !
        ! single-phase built-in fcc material
        !
        config%material = MaterialConfig(1)
        
        associate(phase => config%material%phases(1))
            phase%name = config%jobtitle
            phase%deformation_mechanism%id = DM_fcc12
            ! leave default hardening
            phase%intraphase_interfaces%file_path = 'micro1.smt'
        end associate
        
        ! Simple data persistency: output
        associate (cnf => config%state_output)
            cnf = StatePersistenceConfig(CNF_StatePersistencyNative, &
                                         StatePersistence_Write, &
                                         path = config%jobtitle, &
                                         n_phases = 1)
            ! Override the defaults
            cnf%phases(1)%odf%format_id = TF_CUR
            cnf%phases(1)%odf%file_name = trim(config%jobtitle)
        end associate
        !
        ! Create steps: 
        ! 0 - initialization
        ! 1 - assembly
        ! 2,4, ... - output
        ! 3,5, ... - analysis
        
        ! All analysis steps are identical:
        analysis_step%input(1,1) = 0.025
        analysis_step%input(3,3) = -0.025
        analysis_step%nincrements = 10
        analysis_step%model_type = CNF_modelAlamel
        !
        ! All output steps are identical
        !
        allocate(config%steps(0:max_step))
        !
        associate (steps => config%steps)
            ! Initialization step
            allocate(steps(0)%initialization_step)
            associate(input => steps(0)%initialization_step%input)
                input = StatePersistenceConfig(CNF_StatePersistencyNative, &
                                               StatePersistence_Read, &
                                               n_phases=1)
                ! ODF data - SMT format.
                input%phases(1)%odf%format_id = TF_SMT
                input%phases(1)%odf%file_name = 'A612LM.smt'
            end associate
            !
            ! Assembly step
            allocate(steps(1)%assembly_step)
            associate(step => steps(1)%assembly_step)
                ! Put directives: ALAMEL clusters
                ! Considerable limitation: you have to know how many 
                ! grains are available. Consider some "auto" option.
                step%directives = [ AssemblyMultiPhaseDirective([1, 1], 1500) ]
            end associate
            !
            ! Set output steps
            do i = 2, max_step, 2
                steps(i)%output_step = output_step
            enddo
            ! Set Analysis steps. explicit loop is needed
            ! lhs allocatable member is not allowed in array section:
            ! steps(2:max_step22)%analysis_step = analysis_step
            do i = 3, max_step, 2
                steps(i)%analysis_step = analysis_step
            enddo
        end associate
    
        info = criSuccess
    !
    end function
    
    integer function altayInputConfigReader_fakeread_dual(this, config) result(info)
    class(altayInputConfigReader),intent(inout)     :: this
    class(SimulationConfig),intent(out)             :: config
    !
    type(AnalysisStepConfig) :: analysis_step
    type(OutputStepConfig) :: output_step
    integer,parameter :: max_step = 10
    integer,parameter :: n_phases = 2
    integer :: i
        
        config%jobtitle = 'T612V4'
        !
        ! dual-phase built-in fcc material
        !
        config%material = MaterialConfig(n_phases)
        ! Phase 1
        associate(phase => config%material%phases(1))
            phase%name = 'phase1'
            phase%deformation_mechanism%id = DM_fcc12
            ! leave default hardening
            phase%intraphase_interfaces%file_path = 'micro1_1_500.smt'
        end associate
        ! Phase 1
        associate(phase => config%material%phases(2))
            phase%name = 'phase2'
            phase%deformation_mechanism%id = DM_fcc12
            ! leave default hardening
            phase%intraphase_interfaces%file_path = 'micro1_1_500.smt'
        end associate
        ! Interfaces between the phases:
        config%material%interphase_interfaces = [ MesostructureConfig(FMicro=unit_sr_matrix, &
                                                                      file_path = 'micro1_501_1000.smt') ]
        ! Simple data persistency: output
        associate (cnf => config%state_output)
            cnf = StatePersistenceConfig(CNF_StatePersistencyNative, &
                                         StatePersistence_Write, &
                                         path = config%jobtitle, &
                                         n_phases = n_phases)
            ! Override the defaults: just the format.
            ! The name will be deduced from the phasename and format_id
            cnf%phases(1)%odf%format_id = TF_CUR
            cnf%phases(2)%odf%format_id = TF_CUR
        end associate
        !
        ! Create steps: 
        ! 0 - initialization
        ! 1,3, ... - output
        ! 2,4, ... - analysis
        
        ! All analysis steps are identical:
        analysis_step%input(1,1) = 0.025
        analysis_step%input(3,3) = -0.025
        analysis_step%nincrements = 10
        analysis_step%model_type = CNF_modelAlamel
        !
        ! All output steps are identical
        !
        allocate(config%steps(0:max_step))
        !
        associate (steps => config%steps)
            ! Initialization step
            allocate(steps(0)%initialization_step)
            associate(input => steps(0)%initialization_step%input)
                input = StatePersistenceConfig(CNF_StatePersistencyNative, &
                                               StatePersistence_Read, &
                                               n_phases=2)
                ! ODF data - SMT format.
                input%phases(:)%odf%format_id = TF_SMT
                input%phases(1)%odf%file_name = 'A612LM_half1.smt'
                input%phases(2)%odf%file_name = 'A612LM_half2.smt'
            end associate
            !
            ! Assembly step
            allocate(steps(1)%assembly_step)
            associate(step => steps(1)%assembly_step)
                ! Put directives: ALAMEL clusters
                ! Considerable limitation: you have to know how many 
                ! grains are available. Consider some "auto" option.
                step%directives = [ AssemblyMultiPhaseDirective([1, 1], 500), &
                                    AssemblyMultiPhaseDirective([1, 2], 500), &
                                    AssemblyMultiPhaseDirective([2, 2], 500) ]
            end associate

            !
            ! Set output steps
            do i = 2, max_step, 2
                steps(i)%output_step = output_step
            enddo
            ! Set Analysis steps. explicit loop is needed
            ! lhs allocatable member is not allowed in array section:
            ! steps(2:max_step22)%analysis_step = analysis_step
            do i = 3, max_step, 2
                steps(i)%analysis_step = analysis_step
            enddo
        end associate
    
        info = criSuccess
    !
    end function
end module
