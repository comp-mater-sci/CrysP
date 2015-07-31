! $Id!

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
    
        ! FIXME:
        ! procedure :: read => altayInputConfigReader_read
        procedure :: read => altayInputConfigReader_fakeread
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
    
    
        
    integer function altayInputConfigReader_fakeread(this, config) result(info)
    class(altayInputConfigReader),intent(inout)     :: this
    class(SimulationConfig),intent(out)              :: config
    !
        
        config%jobtitle = 'samplejob'
        
        allocate(config%steps(3))
        !
        associate (steps => config%steps)
            ! Initialization step
            allocate(steps(1)%initialization_step)
            steps(1)%initialization_step%data_persistency_scheme = CFN_NativeDataPersistency
            steps(1)%initialization_step%file_path = 'A612LM.SMT'
            !
            allocate(steps(2)%analysis_step)
            steps(2)%analysis_step%input = 0.05 * unit_sr_matrix
            allocate(steps(3)%output_step)
            
            
        end associate
    
        info = criError
    !
    end function
    
    
end module
