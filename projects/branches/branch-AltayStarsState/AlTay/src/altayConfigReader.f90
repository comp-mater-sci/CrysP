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
    
        !> \fixme Use actual read in place of fakeread
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
        
        config%jobtitle = 'T612V4'
        
        ! single-phase built-in fcc material
        allocate(config%material%phases(1))
        associate(phase => config%material%phases(1))
            phase%deformation_mechanism%id = DM_fcc12
            ! leave default hardening
            phase%intraphase_interfaces%file_path = 'micro1.smt'
        end associate
        !
        allocate(config%steps(0:2))
        !
        associate (steps => config%steps)
            ! Initialization step
            allocate(steps(0)%initialization_step)
            associate(input => steps(0)%initialization_step%input)
                input%scheme_id = CFN_DataPersistencyNative
                input%file_path = 'A612LM.SMT'
                input%odf_config%file_name = input%file_path
            end associate
            ! Analysis step
            allocate(steps(1)%analysis_step)
            associate(step => steps(1)%analysis_step)
                step%input = 0.05 * unit_sr_matrix
                step%model_type = CNF_modelAlamel
            end associate
            ! Output step
            allocate(steps(2)%output_step)
            associate(step => steps(2)%output_step)
                step%file_path = trim(config%jobtitle)//'.out'
            end associate
            
        end associate
    
        info = criError
    !
    end function
    
    
end module
