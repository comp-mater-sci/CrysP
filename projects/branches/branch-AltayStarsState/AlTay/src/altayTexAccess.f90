module altayTexAccess
use altayStateTypes
implicit none
    
    type :: TextureAssembly
        
        type(TextureData),pointer   :: texture => null()
        
        type(MaterialFrame),pointer :: mf  => null()
        
    end type
    
    interface TextureAssembly
        module procedure TextureAssembly_init
    end interface
    
    type,abstract,extends(TextureAssembly) :: TextureAccess
        
    contains
        
        ! Generic high-level procedures
        procedure(texRead_interface),pass(this),deferred    :: read
        procedure(texWrite_interface),pass(this),deferred   :: write
 
        ! Implementation procedures
        procedure(texReadMetaData_interface),pass(this),deferred    :: readMetaData
        
        procedure(texWriteMetaData_interface),pass(this),deferred   :: writeMetaData
        
        procedure(texReadBlock_interface),pass(this),deferred    :: readBlock
        
        procedure(texWriteBlock_interface),pass(this),deferred   :: writeBlock
        
    end type
    
    !> \todo provide the actual interfaces
    abstract interface
    
        subroutine texRead_interface(this, info)
            import :: TextureAccess
            implicit none
            class(TextureAccess),intent(inout)    :: this
            integer,intent(out)                     :: info
        end subroutine
            
        subroutine texWrite_interface(this, info)
            import :: TextureAccess
            implicit none
            class(TextureAccess),intent(in)       :: this
            integer,intent(out)                     :: info
        end subroutine
        
        subroutine texReadMetaData_interface(this, info)
            import :: TextureAccess
            implicit none
            class(TextureAccess),intent(inout)    :: this
            integer,intent(out)                     :: info
        end subroutine
            
        subroutine texWriteMetaData_interface(this, info)
            import :: TextureAccess
            implicit none
            class(TextureAccess),intent(in)       :: this
            integer,intent(out)                     :: info
        end subroutine
        
        subroutine texReadBlock_interface(this, info)
            import :: TextureAccess
            implicit none
            class(TextureAccess),intent(inout)    :: this
            integer,intent(out)                     :: info
        end subroutine
            
        subroutine texWriteBlock_interface(this, info)
            import :: TextureAccess
            implicit none
            class(TextureAccess),intent(in)       :: this
            integer,intent(out)                     :: info
        end subroutine
        
    end interface
      
contains
    
    function TextureAssembly_init(texture, mf) result(this)
    implicit none
    type(TextureAssembly)   :: this
    type(TextureData),target   :: texture
    type(MaterialFrame),target :: mf
    !
        this%texture => texture
        this%mf => mf
    !
    end function
    
end module