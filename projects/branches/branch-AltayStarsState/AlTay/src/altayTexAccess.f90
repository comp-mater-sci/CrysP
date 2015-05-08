module altayTexAccess
use altayStateTypes
implicit none

    integer,parameter,private :: textureTitle_length = 40


    !> Representation of a single constituent of a discrete ODF
    type :: TextureConstituent
        !> Euler angles of the discrete ODF constituent
        type(EulerAngles) :: euler
        
        !> Weight of the discrete ODF constituent
        double precision :: weight = 1.D0
        
        !> Total plastic slip
        !>
        !> This field provides an average quantification of the deformation
        !> to which the grain was previously subjected.
        double precision :: tGAM = 0.D0

    end type


    !> Aggregate of grains. The aggregate represents a discrite ODF.
    type :: TextureData
        
        !> Meta-data: title/comment 
        character(len=textureTitle_length)                :: title = ''
        
        type(TextureConstituent),dimension(:),allocatable :: constituents
        
    end type


    type :: TextureAssembly
        
        type(TextureData),pointer   :: texture => null()
        
        type(MesostructureState),pointer :: meso  => null()
        
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
    
    !> Expand/shrink the storage for crystals in the TextureData object.
    !>
    !> The old content is preserved if `keep_state` parameter is True.
    !> \todo do we actually need this feature? Wouldn't a regular "allocate" be sufficient?
    integer function textureData_resize(this, newsize, keep_state) result(info)
    implicit none
    type(TextureData),intent(inout)     :: this     !< object to be modified
    !> new size, i.e. number of crystals that can be stored in `this`
    integer,intent(in)                  :: newsize
    !> Flag: if true, the existing state to be preserved on resize. Default: .false.
    !> If the newsize is larger than the previous size, only the first `newsize`
    !> elements will be preserved.
    logical,intent(in),optional         :: keep_state
    !
    logical :: keep
    integer :: memstat, ntransf
    type(TextureConstituent),dimension(:),allocatable  :: tmp
    !
        keep = .false.
        if (present(keep_state)) keep = keep_state
        !
        if (.not. allocated(this%constituents)) then
            allocate(this%constituents(newsize), stat=memstat)
        else
            if (size(this%constituents) == newsize) then
                    ! Nothing to do.
                    info = criSuccess
                    return
            endif
            if (keep) then
                ! Transfer npoints 
                allocate(tmp(newsize),stat=memstat)
                if (memstat == 0) then
                    ntransf = min(newsize,size(this%constituents))
                    tmp(1:ntransf) = this%constituents(1:ntransf)
                    deallocate(this%constituents)
                    call move_alloc(tmp,this%constituents)
                endif
            else
                deallocate(this%constituents)
                allocate(this%constituents(newsize),stat=memstat)
            endif
        endif
        info = merge(criSuccess, criErr_MemAlloc, (memstat == 0))
    !
    end function

    
    function TextureAssembly_init(texture, meso) result(this)
    implicit none
    type(TextureAssembly)   :: this
    type(TextureData),target   :: texture
    type(MesostructureState),target :: meso
    !
        this%texture => texture
        this%meso => meso
    !
    end function
    
end module