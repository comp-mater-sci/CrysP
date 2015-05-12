module altayTexAccess
use criErrcodes
use criMathUtils
implicit none

    integer,parameter,private :: ODF_title_length = 40


    !> Representation of a single constituent of a discrete ODF
    type :: DiscreteOrientation
        !> Euler angles of the discrete ODF constituent
        type(EulerAngles) :: euler
        
        !> Weight of the discrete ODF constituent
        double precision :: weight = 1.D0

    end type


    !> Aggregate of grains. The aggregate represents a discrite ODF.
    type :: DiscreteODF
        
        !> Meta-data: title/comment 
        character(len=ODF_title_length)                :: title = ''
        
        type(DiscreteOrientation),dimension(:),allocatable :: orientations
        
    end type

    interface size
        module procedure DiscreteODF_size
    end interface

    type :: TextureAssembly
        
        type(DiscreteODF),pointer   :: texture => null()
        
        double precision, dimension (:,:),pointer :: mesodeformationgradient
        
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
    
    !> Expand/shrink the storage for crystals in the DiscreteODF object.
    !>
    !> The old content is preserved if `keep_state` parameter is True.
    !> \todo do we actually need this feature? Wouldn't a regular "allocate" be sufficient?
    integer function DiscreteODF_resize(this, newsize, keep_state) result(info)
    implicit none
    type(DiscreteODF),intent(inout)     :: this     !< object to be modified
    !> new size, i.e. number of crystals that can be stored in `this`
    integer,intent(in)                  :: newsize
    !> Flag: if true, the existing state to be preserved on resize. Default: .false.
    !> If the newsize is larger than the previous size, only the first `newsize`
    !> elements will be preserved.
    logical,intent(in),optional         :: keep_state
    !
    logical :: keep
    integer :: memstat, ntransf
    type(DiscreteOrientation),dimension(:),allocatable  :: tmp
    !
        keep = .false.
        if (present(keep_state)) keep = keep_state
        !
        if (.not. allocated(this%orientations)) then
            allocate(this%orientations(newsize), stat=memstat)
        else
            if (size(this%orientations) == newsize) then
                    ! Nothing to do.
                    info = criSuccess
                    return
            endif
            if (keep) then
                ! Transfer npoints 
                allocate(tmp(newsize),stat=memstat)
                if (memstat == 0) then
                    ntransf = min(newsize,size(this%orientations))
                    tmp(1:ntransf) = this%orientations(1:ntransf)
                    deallocate(this%orientations)
                    call move_alloc(tmp,this%orientations)
                endif
            else
                deallocate(this%orientations)
                allocate(this%orientations(newsize),stat=memstat)
            endif
        endif
        info = merge(criSuccess, criErr_MemAlloc, (memstat == 0))
    !
    end function


    !> Give the number of orientations in the discrete ODF
    elemental integer function DiscreteODF_size(this) result(n)
    implicit none
    type(DiscreteODF),intent(in)     :: this     !< object to be modified
    !
        n = 0
        if (allocated(this%orientations)) n = size(this%orientations)
    !
    end function
    
    
    function TextureAssembly_init(texture, mesodeformationgradient) result(this)
    implicit none
    type(TextureAssembly)   :: this
    type(DiscreteODF),target   :: texture
    double precision,dimension(3,3),target :: mesodeformationgradient
    !
        this%texture => texture
        this%mesodeformationgradient => mesodeformationgradient
    !
    end function
    
end module