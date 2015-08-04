!
! $Id$
!

module altayTexAccess
use altayODFTypes
use altayIOContext
use criErrcodes
use criMathUtils
use criPath
implicit none

    
    !> Abstract class for methods of accessing external texture data.
    type,abstract :: TextureAccess
        
    contains
        
        !>@{ \name Generic high-level procedures
    
        !> Read from external data
        procedure(TexAccess_read_interface),pass(this),deferred    :: read
        
        !> Write out external data
        procedure(TexAccess_write_interface),pass(this),deferred   :: write
        !>@}

    end type
    
    abstract interface
    
        function TexAccess_read_interface(this, blockid, odf) result(info)
            import :: TextureAccess, DiscreteODF
            implicit none
            class(TextureAccess),intent(inout)      :: this
            integer,intent(in)                      :: blockid
            type(DiscreteODF),intent(inout)         :: odf
            integer                                 :: info
        end function
            
        function TexAccess_write_interface(this, blockid, odf) result(info)
            import :: TextureAccess, DiscreteODF
            implicit none
            class(TextureAccess),intent(inout)      :: this
            integer,intent(in)                      :: blockid
            type(DiscreteODF),intent(in)            :: odf
            integer                                 :: info
        end function
    end interface

    
    
    !> Class for accessing texture data that are stored in a format that can
    !> be processed by Fortran native IO operations.
    type,abstract,extends(TextureAccess) :: TextureRawFileAccess
        
        !> Input/output unit
        integer     :: iounit = 0
        
    contains
        procedure,pass(this) :: initialize => TextureRawFileAccess_initialize

        !>@{ \name Deferred implementation of the TextureAccess interface
        !>         according to the concept of metadata and datablocks
        procedure,pass(this) :: read => TextureRawFileAccess_read
        procedure,pass(this) :: write => TextureRawFileAccess_write
        !>@}
        
        !>@{ \name Implementation procedures
        
        !> Read meta-data, such as attributes, size, shape, title, description etc.
        procedure(TexAccess_readMetaData_interface),pass(this),deferred    :: readMetaData
        
        !> Read meta-data, such as attributes, size, shape, title, description etc.
        procedure(TexAccess_writeMetaData_interface),pass(this),deferred   :: writeMetaData
        
        !> Read data block of DiscreteODF
        procedure(TexAccess_readBlock_interface),pass(this),deferred    :: readBlock
        
        !> Write out data block of DiscreteODF
        procedure(TexAccess_writeBlock_interface),pass(this),deferred   :: writeBlock
        !>@{

    end type
    
    
    abstract interface
        subroutine TexAccess_readMetaData_interface(this, odf, info)
            import :: TextureRawFileAccess, DiscreteODF
            implicit none
            class(TextureRawFileAccess),intent(inout)      :: this
            type(DiscreteODF),intent(inout)         :: odf
            integer,intent(out)                     :: info
        end subroutine
            
        subroutine TexAccess_writeMetaData_interface(this, odf, info)
            import :: TextureRawFileAccess, DiscreteODF
            implicit none
            class(TextureRawFileAccess),intent(inout)      :: this
            type(DiscreteODF),intent(in)            :: odf
            integer,intent(out)                     :: info
        end subroutine
        
        subroutine TexAccess_readBlock_interface(this, blockid, odf, info)
            import :: TextureRawFileAccess, DiscreteODF
            implicit none
            class(TextureRawFileAccess),intent(inout)      :: this
            integer,intent(in)                      :: blockid
            type(DiscreteODF),intent(inout)         :: odf
            integer,intent(out)                     :: info
        end subroutine
            
        subroutine TexAccess_writeBlock_interface(this, blockid, odf, info)
            import :: TextureRawFileAccess, DiscreteODF
            implicit none
            class(TextureRawFileAccess),intent(inout)      :: this
            integer,intent(in)                      :: blockid
            type(DiscreteODF),intent(in)            :: odf
            integer,intent(out)                     :: info
        end subroutine
        
    end interface
    
    
    
    type,abstract,extends(TextureRawFileAccess) :: TextureMetaRawFileAccess
        
        integer                 :: ngrains = 0
        
        double precision, dimension(sr_tensor_dim,sr_tensor_dim) :: mesodeformationgradient = unit_sr_Matrix
        integer                                     :: step_number = 0
        double precision,dimension(:),allocatable   :: accumulatedshear
    contains
        procedure,pass(this)    :: setExtendedMetaData => TextureMetaRawFileAccess_setExtendedMetaData
        procedure,pass(this)    :: getExtendedMetaData => TextureMetaRawFileAccess_getExtendedMetaData 
    end type
    
    
contains
    
    
    subroutine TextureRawFileAccess_initialize(this, context, readonly, info)
    class(TextureRawFileAccess),intent(inout)   :: this
    type(RawFileContext),intent(inout)          :: context
    logical,intent(in)                          :: readonly
    integer,intent(out)                         :: info
    !
        info = context%open(mode=merge('r','w',readonly))
        if (info == criSuccess) this%iounit = context%iounit
        !
    end subroutine
   
    
    !> Read texture data
    function TextureRawFileAccess_read(this, blockid, odf) result(info)
    implicit none
    class(TextureRawFileAccess),intent(inout)  :: this
    integer,intent(in)                          :: blockid
    type(DiscreteODF),intent(inout)             :: odf
    integer                                     :: info     !< exit code
    !
        call this%readMetaData(odf, info)
        if (info == criSuccess) then
            call this%readBlock(blockid, odf, info)
        endif
    !
    end function
    
    !> Write texture data
    function TextureRawFileAccess_write(this, blockid, odf) result(info)
    implicit none
    class(TextureRawFileAccess),intent(inout)   :: this
    integer,intent(in)                          :: blockid
    type(DiscreteODF),intent(in)                :: odf
    integer                                     :: info     !< exit code
    !
        call this%writeMetaData(odf, info)
        if (info == criSuccess) then
            call this%writeBlock(blockid, odf, info)
        endif
    !
    end function

    
    subroutine TextureMetaRawFileAccess_setExtendedMetaData(this, F, step_number,accumulatedshear, info)
    implicit none
    class(TextureMetaRawFileAccess),intent(inout)            :: this
    double precision, dimension(sr_tensor_dim,sr_tensor_dim),intent(in) :: F
    integer,intent(in)                                      :: step_number
    double precision,dimension(:),intent(in)                :: accumulatedshear
    integer,intent(out)                                     :: info
    !
        this%mesodeformationgradient = F
        this%step_number = step_number
        this%accumulatedshear = accumulatedshear ! F2003 automatic allocation
        info = criSuccess
    !
    end subroutine
    
    subroutine TextureMetaRawFileAccess_getExtendedMetaData(this, F, step_number, accumulatedshear, info)
    implicit none
    class(TextureMetaRawFileAccess),intent(in)               :: this
    double precision, dimension(sr_tensor_dim,sr_tensor_dim),intent(inout) :: F
    integer,intent(out)                                     :: step_number
    double precision,dimension(:),allocatable,intent(out)   :: accumulatedshear
    integer,intent(out)                                     :: info
    !
        F = this%mesodeformationgradient
        step_number = this%step_number
        if (allocated(this%accumulatedshear)) then
            accumulatedshear = this%accumulatedshear ! F2003 automatic allocation
        endif
        info = criSuccess
    !
    end subroutine
    
end module
