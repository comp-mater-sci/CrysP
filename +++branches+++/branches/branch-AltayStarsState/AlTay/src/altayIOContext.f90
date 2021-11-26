!
! $Id$
!
!> Generalization of IO handlers.
module altayIOContext
use criPath
use criErrcodes
implicit none
#include "criMacros.fpp"

    type,abstract :: IOContext
        
        character(len=max_pathlen)  :: path = './'
        
        integer :: iostatus = 0
    contains
        procedure(IOContext_isOpen_interface),pass(this),deferred :: isOpen
    end type
    
    integer, parameter,private :: iounit_default = 0
    
    type,extends(IOContext) :: RawFileContext
        
        integer :: iounit = iounit_default
        
    contains
        procedure :: open => RawFileContext_open
        procedure :: close => RawFileContext_close
        procedure :: isOpen => RawFileContext_isOpen
    end type
    
    interface RawFileContext
        module procedure RawFileContext_init
    end interface
    
    abstract interface
        logical function IOContext_isOpen_interface(this) 
        import :: IOContext
        class(IOContext),intent(in) :: this
        end function
    end interface
    
contains
    
    function RawFileContext_init(path, mode) result(this)
    implicit none
    character(len=*),intent(in)             :: path
    character(len=*),intent(in),optional    :: mode
    type(RawFileContext)                    :: this
    !
    integer :: info
    !
        this%path = path
        if (present(mode)) info = this%open(mode)
    !
    end function
    
    ! \todo improve/generalize the implementation 
    function RawFileContext_open(this, mode) result(info)
    implicit none
    class(RawFileContext),intent(inout) :: this
    !> Access mode. One of: ['r'='rf', 'w'='wf', 'rb', 'wb'] (case sensitive)
    character(len=*),intent(in)     :: mode 
    integer :: info
    !
    character(len=12) :: stat , form
    !
        info = criErr_BadArgs
        ! The defaults: mode = 'rf'='r'
        stat = 'old'
        form = 'formatted'
        select case(mode)
        case('r','rf')
            continue
        case('w','wf')
                stat = 'replace'
        
        case('rb')
                form='unformatted'
        case('wb')
                stat = 'replace'
                form='unformatted'
        case default 
                return
        end select
        !
        open(newunit=this%iounit, file=this%path, status=stat, form=form, iostat=this%iostatus)
        CHOOSE(info, this%iostatus == 0, criSuccess, criErr_IOOpen)
    !
    end function

    function RawFileContext_close(this) result(info)
    implicit none
    class(RawFileContext),intent(inout) :: this
    integer :: info
    !
        info = 0
        if (this%isOpen()) then 
            close(unit=this%iounit, iostat=info)
            this%iounit = iounit_default
        endif
        CHOOSE(info, info == 0, criSuccess, criErr_IO)
    !
    end function
    
    logical function RawFileContext_isOpen(this) result(is_open)
    implicit none
    class(RawFileContext),intent(in) :: this
    !
        is_open = (this%iounit /= iounit_default)
    !
    end function
end module
