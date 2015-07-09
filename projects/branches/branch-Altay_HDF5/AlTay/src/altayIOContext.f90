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
        
    end type
    
    integer, parameter,private :: iounit_default = 0
    
    type,extends(IOContext) :: RawFileContext
        
        integer :: iounit = iounit_default
        
    contains
        procedure :: open => RawFileContext_open
        procedure :: close => RawFileContext_close
    end type
    
    interface RawFileContext
        module procedure RawFileContext_init
    end interface
    
contains
    
    function RawFileContext_init(path) result(this)
    character(len=*),intent(in)     :: path
    type(RawFileContext) :: this
    !
        this%path = path
    !
    end function
    
    ! \todo improve/generalize the implementation 
    function RawFileContext_open(this, mode) result(info)
    implicit none
    class(RawFileContext),intent(inout) :: this
    character(len=*),intent(in)     :: mode !< Mode. One of: ['r'='rf', 'w'='wf', 'rb', 'wb']
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
        if (this%iounit /= iounit_default) close(unit=this%iounit, iostat=info)
        CHOOSE(info, info == 0, criSuccess, criErr_IO)
    !
    end function
    
end module
