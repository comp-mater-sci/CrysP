!
! $Id$
!
#ifndef HDF5_DISABLE
module altayHDF5Context
use altayIOContext
use FH5, only: hid_t

    type,extends(IOContext) :: HDF5Context
        
        integer(kind=hid_t)     :: file_id = 0
        
        integer(kind=hid_t)     :: group_id = 0
    contains
        procedure :: isOpen => HDF5Context_isOpen
    end type
    
contains
    
    
    logical function HDF5Context_isOpen(this) result(is_open)
    implicit none
    class(HDF5Context),intent(in) :: this
    !
        !> \todo Provide implementation of HDF5Context_isOpen
        is_open = .false.
    !
    end function

end module
#endif
