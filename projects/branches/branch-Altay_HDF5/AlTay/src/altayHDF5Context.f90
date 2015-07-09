!
! $Id$
!
module altayHDF5Context
use altayIOContext
use hdf5

    type,extends(IOContext) :: HDF5Context
        
        integer(kind=hid_t)     :: file_id = 0
        
        integer(kind=hid_t)     :: group_id = 0
        
    end type
    
end module