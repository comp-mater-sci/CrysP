module FH5Constants
use hdf5, only: hid_t
implicit none
    
    !> Value of uninitialized id. 
    !>
    !> HDF5 documentation guarantees that valid id is greater than 0.
    integer(hid_t),parameter :: id_none = 0

    integer,parameter   :: FH5_compression_none = 0,   &
                           FH5_compression_zip = 1,    &
                           FH5_compression_szip = 2
    
    !> Path separator in HDF5 locations.
    character,parameter :: FH5_path_sep = '/'
    
end module
