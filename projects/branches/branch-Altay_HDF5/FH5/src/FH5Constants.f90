module FH5Constants
use hdf5, only: hid_t
implicit none
    
    integer(hid_t),parameter :: id_none = 0

    integer,parameter   :: FH5_compression_none = 0,   &
                           FH5_compression_zip = 1,    &
                           FH5_compression_szip = 2
end module
