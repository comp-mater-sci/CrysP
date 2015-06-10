program FH5Sample_main
use FH5Sample_File
use HDF5
implicit none
integer :: info
    
    info = FH5_initialize()

    ! call test_FileOpen()

    call test_DatasetWrite()
    
    call test_ReadAPI()
    
    call test_DatasetRead()
    
    info = FH5_finalize()


    
end program