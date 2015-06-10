module FH5Sample_File
use FH5

contains
    
    subroutine makeTestArray(n, m, array, random)
    implicit none
    integer,intent(in) :: n, m
    double precision,dimension(:,:),allocatable,intent(out) :: array
    logical,intent(in) :: random
    !
    integer :: i,j
    !
        allocate(array(n, m))
        if (random) then
            call random_number(array)
        else
            forall (i=1:n,j=1:m)
                array(i,j) = (i-1)*n + j
            endforall
        endif
    !
    end subroutine
    
    
    subroutine test_FileOpen()
    implicit none
    !
    type(FH5File) :: fh5_file
    integer :: info
    !
        write(*,*) 'test_FileOpen'
        info = fh5_file%open(path='filename.h5', mode='a')
        
        info = fh5_file%close()
    !
    end subroutine
    
    subroutine test_DatasetWrite()
    implicit none
    !
    type(FH5File)       :: fh5_file
    type(FH5Dataset)    :: fh5_dataset
    double precision,dimension(:,:),allocatable :: array
    integer :: info, i,j, n,m
    !
        write(*,*) 'test_DatasetWrite'
        call makeTestArray(5000, 4, array, .false.)
        
        info = fh5_file%open(path='filename.h5', mode='w')
        
        info = FH5Dataset_init(fh5_dataset, fh5_file, shape=shape(array), compression=FH5_compression_szip)
        
        info = fh5_dataset%write('mydata', array)
        
        call FH5Dataset_finalize(fh5_dataset)
        
        call print_open_handles(fh5_file%object_id)
        
        info = fh5_file%close()
    !
    end subroutine
    
    subroutine test_DatasetRead()
    implicit none
    !
    type(FH5File)       :: fh5_file
    type(FH5Dataset)    :: fh5_dataset
    double precision,dimension(:,:),allocatable :: array, rd_array
    integer :: info, i,j, n,m
    !
        write(*,*) 'test_DatasetRead'
        call makeTestArray(5000, 4, array, .false.)
        
        info = fh5_file%open(path='filename.h5', mode='rw')
        
        info = FH5Dataset_init(fh5_dataset, fh5_file, 'mydata')
        
        call print_open_handles(fh5_file%object_id)
        
        info = fh5_dataset%read(rd_array)
        
        call FH5Dataset_finalize(fh5_dataset)
        
        call print_open_handles(fh5_file%object_id)
        
        if (allocated(rd_array)) then
            write(*,*) all(array == rd_array)
        else
            write(*,*) 'Failed to read the data'
        endif        
        info = fh5_file%close()
    !
    end subroutine
    
    
    subroutine test_ReadAPI()
    use FH5Simple
    double precision,dimension(:,:),allocatable :: array, rd_array
    integer(hid_t) :: file_id 
    integer :: error
    !
        write(*,*) 'test_ReadAPI'
        call makeTestArray(5000, 4, array, .false.)
    
        ! Open an existing file.
        !
        call h5fopen_f ('filename.h5', H5F_ACC_RDWR_F, file_id, error, access_prp=H5P_DEFAULT_F)
        ! use wrapper
        call getArray2D_double(file_id, dset_name='mydata', array=array, info=error)
        
        call print_open_handles(file_id)
        
        if (allocated(rd_array)) then
            write(*,*) all(array == rd_array)
        else
            write(*,*) 'Failed to read the data'
        endif
        call h5fclose_f(file_id, error)
    !
    end subroutine
    
    
end module
    