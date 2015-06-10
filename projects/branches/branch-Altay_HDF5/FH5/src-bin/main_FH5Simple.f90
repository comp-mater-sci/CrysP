program FH5Simple_main
use FH5Simple
implicit none

character(len=8), parameter :: filename = "filef.h5" ! file name
integer(hid_t) :: file_id                            ! file identifier
character(len=8),  parameter :: groupname1 = "/mygroup" ! group name
character(len=16), parameter :: groupname2 = "/mygroup/group_a"
                                                        ! group name
character(len=7),  parameter :: groupname3 = "group_b"  ! group name
integer(hid_t) :: group1_id, group2_id, group3_id ! group identifiers
integer,parameter :: n=20, m=10
integer(HSIZE_T),parameter :: dims(2) = [n, m]

double precision,dimension(n,m) :: dble_buf, rd_buf
double precision,dimension(:,:),allocatable :: rd_dynbuf
integer     ::   error  ! error flag
    
integer(hid_t) :: dset_id       ! dataset identifier
integer(hid_t) :: dspace_id     ! dataspace identifier

character(len=256) :: dataset_path
integer(HSIZE_T) :: rd_dims(2), rd_ts
integer :: rd_rank, rd_tc
integer :: i, j

    
    ! initialize data
    forall (i=1:n,j=1:m)
        dble_buf(i,j) = (i-1)*n + j
    endforall
    !
    !    initialize fortran interface.
    !
    call h5open_f (error)
    !
    ! create a new file using default properties.
    ! 
    call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

    !
    ! create group "mygroup" in the root group using absolute name.
    !
    call h5gcreate_f(file_id, groupname1, group1_id, error)

    !
    ! create group "group_a" in group "mygroup" using absolute name.
    !
    call h5gcreate_f(file_id, groupname2, group2_id, error)

    !
    ! create group "group_b" in group "mygroup" using relative name.
    !
    call h5gcreate_f(group1_id, groupname3, group3_id, error)

    
    ! Create dataspaces for datasets
    !
    CALL h5screate_simple_f(1, dims , dspace_id, error)

    CALL h5dcreate_f(group1_id, 'mydata', h5kind_to_type(8,H5_REAL_KIND), dspace_id, dset_id, error)

    CALL h5dwrite_f(dset_id, h5kind_to_type(8,H5_REAL_KIND), c_loc(dble_buf), error)

    
    ! Use Lite API
    call h5ltmake_dataset_double_f(group2_id, dset_name='ltdata', rank=2, dims=dims, &
                                     buf=dble_buf, errcode=error)
    
    dataset_path = groupname2//'/ltdata2'
    call h5ltmake_dataset_double_f(file_id, dset_name=dataset_path, rank=2, dims=dims, &
                                     buf=dble_buf, errcode=error)
    
    dataset_path = groupname2//'/ltdata3'
    call storearray2D_double(file_id, dset_name=dataset_path, array=dble_buf, info=error)

    dataset_path = groupname2//'/compdata'
    call storearray2D_double(file_id, dset_name=dataset_path, array=dble_buf, info=error, &
                             compression=fh5_compression_zip)
    
    !
    ! close the groups.
    !
    call h5gclose_f(group1_id, error)
    call h5gclose_f(group2_id, error)
    call h5gclose_f(group3_id, error)

    !
    ! terminate access to the file.
    !
    call h5fclose_f(file_id, error)
    
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    rd_buf = 0.D0
    !
    ! Open an existing file.
    !
    call h5fopen_f (filename, H5F_ACC_RDWR_F, file_id, error)
    ! use wrapper
    dataset_path = groupname2//'/'//'ltdata'
    call getArray2D_double(file_id, dset_name=dataset_path, array=rd_dynbuf, info=error)
    
    write(*,*) all(rd_dynbuf == dble_buf)
    
    dataset_path = groupname2//'/'//'compdata'
    call getArray2D_double(file_id, dset_name=dataset_path, array=rd_dynbuf, info=error)

    
    !
    !    close fortran interface.
    !
    call h5close_f(error)

end program
