! $Id$

!> Simple wrappers over dataset creation HDF5 interface.
module FH5Simple
use iso_c_binding
use hdf5
use h5lt
use criErrcodes
use FH5Constants

contains
    
    subroutine storeArray2D_double(loc_id, dset_name, array, info, compression)
    implicit none
    integer(HID_T), intent(in)                      :: loc_id    ! file or group identifier 
    character(len=*), intent(in)                    :: dset_name ! name of the dataset 
    double precision,dimension(:,:),intent(in)      :: array
    integer,intent(out)                             :: info ! error code
    integer,intent(in),optional                     :: compression
    !logical,intent(in),optional                     :: create_parents
    !
    integer,parameter :: rank = 2
    integer(HSIZE_T), dimension(rank) :: dims, cdims
    integer(hid_t) :: dataset_id, dataspace_id, plist_id
    integer :: compression_type, errcode
    !
    integer :: szip_options_mask, szip_pixels_per_block
    !
        info = criErr_BadArgs
        !
        compression_type = FH5_compression_none
        if (present(compression)) compression_type = compression
        !
        dims = shape(array, kind=HSIZE_T)
        !
        call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, errcode)
        ! Create dataspace
        call h5screate_simple_f(rank, dims, dataspace_id, errcode)
        !
        ! Properties needed for compression
        select case(compression_type)
        case(FH5_compression_zip, FH5_compression_szip)
            
            ! Dataset must be chunked for compression.
            ! For small sets, just one chunk can be used. Note that 
            ! most likely this will not work well for large datasets.
            cdims = dims
            call h5pset_chunk_f(plist_id, rank, cdims, errcode)
        end select
        !
        ! Compression
        select case(compression_type)
        case(FH5_compression_zip)

            ! Set ZLIB / DEFLATE Compression using compression level 9.
            call h5pset_deflate_f(plist_id, 9, errcode)
        case(FH5_compression_szip)
            ! The parameters taken from HDF5 example h5_cmprss.f90
            szip_options_mask = H5_SZIP_NN_OM_F
            szip_pixels_per_block = 16
            call h5pset_szip_f(plist_id, szip_options_mask, szip_pixels_per_block, errcode)
        end select
        ! Write the dataset

        call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_DOUBLE, dataspace_id, &
                         dataset_id, errcode, dcpl_id=plist_id)
        ! Close the handlers
        call h5sclose_f(dataspace_id, errcode)
        call h5pclose_f(plist_id, errcode)
        call h5dclose_f(dataset_id, errcode)
        ! Translate error code
        info = merge(criSuccess,criError, errcode == 0)
    !
    end subroutine
    
    subroutine getArray2D_double(loc_id, dset_name, array, info)
    implicit none
    integer(HID_T), intent(in)                                  :: loc_id    ! file or group identifier 
    character(len=*), intent(in)                                :: dset_name ! name of the dataset 
    double precision,dimension(:,:),allocatable,intent(out)     :: array
    integer,intent(out)                                         :: info ! error code
    !
    integer,parameter :: rank = 2
    integer :: rd_rank, rd_tc, errcode
    integer(HSIZE_T), dimension(rank) :: rd_dims
    integer(SIZE_T) :: rd_ts ! Type size ! Note: Fortran90  uses SIZE_T here, not HSIZE_T
    !
        info = criErr_BadArgs
        !
        ! There is no need to distinguishing between compressed and non-compressed datasets,
        ! since it is transparently handled by the HDF5, incl. the Lite API.
        ! We may use Lite API for simplicity.
        !
        ! Check whether the dataset exists
        if (h5ltfind_dataset_f(loc_id, trim(dset_name)) == 0) then
            ! Make sure the shape meets the expectations
            
            call h5ltget_dataset_ndims_f(loc_id, dset_name, rd_rank, errcode)
            if ((errcode /= 0) .or. (rd_rank /= rank)) return
            ! use Lite to get shape
            call h5ltget_dataset_info_f(loc_id, dset_name, rd_dims, &
                                   type_class=rd_tc, type_size=rd_ts, errcode=errcode)
            if (any(rd_dims <= 0)) return
            info = -2 ! Change to criErr_MemAlloc
            allocate(array(rd_dims(1),rd_dims(2)),stat=errcode)
            if (errcode /= 0) return
            call h5ltread_dataset_double_f(loc_id, dset_name,dims=rd_dims,buf=array, errcode=errcode)
            ! Translate error code
            info = merge(0,-1, errcode == 0)
        endif
    !
    end subroutine
    
end module