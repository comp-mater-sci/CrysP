! $Id$
#include "criMacros.fpp"

!> Object-oriented Fortan thin wrapper for simplifying HDF5 operations.
!>
!> \note Automatic finalization (destructors) are disabled. Because we can't control
!>       the order in which the finalization of objects is launched,we can't prevent 
!>       the objects from being closed before the objects that refer to them are closed
!>       (unless we put them in different validity scope). For this reason automatic
!>       finalization seems to bring more harm than good.
module FH5
use iso_c_binding
use hdf5
use h5lt
use criErrcodes
use criPath
use FH5Constants
implicit none

    
    type :: FH5Object
        
        !> Handle to the primary HDF5 object
        integer(hid_t) :: object_id = id_none
        
    end type


    type,extends(FH5Object),abstract :: FH5Attribute
        
        character(len=:),allocatable    :: name
        
    end type

    type :: FH5PtrAttribute

        class(FH5Attribute),pointer     :: ptr => null()
    end type
    
    
    type :: FH5AttributeCollection
        
        type(FH5PtrAttribute),allocatable,dimension(:)   :: items
        
    end type


    type,extends(FH5Object) :: FH5Path
        
    contains
        
        procedure,pass(this)    ::  setAttribute_item => FH5Path_setAttribute_item
        procedure,pass(this)    ::  setAttribute_collection => FH5Path_setAttribute_collection
        generic :: setAttribute => setAttribute_item, setAttribute_collection
        !
        procedure,pass(this)    ::  getAttribute_item => FH5Path_getAttribute_item
        procedure,pass(this)    ::  getAttribute_collection => FH5Path_getAttribute_collection
        generic :: getAttribute => getAttribute_item, getAttribute_collection
        
    end type


    type,extends(FH5Path) :: FH5File
        integer(hid_t)   :: fapl_id = id_none !< File access property list
    contains
        procedure,pass(this) :: open => FH5File_open
        
        procedure,pass(this) :: close => FH5File_close
#ifdef FH5_DESTRUCTORS_ENABLED
        final :: FH5File_finalize
#endif
    end type


    type,extends(FH5Path) :: FH5Group
        integer(hid_t)  :: lcpl_id = id_none !< Link creation property list
    contains
        procedure,pass(this) :: create => FH5Group_create
        procedure,pass(this) :: close => FH5Group_close
    end type


    type,extends(FH5Path) :: FH5Dataset
    private
        integer(hid_t)                              :: location_id = id_none
        integer(hid_t)                              :: plist_id = id_none
        integer(hid_t)                              :: dataspace_id = id_none
        integer(hsize_t), dimension(:),allocatable  :: shape
    contains
        procedure,pass(this)    :: setCompression => FH5Dataset_setCompression
        procedure,pass(this)    :: close => FH5Dataset_close
        
        procedure,pass(this) :: FH5Dataset_write_2D_double
        procedure,pass(this) :: FH5Dataset_read_2D_double

        
        generic :: write => FH5Dataset_write_2D_double
        
        generic :: read => FH5Dataset_read_2D_double
#ifdef FH5_DESTRUCTORS_ENABLED
        final :: FH5Dataset_finalize
#endif
    end type


    !> Constructors of FH5Dataset objects
    interface FH5Dataset_init
        module procedure FH5Dataset_initialize_r, FH5Dataset_initialize_rw
    end interface

contains
    
    !
    ! Initialization and finalization of the module: HDF5 API
    !
    
    integer function FH5_initialize() result(info)
    implicit none
    integer :: hdferr
    !
        info = criError
        call h5open_f(hdferr)
        if (hdferr) return
        ! Turns off the automatic error printing from the HDF5 library.
        ! The call below corresponds to C++ Exception::dontPrint()
        call h5eset_auto_f(0, hdferr)
        if (hdferr) return
        info = criSuccess
    !
    end function
    
    
    integer function FH5_finalize() result(info)
    implicit none
    !
    integer :: hdferr
    !
        call h5close_f(hdferr)
        CHOOSE(info, hdferr == 0, criSuccess, criError)
    !
    end function
    
    !
    ! File interface
    !
    
    
    !> \param mode
    !> 'r' - Readonly, file must exist
    !> 'rw' - Read/write, file must exist
    !> 'x' - Create file in read/write mode, fail if exists
    !> 'a' - Read/write if exists, create otherwise
    integer function FH5File_open(this, path, mode) result(info)
    implicit none
    class(FH5File),intent(inout)        :: this
    character(len=*),intent(in)         :: path
    character(len=*),intent(in)         :: mode
    !
    integer :: hdferr
    integer :: access_flag
    logical :: open_file
    !
        info = criErr_BadArgs
        
        open_file = .true.
        select case(mode)
        case('r','R')
            access_flag = H5F_ACC_RDONLY_F
        case('rw', 'RW')
            access_flag = H5F_ACC_RDWR_F
        case('x','X')
            open_file = .false.
            access_flag = H5F_ACC_EXCL_F
        case('w','W')
            access_flag = H5F_ACC_TRUNC_F
            open_file = .false.
!        case('a','A')
!            access_flag = H5F_ACC_RDWR_F
        case default
            return
        end select
#ifndef FH5_NO_PROPLIST
        !
        ! Allow to use more recent file format
        !
        ! Adapted from h5ex_g_compact.f90 example.
        ! Set file access property list to allow the latest file format.
        ! This will allow the library to create new compact format groups.
        call h5pcreate_f (H5P_FILE_ACCESS_F, this%fapl_id, hdferr)
        if (hdferr /= 0) return
        call h5pset_libver_bounds_f (this%fapl_id, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, hdferr)
        if (hdferr /= 0) return
        info = criError
        !
        ! Create file using the new file access property list.
        !
        if (open_file) then
            call h5fopen_f(path, access_flag, this%object_id, hdferr, access_prp=this%fapl_id)
        else
            call h5fcreate_f(path, access_flag, this%object_id, hdferr, access_prp=this%fapl_id)
        endif
#else
        if (open_file) then
            call h5fopen_f(path, access_flag, this%object_id, hdferr, access_prp=H5P_DEFAULT_F)
        else
            call h5fcreate_f(path, access_flag, this%object_id, hdferr, access_prp=H5P_DEFAULT_F)
        endif
#endif
        CHOOSE(info, hdferr == 0, criSuccess, criError)
    !
    end function
    

    integer function FH5File_close(this) result(info)
    implicit none
    class(FH5File),intent(inout)     :: this
    !
    integer :: hdferr
    !
        ! Close the property list
        if (is_valid_id(this%fapl_id)) then
            call h5pclose_f(this%fapl_id, hdferr)
            this%fapl_id = id_none
        endif
        
        call print_open_handles(this%object_id)
        
        ! Close the file
        if (is_valid_id(this%object_id)) then 
            call h5fclose_f(this%object_id, hdferr)
            this%object_id = id_none
        endif
        info = criSuccess
    !
    end function
    
    
    subroutine FH5File_finalize(this)
    implicit none
    type(FH5File),intent(inout)     :: this
    !
    integer :: info
    !
        info = this%close()
    !
    end subroutine
    
    
    !
    ! Group interface
    !
    
    !> Create a group of name given by path in the location.
    integer function FH5Group_create(this, location, path, intermediate) result(info)
    implicit none
    class(FH5Group),intent(inout)       :: this
    class(FH5Path),intent(in)           :: location
    character(len=*),intent(in)         :: path
    !> Create intermediate directories (like 'mkdir -p')
    logical,intent(in),optional         :: intermediate 
    ! logical,intent(in),optional         :: exclusive
    !
    integer :: hdferr
    integer :: access_flag
    logical :: make_intermediate, link_exists
    !
        info = criErr_BadArgs
        !
        make_intermediate = .true.
        if (present(intermediate)) make_intermediate = intermediate
        !
        call h5lexists_f(location%object_id, path, link_exists, hdferr)
        ! if  return
        if ((.not. link_exists) .or. (hdferr /= 0)) then
            ! Attempt to make the link.
            if (make_intermediate) then
                call h5pcreate_f(H5P_LINK_CREATE_F, this%lcpl_id, hdferr)
                call h5pset_create_inter_group_f(this%lcpl_id, 1, hdferr)
            else
                this%lcpl_id = H5P_DEFAULT_F
            endif
            call h5gcreate_f(location%object_id, path, this%object_id, hdferr, &
                             lcpl_id=this%lcpl_id)
        else
            ! Attempt to open existing group
            call h5gopen_f(location%object_id, path, this%object_id, hdferr) 
        endif
        ! Calculate the status from the last call to HDF5 API
        CHOOSE(info, hdferr == 0, criSuccess, criError)
    !
    end function
    

    integer function FH5Group_close(this) result(info)
    implicit none
    class(FH5Group),intent(inout)     :: this
    !
    integer :: hdferr
    !
        ! Close the property list
        if (is_valid_id(this%lcpl_id) .and. (this%lcpl_id /= H5P_DEFAULT_F)) then
            call h5pclose_f(this%lcpl_id, hdferr)
            this%lcpl_id = id_none
        endif
        
        ! Close the group
        if (is_valid_id(this%object_id)) then 
            call h5gclose_f(this%object_id, hdferr)
            this%object_id = id_none
        endif
        info = criSuccess
    !
    end function
    
    
    
    integer function FH5Dataset_setCompression(this, compression, chunks) result(info)
    implicit none
    class(FH5Dataset),intent(inout)                     :: this
    integer,intent(in),optional                         :: compression
    integer, dimension(:),intent(in),optional           :: chunks
    !
    logical :: do_compression
    integer :: compression_type
    integer :: szip_options_mask, szip_pixels_per_block
    integer :: hdferr
    !
        info = criErr_BadArgs
        compression_type = FH5_compression_none
        if (present(compression)) compression_type = compression
        !
        ! Create property list
        call h5pcreate_f(H5P_DATASET_CREATE_F, this%plist_id, hdferr)
        !
        ! Compression
        do_compression = .true.
        select case(compression_type)
        case(FH5_compression_zip)
            ! Set ZLIB / DEFLATE Compression using compression level 9.
            call h5pset_deflate_f(this%plist_id, 9, hdferr)
        case(FH5_compression_szip)
            ! The parameters taken from HDF5 example h5_cmprss.f90
            !> \todo Check how thesr options can be optimized
            szip_options_mask = H5_SZIP_NN_OM_F
            szip_pixels_per_block = 16
            call h5pset_szip_f(this%plist_id, szip_options_mask, szip_pixels_per_block, hdferr)
        case default
            do_compression = .false.
        end select
        !
        ! Set chunks - either provided or default (if compression is requested, 
        ! chunking is mandatory)
        ! Dataset must be chunked for compression.
        ! For small sets, just one chunk can be used. Note that 
        ! most likely this will not work well for large datasets.
        if (present(chunks) .or. do_compression) then
            if (present(chunks)) then
                call h5pset_chunk_f(this%plist_id, size(chunks), int(chunks, kind=hsize_t), hdferr)
            else
                call h5pset_chunk_f(this%plist_id, size(this%shape), this%shape, hdferr)
            endif
        endif
        info = criSuccess
    !
    end function
    
    
    !> Create dataset for writing
    integer function FH5Dataset_initialize_rw(this, path, shape, compression, chunks) result(info)
    implicit none
    type(FH5Dataset) ,intent(out)                   :: this
    class(FH5Path),intent(in)                       :: path
    integer,dimension(:),intent(in)                 :: shape
    integer,intent(in),optional                     :: compression
    integer,dimension(:),intent(in),optional        :: chunks
    !
    integer :: hdferr
    logical :: is_path_valid, do_create_parents
    !
    integer :: rank
    !
        info = criErr_BadArgs
        rank = size(shape)
        this%shape = int(shape, kind=hsize_t)
        if (any(this%shape == 0)) return
        !
        this%location_id = path%object_id ! Acquire the context
        !
        ! Create dataspace
        call h5screate_simple_f(rank, this%shape, this%dataspace_id, hdferr)
        !
        if (hdferr /= 0) return
        !
        info = this%setCompression(compression, chunks)
    !
    end function
    
    
    !> Create dataset for reading
    integer function FH5Dataset_initialize_r(this, path, name) result(info)
    implicit none
    type(FH5Dataset),intent(out)                    :: this
    class(FH5Path),intent(in)                       :: path
    character(len=*),intent(in)                     :: name
    integer :: hdferr
    !
    integer :: rank
    integer(hsize_t),dimension(:),allocatable :: maxdims
    !
        info = criErr_IO
        this%location_id = path%object_id ! Acquire the context
        !
        call h5dopen_f(this%location_id, name, this%object_id, hdferr)
        if (hdferr /= 0) return
        ! Get the dataspace
        call h5dget_space_f(this%object_id, this%dataspace_id, hdferr)
        if (hdferr /= 0) return
        ! Get the rank and shape
        rank = 0
        call h5sget_simple_extent_ndims_f(this%dataspace_id, rank, hdferr)
        if ((hdferr /= 0) .or. (rank <= 0)) return
        !
        allocate(this%shape(rank), maxdims(rank))
        ! In the call to h5sget_simple_extent_dims_f hdferr will contain dataspace rank on success
        ! and -1 on failure.
        call h5sget_simple_extent_dims_f(this%dataspace_id, this%shape, &
                                         maxdims, hdferr)
        CHOOSE(info, hdferr == rank, criSuccess, criError)
    !
    end function
    
    
    integer function FH5Dataset_write_2D_double(this, name, array) result(info)
    implicit none
    class(FH5Dataset),intent(inout)                  :: this
    character(len=*),intent(in)                     :: name
    double precision,dimension(:,:)                 :: array
    
    !
    integer :: hdferr
    !
        info = criErr_IO
        this%shape = shape(array, kind=hsize_t)
        ! Write the dataset
        call h5dcreate_f(this%location_id, name, H5T_NATIVE_DOUBLE, this%dataspace_id, &
                         this%object_id, hdferr, dcpl_id=this%plist_id)
        if (hdferr /= 0) return
        call h5dwrite_f(this%object_id, mem_type_id=H5T_NATIVE_DOUBLE, buf=array, &
                        dims=this%shape, hdferr=hdferr)
        CHOOSE(info, hdferr == 0, criSuccess, criErr_IOWrite)
    !
    end function
    
    
    integer function FH5Dataset_read_2D_double(this, array) result(info)
    implicit none
    class(FH5Dataset),intent(inout)                  :: this
    double precision,dimension(:,:),allocatable      :: array
    !
    integer :: hdferr
    integer, parameter :: rank = 2
    !
        info = criErr_BadArgs
        ! Check if object state is consistent
        if (allocated(this%shape) .and. &
            is_valid_id(this%object_id) .and. &
            is_valid_id(this%dataspace_id)) then
            !
            if (any(this%shape <= 0).or. (size(this%shape) /= rank)) return
            !
            allocate(array(this%shape(1),this%shape(2)))
            call h5dread_f(this%object_id, mem_type_id=H5T_NATIVE_DOUBLE, buf=array, &
                           dims=this%shape, hdferr=hdferr)
            CHOOSE(info, hdferr == 0, criSuccess, criErr_IORead)
        endif
    end function
    
    
    integer function FH5Dataset_close(this) result(info)
    implicit none
    class(FH5Dataset),intent(inout)     :: this
    !
    integer :: hdferr
    !
        ! Close all handlers
        !
        if (is_valid_id(this%dataspace_id)) then
            call h5sclose_f(this%dataspace_id, hdferr)
            this%dataspace_id = id_none
        endif
        !
        if (is_valid_id(this%plist_id)) then 
            call h5pclose_f(this%plist_id, hdferr)
            this%plist_id = id_none
        endif
        !
        if (is_valid_id(this%object_id)) then 
            call h5dclose_f(this%object_id, hdferr)
            this%object_id = id_none
        endif
        info = criSuccess
    !
    end function

    
    subroutine FH5Dataset_finalize(this)
    implicit none
    type(FH5Dataset),intent(inout)     :: this
    !
    integer :: info
    !
        info = this%close()
    !
    end subroutine
    

    integer function FH5Path_getAttribute_item(this, key, value) result(info)
    implicit none
    class(FH5Path),intent(inout)            :: this
    character(len=*),intent(in)             :: key
    class(FH5Attribute),intent(out)         :: value
    !
        info = criError
    !
    end function
    
    
    integer function FH5Path_getAttribute_collection(this, key, value) result(info)
    implicit none
    class(FH5Path),intent(inout)            :: this
    character(len=*),intent(in)             :: key
    class(FH5AttributeCollection),allocatable,intent(out)   :: value
    !
        info = criError
    !

    end function
    

    integer function FH5Path_setAttribute_item(this, key, value, old) result(info)
    implicit none
    class(FH5Path),intent(inout)            :: this
    character(len=*),intent(in)             :: key
    class(FH5Attribute),intent(in)          :: value
    class(FH5Attribute),intent(out),optional :: old
    !
        info = criError
    !
    end function
    
    
    integer function FH5Path_setAttribute_collection(this, key, value) result(info)
    implicit none
    class(FH5Path),intent(inout)            :: this
    character(len=*),intent(in)             :: key
    class(FH5AttributeCollection),intent(in)          :: value
    !
        info = criError
    !
    end function


    !
    ! Utility functions
    !

    logical function is_valid_id(id)
    implicit none
    integer(HID_T),intent(in)       :: id
    !
    logical :: valid
    integer :: hdferr
    !
        is_valid_id = .false.
        if (id /= id_none) then
            call h5iis_valid_f(id, valid, hdferr)
            if (valid .and. (hdferr == 0)) is_valid_id = .true.
        endif
    !
    end function
    
    
    
    subroutine print_open_handles(file_id)
    implicit none
    integer(hid_t), intent(in)  :: file_id
    !
    integer :: hdferr
    integer(kind=SIZE_T) :: still_open_cnt
    integer,dimension(5) :: obj_types
    integer :: i
    character(len=20),dimension(5) :: labels
    !
        obj_types = [   H5F_OBJ_ALL_F, & 
                        H5F_OBJ_FILE_F, &
                        H5F_OBJ_GROUP_F, &
                        H5F_OBJ_DATASET_F, &
                        H5F_OBJ_DATATYPE_F ]
        labels = [  'H5F_OBJ_ALL_F', & 
                    'H5F_OBJ_FILE_F', &
                    'H5F_OBJ_GROUP_F', &
                    'H5F_OBJ_DATASET_F', &
                    'H5F_OBJ_DATATYPE_F' ]
        
        do i = 1, size(obj_types)
            call H5Fget_obj_count_f(file_id, obj_types(i), still_open_cnt, hdferr)
            write(*,*) 'Still open (',labels(i) , ')', still_open_cnt
        enddo
    !
    end subroutine

    !> Split extended path into file path (filesystem path) and object path
    ! (HDF5 group path)
    !>
    !> extpath is in the form:
    !> file_path:object_path
    !> Both file_path and object_path can be relative or absolute. 
    !> Neither file_path nor object_path can be empty
    !> If object_path is a relative path, it will be assumed that it is 
    !> relative to the root group of the HDF5. The root group path must be
    !> referenced as '/'. 
    !> 
    subroutine splitHDF5extpath(extpath, file_path, object_path, info)
    implicit none
    character(len=*),intent(in)         :: extpath
    character(len=*),intent(out)        :: file_path
    character(len=*),intent(out)        :: object_path
    integer,intent(out)                 :: info
    !
    character,parameter :: xsep = ':'
    integer :: col_pos, extpath_len
    character(len=:),allocatable :: ep_tmp
    !
        ! validate the input
        info = criErr_BadArgs
        ep_tmp = adjustl(extpath)
        extpath_len = len_trim(ep_tmp)
        ! extpath must contain at least 3 characters: 
        !  - at least one character in file_path and 
        !  - ':' and
        !  - object_path, which is either '/' or a character
        if (extpath_len < 3) return
        ! Check if xsep appears exactly once, and at least at the 2nd position,
        ! and if the output strings are large enough to fit the results.
        col_pos = index(ep_tmp, xsep)
        if ((col_pos < 2) .or. &
            (col_pos /= index(ep_tmp, xsep, back=.true.)) .or. &
            (col_pos > len(file_path)) .or. &
            (extpath_len - col_pos - 1) > len(object_path)) return
        !
        file_path = trim(ep_tmp(:col_pos-1))
        object_path = trim(ep_tmp(col_pos+1:))
        info = criSuccess
    !
    end subroutine
end module
