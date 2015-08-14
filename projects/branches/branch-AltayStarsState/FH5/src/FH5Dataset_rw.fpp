!
! $Id$
!
    !
    ! Parametric template of FH5Dataset write and read operations
    !
    
    ! Parameters:
    ! 
    ! FX_NAME_WRITE e.g. FH5Dataset_write_2D_double
    ! FX_NAME_READ  e.g. FH5Dataset_read_2D_double
    ! FORTRAN_TYPE e.g. double precision,dimension(:,:)
    ! HDF5_TYPE e.g. H5T_NATIVE_DOUBLE
    ! RANK e.g. 2
    ! SHAPESPEC which must correspond to RANK and be in form: 
    !           e.g. this%shape(1),this%shape(2)




    integer function  FX_NAME_WRITE(this, name, array) result(info)
    implicit none
    class(FH5Dataset),intent(inout)                 :: this
    character(len=*),intent(in)                     :: name
    FORTRAN_TYPE,intent(in) :: array
    
    !
    integer :: hdferr
    !
        info = criErr_IO
        this%shape = shape(array, kind=hsize_t)
        ! Write the dataset
        call h5dcreate_f(this%location_id, name, HDF5_TYPE, this%dataspace_id, &
                         this%object_id, hdferr, dcpl_id=this%plist_id)
        if (hdferr /= 0) return
        call h5dwrite_f(this%object_id, mem_type_id=HDF5_TYPE, buf=array, &
                        dims=this%shape, hdferr=hdferr)
        CHOOSE(info, hdferr == 0, criSuccess, criErr_IOWrite)
    !
    end function
    
    
    integer function FX_NAME_READ(this, array) result(info)
    implicit none
    class(FH5Dataset),intent(inout)                  :: this
    FORTRAN_TYPE,allocatable,intent(out) :: array
    !
    integer :: hdferr
    integer, parameter :: rank = RANK
    !
        info = criErr_BadArgs
        ! Check if object state is consistent
        if (allocated(this%shape) .and. &
            is_valid_id(this%object_id) .and. &
            is_valid_id(this%dataspace_id)) then
            !
            if (any(this%shape <= 0).or. (size(this%shape) /= rank)) return
            !
            allocate(array(SHAPESPEC))
            call h5dread_f(this%object_id, mem_type_id=HDF5_TYPE, buf=array, &
                           dims=this%shape, hdferr=hdferr)
            CHOOSE(info, hdferr == 0, criSuccess, criErr_IORead)
        endif
    end function

    
    !> Create dataset and write data.
    !> \todo Consider transfering to FH5Group.
    integer function FX_NAME_MAKE(this, path, name, array, compression, chunks) result(info)
    implicit none
    class(FH5Dataset),intent(out)                   :: this
    class(FH5Path),intent(in)                       :: path
    character(len=*),intent(in)                     :: name
    FORTRAN_TYPE,intent(in)                         :: array
    integer,intent(in),optional                     :: compression
    integer,dimension(:),intent(in),optional        :: chunks
    !
    integer :: ierr
    !
        info = FH5Dataset_initialize_rw(this, path, shape(array), compression, chunks)
        if (info == criSuccess) then
            info = this%write(name, array)
        endif
        ierr = this%close()
    !
    end function


    
! Cleanup
    
#undef FX_NAME_WRITE
#undef FX_NAME_READ
#undef FX_NAME_MAKE
#undef FORTRAN_TYPE
#undef RANK
#undef SHAPESPEC
#undef HDF5_TYPE H5T_NATIVE_DOUBLE

