#ifndef HDF5_DISABLE
module altayHDF5Access
use criErrcodes
use FH5
use altayTexAccess
use altayHDF5Context
implicit none

    character(len=12),parameter :: HDF5Access_default_dataset_name = 'DiscreteODF'

    !>
    !>
    !> \todo Consider implementing HDF5 type(s) for DiscreteODF. This would 
    !>       eliminate the temporary arrays in read/write.
    type,extends(TextureAccess) :: HDF5Access
        
        type(HDF5Context)       :: context
        
    contains
        procedure,pass(this)    :: initialize => HDF5Access_initialize
        procedure,pass(this)    :: read => HDF5Access_read
        procedure,pass(this)    :: write => HDF5Access_write

    end type

contains
    
    
    subroutine HDF5Access_initialize(this, context, readonly, info)
    class(HDF5Access),intent(inout)   :: this
    type(HDF5Context),intent(inout)             :: context
    logical,intent(in)                          :: readonly
    integer,intent(out)                         :: info
    !
        this%context = context
        info = criSuccess
    !
    end subroutine
   
    
    !> Read texture data
    function HDF5Access_read(this, blockid, odf) result(info)
    implicit none
    class(HDF5Access),intent(inout)             :: this
    integer,intent(in)                          :: blockid
    type(DiscreteODF),intent(inout)             :: odf
    integer                                     :: info     !< exit code
    !
    type(FH5DatasetScoped)      :: dataset
    type(FH5Path)               :: h5_path
    ! Temporary for plain data from DiscreteODF
    double precision,dimension(:,:),allocatable :: tmp_arr
    integer, parameter :: nfields = 4
    ! explicit temporary to avoid warnings from runtime checks
    double precision,dimension(3) :: tmp_euler
    integer :: i, ierr, npoints
    !
        info = criErr_BadArgs
        !
        ! Create dataset inside the group given by the context.
        h5_path%object_id = this%context%group_id
        info = FH5Dataset_init(dataset, h5_path, &
                               HDF5Access_default_dataset_name)
        if (info /= criSuccess) return
        ! Do the IO and close the dataset.
        info = dataset%read(tmp_arr)
        ! Pack the data into DiscreteODF
        if (allocated(tmp_arr) .and. (info == criSuccess)) then
            ! Get the size of the odf
            npoints = size(tmp_arr, dim=1)
            if (npoints == 0) return
            info = DiscreteODF_resize(odf, npoints)
            do i=1,npoints
                tmp_euler = tmp_arr(i,1:3)
                odf%orientations(i)%euler = Arr2EulerAngles(tmp_euler)
                odf%orientations(i)%weight = tmp_arr(i,4)
            enddo
        else
            info = criError
        endif
    !
    end function
    
    !> Write texture data
    function HDF5Access_write(this, blockid, odf) result(info)
    implicit none
    class(HDF5Access),intent(inout)   :: this
    integer,intent(in)                          :: blockid
    type(DiscreteODF),intent(in)                :: odf
    integer                                     :: info     !< exit code
    !
    type(FH5DatasetScoped)  :: dataset
    type(FH5Path)           :: h5_path
    ! Temporary plain array for structured data from DiscreteODF
    double precision,dimension(:,:),allocatable :: tmp_arr
    integer, parameter :: nfields = 4
    integer :: i, ierr, npoints
    !
        info = criErr_BadArgs

        ! Get the size of the odf
        npoints = size(odf)
        if (npoints <= 0) return
        info = criErr_MemAlloc
        allocate(tmp_arr(npoints, nfields), stat=ierr)
        if (ierr /= 0) return
        forall (i=1:npoints)
            tmp_arr(i,1:3) = EulerAngles2Arr(odf%orientations(i)%euler)
            tmp_arr(i,4) = odf%orientations(i)%weight
        endforall
        !
        ! Create dataset inside the group given by the context.
        h5_path%object_id = this%context%group_id
        info = FH5Dataset_init(dataset, h5_path, &
                               shape=shape(tmp_arr), &
                               compression=FH5_compression_szip)
        if (info /= criSuccess) return
        ! Do the IO and close the dataset.
        info = dataset%write(HDF5Access_default_dataset_name, tmp_arr)
    !
    end function

end module
#endif
