!
! $Id$
!
!> Data type for discrete ODF and primitive operations on the type.
module altayODfTypes
use criErrcodes
use criMathUtils
implicit none

    integer,parameter,private :: ODF_title_length = 40


    !> Representation of a single constituent of a discrete ODF: 
    !> discrete orientation.
    type :: DiscreteOrientation
        !> Orientation given by Euler angles
        type(EulerAngles) :: euler
        
        !> Weight factor associated with the orientation
        double precision :: weight = 1.D0

    end type


    !> Discrete ODF.
    !>
    !> A discrete ODF is usually seen as an aggregate of grains.
    type :: DiscreteODF
        
        !> Meta-data: title/comment 
        character(len=ODF_title_length)                :: title = ''
        
        type(DiscreteOrientation),dimension(:),allocatable :: orientations
        
    end type

    interface size
        module procedure DiscreteODF_size
    end interface

contains

    !> Expand/shrink the storage for crystals in the DiscreteODF object.
    !>
    !> The old content is preserved if `keep_state` parameter is True.
    !> \todo do we actually need this feature? Wouldn't a regular "allocate" be sufficient?
    integer function DiscreteODF_resize(this, newsize, keep_state) result(info)
    implicit none
    type(DiscreteODF),intent(inout)     :: this     !< object to be modified
    !> new size, i.e. number of crystals that can be stored in `this`
    integer,intent(in)                  :: newsize
    !> Flag: if true, the existing state to be preserved on resize. Default: .false.
    !> If the newsize is larger than the previous size, only the first `newsize`
    !> elements will be preserved.
    logical,intent(in),optional         :: keep_state
    !
    logical :: keep
    integer :: memstat, ntransf
    type(DiscreteOrientation),dimension(:),allocatable  :: tmp
    !
        keep = .false.
        if (present(keep_state)) keep = keep_state
        !
        if (.not. allocated(this%orientations)) then
            allocate(this%orientations(newsize), stat=memstat)
        else
            if (size(this%orientations) == newsize) then
                    ! Nothing to do.
                    info = criSuccess
                    return
            endif
            if (keep) then
                ! Transfer npoints 
                allocate(tmp(newsize),stat=memstat)
                if (memstat == 0) then
                    ntransf = min(newsize,size(this%orientations))
                    tmp(1:ntransf) = this%orientations(1:ntransf)
                    deallocate(this%orientations)
                    call move_alloc(tmp,this%orientations)
                endif
            else
                deallocate(this%orientations)
                allocate(this%orientations(newsize),stat=memstat)
            endif
        endif
        info = merge(criSuccess, criErr_MemAlloc, (memstat == 0))
    !
    end function


    !> Give the number of orientations in the discrete ODF
    elemental integer function DiscreteODF_size(this) result(n)
    implicit none
    type(DiscreteODF),intent(in)     :: this     !< object to be queried
    !
        n = 0
        if (allocated(this%orientations)) n = size(this%orientations)
    !
    end function
    
end module
