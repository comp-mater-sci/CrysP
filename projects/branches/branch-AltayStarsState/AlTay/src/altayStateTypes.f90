module altayStateTypes
use criErrcodes
use criMathUtils
implicit none

    !> Representation of a single grain/crystal.
    !> \todo Identify & remove redundant fields.
    type :: Grain
        !> \todo replace tFI1, tPHI and tFI2 by EulerAngles object.
        double precision :: tFI1 = 0.D0,tPHI = 0.D0, tFI2 = 0.D0
        double precision :: tGEW = 1.D0 ,tGAM = 0.D0
        double precision, dimension(3,3) :: tT = 0.D0
        double precision, dimension(3,3) :: tF = unit_sr_matrix
        double precision, dimension(3,3) :: tZERO = 0.D0,tRHO = 0.D0
    end type


    !> \todo Find a more suitable name for the data type MaterialFrame
    !> \todo consider removal of the MaterialFrame data type
    type :: MaterialFrame
        double precision,dimension(3,3) :: FALG = unit_sr_matrix
    end type
    
    integer,parameter,private :: textureTitle_length = 40
    
    type :: TextureData
        !> \todo consider either moving nrstep somewhere else or removing it completely.
        !> Meta-data: step number
        integer                                 :: nrstep = 0
        
        !> Meta-data: title/comment 
        character(len=textureTitle_length)      :: title = ''
        
        type(Grain),dimension(:),allocatable    :: grains
        
    end type
    
    
    contains
    
    
    !> Expand/shrink the storage for crystals in the TextureData object.
    !>
    !> The old content is preserved if `keep_state` parameter is True.
    !> \todo do we actually need this feature? Wouldn't a regular "allocate" be sufficient?
    integer function textureData_resize(this, newsize, keep_state) result(info)
    implicit none
    type(TextureData),intent(inout)     :: this     !< object to be modified
    !> new size, i.e. number of crystals that can be stored in `this`
    integer,intent(in)                  :: newsize
    !> Flag: if true, the existing state to be preserved on resize. Default: .false.
    !> If the newsize is larger than the previous size, only the first `newsize`
    !> elements will be preserved.
    logical,intent(in),optional         :: keep_state
    !
    logical :: keep
    integer :: memstat, ntransf
    type(grain),dimension(:),allocatable  :: tmp
    !
        keep = .false.
        if (present(keep_state)) keep = keep_state
        !
        if (.not. allocated(this%grains)) then
            allocate(this%grains(newsize), stat=memstat)
        else
            if (size(this%grains) == newsize) then
                    ! Nothing to do.
                    info = criSuccess
                    return
            endif
            if (keep) then
                ! Transfer npoints 
                allocate(tmp(newsize),stat=memstat)
                if (memstat == 0) then
                    ntransf = min(newsize,size(this%grains))
                    tmp(1:ntransf) = this%grains(1:ntransf)
                    deallocate(this%grains)
                    call move_alloc(tmp,this%grains)
                endif
            else
                deallocate(this%grains)
                allocate(this%grains(newsize),stat=memstat)
            endif
        endif
        info = merge(criSuccess, criErr_MemAlloc, (memstat == 0))
    !
    end function
    
    !> Finish initialization of the grain structures. This function must be called
    !> after initial setting up the texture data.
    subroutine textureData_init(this, mf, info)
    implicit none
    type(TextureData),intent(inout)     :: this
    type(MaterialFrame),intent(in)      :: mf
    integer,intent(out)                 :: info
    !
    integer :: i
    !
        info = criErr_BadArgs
        if (allocated(this%grains)) then
            do i=1, size(this%grains)
                call grain_init(this%grains(i), mf, info)
            enddo
            info = criSuccess
        endif
    !
    end subroutine
    
            
    !> Set the computed fields in grain structure.
    !>
    !> The following fields are modified:
    !>  - tT is calculated from Euler angles as defined by the tfi1,
    !>    tPHI and tfi2 fields
    !>  - tAXES,tEULR,tF,tCIJ,tTAX - inherit corresponding properties 
    !>    from the MaterialFrame mf
    !>  - tZERO and tRHO - are zeroed.
    !> \todo consider converting grain_init into elemental subroutine
    subroutine grain_init(this, mf, info)
    implicit none
    type(Grain),intent(inout)       :: this
    type(MaterialFrame),intent(in)  :: mf
    integer,intent(out)             :: info
    !
        this%tT = rotmat(this%tfi1,this%tPHI,this%tfi2)
        !
        ! initialize the remaining components with mf data...
        this%tF   = mf%FALG
        ! ... and zero all the rest.
        this%tZERO = 0.D0
        this%tRHO  = 0.D0
        info = criSuccess
    !
    end subroutine
    
    
end module