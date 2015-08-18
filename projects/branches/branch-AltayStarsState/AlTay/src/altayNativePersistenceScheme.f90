!
! $Id$
!
#include "criMacros.fpp"

!> State persistence based on native Fortran IO operations.
module altayNativePersistenceScheme
use criErrcodes
use altayStateTypes
use altayStatePersistence
use altayTexFormatConstants
use altayTexFormats
use altayIOContext
implicit none


    type,private :: NativePersistencePhaseStorage
        
        integer                                 :: phase_id = 0
        
        class(TextureRawFileAccess),pointer     :: texture_storage
        
        integer                                 :: texture_format_id = TF_SMT
        
        integer                                 :: texture_blockid = 0
        
        type(RawFileContext)                    :: texture_storage_context
        
        type(RawFileContext)                    :: hardening_storage_context

    contains
        procedure,pass(this) :: initialize => NativePersistencePhaseStorage_initialize
        procedure,pass(this) :: loadState => NativePersistencePhaseStorage_loadState
        procedure,pass(this) :: saveState => NativePersistencePhaseStorage_saveState
        
        procedure,pass(this) :: getExtendedState => NativePersistencePhaseStorage_getExtendedState
        
        final :: NativePersistencePhaseStorage_finalize
    end type

    !> State persistence based on native Fortran IO operations
    type,extends(StatePersistenceScheme) :: NativePersistenceScheme
        
        integer,private :: added_storage_index = 0
        
        type(NativePersistencePhaseStorage),dimension(:),allocatable :: storage
        
    contains
        procedure :: initialize => NativePersistenceScheme_initialize
        procedure :: setPhase => NativePersistenceScheme_setPhase
        
        procedure :: saveState => NativePersistenceScheme_saveState
        procedure :: loadState => NativePersistenceScheme_loadState
        
        final :: NativePersistenceScheme_finalize
        
    end type

contains

    
    
    subroutine NativePersistenceScheme_initialize(this, access_mode, n_phases, info)
    implicit none
    class(NativePersistenceScheme),intent(inout)    :: this
    integer,intent(in)                              :: access_mode
    integer,intent(in)                              :: n_phases
    integer,intent(out)                             :: info
    !
    integer :: ierr
        info = criErr_BadArgs
        if ((n_phases <= 0) .or. all(StatePersistence_AccessModes /= access_mode)) return
        !
        this%access_mode = access_mode
        this%added_storage_index = 0
        if (allocated(this%storage)) deallocate(this%storage)
        allocate(this%storage(n_phases),stat=ierr)
        CHOOSE(info, ierr == 0, criSuccess, criErr_MemAlloc)
    !
    end subroutine
    
    
    !> 
    subroutine NativePersistenceScheme_finalize(this)
    implicit none
    type(NativePersistenceScheme),intent(inout) :: this
    !
    integer :: i
    !
        ! Note: deallocation of this%storage does not trigger automatic
        !       finalization of the members...
        if (allocated(this%storage)) then
            do i = 1, size(this%storage)
                call NativePersistencePhaseStorage_finalize(this%storage(i))
            enddo
            deallocate(this%storage)
        endif
    !
    end subroutine
    
    subroutine  NativePersistenceScheme_setPhase(this,phase_id, format_id, path, block_id, info)
    implicit none
    class(NativePersistenceScheme),intent(inout)    :: this
    integer,intent(in)                              :: phase_id
    integer,intent(in)                              :: format_id
    character(len=*),intent(in)                     :: path
    integer,intent(in)                              :: block_id
    integer,intent(out)                             :: info
    !
    integer :: storage_size
    !
        info = criError
        ALLOCATED_SIZE(storage_size, this%storage)
        if (this%added_storage_index > storage_size) return
        !
        this%added_storage_index = this%added_storage_index + 1
        call this%storage(this%added_storage_index)%initialize(phase_id, format_id, path, block_id,info)
    !
    end subroutine
    
    !> Save state variables to an external storage.
    subroutine NativePersistenceScheme_saveState(this, state, info)
    implicit none
    class(NativePersistenceScheme),intent(inout) :: this
    type(altayStateVariables),intent(in)        :: state
    integer,intent(out)                         :: info
    !
    integer :: storage_size, i
    !
        info = criError
        ALLOCATED_SIZE(storage_size, this%storage)
        do i = 1, storage_size
            call this%storage(i)%saveState(state, info)
            if (info /= criSuccess) return
        enddo
    !
    end subroutine
    
    !> Load state variables from external storage.
    subroutine NativePersistenceScheme_loadState(this, state, info)
    implicit none
    class(NativePersistenceScheme),intent(inout):: this
    type(altayStateVariables),intent(inout)     :: state
    integer,intent(out)                         :: info
    !
    type(DiscreteODF),dimension(:),allocatable  :: odfs
    integer :: i, j, storage_size, ierr, n_grains
    double precision,dimension(:),allocatable :: accumulatedshear
    !
        ALLOCATED_SIZE(storage_size, this%storage)
        RETURN_IF(storage_size /= size(state%material%phases), info = criErr_BadArgs)
        ! Load state for all phase into cache of NativePersistencePhaseStorage.
        ! Once all these are all known, we may place them into state.
        allocate(odfs(storage_size), stat=ierr)
        do i = 1, storage_size
            call this%storage(i)%loadState(state, odfs(i), info)
        enddo
        call initialize(state%grainstates, odfs, state%material, info)
        if (info /= criSuccess) return
        !
        ! Intrusive operation: we set components of grain state.
        associate(s => state%grainstates%grains)
            j = 1
            do i = 1, storage_size
                !> \fixme NativePersistenceScheme_loadState: deformation gradient should
                !>        be loaded to the phase-wide mesostructure. Otherwise the global
                !>        deformation gradient will originate from the last phase.
                call this%storage(i)%getExtendedState(accumulatedshear, &
                                                      state%mesostructure%deformationgradient, &
                                                      info)
                ALLOCATED_SIZE(n_grains, accumulatedshear)
                if (n_grains > 0) then
                    s(j:j+n_grains)%accumulatedshear = accumulatedshear
                    deallocate(accumulatedshear)
                endif
                ! Move on to the beginning of the grain state from the next ODF
                j = j + size(odfs(i))
            enddo
        end associate
    !
    end subroutine

    
    subroutine NativePersistencePhaseStorage_initialize(this, phase_id, fileformat, path, blockid, info)
    implicit none
    class(NativePersistencePhaseStorage),intent(inout) :: this
    integer,intent(in)                          :: phase_id
    integer,intent(in)                          :: fileformat
    character(len=*),intent(in)                 :: path
    integer,intent(in)                          :: blockid
    integer,intent(out)                         :: info
    !
        info = criErr_BadArgs
        this%phase_id = phase_id
        select case(fileformat)
        case(TF_SMT, TF_CUR, TF_CUB)
            this%texture_format_id = fileformat
            this%texture_blockid = blockid
            this%texture_storage_context = RawFileContext(path)
        case default
            return
        end select
        info = criSuccess
    !
    end subroutine
    
    
    !> Save phase state variables to an external storage.
    subroutine NativePersistencePhaseStorage_saveState(this, state, info)
    implicit none
    class(NativePersistencePhaseStorage),intent(inout) :: this
    type(altayStateVariables),intent(in)        :: state
    integer,intent(out)                         :: info
    !
    integer, parameter :: step_number = 0
    type(DiscreteODF) :: odf
    integer,dimension(:),allocatable :: map
    !
        info = criSuccess
        ! Prepare access to the storage
        if (.not. associated(this%texture_storage)) then
            ! Unless non-empty filename is provided, use phase name and change extension
            associate (outpath => this%texture_storage_context%path)
                if (len_trim(outpath) == 0) then
                    outpath = state%material%phases(this%phase_id)%name
                    select case(this%texture_format_id)
                    case(TF_SMT)
                        outpath = trim(outpath)//'.smt'
                    case(TF_CUR)
                        outpath = trim(outpath)//'.cur'
                    case(TF_CUB)
                        outpath = trim(outpath)//'.cub'
                    end select
                endif
            end associate
            this%texture_storage => textureAccessFactory(this%texture_format_id, &
                                                         this%texture_storage_context, &
                                                         readonly=.false., info=info)
        endif
        if (.not. associated(this%texture_storage) .or. (info /= criSuccess)) return
        ! Get ODF of the phase
        call GrainStateCollection_DiscreteODF(state%grainstates, odf, this%phase_id, info) 
        if (info /= criSuccess) return
        !
        ! Set other state components, if the access method allows them.
        select type(ptr => this%texture_storage)
        class is(TextureMetaRawFileAccess)
            ! Extract the right data: we need to get indices of grains 
            ! associated to the phase
            call GrainStateCollection_reverseMapping(state%grainstates,this%phase_id, map, info)
            if (info /= criSuccess) return
             !> \fixme NativePersistencePhaseStorage_saveState: defromation gradient should
            !>        be loaded to the phase-wide mesostructure
            call ptr%setExtendedMetaData(state%mesostructure%deformationgradient, &
                                            step_number, &
                                            state%grainstates%grains(map)%accumulatedshear,&
                                            info)
        end select
        ! Do the IO
        info = this%texture_storage%write(this%texture_blockid, odf)
        !
        ! Finish texture storage if it uses a single-block file format.
        select case(this%texture_format_id)
        case(TF_SMT, TF_CUB)
            deallocate(this%texture_storage)
            nullify(this%texture_storage)
        end select

    !
    end subroutine
    
    
    !> Load state variables from external storage.
    subroutine NativePersistencePhaseStorage_loadState(this, state, odf, info)
    implicit none
    class(NativePersistencePhaseStorage),intent(inout):: this
    type(altayStateVariables),intent(inout)     :: state
    type(DiscreteODF),intent(inout)             :: odf
    integer,intent(out)                         :: info
    !
        ! Prepare access to the storage
        this%texture_storage => textureAccessFactory(this%texture_format_id, &
                                                     this%texture_storage_context, &
                                                     readonly=.true., info=info)
        if (.not. associated(this%texture_storage) .or. (info /= criSuccess)) return
        info = this%texture_storage%read(this%texture_blockid, odf)
    !
    end subroutine
    
        
    subroutine NativePersistencePhaseStorage_getExtendedState(this, accumulatedshear, deformationgradient, info)
    implicit none
    class(NativePersistencePhaseStorage),intent(inout)  :: this
    double precision,dimension(:),allocatable,intent(out):: accumulatedshear
    double precision,dimension(3,3),intent(inout)        :: deformationgradient
    integer,intent(out)                                  :: info
    !
    integer :: step_number
    !
        info = criErr_BadArgs
        if (.not. associated(this%texture_storage)) return
        !
        info = criSuccess
        ! Get other state components, if the reader offers them.
        select type(ptr => this%texture_storage)
        class is(TextureMetaRawFileAccess)
            ! Initialize first:
            call ptr%getExtendedMetaData(deformationgradient, &
                                         step_number, &
                                         accumulatedshear, &
                                         info)
        end select
    !
    end subroutine

    
    subroutine NativePersistencePhaseStorage_finalize(this)
    implicit none
    type(NativePersistencePhaseStorage),intent(inout) :: this
    !
    integer :: info
    !
        if (associated(this%texture_storage)) then
            deallocate(this%texture_storage, stat=info)
            nullify(this%texture_storage)
        endif
        info = this%texture_storage_context%close()
        info = this%hardening_storage_context%close()
    !
    end subroutine

    
end module
