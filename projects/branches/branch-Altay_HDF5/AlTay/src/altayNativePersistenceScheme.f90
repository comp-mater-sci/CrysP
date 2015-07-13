!
! $Id$
!

!> State persistence based on native Fortran IO operations.
module altayNativePersistence
use criErrcodes
use altayStatePersistence
use altayState
use altayTexFormatConstants
use altayTexFormats
use altayIOContext
implicit none


    !> State persistence based on native Fortran IO operations
    type,extends(StatePersistenceScheme) :: NativePersistenceScheme
        
        class(TextureRawFileAccess),pointer     :: texture_storage
        
        integer                                 :: texture_format_id = TF_SMT
        
        integer                                 :: texture_blockid = 0

        type(RawFileContext)                    :: texture_storage_context
        
        type(RawFileContext)                    :: hardening_storage_context
        
    contains
        procedure :: initialize => NativePersistenceScheme_initialize
        procedure :: saveState => NativePersistenceScheme_saveState
        procedure :: loadState => NativePersistenceScheme_loadState
        
        final :: NativePersistenceScheme_finalize
        
    end type

contains

    
    
    subroutine NativePersistenceScheme_initialize(this, fileformat, path, blockid, info)
    implicit none
    class(NativePersistenceScheme),intent(inout) :: this
    integer,intent(in)                          :: fileformat
    character(len=*),intent(in)                 :: path
    integer,intent(in)                          :: blockid
    integer,intent(out)                         :: info
    !
        info = criErr_BadArgs
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
    
    
    !> 
    subroutine NativePersistenceScheme_finalize(this)
    implicit none
    type(NativePersistenceScheme),intent(inout) :: this
    !
    integer :: info
    !
        if (associated(this%texture_storage)) deallocate(this%texture_storage, stat=info)
        info = this%texture_storage_context%close()
        info = this%hardening_storage_context%close()
    !
    end subroutine
    
    
    !> Save state variables to an external storage.
    subroutine NativePersistenceScheme_saveState(this, state, info)
    implicit none
    class(NativePersistenceScheme),intent(inout) :: this
    type(altayStateVariables),intent(in)        :: state
    integer,intent(out)                         :: info
    !
    integer, parameter :: step_number = 0
    !
        ! Prepare access to the storage
        this%texture_storage => textureAccessFactory(this%texture_format_id, &
                                                     this%texture_storage_context, &
                                                     readonly=.false., info=info)
        if (.not. associated(this%texture_storage) .or. (info /= criSuccess)) return
        ! Set other state components, if the access method allows them.
        select type(ptr => this%texture_storage)
        class is(TextureMetaRawFileAccess)
            call ptr%setExtendedMetaData(state%mesostructure%deformationgradient, step_number, info)
        end select
        ! Do the IO
        info = this%texture_storage%write(this%texture_blockid, state%texture)
    !
    end subroutine
    
    
    !> Load state variables from external storage.
    subroutine NativePersistenceScheme_loadState(this, state, info)
    implicit none
    class(NativePersistenceScheme),intent(inout):: this
    type(altayStateVariables),intent(inout)     :: state
    integer,intent(out)                         :: info
    !
    integer :: step_number
    !
        ! Prepare access to the storage
        this%texture_storage => textureAccessFactory(this%texture_format_id, &
                                                     this%texture_storage_context, &
                                                     readonly=.true., info=info)
        if (.not. associated(this%texture_storage) .or. (info /= criSuccess)) return
        info = this%texture_storage%read(this%texture_blockid, state%texture) 
        if (info /= criSuccess) return
        ! Set other state components, if the reader offers them.
        select type(ptr => this%texture_storage)
        class is(TextureMetaRawFileAccess)
            call ptr%getExtendedMetaData(state%mesostructure%deformationgradient, step_number, info)
        end select
    !
    end subroutine

end module
