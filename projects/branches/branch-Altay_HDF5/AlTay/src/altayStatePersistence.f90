!
! $Id$
!

module altayStatePersistence
use criErrcodes
use altayState
use altayTexFormatConstants
use altayTexFormats
use altayIOContext
use altayHDF5Context
implicit none

    type,abstract :: StatePersistenceScheme
        
    contains
        procedure(StatePersistenceScheme_saveState_interface),pass(this),deferred :: saveState
        procedure(StatePersistenceScheme_loadState_interface),pass(this),deferred :: loadState
    end type
    
    type,extends(StatePersistenceScheme) :: NativePersistenceScheme
        
        class(TextureRawFileAccess),pointer     :: texture_storage
        
        integer                                 :: input_texture_format_id = TF_SMT
        
        integer                                 :: input_texture_blockid = 0
        
        type(RawFileContext)                    :: texture_storage_context
        
        type(RawFileContext)                    :: hardening_storage_context
        
    contains
        procedure :: initialize => NativePersistenceScheme_initialize
        procedure :: saveState => NativePersistenceScheme_saveState
        procedure :: loadState => NativePersistenceScheme_loadState
        
    end type
    
    type,extends(StatePersistenceScheme) :: HDF5PersitenceScheme

        type(HDF5Context)      :: mesostructure_storage

        type(HDF5Context)      :: texture_storage
        
        type(HDF5Context)      :: hardening_storage
       
    contains
        procedure :: saveState => HDF5PersitenceScheme_saveState
        procedure :: loadState => HDF5PersitenceScheme_loadState

    end type
    
    
    abstract interface
        
        subroutine StatePersistenceScheme_saveState_interface(this, state, info)
            import :: StatePersistenceScheme, altayStateVariables
            class(StatePersistenceScheme),intent(inout)  :: this
            type(altayStateVariables),intent(in)        :: state
            integer,intent(out)                         :: info
        end subroutine
        
        
        subroutine StatePersistenceScheme_loadState_interface(this, state, info)
            import :: StatePersistenceScheme, altayStateVariables
            class(StatePersistenceScheme),intent(inout) :: this
            type(altayStateVariables),intent(inout)     :: state
            integer,intent(out)                         :: info
        end subroutine

    end interface
    
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
            this%input_texture_format_id = fileformat
            this%input_texture_blockid = blockid
            this%texture_storage_context = RawFileContext(path)
        case default
            return
        end select
        info = criSuccess
    !
    end subroutine
    
    subroutine NativePersistenceScheme_saveState(this, state, info)
    implicit none
    class(NativePersistenceScheme),intent(inout) :: this
    type(altayStateVariables),intent(in)        :: state
    integer,intent(out)                         :: info
    !
        
        info = criSuccess
    !
    end subroutine
        
        
    subroutine NativePersistenceScheme_loadState(this, state, info)
    implicit none
    class(NativePersistenceScheme),intent(inout)  :: this
    type(altayStateVariables),intent(inout)     :: state
    integer,intent(out)                         :: info
    !
    integer :: step_number
    !
        ! Prepare access to the storage
        this%texture_storage => textureAccessFactory(this%input_texture_format_id, &
                                                     this%texture_storage_context, &
                                                     readonly=.true., info=info)
        if (.not. associated(this%texture_storage) .or. (info /= criSuccess)) return
        info = this%texture_storage%read(this%input_texture_blockid, state%texture) 
        if (info /= criSuccess) return
        ! Set other state components, if the reader offers them.
        select type(ptr => this%texture_storage)
        class is(TextureMetaRawFileAccess)
            call ptr%getExtendedMetaData(state%mesostructure%deformationgradient, step_number, info)
        end select
    !
    end subroutine

    
    subroutine HDF5PersitenceScheme_saveState(this, state, info)
    implicit none
    class(HDF5PersitenceScheme),intent(inout)  :: this
    type(altayStateVariables),intent(in)        :: state
    integer,intent(out)                         :: info
    !
        info = criSuccess
    !

    end subroutine
        
        
    subroutine HDF5PersitenceScheme_loadState(this, state, info)
    implicit none
    class(HDF5PersitenceScheme),intent(inout)  :: this
    type(altayStateVariables),intent(inout)     :: state
    integer,intent(out)                         :: info
    !
        info = criSuccess
    !
    end subroutine
    
        
        
        

        
end module
