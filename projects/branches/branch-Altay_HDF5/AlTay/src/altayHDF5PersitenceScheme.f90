!
! $Id$
!

!> State persistence based on HDF5 API.
module altayHDF5PersistenceScheme
use criErrcodes
use altayStatePersistence
use altayState
use altayHDF5Context
implicit none

    
    type,extends(StatePersistenceScheme) :: HDF5PersistenceScheme

        type(HDF5Context)      :: mesostructure_storage

        type(HDF5Context)      :: texture_storage
        
        type(HDF5Context)      :: hardening_storage
       
    contains
    
        procedure :: initialize => HDF5PersistenceScheme_initialize
        procedure :: saveState => HDF5PersistenceScheme_saveState
        procedure :: loadState => HDF5PersistenceScheme_loadState

        final :: HDF5PersistenceScheme_finalize
        
    end type
    
    
contains

    subroutine HDF5PersistenceScheme_initialize(this, fileformat, path, blockid, info)
    implicit none
    class(HDF5PersistenceScheme),intent(inout) :: this
    integer,intent(in)                          :: fileformat
    character(len=*),intent(in)                 :: path
    integer,intent(in)                          :: blockid
    integer,intent(out)                         :: info
    !
        info = criSuccess
    !
    end subroutine
    
    
    !> Release resources and finalize the instance.
    subroutine HDF5PersistenceScheme_finalize(this)
    implicit none
    type(HDF5PersistenceScheme),intent(inout) :: this
    !
    integer :: info
    !
        !info = this%mesostructure_storage%close()
        !info = this%texture_storage_context%close()
        !info = this%hardening_storage_context%close()
    !
    end subroutine
    
    
    
    subroutine HDF5PersistenceScheme_saveState(this, state, info)
    implicit none
    class(HDF5PersistenceScheme),intent(inout)   :: this
    type(altayStateVariables),intent(in)        :: state
    integer,intent(out)                         :: info
    !
        info = criSuccess
    !
    end subroutine
        
        
    subroutine HDF5PersistenceScheme_loadState(this, state, info)
    implicit none
    class(HDF5PersistenceScheme),intent(inout)  :: this
    type(altayStateVariables),intent(inout)     :: state
    integer,intent(out)                         :: info
    !
        info = criSuccess
    !
    end subroutine
    
        
        
        

        
end module
