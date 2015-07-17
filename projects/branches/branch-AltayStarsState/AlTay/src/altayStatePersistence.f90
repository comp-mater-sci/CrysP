!
! $Id$
!

module altayStatePersistence
use criErrcodes
use altayState
use altayTexFormatConstants
implicit none

    type,abstract :: StatePersistenceScheme
        
    contains
        procedure(StatePersistenceScheme_saveState_interface),pass(this),deferred :: saveState
        procedure(StatePersistenceScheme_loadState_interface),pass(this),deferred :: loadState
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

end module
