!
! $Id$
!

!> Auxiliary functions that support state persistence schemes.
module altayStatePersistenceUtils
use criErrcodes
use altayConfig
use altayStatePersistence
use altayNativePersistenceScheme
use altayHDF5PersistenceScheme
implicit none

contains
    
    !> \fixme StatePersistenceConfig should be used as the argument 
    function statePersistenceFactory(texcnf, as_input) result(instance)
    implicit none
    class(StatePersistenceScheme),pointer       :: instance
    type(TextureConfig),intent(in)              :: texcnf
    logical,intent(in)                          :: as_input
    !
    integer :: info
    !
        nullify(instance)
        ! Instantiate
        select case(texcnf%format_id)
        case(TF_SMT,TF_CUR, TF_CUB)
            allocate(NativePersistenceScheme :: instance)

        case(TF_HDF5)
            allocate(HDF5PersistenceScheme :: instance)
        end select
        !
        ! Cast the type to get proper initialization method
        select type(instance)
        class is(NativePersistenceScheme)
            call instance%initialize(texcnf%format_id, trim(texcnf%file_name), &
                                        texcnf%block_id, info)
        class is(HDF5PersistenceScheme)
            !> \fixme Get rid of hard-coded constant in `groupname` argument
            call instance%initialize(trim(texcnf%file_name), &
                                        groupname='Step', &
                                        is_incremental=.not.as_input, &
                                        is_readonly=as_input, &
                                        info=info)
        end select
    !
    end function
    
end module
