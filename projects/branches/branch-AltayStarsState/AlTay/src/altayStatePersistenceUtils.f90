!
! $Id$
!
#include "criMacros.fpp"

!> Auxiliary functions that support state persistence schemes.
module altayStatePersistenceUtils
use criErrcodes
use altayConfig
use altayNativePersistenceScheme
use altayHDF5PersistenceScheme
implicit none

    contains

    function statePersistenceFactory(config, info) result(instance)
    implicit none
    class(StatePersistenceScheme),pointer       :: instance
    class(StatePersistenceConfig),intent(in)    :: config
    integer,intent(out)                         :: info
    !
    integer :: i, ierr
    type(NativePersistenceScheme),pointer :: ptr_native
    type(HDF5PersistenceScheme),pointer :: ptr_hdf5
    !
        nullify(instance)
        ! Instantiate
        select case(config%scheme_id)
        case(CNF_StatePersistencyNative)
            RETURN_ON_WITH(allocate(ptr_native,stat=ierr), ierr/=0, info=criErr_MemAlloc)
            call ptr_native%initialize(config%access_mode, size(config%phases), info)
            ! Add records of individual phases
            do i = lbound(config%phases,dim=1), ubound(config%phases,dim=1)
                associate (tex =>config%phases(i)%odf)
                    call ptr_native%setPhase(i, tex%format_id, tex%file_name, tex%block_id, info)
                end associate
            enddo
            instance => ptr_native
        case(CNF_StatePersistencyHDF5)
            allocate(HDF5PersistenceScheme :: ptr_hdf5, stat=ierr)

             !> \fixme Get rid of hard-coded constant in `groupname` argument
            call ptr_hdf5%initialize(trim(config%path), &
                                        groupname='Step', &
                                        is_incremental=(config%access_mode /= StatePersistence_Read), &
                                        access_mode=config%access_mode, &
                                        info=info)

            instance => ptr_hdf5
        end select
    !
    end function
    
end module
