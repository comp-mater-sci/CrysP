!
! $Id$
!
module altaySimulation
use criErrcodes
use altayConfig
use altayStatePersistence
use altayNativePersistenceScheme
use altayHDF5PersistenceScheme

implicit none

    type :: Simulation
        
        
        
    end type


contains


        function statePersistenceFactory(texcnf, readonly) result(instance)
        implicit none
        class(StatePersistenceScheme),pointer       :: instance
        type(TextureConfig),intent(in)              :: texcnf
        logical,intent(in)                          :: readonly
        !
        integer :: info
        !
            nullify(instance)
            ! Instantiate
            select case(texcnf%input_type)
            case(TF_SMT,TF_CUR, TF_CUB)
                allocate(NativePersistenceScheme :: instance)

            case(TF_HDF5)
                allocate(HDF5PersistenceScheme :: instance)
            end select
            !
            ! Cast the type to get proper initialization method
            select type(instance)
            class is(NativePersistenceScheme)
                call instance%initialize(texcnf%input_type, trim(texcnf%input_fname), &
                                            texcnf%block_id, info)
            class is(HDF5PersistenceScheme)
                call instance%initialize(trim(texcnf%input_fname), &
                                         groupname='default', &
                                         is_incremental=.false., &
                                         is_readonly=readonly, &
                                         info=info)
            end select
        !
        end function
        
        
        ! Initialize state variables:
        !   - get state data from the storage, OR
        !   - get part of the state from storage, generate other
        !   - make all assemblies from the state variables
        ! Initialize runtime components:
        !   - output request filters 
        
        ! for step in steps:
            ! prepare the step data + state variables
            ! run calculations
            ! save state variables to storage (if requested)
            ! put SV and SDV through the output request filters
        
        
end module
