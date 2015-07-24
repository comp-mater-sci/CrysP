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
