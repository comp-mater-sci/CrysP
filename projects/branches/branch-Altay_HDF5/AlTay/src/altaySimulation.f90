!
! $Id$
!
module altaySimulation
use altayTexAccess
use altayStatePersistence
use altayConfig
implicit none

    type :: Simulation
        
        
        
    end type


contains


        function statePersistenceFactory(texcnf) result(instance)
        implicit none
        class(StatePersistenceScheme),pointer      :: instance
        type(TextureConfig),intent(in) :: texcnf
        !
        integer :: info
            nullify(instance)

            ! Currenly we use just the Native scheme
            
            allocate(NativePersistenceScheme :: instance)
            select type(instance)
            class is(NativePersistenceScheme)
                call instance%initialize(texcnf%input_type, trim(texcnf%input_fname),&
                                         texcnf%block_id, info)
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
