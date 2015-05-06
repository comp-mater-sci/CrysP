module altayStateTypes
use criErrcodes
use criMathUtils
implicit none


    type :: MesostructureState
        
        double precision, dimension (3,3) :: DeformationGradient = unit_sr_matrix
        
        type(EulerAngles), dimension(:), allocatable   :: GrainBoundaryEuler_initial
        
    end type
        
    !> \todo is this structure usefull? keep, modify or remove?
    type :: ClusterState
        
        !type(Grain),dimension(:),pointer :: grain !=> null()
        
        !> \todo add the appropriate grain boundary data: euler angles + weighting factor
        !type(EulerAngles) :: GBeuler
        
    end type

    
    contains
    
    
end module