!
! $Id$
!

!> Top-level collection of hardening types
module altayHardTypes
use altayHardConstants
use altayCRSSTypes
use altayHardLaw_Simple
use altayHardLaw_KM
implicit none
      
    type :: HardeningModelParams
        
        integer             :: hardLawID = hard_none
        
        type(VoceParams)    :: voceParams
        
        type(SwiftParams)   :: swiftParams
        
        type(KMParameters)  :: kmParams
        
        !> \todo proper initialization of the ratios must be implemented
        type(CRSSData)      :: crss_ratios
        
    end type
    
end module
