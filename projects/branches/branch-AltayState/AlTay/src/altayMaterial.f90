! $Id$

!> Material model used in AlTay
module altayMaterial
use criErrcodes
use altayHard, only: HardeningModels
implicit none


    
    !> Data type that characterizes the material
    type :: altayMaterialData
        
        !> Number of slip systems in the material structure
        integer                 :: n_slip_systems = 0
        
        type(HardeningModels)   :: hardening
        
    end type
    

end module
