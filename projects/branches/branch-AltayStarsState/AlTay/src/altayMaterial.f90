! $Id$

!> Material model used in AlTay
module altayMaterial
use criErrcodes
use altayHard, only: HardeningModels
implicit none


    
    !> Data type that characterizes the material
    type :: altayMaterialData
        
        type(HardeningModels)   :: hardening
        
    end type
    

end module
