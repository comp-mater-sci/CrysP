! $Id$

!> Material model used in AlTay
module altayMaterial
use criErrcodes
implicit none


    
    !> Data type that characterizes the material
    type :: MaterialData
        
        !> Number of slip systems in the material structure
        integer :: n_slip_systems = 0
        
        
        
    end type
    

end module
