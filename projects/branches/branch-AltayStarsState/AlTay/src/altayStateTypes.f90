module altayStateTypes
use criErrcodes
use criMathUtils
implicit none


    !> \todo Find a more suitable name for the data type MaterialFrame
    !> \todo consider removal of the MaterialFrame data type
    type :: MaterialFrame
        
        integer :: nstep = 0
        
        double precision,dimension(3,3) :: FALG = unit_sr_matrix
    end type

    
    contains
    
    
end module