module altayMillerIndices

    type,public :: MillerIndices
        
        integer,dimension(3) :: index
        
    contains
        
        procedure,pass :: is0 => Miller_is0
        
    end type
    
    
    contains
       
    logical function Miller_is0(this)
    implicit none
    class(MillerIndices), intent(in) :: this
        !
        Miller_is0 = this%index(1)==0 .and. this%index(2)==0 .and. this%index(3)==0 
        !
    end function
 
    
end module
    