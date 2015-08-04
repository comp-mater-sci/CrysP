!
! $Id$
!

module altayMillerIndices

    type,public :: MillerIndices
        
        integer,dimension(3) :: index = 0
        
    contains
        
        procedure,pass(this) :: is0 => Miller_is0
        
    end type
    
    
    contains
       
    logical function Miller_is0(this)
    implicit none
    class(MillerIndices), intent(in) :: this
        !
        Miller_is0 = all(this%index(:) == 0)
        !
    end function
 
    
end module
    