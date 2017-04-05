! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2011-09-17 (under the name dmcUtils)
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!> Various utility subroutines and functions
module dmcUtils
use criMathUtils
use criRuntime
implicit none
      
      integer,parameter       :: display_unit = 6

      character,parameter     :: default_comment_sign = '#'
     
contains

      double precision pure function average(a)
      double precision,dimension(:),intent(in) :: a
      integer :: n
      !
            n = size(a)
            if (n >= 1) average = sum(a) / dble(n)                 
            ! Undefined for empty array
      end function

end module
