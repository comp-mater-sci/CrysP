! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2011-09-17
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!> Various utility subroutines and functions
module alamUtils

      double precision,parameter ::  rad2deg = (180.D0 / acos(-1.D0)), deg2rad = (acos(-1.D0) / 180.D0)

      double precision,parameter ::  root23 = sqrt(2.D0/3.D0)

      integer,parameter       :: display_unit = 6

contains

      pure function vec_norm2(v)
      implicit none
      double precision :: vec_norm2
      double precision,dimension(:),intent(in) :: v
      vec_norm2 = sqrt(dot_product(v,v))
      end function

end module
