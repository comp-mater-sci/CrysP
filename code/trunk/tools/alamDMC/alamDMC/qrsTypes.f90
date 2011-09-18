!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2011-09-14
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

!> Useful data types for calculation of anisotropic characteristics
module qrsTypes

      type qrsData
            double precision :: qvalue = 0.D0
            double precision :: rvalue = 0.D0
            double precision :: svalue = 0.D0
      end type    

end module