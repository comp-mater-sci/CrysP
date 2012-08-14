!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2010-10-18
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!>    \file altayInterface.f90 The file contains automatically generated inferfaces
!>          that simplify consistency check at compile time.      
!>                                  
!               
module altayInterface

      ! Explicit interfaces of the subroutines in the F77 scope.
      ! Remark: automatically generated interfaces are used.
      interface
            !
            SUBROUTINE SIMUL(IW,EPS,NFILE0)
              INTEGER(KIND=4) :: IW
              REAL(KIND=8) :: EPS
              INTEGER(KIND=4) :: NFILE0
            END SUBROUTINE SIMUL
            !
            SUBROUTINE LEESOR(NUNIT,MPOINT)
              INTEGER(KIND=4) :: NUNIT
              INTEGER(KIND=4) :: MPOINT
            END SUBROUTINE LEESOR
            !
            SUBROUTINE GRFIL(IERR)
              INTEGER(KIND=4) :: IERR
            END SUBROUTINE GRFIL
            !           
      end interface

end module

