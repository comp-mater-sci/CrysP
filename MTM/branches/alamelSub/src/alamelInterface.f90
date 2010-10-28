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
!>    \file alamelInterface.f90 The file contains automatically generated inferfaces
!>          that simplify consistency check at compile time.      
!>                                  
!                                                
module alamelInterface

      ! Give explicit interfaces to subroutines beig called from this scope.
      ! Remark: automatically generated interfaces are used.
      interface
            !
            SUBROUTINE SIMUL(IW,JPAR,EPS,NFILE0,NUNIT,INSG0)
              INTEGER(KIND=4) :: IW
              INTEGER(KIND=4) :: JPAR
              REAL(KIND=8) :: EPS
              INTEGER(KIND=4) :: NFILE0
              INTEGER(KIND=4) :: NUNIT
              INTEGER(KIND=4) :: INSG0
            END SUBROUTINE SIMUL
            !
            SUBROUTINE LEESOR(NUNIT,MPOINT)
              INTEGER(KIND=4) :: NUNIT
              INTEGER(KIND=4) :: MPOINT
            END SUBROUTINE LEESOR
            !
            SUBROUTINE GRFIL()
            END SUBROUTINE GRFIL
            !           
      end interface

end module

