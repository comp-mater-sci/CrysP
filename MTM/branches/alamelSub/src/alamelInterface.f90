!
! $Id$
!
module alamelInterface

      ! Give explicit interfaces to subroutines beig called from this scope.
      ! Remark: automatically generated interfaces are used
      interface
            !
            SUBROUTINE SIMUL(IW,JPAR,EPS,NFILE0,NUNIT,INSG0,ACNF)
              USE ALAMELCONFIG
              INTEGER(KIND=4) :: IW
              INTEGER(KIND=4) :: JPAR
              REAL(KIND=8) :: EPS
              INTEGER(KIND=4) :: NFILE0
              INTEGER(KIND=4) :: NUNIT
              INTEGER(KIND=4) :: INSG0
              TYPE (ALAMELCONFIGDATA) :: ACNF
            END SUBROUTINE SIMUL
            !
            SUBROUTINE LEESOR(NUNIT,MPOINT,ACNF)
              USE ALAMELCONFIG
              INTEGER(KIND=4) :: NUNIT
              INTEGER(KIND=4) :: MPOINT
              TYPE (ALAMELCONFIGDATA) :: ACNF
            END SUBROUTINE LEESOR
	      !
            SUBROUTINE GRFIL(ACNF)
              USE ALAMELCONFIG
              TYPE (ALAMELCONFIGDATA) :: ACNF
            END SUBROUTINE GRFIL
	      !	      
      end interface

end module