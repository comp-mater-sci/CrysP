    !
    ! NOTE: the subroutines below are left only for backward compatibility with 
    !       altayDynfil module in SIMUL. They will be removed in the near future
    !       and the module altayDynfil will be removed as well.
    !
    
module altayDynfilStitch
use altayState, only: altayStateVariables
use criMathUtils
implicit none

contains

      !> Extract the global material data
      subroutine DYNFIL2(statevars, F)
      implicit none
      type(altayStateVariables),intent(in) :: statevars
      double precision,intent(out) :: F(3,3)
      !
      associate (meso => statevars%mesostructure)
            F=meso%deformationgradient
      end associate
      !
      end subroutine DYNFIL2

      !> Write the global material data
      subroutine DYNFIL3(statevars,F)
      implicit none
      type(altayStateVariables),intent(inout) :: statevars
      double precision,intent(in)   :: F(3,3)
      !
        associate (meso => statevars%mesostructure)
            meso%deformationgradient=F
        end associate
      !
      end subroutine DYNFIL3

      !> Get the record data for i-th grain
      subroutine DYNFIL4(statevars, i, orientation, GEW, GAM)
      implicit none
      type(altayStateVariables),intent(in) :: statevars
      integer,intent(in) :: i
      type(EulerAngles),intent(out):: orientation
      double precision,intent(out) :: GEW,GAM
      !
      associate (DFIL => statevars%grainstates%grainstate)
            orientation = DFIL(i)%orientation%euler
            GEW=DFIL(i)%orientation%weight
            GAM=DFIL(i)%accumulatedshear
      end associate      
      !
      end subroutine DYNFIL4


      !> Put the record data for i-th grain
      subroutine DYNFIL5(statevars, i, orientation, GEW, GAM)
      implicit none
      type(altayStateVariables),intent(inout) :: statevars
      integer,intent(in) :: i
      type(EulerAngles),intent(in):: orientation
      double precision,intent(in) :: GEW,GAM
      !
      associate (DFIL => statevars%grainstates%grainstate)
            DFIL(i)%orientation%euler = orientation
            DFIL(i)%orientation%weight=GEW
            DFIL(i)%accumulatedshear=GAM
      end associate 
      !
      end subroutine DYNFIL5

end module

