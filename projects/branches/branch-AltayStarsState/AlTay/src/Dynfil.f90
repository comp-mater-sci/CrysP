    !
    ! NOTE: the subroutines below are left only for backward compatibility with 
    !       altayDynfil module in SIMUL. They will be removed in the near future
    !       and the module altayDynfil will be removed as well.
    !
    
module altayDynfilStitch
use altayState, only: altayStateVariables
implicit none

contains

      !> Extract the global material data
      subroutine DYNFIL2(statevars, n,F)
      implicit none
      type(altayStateVariables),intent(in) :: statevars
      integer,intent(out)     :: n
      double precision,intent(out) :: F(3,3)
       !
       n=statevars%texture%nrstep
       associate (mf => statevars%frame)
            F=mf%FALG
      end associate
      !
      end subroutine DYNFIL2

      !> Write the global material data
      subroutine DYNFIL3(statevars,n,F)
      implicit none
      type(altayStateVariables),intent(inout) :: statevars
      integer,intent(in)            :: n
      double precision,intent(in)   :: F(3,3)
      !
        statevars%texture%nrstep = n
        associate (mf => statevars%frame)
            mf%FALG=F
        end associate
      !
      end subroutine DYNFIL3

      !> Get the record data for i-th grain
      subroutine DYNFIL4(statevars,i,FI1,PHI,FI2,T,                                &
                        GEW,GAM,F,ZERO)
      implicit none
      type(altayStateVariables),intent(in) :: statevars
      integer,intent(in) :: i
      double precision,intent(out) :: FI1,PHI,FI2,GEW,GAM
      double precision,intent(out) :: F(3,3),T(3,3),ZERO(3,3)
      !
      associate (DFIL => statevars%texture%grains)
            FI1=DFIL(i)%tFI1
            PHI=DFIL(i)%tPHI
            FI2=DFIL(i)%tFI2
            GEW=DFIL(i)%tGEW
            GAM=DFIL(i)%tGAM
            T=DFIL(i)%tT
            F=DFIL(i)%tF
            ZERO=DFIL(i)%tZERO
      end associate
      !
      end subroutine DYNFIL4


      !> Put the record data for i-th grain
      subroutine DYNFIL5(statevars,i,FI1,PHI,FI2,T,                                &
                        GEW,GAM,F,ZERO)
      implicit none
      type(altayStateVariables),intent(inout) :: statevars
      integer,intent(in) :: i
      double precision,intent(in) :: FI1,PHI,FI2,GEW,GAM
      double precision,intent(in) :: F(3,3),T(3,3),ZERO(3,3)
      !
      associate (DFIL => statevars%texture%grains)
            DFIL(i)%tFI1=FI1
            DFIL(i)%tPHI=PHI
            DFIL(i)%tFI2=FI2
            DFIL(i)%tGEW=GEW
            DFIL(i)%tGAM=GAM
            DFIL(i)%tT=T
            DFIL(i)%tF=F
            DFIL(i)%tZERO=ZERO
      end associate
      !
      end subroutine DYNFIL5

end module

