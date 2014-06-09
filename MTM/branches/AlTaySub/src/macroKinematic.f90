module altayMacroKinematic
use altayMiscutils, only: unitMatrix
implicit none 

!> Velocity Gradient
double precision, dimension(3,3), public, save :: MaKi_VelGrad = 0.0D0

!> Strain Rate, i.e. symmetric part of the velocity gradient
double precision, dimension(3,3), public, save :: MaKi_StrainRate = 0.0D0

!> Norm of strain rate
double precision,                 public, save :: MaKi_NormStrainRate = 0.0D0

!> Strain Mode normalized by the norm of strain rate
double precision, dimension(3,3), public, save :: MaKi_StrainMode = 0.0D0

!> von Mises equivalent strain rate
double precision,                 public, save :: MaKi_vMeqStrainRate = 0.0D0

!> Strain Mode normalized by von Mises equivalent strain rate
double precision, dimension(3,3), public, save :: MaKi_StrainModevM = 0.0D0

!> Spin, i.e. anti-symmetric part of the velocity gradient 
double precision, dimension(3,3), public, save :: MaKi_Spin = 0.0D0

!> Total Deformation Gradient (from undeformed state to the end of current increment)
double precision, dimension(3,3), public, save :: MaKi_TotalDefGrad = unitMatrix !For simulations with predeformation, it is re-initialized with call to dynfil2 subroutine. 

!> Delta Deformation Gradient (from start to end of current increment)
double precision, dimension(3,3), public, save :: MaKi_DeltaDefGrad = unitMatrix

!> Inverse of Delta Deformation Gradient
double precision, dimension(3,3), public, save :: MaKi_DeltaDefGrad_inverse = unitMatrix

!> Delta von Mises equivalent strain (from start to end of current increment)
double precision,                 public, save :: MaKi_DeltavMeqStrain = 0.0D0

contains   

      !Save the public variable Maki_VelGrad & derived quantities
      subroutine SetVelGrad_MacroKinematic(VelGrad)
      double precision, dimension(3,3), intent(in)    :: VelGrad
      !
      !Explicitly make the velocity gradient traceless
      MaKi_VelGrad = VelGrad - UnitMatrix * (VelGrad(1,1)+VelGrad(2,2)+VelGrad(3,3))/3.D0
      !
      Maki_StrainRate = (MaKi_VelGrad+transpose(MaKi_VelGrad))/2.D0
      MaKi_Spin       = (MaKi_VelGrad-transpose(MaKi_VelGrad))/2.D0
      !
      MaKi_NormStrainRate = norm2(Maki_StrainRate)
      MaKi_StrainMode     = Maki_StrainRate / MaKi_NormStrainRate
      MaKi_vMeqStrainRate = sqrt(2.0D0/3.0D0) * MaKi_NormStrainRate
      MaKi_StrainModevM   = Maki_StrainRate / MaKi_vMeqStrainRate
      !
      end subroutine

      subroutine SetNewInc_MacroKinematic()
      double precision, parameter :: deltaTime= 1.0D0
      !
      call Ftensor(MaKi_VelGrad,MaKi_DeltaDefGrad,MaKi_DeltaDefGrad_inverse) 
      call UPDATF(MaKi_TotalDefGrad,MaKi_DeltaDefGrad)
      !
      MaKi_DeltavMeqStrain  = MaKi_vMeqStrainRate * deltaTime
      !
      end subroutine

      subroutine Ftensor(Ldt,F1,F2)
      
      double precision, dimension(3,3), intent(in)  :: Ldt ! = the velocity gradient * time increment
      double precision, dimension(3,3), intent(out) :: F1
      double precision, dimension(3,3), intent(out) :: F2
!
!     F1 is the deformation gradient tensor
!     which corresponds to such velocity gradient
!     and such time increment
!
!     F2 is the inverse of F1
!
!
      double precision, dimension(3,3) :: X,Y,A,B
      integer, parameter :: n=10
      integer :: k

      X = unitMatrix + Ldt/n 
      Y = unitMatrix - Ldt/n 
      F1= X
      F2= Y
      do k=2,n
        A = matmul(X,F1)
        B = matmul(Y,F2)
        F1= A
        F2= B
      end do 
      
      end subroutine

      subroutine UPDATF(F,F1)
      implicit none 
      double precision, dimension(3,3), intent(inout)  :: F
      double precision, dimension(3,3), intent(in)     :: F1

      double precision, dimension(3,3) ::  X 
      double precision ::  y
      
      X=matmul(F1,F) !call MATPROD(X,DeltaDefGrad,TotalDefGrad,3,3,3) !!!!Shouldn't be TotalDefGrad*DeltaDefGrad instead ???
    
      !y is the determinant of X
      y= X(1,1)*(X(2,2)*X(3,3)-X(2,3)*X(3,2))  &
        -X(1,2)*(X(2,1)*X(3,3)-X(2,3)*X(3,1))  &
        +X(1,3)*(X(2,1)*X(3,2)-X(2,2)*X(3,1))

      F = X / y**(1.D0/3.D0) 

      end subroutine
          
end module