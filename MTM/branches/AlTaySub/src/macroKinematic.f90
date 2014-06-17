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

      subroutine SetNewInc_MacroKinematic(info)
      integer, intent(out) :: info
      !
      double precision, parameter:: deltaTime= 1.0D0
      double precision, dimension(3,3):: Ldt= 0.0D0
      !
      Ldt= MaKi_VelGrad*deltaTime
      call MatrixExponent(Ldt,MaKi_DeltaDefGrad,MaKi_DeltaDefGrad_inverse,info) 
      call UPDATF(MaKi_TotalDefGrad,MaKi_DeltaDefGrad)
      !
      MaKi_DeltavMeqStrain  = MaKi_vMeqStrainRate * deltaTime
      !
      end subroutine

      subroutine MatrixExponent(A,expA,InvExpA,info)
      double precision, dimension(3,3), intent(in)  :: A
      double precision, dimension(3,3), intent(out) :: ExpA    !The matrix exponent of A: ExpA = exp(A)
      double precision, dimension(3,3), intent(out) :: InvExpA !The inverse of ExpA:      InvExpA = (exp(A))^(-1)
      integer,                          intent(out) :: info
      !
      !For a given (3,3)-matrix A with ||A|| < 1,
      !this subroutine computes the matrix exponent of A (ExpA) based on the Taylor Series Expansion (see also [1]):
      !  exp(A) == I + A + A^2/(2!) + A^3/(3!) + ... + A^n/(n!) + ...
      !     with I the (3,3) unit matrix.
      !
      !The inverse of the matrix exponent (InvExpA) is calculated as Taylor Series Expansion of -A, since:
      !  exp(-A) * exp(A) = exp(-A+A) = exp(0) = I
      !
      ![1] Moler, C. and Van Loan, C., "Nineteen Dubious ways to compute the exponential of a matrix", Siam Review, vol 20, No 4, 1978.
      !
      double precision, dimension(3,3) :: Term= unitMatrix 
      double precision, parameter      :: NormTerm_cutoff= 1.0D-10 !Treshold to cut off Taylor Series Expansion
      integer                          :: k= 0 !The current term in Taylor Series Expansion
      integer, parameter               :: k_max= 10 !Upper limit of terms in Taylor Series Expansion to be calculated
      !
      !Implemented algorithm is reliable on the condition that ||A|| < 1; if not, catastrophic cancellation in floating point arithmetic 
      ! can lead to totally erroneous results [1].
      if (norm2(A)>1.0D0) then
          info= -1
          return
      else
          info= 0
      end if
      !
      !For the '0-th term in Taylor Series Expansion', the approximation of Taylor Series Expansion is
      k= 0
      Term= unitMatrix
      ExpA= Term
      InvExpA= Term
      !
      !Add terms to Taylor Series Expansion until ||Term|| becomes negligeable or upper limit in number of terms reached
      do while ( (norm2(Term)>NormTerm_cutoff) .AND. (k<k_max) )
          k= k+1
          Term= matmul(Term,A) / k
          ExpA= ExpA + Term
          InvExpA= InvExpA + Term * (-1.D0)**k
      end do
      !
      end subroutine

      subroutine UPDATF(F,F1)
      implicit none 
      double precision, dimension(3,3), intent(inout)  :: F
      double precision, dimension(3,3), intent(in)     :: F1

      double precision, dimension(3,3) ::  X 
      double precision ::  y
      
      X=matmul(F,F1)
    
      !y is the determinant of X
      y= X(1,1)*(X(2,2)*X(3,3)-X(2,3)*X(3,2))  &
        -X(1,2)*(X(2,1)*X(3,3)-X(2,3)*X(3,1))  &
        +X(1,3)*(X(2,1)*X(3,2)-X(2,2)*X(3,1))

      F = X !!!PE!/ y**(1.D0/3.D0) 

      end subroutine
          
end module