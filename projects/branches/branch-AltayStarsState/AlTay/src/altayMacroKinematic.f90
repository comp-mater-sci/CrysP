module altayMacroKinematic
use criMathUtils, only: unit_sr_Matrix
implicit none 

type DeformationRate
    !> Velocity Gradient
    double precision, dimension(3,3) :: VelGrad = 0.0D0 
    !> Strain Rate, i.e. symmetric part of the velocity gradient
    double precision, dimension(3,3) :: StrainRate = 0.0D0
    !> Norm of strain rate
    double precision                 :: NormStrainRate = 0.0D0
    !> Strain Mode normalized by the norm of strain rate
    double precision, dimension(3,3) :: StrainMode = 0.0D0
    !> von Mises equivalent strain rate
    double precision                 :: vMeqStrainRate = 0.0D0
    !> Strain Mode normalized by von Mises equivalent strain rate
    double precision, dimension(3,3) :: StrainModevM = 0.0D0
    !> Spin, i.e. anti-symmetric part of the velocity gradient 
    double precision, dimension(3,3) :: Spin = 0.0D0
end type DeformationRate

type DeformationState
    !> Total Deformation Gradient (from undeformed state to the end of current increment)
    !> For simulations with predeformation, it is re-initialized with call to dynfil2 subroutine. 
    double precision, dimension(3,3) :: TotalDefGrad = unit_sr_matrix 
    !> Incremental Deformation Gradient (from start to end of current increment)
    double precision, dimension(3,3) :: IncrDefGrad = unit_sr_matrix
    !> Inverse of Incremental Deformation Gradient
    double precision, dimension(3,3) :: IncrDefGrad_inverse = unit_sr_matrix
    !> Incremental von Mises equivalent strain (from start to end of current increment)
    double precision                 :: IncrvMeqStrain = 0.0D0
    !> Accumulated von Mises equivalent strain, up to the start of current inc. 
    !> (note: reference state might be different than that of TotalDefGrad)
    double precision                 :: AccumvMeqStrain_ToStartOfInc = 0.0D0
    !> Accumulated von Mises equivalent strain, up to the end of current inc. 
    !> (note: reference state might be different than that of TotalDefGrad)
    double precision                 :: AccumvMeqStrain_ToEndOfInc = 0.0D0    
end type DeformationState

contains   

      !Construct the instance 'this' of type DeformationRate
      subroutine Set_DeformationRate(VelGrad,this)
      double precision, dimension(3,3), intent(in)    :: VelGrad
      type(DeformationRate)           , intent(out)   :: this
      !
      !Explicitly make the velocity gradient traceless
      this%VelGrad = VelGrad - unit_sr_matrix * (VelGrad(1,1)+VelGrad(2,2)+VelGrad(3,3))/3.D0
      !
      this%StrainRate = (this%VelGrad+transpose(this%VelGrad))/2.D0
      this%Spin       = (this%VelGrad-transpose(this%VelGrad))/2.D0
      !
      this%NormStrainRate = norm2(this%StrainRate)
      this%StrainMode     = this%StrainRate / this%NormStrainRate
      this%vMeqStrainRate = sqrt(2.0D0/3.0D0) * this%NormStrainRate
      this%StrainModevM   = this%StrainRate / this%vMeqStrainRate
      !
      end subroutine

      subroutine Update_DeformationState(thisRate,thisState,info,deltaTime_in)
      type(DeformationRate), intent(in)    :: thisRate
      type(DeformationState),intent(inout) :: thisState      
      integer,                    intent(out) :: info
      double precision, optional, intent (in) :: deltaTime_in
      !
      double precision                :: deltaTime= 1.0D0
      double precision, dimension(3,3):: Ldt= 0.0D0
      !
      if(present(deltaTime_in)) then
          deltaTime= deltaTime_in
      else
          deltaTime= 1.0D0
      end if
      !
      Ldt= thisRate%VelGrad * deltaTime
      call MatrixExponentSmallNorm(Ldt,thisState%IncrDefGrad,thisState%IncrDefGrad_inverse,info) 
      !
      !Update thisState%TotalDefGrad
      thisState%TotalDefGrad = matmul(thisState%IncrDefGrad,thisState%TotalDefGrad)
      !
      thisState%IncrvMeqStrain = thisRate%vMeqStrainRate * deltaTime
      thisState%AccumvMeqStrain_ToStartOfInc = thisState%AccumvMeqStrain_ToEndOfInc
      thisState%AccumvMeqStrain_ToEndOfInc   = thisState%AccumvMeqStrain_ToEndOfInc + thisState%IncrvMeqStrain
      !
      end subroutine

      subroutine MatrixExponentSmallNorm(A,expA,InvExpA,info)
      double precision, dimension(3,3), intent(in)  :: A
      double precision, dimension(3,3), intent(out) :: ExpA    !The matrix exponent of A: ExpA = exp(A)
      double precision, dimension(3,3), intent(out) :: InvExpA !The inverse of ExpA:      InvExpA = (exp(A))^(-1)
      integer,                          intent(out) :: info
      !
      !For a given (3,3)-matrix A with small norm, i.e. ||A|| < 1,
      !this subroutine computes the matrix exponent of A (ExpA) based on the Taylor Series Expansion (see also [1]):
      !  exp(A) == I + A + A^2/(2!) + A^3/(3!) + ... + A^n/(n!) + ...
      !     with I the (3,3) unit matrix.
      !
      !The inverse of the matrix exponent (InvExpA) is calculated as Taylor Series Expansion of -A, since:
      !  exp(-A) * exp(A) = exp(-A+A) = exp(0) = I
      !
      ![1] Moler, C. and Van Loan, C., "Nineteen Dubious ways to compute the exponential of a matrix", Siam Review, vol 20, No 4, 1978.
      !
      double precision, dimension(3,3) :: Term= unit_sr_matrix 
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
      Term= unit_sr_matrix
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

          
      !> function that returns “the ratio of the parallel strain rates”
      pure double precision function ratlon(MacroDefRate,relaxationrate_sam)
      implicit none
      type(DeformationRate), intent(in)               :: MacroDefRate
      double precision,dimension(3,3),intent(in)      :: relaxationrate_sam
      !
      ! MacroDefRate%StrainMode & relaxationrate_sam: expressed in same (sample) reference frame
      ratlon= sum( (MacroDefRate%StrainMode + relaxationrate_sam/MacroDefRate%NormStrainRate) *  &
                    MacroDefRate%StrainMode                            )
      !
      end function
      
end module
