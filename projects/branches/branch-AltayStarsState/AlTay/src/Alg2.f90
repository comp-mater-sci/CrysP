#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
      module altayAlgorithms
      use altayMiscutils, only: terminate, stopcode_runtimeerror
      use altayMacroKinematic
      use criErrcodes
      use criMathUtils
      contains
     

      !> The function converts a 5D-vector in deviatoric (stress/strain-rate) space
      !> to a (3,3)-matrix representation of a symmetric and traceless 2nd rank tensor.
      !>
      !> There is a reverse conversion available. \sa SymMat33ToVec5
      pure function Vec5ToSymMat33(vec) result(mat)
      implicit none
      double precision, dimension(5),  intent(in) :: vec
      double precision, dimension(3,3)            :: mat
      double precision, parameter ::           &
          sq22=   sqrt(0.5d0),                 & !0.7071068
          const3= (sqrt(3.0d0)+3.0d0)/6.0d0,   & !0.7886751
          const4= (3.0d0-sqrt(3.0d0))/6.0d0      !0.2113249          
      !
      mat(2,2)=  const3*vec(1)-const4*vec(2)
      mat(3,3)= -const4*vec(1)+const3*vec(2)
      !
      mat(1,1)= -mat(2,2)-mat(3,3)
      !
      mat(2,3)= sq22*vec(3)
      mat(3,1)= sq22*vec(4)
      mat(1,2)= sq22*vec(5)
      !
      mat(3,2)= mat(2,3)   
      mat(1,3)= mat(3,1)       
      mat(2,1)= mat(1,2)       
      !      
      end function Vec5ToSymMat33

    
      !> The function converts a (3,3)-matrix representation of a traceless 2nd rank tensor
      !> to 5D-vector representation in deviatoric (stress/strain-rate) space.
      !> Only the symmetric part of 2nd rank tensor is transformed.
      !>
      !> There is a reverse conversion available. \sa Vec5ToSymMat33
      pure function SymMat33ToVec5(mat) result(vec)
      implicit none
      double precision, dimension(3,3), intent(in) :: mat
      double precision, dimension(5)               :: vec
      double precision, parameter ::           &
          c1= 0.5d0*(sqrt(3.0d0)+1.0d0),       &
          c2= c1-1.0d0,                        &
          c3= sqrt(0.5d0)
      !
      vec(1)= c1*mat(2,2) + c2*mat(3,3)
      vec(2)= c2*mat(2,2) + c1*mat(3,3)
      !
      vec(3)= c3* (mat(2,3)+mat(3,2))
      vec(4)= c3* (mat(3,1)+mat(1,3))
      vec(5)= c3* (mat(1,2)+mat(2,1))
      !      
      end function SymMat33ToVec5
      
      
         
      !> The function converts the vector with dimension 3 into the 
      !>  anti-symmetric rank-two tensor. 
      !>
      !> There is a reverse conversion available. \sa AntiSymMat33ToVec3
      pure function Vec3ToAntiSymMat33(vec) result(mat)
      implicit none
      double precision,dimension(3),intent(in)  :: vec
      double precision,dimension(3,3)           :: mat
      !
      double precision, parameter :: minus = -1.0d0
      !
      mat = 0.0d0
      mat(3,2) = vec(1)
      mat(1,3) = vec(2)
      mat(2,1) = vec(3)
      mat(2,3) = minus * mat(3,2)
      mat(3,1) = minus * mat(1,3)
      mat(1,2) = minus * mat(2,1)
      !
      end function

      
      !> The function converts the anti-symmetric part of rank-two tensor into
      !> vector representation with dimension 3. 
      !>
      !> There is a reverse conversion available. \sa Vec3ToAntiSymMat33
      pure function AntiSymMat33ToVec3(mat) result(vec)
      implicit none
      double precision,dimension(3,3),intent(in)      :: mat
      double precision,dimension(3)                   :: vec
      !
      double precision, parameter :: half = 0.5d0
      !
      vec(1) = half * (mat(3,2)-mat(2,3))
      vec(2) = half * (mat(1,3)-mat(3,1))
      vec(3) = half * (mat(2,1)-mat(1,2))
      !note: same convention in PRETAY; e.g.: "B1(1,IS)=(R(3)*V(2)-R(2)*V(3))/2."
      !
      end function

      
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
      
      
      
    !Calculates the inverse of square matrix A as Ainv
    ! If Ainv doesn't exist, info /= criSuccess is returned.
    ! The allowed dimension of A is n=1,2,..,n_max 
    subroutine invertmatrix(A,n,Ainv,info)
    implicit none
    integer,intent(in)                           :: n
    double precision, dimension(n,n),intent(in)  :: A
    double precision, dimension(n,n),intent(out) :: Ainv
    integer,intent(out)                          :: info
    !
    integer :: j
    double precision :: D
    !> Near-0 threshold for determinant of matrix.
    !> The value is taken from pretay program.
    double precision, parameter :: D_threshold = 1.D-6
    integer, parameter :: n_max = 5
        !
        if (n > n_max) then
            info = criError
            Ainv = 0.D0
            return
        end if
        !
        !Calculate the determinant of A
        D = determinant(A,n)
        !
        if (abs(D) < D_threshold) then
            info = criError
            Ainv = 0.D0
        else
            info = criSuccess
            Ainv = adjoint(A,n) / D
        end if
        !
    end subroutine    
    
    !Calculates the determinant for square matrix A.
    ! The allowed dimension of A is n=1,2,..,n_max
    double precision pure function determinant(A,n) result(D)
    implicit none
    integer,intent(in)                          :: n
    double precision, dimension(n,n),intent(in) :: A
    !
    integer :: j
    integer, parameter :: n_max = 5
        !
        select case (n)
        case (1)
            D = A(1,1)
        case (2)
            D = A(1,1)*A(2,2)-A(1,2)*A(2,1)
        case (3)
            D = A(1,1) * (A(2,2)*A(3,3)-A(3,2)*A(2,3)) &
               -A(1,2) * (A(2,1)*A(3,3)-A(3,1)*A(2,3)) &
               +A(1,3) * (A(2,1)*A(3,2)-A(3,1)*A(2,2))
        case (4:n_max)
            D = 0.D0
            ! determinant development along first row
            do j=1,n
                D = D + A(1,j)*cofactor(A,n,1,j)
            enddo
        case default
            D = 0.D0 !shoudl be NaN or so
        end select
        !
    end function
    
    double precision pure function cofactor(A,n,i,j)
    implicit none
    integer,intent(in)                          :: n
    double precision, dimension(n,n),intent(in) :: A
    integer,intent(in)                          :: i,j
    !
    integer,dimension(n-1) :: ii,jj
    double precision, dimension(n-1,n-1) :: Ared 
        !
        !reduce A: remove row i and column j
        ii = [1:i-1,i+1:n]
        jj = [1:j-1,j+1:n]
        Ared = A(ii,jj)
        !
        !calculate cofactor
        cofactor = (-1.D0)**float(i+j) * determinant(Ared,n-1)
        !
    end function
    
    double precision function adjoint(A,n)
    implicit none
    integer,intent(in)                          :: n
    double precision, dimension(n,n),intent(in) :: A
    dimension                                   :: adjoint(n,n)
    !
    integer :: i,j
        !
        forall (i=1:n,j=1:n) adjoint(j,i) = cofactor(A,n,i,j)
        !
        !do (i=1,n)
        !    do (j=1,n)
        !        adjoint(j,i) = cofactor(A,n,i,j)
        !    end do
        !end do
        !
    end function
      
      end module
      
