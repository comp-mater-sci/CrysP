!> Elementary math constants and functions
!>
!> The module provides a set of typical mathematical constants
!> that are frequently used in various subroutunes in the library.
!> It also provides some simple functions, e.g. conversions.
module criMathUtils
    use utils
    implicit none

    real(DP), parameter         :: root2 = sqrt(2.D0) !< Square root of 2 \f$ \sqrt{2} \f$
    real(DP), parameter         :: root2i = 0.5D0*sqrt(2.D0) !< Inverse of square root of 2 \f$ \frac{1}{\sqrt{2}} \f$
    real(DP), parameter         :: root23 = sqrt(2.D0/3.D0) !< Square root of 2/3 \f$ \sqrt{2/3} \f$
    real(DP), parameter         :: root32 = sqrt(3.D0/2.D0) !< Square root of 3/2 \f$ \sqrt{3/2} \f$

contains

        !> The function converts Voigt-style vector vec into symmetrical rank-two tensor.
    pure function Vec6ToMat33(vec) result(mat)
        real(DP), dimension(6), intent(in)  :: vec
        real(DP), dimension(3, 3)            :: mat

        mat(1, 1) = vec(1)
        mat(2, 2) = vec(2)
        mat(3, 3) = vec(3)
        mat(1, 2) = vec(4)
        mat(2, 3) = vec(5)
        mat(3, 1) = vec(6)
        mat(1, 3) = mat(3, 1)
        mat(2, 1) = mat(1, 2)
        mat(3, 2) = mat(2, 3)
    end function

      !> The function converts the symmetrical rank-two tensors mat into Voigt-style vector representation.
    pure function Mat33ToVec6(mat) result(vec)
        real(DP), dimension(3, 3), intent(in)    :: mat
        real(DP), dimension(6)                  :: vec
      
        vec(1) = mat(1, 1)
        vec(2) = mat(2, 2)
        vec(3) = mat(3, 3)
        vec(4) = mat(1, 2)
        vec(5) = mat(2, 3)
        vec(6) = mat(1, 3)
    end function

      !> The function converts Voigt-style vector vec into rank-two tensor.
    pure function Vec9ToMat33(vec) result(mat)
        real(DP), dimension(9), intent(in)  :: vec
        real(DP), dimension(3, 3)   :: mat
      
        mat(1, 1) = vec(1)
        mat(2, 2) = vec(2)
        mat(3, 3) = vec(3)
        mat(1, 2) = vec(4)
        mat(2, 3) = vec(5)
        mat(3, 1) = vec(6)
        mat(2, 1) = vec(7)
        mat(3, 2) = vec(8)
        mat(1, 3) = vec(9)
    end function

    !> The function converts the rank-two tensor mat into Voigt-style vector representation.
    pure function Mat33ToVec9(mat) result(vec)
        real(DP), dimension(3, 3), intent(in)    :: mat
        real(DP), dimension(9)                  :: vec
      
        vec(1) = mat(1, 1)
        vec(2) = mat(2, 2)
        vec(3) = mat(3, 3)
        vec(4) = mat(1, 2)
        vec(5) = mat(2, 3)
        vec(6) = mat(1, 3)
        vec(7) = mat(2, 1)
        vec(8) = mat(3, 2)
        vec(9) = mat(1, 3)
    end function

    !> Calculates trace of the square n x n matrix X
    pure real(DP) function trace(X) result(res)
        real(DP), dimension(:,:), intent(in)    :: X
        integer:: i
         
        res = 0._DP
        do i = 1, minval(shape(X))
            res = res+X(i, i)
        enddo
    end function



end module
