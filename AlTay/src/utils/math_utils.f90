!> Useful math utilities

module math_utils
    use base_defs

    implicit none

    public

    real(DP), parameter:: REAL_DP_MAX_VAL = huge(0._DP)     !! Placeholder for 'infinity'. Useful as initial value in loops looking for the minimum of some value in a list.
    real(DP), parameter:: PI         = acos(-1.D0)          !! Pi.

        !> Matrix form of the unit second rank tensor
    real(DP), dimension(3, 3), parameter:: UNIT_MATRIX_3X3 = reshape([1._DP, 0._DP, 0._DP, &
                                                                      0._DP, 1._DP, 0._DP, &
                                                                      0._DP, 0._DP, 1._DP], [3, 3])

    character(*), private, parameter:: MOD_NAME = 'utils' !! Module name for easier logging.


    interface normalize !! Normalize a vector or an array of column vectors.
        module procedure normalize_int, normalize_real, normalize_vec_int, normalize_vec_real
    end interface

    interface operator(.dot.) !! Dot product for vectors, or double dot product for matrices.
        module procedure dot_product_wrapper, double_dot_product
    end interface

    interface operator(.cross.) !! Cross product between two vectors.
        module procedure cross_int, cross_real
    end interface

    interface operator(.outer.) !! Outer product (i.e. tensor product) between 2 vectors.
        module procedure outer_product
    end interface

    interface operator(.toframe.) !! Rotate a matrix to a particular reference frame represented in the macroscopic frame (passive convention).
        module procedure rotate_to
    end interface

    interface operator(.fromframe.) !! Rotate a matrix from a particular reference frame to the macroscopic frame (passive convention).
        module procedure rotate_from
    end interface

contains

    !> Wrapper for the Fortran intrinsic dot_product.
    !>
    !> Needed to define the .dot. operator.
    pure real(DP) function dot_product_wrapper(vec1, vec2)
        real(DP), intent(in):: vec1(:), &
                               vec2(size(vec1))

        dot_product_wrapper = dot_product(vec1, vec2)
    end function

    !> The double dot product between 2 matrices
    pure real(DP) function double_dot_product(mat1, mat2)
        real(DP), intent(in):: mat1(:,:), &
                               mat2(size(mat1, 1), size(mat1, 2))

        double_dot_product = sum(mat1*mat2)
    end function

    !> Cross product between 2 integer vectors
    pure function cross_int(v1, v2) result(cross)
        integer, intent(in), dimension(3):: v1, v2
        integer, dimension(3):: cross

        cross(1)=v1(2)*v2(3)-v1(3)*v2(2)
        cross(2)=v1(3)*v2(1)-v1(1)*v2(3)
        cross(3)=v1(1)*v2(2)-v1(2)*v2(1)
    end function

    !> Cross product between 2 real vectors
    pure function cross_real(v1, v2) result(cross)
        real(DP), intent(in), dimension(3):: v1, v2
        real(DP), dimension(3):: cross

        cross(1)=v1(2)*v2(3)-v1(3)*v2(2)
        cross(2)=v1(3)*v2(1)-v1(1)*v2(3)
        cross(3)=v1(1)*v2(2)-v1(2)*v2(1)
    end function

    !> Normalize an array of integer column vectors.
    !>
    !> Each column vector is normalized independently
    pure function normalize_int(arr) result(normalized)
        integer, dimension(:,:), intent(in):: arr
        real(DP), dimension(3, size(arr, 2)):: normalized
        integer:: i

        do i = 1, size(arr, 2)
            normalized(:,i) = real(arr(:,i), DP) / norm2(real(arr(:,i), DP))
        end do
    end function
    !> Normalize an integer vector.
    pure function normalize_vec_int(vec) result(normalized)
        integer, dimension(:), intent(in):: vec
        real(DP), dimension(size(vec)):: normalized
        integer:: i

        normalized = real(vec, DP) / norm2(real(vec, DP))
    end function
    !> Normalize an integer vector.
    pure function normalize_vec_real(vec) result(normalized)
        real(DP), dimension(:), intent(in):: vec
        real(DP), dimension(size(vec)):: normalized
        integer:: i

        normalized = real(vec, DP) / norm2(real(vec, DP))
    end function


    !> Normalize an array of real column vectors.
    !>
    !> Each column vector is normalized independently
    pure function normalize_real(arr) result(normalized)
        real(DP), dimension(:,:), intent(in):: arr
        real(DP), dimension(3, size(arr, 2)):: normalized
        integer:: i

        do i = 1, size(arr, 2)
            normalized(:,i) = arr(:,i) / norm2(arr(:,i))
        end do
    end function

    !> The outer (i.e. tensor) product between 2 vectors.
    pure function outer_product(v1, v2) result(prod)
        real(DP), dimension(:), intent(in)      :: v1, v2
        real(DP), dimension(size(v1), size(v2)):: prod
        integer                                 :: i, j

        forall(i = 1:size(v1), j = 1:size(v2)) prod(i, j) = v1(i) * v2(j)
    end function

    !> Invert a matrix
    !>
    !> Calculated using the LU decomposition.
    !> Depends on LAPACK.
    function invert(A) result(Ainv)
        real(DP), dimension(:,:), intent(in):: A
        real(DP), dimension(size(A, 1), size(A, 2)):: Ainv

        real(DP), dimension(size(A, 1)):: work  ! work array for LAPACK
        integer, dimension(size(A, 1)):: ipiv   ! pivot indices
        integer:: n, info

        ! External procedures defined in LAPACK
        external DGETRF
        external DGETRI

        ! Store A in Ainv to prevent it from being overwritten by LAPACK
        Ainv = A
        n = size(A, 1)

        ! DGETRF computes an LU factorization of a general M-by-N matrix A
        ! using partial pivoting with row interchanges.
        call DGETRF(n, n, Ainv, n, ipiv, info)

        if (info /= 0) error stop 'Matrix is numerically singular!'

        ! DGETRI computes the inverse of a matrix using the LU factorization
        ! computed by DGETRF.
        call DGETRI(n, Ainv, n, ipiv, work, n, info)

        if (info /= 0) error stop 'Matrix inversion failed!'
    end function


    !> Compute matrix exponential for a (3, 3)-matrix
    !>
    !> Uses the Taylor Series Expansion:
    !> exp(A) == I+A + A^2/(2!) + A^3/(3!) + ... + A^n/(n!) + ...
    !> For Matrices with large norm, quadratic scaling is applied to ensure stability of the Taylor series expansion.
    !> Reference: DOI: 10.1137/S00361445024180 (method 3)
    pure function matrix_exponential(A) result(exponential)
        real(DP), dimension(3, 3), intent(in):: A                   !! Input matrix
        real(DP), dimension(3, 3)            :: exponential, &
                                                term, &                   !Term in taylor series expansion
                                                A_scaled(3,3)
        integer                              :: k, &
                                                i, &
                                                n_steps



        !If norm is 0, taking logarithm crashes the program so handle with care
        if (norm2(A) < TOLERANCE) then
            exponential = UNIT_MATRIX_3X3
        else
            !Scale A if its norm is larger than 1
            n_steps = max(0,ceiling(log(norm2(A)) / log(2._DP)))
            a_scaled = A / (2**n_steps)

            exponential = UNIT_MATRIX_3X3
            term        = UNIT_MATRIX_3X3
            k           = 0

            !Calculate the exponential of the scaled A
            do while (norm2(term) > TOLERANCE)
                k = k+1
                term = matmul(term, a_scaled) / real(k, DP)
                exponential = exponential+term
            end do

            !Square the exponential of a_scaled until it corresponds with the exponential of A
            do i=1,n_steps
                exponential = matmul(exponential,exponential)
            end do
        end if
    end function

    !> Rotates the second-rank tensor S to the reference frame given by rotation R.
    pure function rotate_to(S, R) result(Srot)
        real(DP), dimension(3, 3), intent(in):: S    !! Input matrix
        real(DP), dimension(3, 3), intent(in):: R    !! Passive rotation matrix. I.e. the basis of the unrotated frame expressed in the rotated frame.
        real(DP), dimension(3, 3)            :: Srot !! Rotated matrix in Bunge convention.

        Srot = matmul(matmul(R, S), transpose(R))
    end function

    !> Rotates the second-rank tensor S from the reference frame given by rotation R.
    pure function rotate_from(S, R) result(Srot)
        real(DP), dimension(3, 3), intent(in):: S    !! The tensor expressed in the frame defined by R
        real(DP), dimension(3, 3), intent(in):: R    !! Passive rotation matrix. I.e. the basis of the unrotated frame expressed in the rotated frame.
        real(DP), dimension(3, 3)            :: Srot !! Input matrix in the global reference frame.

        Srot = matmul(matmul(transpose(R), S), R)
    end function

    !> Calculate the angle between two vectors.
    !>
    !> Angle is expressed in radians
    pure real(DP) function vec_angle(u, v)
        real(DP), dimension(:), intent(in):: u
        real(DP), dimension(size(u)), intent(in):: v

        vec_angle = acos((u .dot. v) / sqrt((u .dot. u) * (v .dot. v)))
    end function

    !>4th order Runge-Kutta approximation of the differential equation given by d(x)/dt = F(x)
    real(DP) function runge_kutta(x0, dt, fun, args) result(rk)
        real(DP), intent(in)::  x0
        real(DP), intent(in)::  dt
        interface
            function fun(x, args) result(res)
            import DP
                real(DP), intent(in):: x
                real(DP), dimension(:), intent(in):: args
                real(DP):: res
            end function
        end interface
        real(DP), dimension(:), intent(in):: args

        real(DP), dimension(4)::  k

        k(1) = dt*fun(x0, args)
        k(2) = dt*fun(x0+k(1) / 2.D0, args)
        k(3) = dt*fun(x0+k(2) / 2.D0, args)
        k(4) = dt*fun(x0+k(3), args)

        rk = x0 + (k(1) + 2.D0*k(2) + 2.D0*k(3) + k(4)) / 6.D0
    end function

    !> Determine the indices of a basis for a matrix.
    !>
    !> By basis is meant a set of linearly independent columns. The number of these independent columns is equal to the number of rows of the matrix.
    !> Uses LAPACK routine dgetrf. Assumes that the column rank is at least equal to the number of rows.
    function basis_indices(mat) result(ind_basis)
        real(DP), dimension(:,:), intent(in):: mat      !! The matrix. Assumed to be wide (cols > rows). Assumed to have column rank
                                                        !! >= number of rows
        integer, dimension(size(mat, 1)):: ind_basis    !! Indices of the columns of the matrix making up a basis for the column
                                                        !! space. I.e. a minimal set of independent columns.
        integer:: m, &
                  ipiv(size(mat, 1)), &
                  next_col, &
                  i, &
                  info
        real(DP):: mat_lu(size(mat, 1), size(mat, 1))


        m = size(mat, 1)
        ind_basis =  (/(i, i = 1, m)/)
        info = m
        next_col = m
        ipiv = m

        !Dgetrf performs LU factorization. If this fails (info > 0), at least the column at index info is dependent on the
        !preceding columns. Therefore, we keep replacing the column at info by the next column of the input matrix until dgetrf
        !returns successfully.
        do while (info > 0)
            ind_basis(info) = next_col
            next_col = next_col+1
            mat_lu = mat(:,ind_basis)

            call dgetrf(m, m, mat_lu, m, ipiv, info)

            !Because LAPACK uses much lower tolerances than we do, we must check the diagonal elements of U even when accorording to
            !dgetrf the matrix is not singular
            i = 0
            do while (info == 0 .and. i < m)
                i = i+1
                if (mat_lu(i, i) < TOLERANCE) &
                    info = i
            end do
        end do
    end function

    !> Calculate determinant of a symmetric 3x3 matrix.
    !>
    !> Imported from DAMASK
    !> https://damask2.mpie.de/bin/view/Home/WebHome.html
    real(DP) pure function math_detSym33(m)

      real(DP), dimension(3,3), intent(in) :: m

      math_detSym33 = -(m(1,1)*m(2,3)**2 + m(2,2)*m(1,3)**2 + m(3,3)*m(1,2)**2) &
                      + m(1,1)*m(2,2)*m(3,3) + 2.0_DP * m(1,2)*m(1,3)*m(2,3)
    end function  math_detSym33

    ! Determinant of 3x3 matrix
    pure real(DP) function det(A) result(d)
        real(DP), dimension(3,3), intent(in):: A
        d = A(1,1)*(A(2,2)*A(3,3) - A(2,3)*A(3,2)) &
               - A(1,2)*(A(2,1)*A(3,3) - A(2,3)*A(3,1)) &
               + A(1,3)*(A(2,1)*A(3,2) - A(2,2)*A(3,1))
    end function

    !> Limit a scalar value to a certain range (either one or two sided).
    !>
    !> Imported from DAMASK
    !> https://damask2.mpie.de/bin/view/Home/WebHome.html
    real(DP) pure elemental function math_clip(a, left, right)

      real(DP), intent(in) :: a
      real(DP), intent(in), optional :: left, right

      math_clip = a
      if (present(left))  math_clip = max(left,math_clip)
      if (present(right)) math_clip = min(right,math_clip)
      if (present(left) .and. present(right)) then
        if (left>right) error stop 'left > right'
      end if
    end function math_clip

    !> Calculate trace of a 3x3 matrix.
    !>
    !> Imported from DAMASK
    !> https://github.com/damask-multiphysics/DAMASK/blob/4d0e88b0774a15ddeb92c1341981462dad44302b/src/math.f90#L650
    real(DP) pure function math_trace33(m)
      real(DP), dimension(3,3), intent(in) :: m

      math_trace33 = m(1,1) + m(2,2) + m(3,3)
    end function math_trace33

    !> invariants of symmetrix 3x3 matrix
    !>
    !> Imported from DAMASK
    !> https://github.com/damask-multiphysics/DAMASK/blob/4d0e88b0774a15ddeb92c1341981462dad44302b/src/math.f90#L1202
    pure function math_invariantsSym33(m)
        real(DP), dimension(3,3), intent(in):: m
        real(DP), dimension(3):: math_invariantsSym33

        math_invariantsSym33(1) = math_trace33(m)
        math_invariantsSym33(2) = m(1,1)*m(2,2) + m(1,1)*m(3,3) + m(2,2)*m(3,3) - (m(1,2)**2 + m(1,3)**2 + m(2,3)**2)
        math_invariantsSym33(3) = math_detSym33(m)
    end function math_invariantsSym33

    !> Perform polar decomposition on a 3x3 matrix
    !>
    !> According to which output arguments are provided, the stretch and/or rotation is returned.
    !> Performs singular value decomposition via LAPACK under the hood.
    subroutine polar_decomposition(matrix, stretch, rotation)
        real(DP), dimension(3,3), intent(in):: matrix              !! Input matrix. Must be 3x3
        real(DP), dimension(3,3), intent(out), optional:: stretch  !! If provided by caller, contains right stretch tensor.
        real(DP), dimension(3,3), intent(out), optional:: rotation !! If provided by caller, contains rortation tensro.

        integer:: i, &                !! Iterator
                  info                !! Return code for LAPACK call.
        real(DP):: U(3,3), &          !! See LAPACK documentation
                   VT(3,3), &         !! See LAPACK documentation
                   values(3), &       !! Singular values of matrix
                   values_mat(3,3), & !! Matrix form of singular values for easy calculation of stretch tensor.
                   work(15), &        !! Workspace for LAPACK call. See LAPACK documentation for size
                   buffer(3,3)        !! Buffer for the input matrix as it is overwritten by LAPACK.


        ! Perform SVD: M = U * S * VT
        buffer = matrix
        call dgesvd('A', 'A', 3, 3, buffer, 3, values, U, 3, VT, 3, work, size(work), info)

        !Calculate right stretch tensor if requested
        if (present(stretch)) then
            values_mat = 0._DP
            do i=1,3
                values_mat(i,i) = values(i)
            end do
            stretch = matmul(transpose(VT), matmul(values_mat,VT))
        end if

        !Calculate rotation if requested
        if (present(rotation)) &
            rotation = matmul(U,VT)
    end subroutine

    !> Take the matrix logarithm of a 3x3 matrix
    !>
    !> Performs eigenvalue decomposition via LAPACK under the hood.
    function matrix_log(matrix) result(logarithm)
        real(DP), dimension(3,3), intent(in):: matrix  !! 3x3 input matrix.
        real(DP), dimension(3,3):: logarithm           !! Matrix logarithm of the input.

        integer:: i, &                !! Iterator
                  info                !! Return code for LAPACK call.
        real(DP):: values(3), &       !! Singular values of matrix
                   values_mat(3,3), & !! Matrix form of singular values for easy calculation of stretch tensor.
                   work(15), &        !! Workspace for LAPACK call. See LAPACK documentation for size
                   buffer(3,3)        !! Buffer for the input matrix as it is overwritten by LAPACK.

        buffer = matrix
        !Eigenvalue decomposition
        call dsyev('V', 'U', 3, buffer, 3, values, work, size(work), info)

        values_mat = 0._DP
        do i=1,3
            values_mat(i,i) = log(values(i))
        end do
        logarithm = matmul(buffer, matmul(values_mat, transpose(buffer))) !After call to dsyev right_stretch contains
    end function

end module
