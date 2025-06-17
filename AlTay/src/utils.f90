!> Global constant definitions and useful utility procedures used all over AlTay.

module utils
    use, intrinsic:: iso_fortran_env, only: output_unit

    implicit none

    public

    integer, parameter       :: MAX_PATHLEN = 2048          !! Maximum file path length.
    integer, parameter       :: display_unit = output_unit  !! Identifier for stdout. Used in write statements.
    integer, parameter::  DP = selected_real_kind(15, 307)  !! Kind for reals corresponding to the classic notion of a double precision floating point number of 8 bytes.
    real(DP), parameter:: TOLERANCE  = 1.E-9_DP             !! Default tolerance on floating point calculations to compensate for inherent inaccuracy of floating point arithmetic, especially for multithreaded computations.
    real(DP), parameter:: REAL_DP_MAX_VAL = huge(0._DP)     !! Placeholder for 'infinity'. Useful as initial value in loops looking for the minimum of some value in a list.
    real(DP), parameter:: PI         = acos(-1.D0)          !! Pi.
    real(DP), parameter:: RAD_TO_DEG = 180._DP/PI           !! Multiply by this constant to convert a value in radians to degrees. Divide for the reverse operation.

    !> Status codes. Used to communicate information on the completion of a procedure to the caller.
    enum, bind(C)
        enumerator:: VEF_OK      !! Sucessful execution
        enumerator:: VEF_FAIL    !! No errors occured, but the routine did not accomplish its main goal.
        enumerator:: VEF_ERROR   !! Errors occured during exection.
    end enum

    !> Matrix form of the unit second rank tensor
    real(DP), dimension(3, 3), parameter:: UNIT_MATRIX_3X3 = reshape([1._DP, 0._DP, 0._DP, &
                                                                      0._DP, 1._DP, 0._DP, &
                                                                      0._DP, 0._DP, 1._DP], [3, 3])

    character(*), private, parameter:: MOD_NAME = 'utils' !! Module name for easier logging.


    interface normalize !! Normalize a vector or an array of column vectors.
        module procedure normalize_int, normalize_real, normalize_vec_int
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


    interface
        module pure function euler_to_tensor(euler) result(tensor)
            real(DP), dimension(3), intent(in):: euler
            real(DP), dimension(3,3):: tensor
        end function

        module pure function spin_to_tensor(spin) result(tensor)
            real(DP), dimension(3), intent(in):: spin
            real(DP), dimension(3,3):: tensor
        end function

        module pure function deviatoric_to_von_mises(deviatoric) result(von_mises)
            real(DP), dimension(5), intent(in):: deviatoric
            real(DP):: von_mises
        end function
        module pure function deviatoric_to_unscaled_voigt(deviatoric) result(voigt)
            real(DP), dimension(5), intent(in):: deviatoric
            real(DP), dimension(6):: voigt
        end function
        module pure function deviatoric_to_tensor(deviatoric) result(tensor)
            real(DP), dimension(5), intent(in):: deviatoric
            real(DP), dimension(3,3):: tensor
        end function

        module pure function unscaled_voigt_to_deviatoric(voigt) result(deviatoric)
            real(DP), dimension(6), intent(in):: voigt
            real(DP), dimension(5):: deviatoric
        end function
        module pure function unscaled_voigt_to_tensor(voigt) result(tensor)
            real(DP), dimension(6), intent(in):: voigt
            real(DP), dimension(3,3):: tensor
        end function

        module pure function tensor_to_von_mises(tensor) result(von_mises)
            real(DP), dimension(3,3), intent(in):: tensor
            real(DP):: von_mises
        end function
        module function tensor_to_euler(tensor) result(euler)
            real(DP), dimension(3,3), intent(in):: tensor
            real(DP), dimension(3):: euler
        end function
        module pure function tensor_to_spin(tensor) result(spin)
            real(DP), dimension(3,3), intent(in):: tensor
            real(DP), dimension(3):: spin
        end function
        module pure function tensor_to_deviatoric(tensor) result(deviatoric)
            real(DP), dimension(3,3), intent(in):: tensor
            real(DP), dimension(5):: deviatoric
        end function
        module pure function tensor_to_unscaled_voigt(tensor) result(voigt)
            real(DP), dimension(3,3), intent(in):: tensor
            real(DP), dimension(6):: voigt
        end function
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


    !> Compute matrix exponential for a (3, 3)-matrix with small norm, i.e. ||A|| << 1
    !>
    !> If ||A|| > 1, catastrophic cancellation in floating point arithmetic can occur
    !> Uses the Taylor Series Expansion:
    !> exp(A) == I+A + A^2/(2!) + A^3/(3!) + ... + A^n/(n!) + ...
    pure function matrix_exponential_small_norm(A) result(exponential)
        real(DP), dimension(3, 3), intent(in):: A                   !! Input matrix with norm << 1.
        real(DP), dimension(3, 3)            :: exponential, &
                                                term                   !Term in taylor series expansion
        integer                              :: k                      !Index of current term

        exponential = UNIT_MATRIX_3X3
        term        = UNIT_MATRIX_3X3
        k           = 0

        if (norm2(A) > 1._DP+TOLERANCE) error stop 'Norm of matrix should not be greater than 1!'

        do while (norm2(term) > TOLERANCE)
            k = k+1
            term = matmul(term, A) / real(k, DP)
            exponential = exponential+term
        end do
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

    ! Determinant of 3x3 matrix
    pure real(DP) function det(A) result(d)
        real(DP), dimension(3,3), intent(in):: A
        d = A(1,1)*(A(2,2)*A(3,3) - A(2,3)*A(3,2)) &
               - A(1,2)*(A(2,1)*A(3,3) - A(2,3)*A(3,1)) &
               + A(1,3)*(A(2,1)*A(3,2) - A(2,2)*A(3,1))
    end function


    !> Take the symmetric part of a matrix.
    !>
    !> Removes any rotational components
    pure function symmetric_part(mat) result(sym)
        real(DP), dimension(:,:), intent(in):: mat
        real(DP), dimension(size(mat, 1), size(mat, 2)):: sym

        sym = (mat+transpose(mat)) / 2._DP
    end function

    !> Take the antisymmetric (i.e. rotational) part of a matrix.
    pure function antisymmetric_part(mat) result(antisym)
        real(DP), dimension(:,:), intent(in):: mat
        real(DP), dimension(size(mat, 1), size(mat, 2)):: antisym

        antisym = (mat-transpose(mat)) / 2._DP
    end function

        !> Calculate the trace of a matrix.
    pure real(DP) function trace(x) result(res)
        real(DP), dimension(:,:), intent(in):: x !< The matrix. Assumed to be square.

        integer:: i

        res = 0._DP
        do i = 1, size(x, 1)
            res = res+x(i, i)
        enddo
    end function


end module

submodule(Utils) Conversions

    implicit none

    real(DP), parameter:: SQR0P5     = sqrt(0.5_DP)         !! Square root of 1/2.
    real(DP), parameter:: SQR0P67    = sqrt(2._DP/3._DP)    !! Square root of 2/3.
    real(DP), parameter:: SQR1P5     = sqrt(1.5_DP)         !! Square root of 3/2.
    real(DP), parameter:: SQR2       = sqrt(2._DP)          !! Square root of 2.
    real(DP), parameter:: ROOT6I = 1.D0/sqrt(6.D0)          !! 1/sqr(6)

contains

    module procedure euler_to_tensor
        real(DP):: sins(3), &
                   coss(3)

        !Bunge convention: phi1, Phi, phi2
        sins = sin(euler)
        coss = cos(euler)

        tensor(1, 1) = coss(1)*coss(3) - (sins(1)*sins(3)*coss(2))
        tensor(1, 2) = sins(1)*coss(3) + (coss(1)*sins(3)*coss(2))
        tensor(1, 3) = sins(3)*sins(2)
        tensor(2, 1) = -coss(1)*sins(3) - (sins(1)*coss(3)*coss(2))
        tensor(2, 2) = -sins(1)*sins(3) + (coss(1)*coss(3)*coss(2))
        tensor(2, 3) = coss(3)*sins(2)
        tensor(3, 1) = sins(1)*sins(2)
        tensor(3, 2) = -coss(1)*sins(2)
        tensor(3, 3) = coss(2)
    end procedure

    module procedure spin_to_tensor
        tensor = 0._DP
        tensor(2, 3) = spin(1)
        tensor(1, 3) = spin(2)
        tensor(1, 2) = spin(3)
        tensor(2, 1) = -tensor(1, 2)
        tensor(3, 1) = -tensor(1, 3)
        tensor(3, 2) = -tensor(2, 3)
    end procedure

    module procedure deviatoric_to_von_mises
        von_mises = SQR0P67 * norm2(deviatoric)
    end procedure

    module procedure deviatoric_to_unscaled_voigt
        voigt(1) = SQR0P5*deviatoric(1) + ROOT6I*deviatoric(2)
        voigt(2) = -SQR0P5*deviatoric(1) + ROOT6I*deviatoric(2)
        voigt(3) = -SQR0P67*deviatoric(2)
        voigt(4) =  SQR0P5*deviatoric(5)
        voigt(5) =  SQR0P5*deviatoric(3)
        voigt(6) =  SQR0P5*deviatoric(4)
    end procedure

    module procedure deviatoric_to_tensor
        tensor(1, 1) =  SQR0P5*deviatoric(1) + root6i*deviatoric(2)
        tensor(2, 2) = -SQR0P5*deviatoric(1) + root6i*deviatoric(2)
        tensor(3, 3) = -SQR0P67*deviatoric(2)
        tensor(2, 3) =  SQR0P5*deviatoric(3)
        tensor(3, 1) =  SQR0P5*deviatoric(4)
        tensor(1, 2) =  SQR0P5*deviatoric(5)
        tensor(3, 2) = tensor(2, 3)
        tensor(1, 3) = tensor(3, 1)
        tensor(2, 1) = tensor(1, 2)
    end procedure

    module procedure unscaled_voigt_to_deviatoric
        deviatoric(1) =  SQR0P5*(voigt(1) - voigt(2))
        deviatoric(2) = -SQR1P5*(voigt(3) - (sum(voigt(1:3)) / 3._DP))
        deviatoric(3) = SQR2 * voigt(4)
        deviatoric(4) = SQR2 * voigt(5)
        deviatoric(5) = SQR2 * voigt(6)
    end procedure


    module procedure unscaled_voigt_to_tensor
        tensor(1, 1) = voigt(1)
        tensor(2, 2) = voigt(2)
        tensor(3, 3) = voigt(3)
        tensor(1, 2) = voigt(4)
        tensor(2, 3) = voigt(5)
        tensor(3, 1) = voigt(6)
        tensor(1, 3) = tensor(3, 1)
        tensor(2, 1) = tensor(1, 2)
        tensor(3, 2) = tensor(2, 3)
    end procedure

    module procedure tensor_to_von_mises
        von_mises = SQR0P67 * norm2(symmetric_part(tensor))
    end procedure

    module procedure tensor_to_euler
        real(DP) :: U(3,3), VT(3,3), S(3), R(3,3)
        real(DP) :: work(15)
        integer :: info

        R = tensor

        !Perform polar decomposition to isolate rotational component of input matrix.
        !Useful even if the input matrix is a rotation matrix due to accumulation of roundoff errors during the simulation.

        ! Perform SVD: M = U * S * VT
        call dgesvd('A', 'A', 3, 3, R, 3, S, U, 3, VT, 3, work, size(work), info)

        ! Compute orthonormal rotation matrix R = U * VT
        R = matmul(U, VT)

        ! Ensure R is proper rotation (det = +1)
        if (det(R) < 0._DP) then
            U(:,3) = -U(:,3)
            R = matmul(U, VT)
        end if

        if (R(3,3) + TOLERANCE > 1._DP) then !Phi is very close to being out of bounds
            euler(1) = atan2(-R(2, 1), R(2, 2))  ! range: [-pi, pi[
            euler(2) = 0._DP
            euler(3) = 0._DP
        else if (R(3,3) - TOLERANCE < -1._DP) then !Phi is very close to being out of bounds
            euler(1) = atan2(R(2, 1), -R(2, 2))  ! range: [-pi, pi[
            euler(2) = 0._DP
            euler(3) = 0._DP
        else
            euler(1) = atan2(R(3, 1), -R(3, 2))  ! range: [-pi, pi[
            euler(2) = acos(R(3,3))
            euler(3) = atan2(R(1, 3), R(2, 3))  ! range: [-pi, pi[
        end if

        !No need to check angles(2) because acos(-1+TOLERANCE) << (PI - TOLERANCE)
        if (euler(1) < 0._DP) euler(1) = euler(1)+2._DP*PI   ![-pi, pi[ -> [0, 2*pi[
        if (euler(3) < 0._DP) euler(3) = euler(3)+2._DP*PI   ![-pi, pi[ -> [0, 2*pi[
    end procedure

    module procedure tensor_to_spin
        real(DP), dimension(3, 3):: antisym

        antisym = antisymmetric_part(tensor)
        spin = [antisym(2, 3), antisym(1, 3), antisym(1, 2)]
    end procedure

    module procedure tensor_to_deviatoric
        deviatoric(1) =  SQR0P5*(tensor(1, 1) - tensor(2, 2))
        deviatoric(2) = -SQR1P5*(tensor(3, 3) - (tensor(1, 1) + tensor(2, 2) + tensor(3, 3)) / 3._DP)
        deviatoric(3) =  SQR0P5*(tensor(2, 3) + tensor(3, 2))
        deviatoric(4) =  SQR0P5*(tensor(3, 1) + tensor(1, 3))
        deviatoric(5) =  SQR0P5*(tensor(1, 2) + tensor(2, 1))
    end procedure

    module procedure tensor_to_unscaled_voigt
        voigt(1) = tensor(1, 1)
        voigt(2) = tensor(2, 2)
        voigt(3) = tensor(3, 3)
        voigt(4) = (tensor(1, 2) + tensor(2,1)) / 2._DP
        voigt(5) = (tensor(2, 3) + tensor(3,2)) / 2._DP
        voigt(6) = (tensor(1, 3) + tensor(3,1)) / 2._DP
    end procedure
end submodule
