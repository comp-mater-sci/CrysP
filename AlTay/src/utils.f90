!>Global definitions used in multiple modules within AlTay
module utils
    use, intrinsic:: iso_fortran_env, only: output_unit

    implicit none
    public


    integer, parameter::  DP = selected_real_kind(15, 307)
    real(DP), parameter:: TOLERANCE  = 1.E-9_DP, &
                          REAL_DP_MAX_VAL = huge(0._DP), &
                          PI         = acos(-1.D0), &
                          RAD_TO_DEG = 180._DP/PI, &
                          SQR0P5     = sqrt(0.5_DP), &
                          SQR0P67    = sqrt(2._DP/3._DP), &
                          SQR1P5     = sqrt(1.5_DP), &
                          SQR2       = sqrt(2._DP)

    !Status codes
    enum, bind(C)
        enumerator:: VEF_OK, &
                      VEF_FAIL, &
                      VEF_ERROR
    end enum
    integer, parameter       :: display_unit = output_unit

    integer:: IMP1 = 7  !< output-file with successive "current situations"
    integer:: IMP5 = 111     !< output of stress-strain or slip-stress

    !> Maximal length of path acceptable by the filesystem
    integer, parameter       :: MAX_PATHLEN = 2048

    !> Matrix form of the unit second rank tensor
    real(DP), dimension(3, 3), parameter:: UNIT_MATRIX_3X3 = reshape([1._DP, 0._DP, 0._DP, &
                                                                       0._DP, 1._DP, 0._DP, &
                                                                       0._DP, 0._DP, 1._DP], [3, 3])

    character(*), private, parameter:: MOD_NAME = 'utils'

    interface normalize
        module procedure normalize_int, normalize_real, normalize_vec_int
    end interface

    interface convert_stress_strain_space
        module procedure convert_stress_strain_mat_vec, &
                         convert_stress_strain_vec_mat
    end interface

    interface convert_spin
        module procedure convert_spin_mat_vec, &
                         convert_spin_vec_mat
    end interface

    interface operator(.dot.)
        module procedure dot_product_wrapper, double_dot_product
    end interface

    interface operator(.cross.)
        module procedure cross_int, cross_real
    end interface

    interface operator(.outer.)
        module procedure outer_product
    end interface

    interface operator(.toframe.)
        module procedure rotate_to
    end interface

    interface operator(.fromframe.)
        module procedure rotate_from
    end interface
contains

    !> Convert second-rank tensor t into 5D vector following Van Houtte et al., 1992.
    !> Hydrostatic component is subtracted and tensor is symmetrized
    pure function convert_stress_strain_mat_vec(t) result(v)
        real(DP), dimension(3, 3), intent(in)   :: t
        real(DP), dimension(5)                :: v

        v(1) =  SQR0P5*(t(1, 1) - t(2, 2))
        v(2) = -SQR1P5*(t(3, 3) - (t(1, 1) + t(2, 2) + t(3, 3)) / 3._DP)
        v(3) =  SQR0P5*(t(2, 3) + t(3, 2))
        v(4) =  SQR0P5*(t(3, 1) + t(1, 3))
        v(5) =  SQR0P5*(t(1, 2) + t(2, 1))
    end function

    !> Convert 5D vector v into second-rank tensor
    pure function convert_stress_strain_vec_mat(v) result(t)
        real(DP), dimension(5), intent(in)    :: v
        real(DP), dimension(3, 3)             :: t
        real(DP), parameter ::  root6i = 1.D0/sqrt(6.D0)

        t(1, 1) =  SQR0P5*v(1) + root6i*v(2)
        t(2, 2) = -SQR0P5*v(1) + root6i*v(2)
        t(3, 3) = -SQR0P67*v(2)
        t(2, 3) =  SQR0P5*v(3)
        t(3, 1) =  SQR0P5*v(4)
        t(1, 2) =  SQR0P5*v(5)
        t(3, 2) = t(2, 3)
        t(1, 3) = t(3, 1)
        t(2, 1) = t(1, 2)
    end function

    pure real(DP) function dot_product_wrapper(vec1, vec2)
        real(DP), intent(in):: vec1(:), &
                                vec2(size(vec1))

        dot_product_wrapper = dot_product(vec1, vec2)
    end function

    pure real(DP) function double_dot_product(mat1, mat2)
        real(DP), intent(in):: mat1(:,:), &
                               mat2(size(mat1, 1), size(mat1, 2))

        double_dot_product = sum(mat1*mat2)
    end function

    pure function cross_int(v1, v2) result(cross)
        integer, intent(in), dimension(3):: v1, v2
        integer, dimension(3):: cross

        cross(1)=v1(2)*v2(3)-v1(3)*v2(2)
        cross(2)=v1(3)*v2(1)-v1(1)*v2(3)
        cross(3)=v1(1)*v2(2)-v1(2)*v2(1)
    end function

    pure function cross_real(v1, v2) result(cross)
        real(DP), intent(in), dimension(3):: v1, v2
        real(DP), dimension(3):: cross

        cross(1)=v1(2)*v2(3)-v1(3)*v2(2)
        cross(2)=v1(3)*v2(1)-v1(1)*v2(3)
        cross(3)=v1(1)*v2(2)-v1(2)*v2(1)
    end function

    pure function normalize_int(arr) result(normalized)
        integer, dimension(:,:), intent(in):: arr
        real(DP), dimension(3, size(arr, 2)):: normalized
        integer:: i

        do i = 1, size(arr, 2)
            normalized(:,i) = real(arr(:,i), DP) / norm2(real(arr(:,i), DP))
        end do
    end function
     pure function normalize_vec_int(vec) result(normalized)
        integer, dimension(:), intent(in):: vec
        real(DP), dimension(size(vec)):: normalized
        integer:: i

        normalized = real(vec, DP) / norm2(real(vec, DP))
    end function

    pure function normalize_real(arr) result(normalized)
        real(DP), dimension(:,:), intent(in):: arr
        real(DP), dimension(3, size(arr, 2)):: normalized
        integer:: i

        do i = 1, size(arr, 2)
            normalized(:,i) = arr(:,i) / norm2(arr(:,i))
        end do
    end function


    pure function outer_product(v1, v2) result(prod)
        real(DP), dimension(:), intent(in)      :: v1, v2
        real(DP), dimension(size(v1), size(v2)):: prod
        integer                                 :: i, j

        forall(i = 1:size(v1), j = 1:size(v2)) prod(i, j) = v1(i) * v2(j)
    end function

    pure function symmetric_part(mat) result(sym)
        real(DP), dimension(:,:), intent(in):: mat
        real(DP), dimension(size(mat, 1), size(mat, 2)):: sym

        sym = (mat+transpose(mat)) / 2._DP
    end function


    pure function antisymmetric_part(mat) result(antisym)
        real(DP), dimension(:,:), intent(in):: mat
        real(DP), dimension(size(mat, 1), size(mat, 2)):: antisym

        antisym = (mat-transpose(mat)) / 2._DP
    end function

    pure function convert_spin_vec_mat(vec) result(mat)
        real(DP), intent(in):: vec(3)
        real(DP)::             mat(3, 3)

        mat = 0._DP
        mat(1, 2) = vec(1)
        mat(1, 3) = vec(2)
        mat(2, 3) = vec(3)
        mat(2, 1) = -mat(1, 2)
        mat(3, 1) = -mat(1, 3)
        mat(3, 2) = -mat(2, 3)
    end function
    pure function convert_spin_mat_vec(t) result(rot)
        real(DP), dimension(3, 3), intent(in):: t
        real(DP), dimension(3)               :: rot
        real(DP), dimension(3, 3)             :: antisym

        antisym = antisymmetric_part(t)
        rot = [antisym(1, 2), antisym(1, 3), antisym(2, 3)]
    end function

    ! Returns the inverse of a matrix calculated by finding the LU
    ! decomposition.  Depends on LAPACK.
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

    !Compute matrix exponential for a (3, 3)-matrix with small norm, i.e. ||A|| < 1
    !If ||A|| > 1, catastrophic cancellation in floating point arithmetic can occur
    !Uses the Taylor Series Expansion:
    !exp(A) == I+A + A^2/(2!) + A^3/(3!) + ... + A^n/(n!) + ...
    pure function matrix_exponential_small_norm(A) result(exponential)
        real(DP), dimension(3, 3), intent(in):: A
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

    !> @brief Rotates the second-rank tensor S to the reference frame given by rotation R.
    !! @return The tensor in the frame defined by R
    !! @param S the tensor before rotation
    !! @param R Rotation matrix following Bunge convention. I.e. the basis of the unrotated frame expressed in the rotated frame.
    pure function rotate_to(S, R) result(Srot)
        real(DP), dimension(3, 3), intent(in)    ::  S, &
                                                    R
        real(DP), dimension(3, 3)                ::  Srot

        Srot = matmul(matmul(R, S), transpose(R))
    end function

    !> @brief Rotates the second-rank tensor S from the reference frame given by rotation R.
    !! @return The rotated tensor
    !! @param S the tensor expressed in the frame defined by R
    !! @param R Rotation matrix following Bunge convention. I.e. the basis of the unrotated frame expressed in the rotated frame.
    pure function rotate_from(S, R) result(Srot)
        real(DP), dimension(3, 3), intent(in)    ::  S, &
                                                    R
        real(DP), dimension(3, 3)                ::  Srot

        Srot = matmul(matmul(transpose(R), S), R)
    end function

    !> @brief Calculate the angle between two vectors.
    !> @return The angle between the input vector in radians.
    pure real(DP) function vec_angle(u, v)
        real(DP), dimension(:), intent(in):: u          !< First vector. Must be non-zero.
        real(DP), dimension(size(u)), intent(in):: v    !< Second vector. Must be non-zero.

        vec_angle = acos((u .dot. v) / sqrt((u .dot. u) * (v .dot. v)))
    end function

    !> @brief Convert Euler angles to a rotation matrix.
    !> @return 3x3 rotation matrix corresponding to the given Euler angles.
    pure function from_euler_angles(angles) result(mat)
    real(DP), dimension(3), intent(in):: angles !< Euler angles in Bunge convention
    real(DP), dimension(3, 3)    :: mat
    real(DP):: cos_phi1, cos_phi2, cos_PHI, &
               sin_phi1, sin_phi2, sin_PHI

        cos_phi1 = cos(angles(1))
        cos_PHI = cos(angles(2))
        cos_phi2 = cos(angles(3))
        sin_phi1 = sin(angles(1))
        sin_PHI = sin(angles(2))
        sin_phi2 = sin(angles(3))

        mat(1, 1) = cos_phi1*cos_phi2 - (sin_phi1*sin_phi2*cos_PHI)
        mat(1, 2) = sin_phi1*cos_phi2 + (cos_phi1*sin_phi2*cos_PHI)
        mat(1, 3) = sin_phi2*sin_PHI
        mat(2, 1) = -cos_phi1*sin_phi2 - (sin_phi1*cos_phi2*cos_PHI)
        mat(2, 2) = -sin_phi1*sin_phi2 + (cos_phi1*cos_phi2*cos_PHI)
        mat(2, 3) = cos_phi2*sin_PHI
        mat(3, 1) = sin_phi1*sin_PHI
        mat(3, 2) = -cos_phi1*sin_PHI
        mat(3, 3) = cos_PHI
    end function

    !> @brief Converts a rotation matrix to Euler angles.
    !> @return 3-element vector containing the Euler angles corresponding to the given rotation matrix in Bunge convention.
    pure function to_euler_angles(mat) result(ang)
        real(DP), dimension(3, 3), intent(in)::  mat !< Rotation matrix.
        real(DP)::              ang(3), &
                                phi1, &
                                PHI, &
                                phi2, &
                                cos_PHI

        cos_PHI = mat(3, 3) / sqrt( mat(1, 3)**2+mat(2, 3)**2+mat(3, 3)**2 )
        PHI = acos(cos_PHI)  ! range: [0, pi]

        if (abs(cos_PHI)==1.0D0) then  ! case that PHI = 0\B0 or PHI = 180\B0
            !Set phi2 to 0.0D0, given that:
            !  (phi1;   0\B0; phi2) equivalent to (phi1+phi2;    0; 0).
            !  (phi1; 180\B0; phi2) equivalent to (phi1+phi2; 180\B0; 0).
            phi2 = 0.0D0
            phi1 = atan2(-mat(2, 1)/cos_PHI, mat(2, 2)/cos_PHI)  ! range: [-pi, pi[
        else
            phi1 = atan2(mat(3, 1), -mat(3, 2))  ! range: [-pi, pi[
            phi2 = atan2(mat(1, 3), mat(2, 3))  ! range: [-pi, pi[
        end if

        !If needed, replace Euler angles with equivalent values within proper bounds.
        if (PHI == PI)     PHI  = 0._DP       ![  0, pi] -> [0,  pi[
        if (phi1 < 0._DP) phi1 = phi1+2._DP*PI   ![-pi, pi[ -> [0, 2*pi[
        if (phi2 < 0._DP) phi2 = phi2+2._DP*PI   ![-pi, pi[ -> [0, 2*pi[

        ang(1) = phi1
        ang(2) = phi
        ang(3) = phi2
    end function

    !> @brief Calculate the trace of a matrix.
    pure real(DP) function trace(x) result(res)
        real(DP), dimension(:,:), intent(in):: x !< The matrix. Must be square.

        integer:: i

        res = 0._DP
        do i = 1, size(x, 1)
            res = res+x(i, i)
        enddo
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

    !>@Brief Determine the indices of a basis for a matrix. I.e. a set of indices pointing to linearly independent columns of the
    !matrix.
    !>@Details Uses LAPACK routine dgetrf. Assumes that the column rank is at least equal to the number of rows.
    function basis_indices(mat) result(ind_basis)
        real(DP), dimension(:,:), intent(in):: mat      !> The matrix. Assumed to be wide (cols > rows). Assumed to have column rank
                                                        !! >= number of rows
        integer, dimension(size(mat, 1)):: ind_basis    !> Indices of the columns of the matrix making up a basis for the column
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
end module
