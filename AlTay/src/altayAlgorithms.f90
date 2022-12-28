include 'lapack.f90'

module altayAlgorithms
    use altayMiscutils, only: terminate, stopcode_runtimeerror
    use altay_definitions
    use criMathUtils
    use altay_log

    implicit none
    private
    real(dp), parameter     :: SQRT_P5 = sqrt(0.5d0)
    character(*), parameter :: MODULE_NAME = "altayAlgorithms"

    public  ::  updatC,             &
                symMatrix,          &
                vector5D,           &
                transf,             &
                rotmat, &
                rotateSRTensorFrom, &
                kleinKwa,           &
                getang

contains

    !> Updating of CIJ matrix of ellipsoid
    subroutine updatC(CIJ, Finv)
        real(dp), dimension(3,3), intent(in)    :: Finv !< Inverse of the F-tensor which describes the strain increment.
        real(dp), dimension(3,3), intent(inout) :: CIJ

        CIJ = matmul(matmul(transpose(Finv), CIJ), Finv)
    end subroutine

    !> Transform a 5D-vector in deviatoric (stress/strain-rate) space to a (3,3)-matrix representation of a symmetric and traceless 2nd rank tensor.
    !> Note: The reverse transformation is done by function 'Vector5D'.
    function SymMatrix(vec) result(sym)
        real(dp), dimension(5), intent(in)  ::  vec
        real(dp), dimension(3,3)            ::  sym
        real(dp), parameter                 ::  C1 = (sqrt(3.0d0) + 3.0d0) / 6.0d0, &
                                                C2 = (3.0d0 - sqrt(3.0d0)) / 6.0d0


        sym(2,2) =  C1 * vec(1) - C2 * vec(2)
        sym(3,3) = -C2 * vec(1) + C1 * vec(2)
        sym(1,1) = -sym(2,2) - sym(3,3)
        sym(2,3) = SQRT_P5 * vec(3)
        sym(3,1) = SQRT_P5 * vec(4)
        sym(1,2) = SQRT_P5 * vec(5)
        sym(3,2) = sym(2,3)
        sym(1,3) = sym(3,1)
        sym(2,1) = sym(1,2)
    end function

    !> Transform a (3,3)-matrix representation of a traceless 2nd rank tensor to 5D-vector representation in deviatoric (stress/strain-rate) space.
    !> Notes:
    !>    - Only the symmetric part of 2nd rank tensor is transformed.
    !>    - The reverse transformation is done by function 'SymMatrix'.
    function vector5D(mat) result(vec)
        real(dp), dimension(3,3), intent(in) :: mat
        real(dp), dimension(5)               :: vec
        real(dp), parameter                  :: C1 = 0.5d0 * (sqrt(3.0d0) + 1.0d0), &
                                                C2 = C1 - 1.0d0

        vec(1) = C1 * mat(2,2) + C2 * mat(3,3)
        vec(2) = C2 * mat(2,2) + C1 * mat(3,3)
        vec(3) = SQRT_P5 * (mat(2,3) + mat(3,2))
        vec(4) = SQRT_P5 * (mat(3,1) + mat(1,3))
        vec(5) = SQRT_P5 * (mat(1,2) + mat(2,1))
    end function

    !Calculate the CIJ matrix of an ellipsoid with half axes stored in Gaxes. T defines the orientation of the axes.
    !This version assumes that A is a diagonal matrix
    subroutine transf(Gaxes, Aprime, T)
        real(dp), dimension(3), intent(in)      :: Gaxes
        real(dp), dimension(3,3), intent(inout) :: Aprime
        real(dp), dimension(3,3), intent(in)    :: T
        integer                                 :: i, j, k
        real(dp)                                :: y
        real(dp), dimension(3)                  :: A
        real(dp), dimension(3,3)                :: X

        A = 1.D0 / Gaxes ** 2

        do j = 1,3
            X(:,j) = t(:,j) * A
        end do

        do i = 1, 3
            do j = 1, 3
                y = 0.0
                do k = 1,3
                    y = y + T(k,i) * X(k,j)
                end do
                Aprime(i, j) = y
            end do
        end do
    end subroutine

    !find half-lengths of ellipsoid axes from CIJ matrix
    !store them in prval
    !find Euler angles of these axes, store in GEULR
    subroutine GETANG(CIJ, prval, GEULR, TMAT)
        real(dp), dimension(3,3), intent(in)    :: CIJ
        real(dp), dimension(3),   intent(inout) :: GEULR, prval
        real(dp), dimension(3,3), intent(inout) :: TMAT
        integer                                 :: i
        real(dp)                                :: CIJTR, enrm
        real(dp), dimension(3,3)                :: e
        type(EulerAngles)                       :: CEuler

        CIJTR = (CIJ(1,1) + CIJ(2,2) + CIJ(3,3)) / 3.D0
        e = CIJ
        do i = 1, 3
            e(i,i) = e(i,i) - CIJTR
        end do

        call eigenv(e, prval)

        prval = prval + CIJTR

        prval = 1.D0 / sqrt(prval)
        TMAT = e
        CEuler = EuleranglesType(TMAT)
        GEULR = EulerAngles2Arr(CEuler)
    end subroutine

    subroutine eigenv(e, prval)
        real(dp), dimension(3,3), intent(inout) :: e
        real(dp), dimension(3), intent(out)     :: prval
        integer                                 :: i, info
        integer, dimension(18)                  :: iwork
        real(dp), dimension(37)                 :: work

        call dsyevd('V', 'U', 3, e, 3, prval, work, 37, iwork, 18, info)

        do i=1,3
            call normaliz(e(:,i))
        end do

        call vef_trace(MODULE_NAME, 'eigenv', prval)
    end subroutine

    subroutine normaliz(prdir)
        real(dp), dimension(3), intent(inout)   :: prdir
        real(dp)                                :: x
        real(dp), parameter     :: RESOLUTION = 0.5e-5

        x = norm2(prdir)
        if (x > RESOLUTION) then
            prdir = prdir/x
            call vef_trace(MODULE_NAME, 'normaliz', prdir)
        end if

    end subroutine

    !>should find the roots of an equation
    !>x**3 - A x + B = 0
    !>The roots are suppposed to be real.
    subroutine canoni(a, b, X, theta)
        real(dp), intent(in)                :: a, b
        real(dp), intent(out)               :: theta
        real(dp), dimension(3), intent(out) :: X
        real(dp)                            :: roota, delta

        if (a >= 0.5e-11 ) then
            roota = sqrt(a**3 / 27.0)
            delta = 0.5 * b / roota
            if (abs(delta) < (1.0 + 1.0D-6)) then
                if (delta > 1.0) delta = 1.0
                if (delta < -1.0) delta = -1.0

                theta = acos(delta)
                delta = -2.0 * sqrt(a / 3.0)

                X(1) = delta * cos(theta / 3.0)
                X(2) = delta * cos((theta + 2.0 * PI) / 3.0)
                X(3) = delta * cos((theta + 4.0 * PI) / 3.0)

                call vef_trace(MODULE_NAME, 'canoni', X)
                return
            end if
        end if
        call vef_exception(MODULE_NAME, 'canoni', VEF_BADVAL, 'Two roots seem to be complex')
    end subroutine

    !>N1=number of equations
    !>N2=number of unknowns
    !>A=coefficient matrix
    !>B=right hand sides
    !>BA=solution on output
    !>RES=residu (sum of squares)
    !>M1,M2=dimensions
    subroutine kleinkwa(N1, N2, M1, M2, A, B, BA, res)
        real(dp), dimension(M2),    intent(in)                                  :: B
        real(dp), dimension(M1,M2), intent(in)                                  :: A
        real(dp), dimension(M2),    intent(inout)                               :: BA
        real(dp),                   intent(inout)                               :: res
        integer,                    intent(in)                                  :: M1, M2, N1, N2
        integer                                                                 :: i, j, rank, info
        integer, dimension(N2)                                                  :: jpvt
        real(dp)                                                                :: y
        real(dp), dimension(max(min(N1,N2) + 3 * N2 + 1, 2 * min(N1,N2) + 1))   :: work
        real(dp), dimension(M1,M2)                                              :: A_COPY

        A_COPY = A
        BA = B
        jpvt = 0

        call dgelsy(N1, N2, 1, A_COPY, M1, BA, M2, jpvt, 0.01_dp, rank, work, size(work), info)

        res = 0.0
        do i=1,N1
            y = 0.0
            do j=1,N2
                y = y + A(i,j) * BA(j)
            end do
            RES = RES + (y - B(i))**2
        end do

        call vef_trace(MODULE_NAME, "kleinKwa", BA(1:N2))
    end subroutine
end module
