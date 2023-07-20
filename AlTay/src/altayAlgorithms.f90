module altayAlgorithms
    use definitions
    use criMathUtils
    use logging

    implicit none
    private

    real(DP), parameter     :: SQRT_P5 = sqrt(0.5_dp)
    character(*), parameter :: MODULE_NAME = "altayAlgorithms"

    public  ::  updatC,             &
                symMatrix,          &
                vector5D,           &
                rotmat, &
                kleinKwa,           &
                getang

contains

    !> Updating of CIJ matrix of ellipsoid
    subroutine updatC(CIJ, Finv)
        real(DP), dimension(3,3), intent(in)    :: Finv !< Inverse of the F-tensor which describes the strain increment.
        real(DP), dimension(3,3), intent(inout) :: CIJ

        CIJ = matmul(matmul(transpose(Finv), CIJ), Finv)
    end subroutine

    !> Transform a 5D-vector in deviatoric (stress/strain-rate) space to a (3,3)-matrix representation of a symmetric and traceless 2nd rank tensor.
    !> Note: The reverse transformation is done by function 'Vector5D'.
    function SymMatrix(vec) result(sym)
        real(DP), dimension(5), intent(in)  ::  vec
        real(DP), dimension(3,3)            ::  sym
        real(DP), parameter                 ::  C1 = (sqrt(3.0_dp) + 3.0_dp) / 6.0_dp, &
                                                C2 = (3.0_dp - sqrt(3.0_dp)) / 6.0_dp


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
        real(DP), dimension(3,3), intent(in) :: mat
        real(DP), dimension(5)               :: vec
        real(DP), parameter                  :: C1 = 0.5_dp * (sqrt(3.0_dp) + 1.0_dp), &
                                                C2 = C1 - 1.0_dp

        vec(1) = C1 * mat(2,2) + C2 * mat(3,3)
        vec(2) = C2 * mat(2,2) + C1 * mat(3,3)
        vec(3) = SQRT_P5 * (mat(2,3) + mat(3,2))
        vec(4) = SQRT_P5 * (mat(3,1) + mat(1,3))
        vec(5) = SQRT_P5 * (mat(1,2) + mat(2,1))
    end function


    !find half-lengths of ellipsoid axes from CIJ matrix
    !store them in prval
    subroutine GETANG(CIJ, prval, TMAT)
        real(DP), dimension(3,3), intent(in)  :: CIJ
        real(DP), dimension(3),   intent(out) ::  prval
        real(DP), dimension(3,3), intent(out) :: TMAT
        integer                                 :: i
        real(DP)                                :: CIJTR

        CIJTR = (CIJ(1,1) + CIJ(2,2) + CIJ(3,3)) / 3._dp
        TMAT = CIJ
        do i = 1, 3
            TMAT(i,i) = TMAT(i,i) - CIJTR
        end do

        call eigenv(TMAT, prval)

        prval = prval + CIJTR
        prval = 1._dp / sqrt(prval)
    end subroutine

    subroutine eigenv(e, prval)
        real(DP), dimension(3,3), intent(inout) :: e
        real(DP), dimension(3), intent(out)     :: prval
        integer                                 :: i, info
        integer, dimension(18)                  :: iwork
        real(DP), dimension(37)                 :: work

        call dsyevd('V', 'U', 3, e, 3, prval, work, 37, iwork, 18, info)

        do i=1,3
            call normaliz(e(:,i))
        end do

        call log_trace(MODULE_NAME, 'eigenv', prval)
    end subroutine

    subroutine normaliz(prdir)
        real(DP), dimension(3), intent(inout)   :: prdir
        real(DP)                                :: x
        real(DP), parameter     :: RESOLUTION = 0.5e-5_DP

        x = norm2(prdir)
        if (x > RESOLUTION) then
            prdir = prdir/x
            call log_trace(MODULE_NAME, 'normaliz', prdir)
        end if

    end subroutine

    !>N1=number of equations
    !>N2=number of unknowns
    !>A=coefficient matrix
    !>B=right hand sides
    !>BA=solution on output
    !>RES=residu (sum of squares)
    !>M1,M2=dimensions
    subroutine kleinkwa(N1, N2, M1, M2, A, B, BA, res)
        integer,                    intent(in)                                  :: M1, M2, N1, N2
        real(DP), dimension(M2),    intent(in)                                  :: B
        real(DP), dimension(M1,M2), intent(in)                                  :: A
        real(DP), dimension(M2),    intent(out)                                 :: BA
        real(DP),                   intent(inout)                               :: res
        integer                                                                 :: i, rank, info
        integer, dimension(N2)                                                  :: jpvt
        real(DP)                                                                :: y
        real(DP), dimension(max(min(N1,N2) + 3 * N2 + 1, 2 * min(N1,N2) + 1))   :: work
        real(DP), dimension(M1,M2)                                              :: A_COPY

        A_COPY = A
        BA = B
        jpvt = 0

        call dgelsy(N1, N2, 1, A_COPY, M1, BA, M2, jpvt, 0.01_dp, rank, work, size(work), info)

        res = 0.0_DP
        do i=1,N1
            y = sum(A(i,1:N2)*BA(1:N2))
            RES = RES + (y - B(i))**2
        end do

        call log_trace(MODULE_NAME, "kleinKwa", BA(1:N2))
    end subroutine
end module
