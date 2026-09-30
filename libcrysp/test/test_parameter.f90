!> Round-trip every value type supported by crysp_serialization through a Parameter.
!>
!> Covers both ways of creating a parameter (assignment and the `serialize` function family), the `typeof` and `shape_of`
!> queries, and reading the value back via assignment.
!>
!> Ends with a negative test: reading a matrix parameter into a destination of the wrong shape must abort through `log_error`.
!> Since that terminates the program, it runs last, after the summary. CTest therefore judges this test on its output rather
!> than its exit status (see CMakeLists.txt): any "FAILED" line fails it, and the logged exception is required for it to pass.
program test_parameter
    use iso_c_binding
    use base_defs, only: DP
    use crysp_serialization
    use testing

    implicit none

    call test_integer()
    call test_int_array()
    call test_real()
    call test_real_array()
    call test_real_matrix()
    call test_string()

    call finish()

    ! Must be last: terminates the program.
    call test_shape_mismatch_aborts()

contains

    subroutine test_integer()
        type(Parameter):: p, q
        integer:: out

        p = 42
        call check_equal(int(typeof(p)), TYPE_INTEGER, 'integer: typeof')
        call check_equal(size(shape_of(p)), 0, 'integer: shape_of is scalar')
        out = p
        call check_equal(out, 42, 'integer: roundtrip via assignment')

        q = serialize(-7)
        out = q
        call check_equal(out, -7, 'integer: roundtrip via serialize()')
    end subroutine

    subroutine test_int_array()
        type(Parameter):: p, q
        integer, dimension(5):: in = [3, 1, 4, 1, 5]
        integer, dimension(5):: out
        integer, dimension(:), allocatable:: dyn, s

        p = in
        call check_equal(int(typeof(p)), TYPE_INT_ARRAY, 'int array: typeof')
        call check_equal(shape_of(p), [5], 'int array: shape_of')
        out = 0
        out = p
        call check_equal(out, in, 'int array: roundtrip via assignment')

        ! Typical consumer pattern: query the shape, allocate, then read.
        q = serialize(in(2:4))
        s = shape_of(q)
        allocate(dyn(s(1)))
        dyn = q
        call check_equal(dyn, [1, 4, 1], 'int array: roundtrip via serialize() into allocated buffer')
    end subroutine

    subroutine test_real()
        type(Parameter):: p, q
        real(DP):: out

        p = 2.5_DP
        call check_equal(int(typeof(p)), TYPE_REAL, 'real: typeof')
        call check_equal(size(shape_of(p)), 0, 'real: shape_of is scalar')
        out = p
        call check_equal(out, 2.5_DP, 'real: roundtrip via assignment')

        q = serialize(-1.0e-300_DP)
        out = q
        call check_equal(out, -1.0e-300_DP, 'real: roundtrip via serialize() preserves tiny values exactly')
    end subroutine

    subroutine test_real_array()
        type(Parameter):: p, q
        real(DP), dimension(3):: in = [1._DP, -2._DP, 0.125_DP]
        real(DP), dimension(3):: out
        real(DP), dimension(:), allocatable:: dyn
        integer, dimension(:), allocatable:: s

        p = in
        call check_equal(int(typeof(p)), TYPE_REAL_ARRAY, 'real array: typeof')
        call check_equal(shape_of(p), [3], 'real array: shape_of')
        out = 0._DP
        out = p
        call check_equal(out, in, 'real array: roundtrip via assignment')

        q = serialize(in)
        s = shape_of(q)
        allocate(dyn(s(1)))
        dyn = q
        call check_equal(dyn, in, 'real array: roundtrip via serialize() into allocated buffer')
    end subroutine

    subroutine test_real_matrix()
        type(Parameter):: p, q, r
        real(DP), dimension(2,3):: in, out
        real(DP), dimension(:,:), allocatable:: dyn
        integer:: i, j

        do j = 1, 3
            do i = 1, 2
                in(i,j) = 10._DP * i + j
            end do
        end do

        p = in
        call check_equal(int(typeof(p)), TYPE_REAL_MATRIX, 'real matrix: typeof')
        call check_equal(shape_of(p), [2, 3], 'real matrix: shape_of preserves extent order')
        out = 0._DP
        out = p
        call check_equal(out, in, 'real matrix: roundtrip via assignment')

        ! deserialize_real_matrix returns an allocatable of the stored shape without the caller knowing it in advance.
        q = serialize(in)
        dyn = deserialize_real_matrix(q)
        call check_equal(dyn, in, 'real matrix: roundtrip via deserialize_real_matrix')

        ! A zero-extent matrix must survive as well (models with no parameters may produce these).
        r = serialize(reshape([real(DP)::], [0, 4]))
        call check_equal(shape_of(r), [0, 4], 'real matrix: zero-extent shape preserved')
    end subroutine

    subroutine test_string()
        type(Parameter):: p, q
        character(:), allocatable:: out

        p = 'Boundaries'
        call check_equal(int(typeof(p)), TYPE_STRING, 'string: typeof')
        call check_equal(shape_of(p), [10], 'string: shape_of is the length')
        out = p
        call check_equal(out, 'Boundaries', 'string: roundtrip via assignment')

        q = serialize('')
        out = q
        call check_equal(len(out), 0, 'string: empty string roundtrip')
    end subroutine

    !> Negative test. Expected to end the program via log_error(..., ERR_DIMS, ...); reaching the print means the check was skipped.
    subroutine test_shape_mismatch_aborts()
        type(Parameter):: p
        real(DP), dimension(3,3):: source = 0._DP
        real(DP), dimension(2,2):: too_small

        p = source
        too_small = p

        print '(A)', '  FAILED: real matrix: shape mismatch was not detected'
    end subroutine
end program
