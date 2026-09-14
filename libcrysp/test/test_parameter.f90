!> Round-trip every value type supported by crysp_serialization through a Parameter.
!>
!> Covers both ways of creating a parameter (assignment and the `serialize` function family), the `type_of` and `shape_of`
!> queries, and reading the value back via assignment. The `serialize` family is tested in both of its flavours: the assumed-shape
!> `_desc` forms used from Fortran, and the explicit-extent forms that C callers reach with a bare pointer plus its extents.
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
    call test_int_array_explicit()
    call test_real()
    call test_real_array()
    call test_real_array_explicit()
    call test_real_matrix()
    call test_real_matrix_explicit()
    call test_string()

    call finish()

    ! Must be last: terminates the program.
    call test_shape_mismatch_aborts()

contains

    subroutine test_integer()
        type(Parameter):: p, q
        integer:: out
        integer(C_INT), dimension(2):: s

        p = 42
        call check_equal(int(type_of(p)), TYPE_INTEGER, 'integer: type_of')
        call shape_of(p, s)
        call check_equal(int(s), [1, 0], 'integer: shape_of reports a single element')
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
        integer, dimension(:), allocatable:: dyn
        integer(C_INT), dimension(2):: s

        p = in
        call check_equal(int(type_of(p)), TYPE_INT_ARRAY, 'int array: type_of')
        call shape_of(p, s)
        call check_equal(int(s), [5, 0], 'int array: shape_of')
        out = 0
        out = p
        call check_equal(out, in, 'int array: roundtrip via assignment')

        ! Typical consumer pattern: query the shape, allocate, then read.
        q = serialize(in(2:4))
        call shape_of(q, s)
        allocate(dyn(s(1)))
        dyn = q
        call check_equal(dyn, [1, 4, 1], 'int array: roundtrip via serialize() into allocated buffer')
    end subroutine

    !> The explicit-extent counterpart of test_int_array, which is how C callers build an int array parameter.
    !>
    !> The buffer passed in is longer than `len` asks for, so the test fails if the extent argument is ignored in favour of the
    !> size of the actual argument. A C caller relies on exactly that when it serializes a slice of a bigger allocation.
    subroutine test_int_array_explicit()
        type(Parameter):: p
        integer(C_INT), dimension(5):: buffer = [3, 1, 4, 1, 5]
        integer, dimension(:), allocatable:: out
        integer(C_INT), dimension(2):: s

        p = serialize(buffer, 3)
        call check_equal(int(type_of(p)), TYPE_INT_ARRAY, 'int array (explicit len): type_of')
        call shape_of(p, s)
        call check_equal(int(s), [3, 0], 'int array (explicit len): len argument sets the size, not the buffer')
        allocate(out(s(1)))
        out = p
        call check_equal(out, [3, 1, 4], 'int array (explicit len): leading elements are the ones stored')
    end subroutine

    subroutine test_real()
        type(Parameter):: p, q
        real(DP):: out
        integer(C_INT), dimension(2):: s

        p = 2.5_DP
        call check_equal(int(type_of(p)), TYPE_REAL, 'real: type_of')
        call shape_of(p, s)
        call check_equal(int(s), [1, 0], 'real: shape_of reports a single element')
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
        integer(C_INT), dimension(2):: s

        p = in
        call check_equal(int(type_of(p)), TYPE_REAL_ARRAY, 'real array: type_of')
        call shape_of(p, s)
        call check_equal(int(s), [3, 0], 'real array: shape_of')
        out = 0._DP
        out = p
        call check_equal(out, in, 'real array: roundtrip via assignment')

        q = serialize(in)
        call shape_of(q, s)
        allocate(dyn(s(1)))
        dyn = q
        call check_equal(dyn, in, 'real array: roundtrip via serialize() into allocated buffer')
    end subroutine

    !> The explicit-extent counterpart of test_real_array. See test_int_array_explicit for why the buffer is oversized.
    subroutine test_real_array_explicit()
        type(Parameter):: p
        real(C_DOUBLE), dimension(5):: buffer = [1._DP, -2._DP, 0.125_DP, 99._DP, 99._DP]
        real(DP), dimension(:), allocatable:: out
        integer(C_INT), dimension(2):: s

        p = serialize(buffer, 3)
        call check_equal(int(type_of(p)), TYPE_REAL_ARRAY, 'real array (explicit len): type_of')
        call shape_of(p, s)
        call check_equal(int(s), [3, 0], 'real array (explicit len): len argument sets the size, not the buffer')
        allocate(out(s(1)))
        out = p
        call check_equal(out, buffer(1:3), 'real array (explicit len): leading elements are the ones stored')
    end subroutine

    subroutine test_real_matrix()
        type(Parameter):: p, q, r
        real(DP), dimension(2,3):: in, out
        real(DP), dimension(:,:), allocatable:: dyn
        integer(C_INT), dimension(2):: s
        integer:: i, j

        do j = 1, 3
            do i = 1, 2
                in(i,j) = 10._DP * i + j
            end do
        end do

        p = in
        call check_equal(int(type_of(p)), TYPE_REAL_MATRIX, 'real matrix: type_of')
        call shape_of(p, s)
        call check_equal(int(s), [2, 3], 'real matrix: shape_of preserves extent order')
        out = 0._DP
        out = p
        call check_equal(out, in, 'real matrix: roundtrip via assignment')

        ! deserialize_real_matrix returns an allocatable of the stored shape without the caller knowing it in advance.
        q = serialize(in)
        dyn = deserialize_real_matrix(q)
        call check_equal(dyn, in, 'real matrix: roundtrip via deserialize_real_matrix')

        ! A zero-extent matrix must survive as well (models with no parameters may produce these).
        r = serialize(reshape([real(DP)::], [0, 4]))
        call shape_of(r, s)
        call check_equal(int(s), [0, 4], 'real matrix: zero-extent shape preserved')
    end subroutine

    !> The explicit-extent counterpart of test_real_matrix. See test_int_array_explicit for why the buffer is oversized.
    !>
    !> The buffer is read in element (column-major) order, so the first rows*cols elements are taken and cut into columns of
    !> `rows`. A C caller hands over a bare pointer to that same element sequence; from Fortran the actual argument has to be a
    !> matrix, since the generic `serialize` resolves on rank and would not match a flat array here.
    subroutine test_real_matrix_explicit()
        type(Parameter):: p
        real(C_DOUBLE), dimension(2,4):: buffer = reshape([11._DP, 21._DP, 12._DP, 22._DP, 13._DP, 23._DP, 99._DP, 99._DP], [2, 4])
        real(DP), dimension(:,:), allocatable:: out
        integer(C_INT), dimension(2):: s

        p = serialize(buffer, 2, 3)
        call check_equal(int(type_of(p)), TYPE_REAL_MATRIX, 'real matrix (explicit extents): type_of')
        call shape_of(p, s)
        call check_equal(int(s), [2, 3], 'real matrix (explicit extents): extent arguments set the shape')
        out = deserialize_real_matrix(p)
        call check_equal(out, buffer(:,1:3), 'real matrix (explicit extents): buffer is cut into columns')
    end subroutine

    subroutine test_string()
        type(Parameter):: p, q
        character(:), allocatable:: out
        integer(C_INT), dimension(2):: s

        p = 'Boundaries'
        call check_equal(int(type_of(p)), TYPE_STRING, 'string: type_of')
        call shape_of(p, s)
        call check_equal(int(s), [10, 0], 'string: shape_of is the length')
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
