!> Round-trip every value type supported by crysp_parameters through a Parameter.
!>
!> Covers both ways of creating a parameter (assignment and the `serialize` function family), the `type_of` and `shape_of`
!> queries, and reading the value back via assignment. Also covers the edge cases and usage patterns that have tripped up a
!> compiler before: empty arrays, array constructors of `serialize` results, and copies of parameters. The C-facing conversion
!> lives in the C layer and is not tested here.
!>
!> Ends with a negative test: reading a matrix parameter into a destination of the wrong shape must abort through `error stop`.
!> Since that terminates the program, it runs last, after the summary. CTest therefore judges this test on its output rather
!> than its exit status (see CMakeLists.txt): any "FAILED" line fails it, and the stop message is required for it to pass.
program test_parameter
    use base_defs, only: DP
    use crysp_parameters
    use testing

    implicit none

    call test_integer()
    call test_int_array()
    call test_real()
    call test_real_array()
    call test_real_matrix()
    call test_string()
    call test_read_does_not_consume()
    call test_copy_is_deep()
    call test_array_constructor()

    call finish()

    ! Must be last: terminates the program.
    call test_shape_mismatch_aborts()

contains

    subroutine test_integer()
        type(Parameter):: p, q
        integer:: out

        p = 42
        call check_equal(type_of(p), TYPE_INTEGER, 'integer: type_of')
        call check_equal(shape_of(p), [1, 1], 'integer: shape_of reports a single element')
        out = p
        call check_equal(out, 42, 'integer: roundtrip via assignment')

        q = serialize(-7)
        out = q
        call check_equal(out, -7, 'integer: roundtrip via serialize()')
    end subroutine

    subroutine test_int_array()
        type(Parameter):: p, q, r, t
        integer, dimension(5):: in = [3, 1, 4, 1, 5]
        integer, dimension(5):: out
        integer, dimension(1):: single
        integer, dimension(:), allocatable:: dyn
        integer, dimension(2):: s

        p = in
        call check_equal(type_of(p), TYPE_INTEGER, 'int array: type_of')
        call check_equal(shape_of(p), [5, 1], 'int array: shape_of')
        out = 0
        out = p
        call check_equal(out, in, 'int array: roundtrip via assignment')

        ! Typical consumer pattern: query the shape, allocate, then read.
        q = serialize(in(2:4))
        s = shape_of(q)
        allocate(dyn(s(1)))
        dyn = q
        call check_equal(dyn, [1, 4, 1], 'int array: roundtrip via serialize() into allocated buffer')

        ! GFortran loses the dynamic type of an empty array allocated with source=, see the note on Parameter.
        r = serialize([integer::])
        call check_equal(shape_of(r), [0, 1], 'int array: empty shape preserved')
        deallocate(dyn)
        allocate(dyn(0))
        dyn = r
        call check_equal(size(dyn), 0, 'int array: empty roundtrip')

        ! Same extents as a scalar, but still an array.
        t = serialize([9])
        single = t
        call check_equal(single(1), 9, 'int array: one-element roundtrip')
    end subroutine

    subroutine test_real()
        type(Parameter):: p, q
        real(DP):: out

        p = 2.5_DP
        call check_equal(type_of(p), TYPE_REAL, 'real: type_of')
        call check_equal(shape_of(p), [1, 1], 'real: shape_of reports a single element')
        out = p
        call check_equal(out, 2.5_DP, 'real: roundtrip via assignment')

        q = serialize(-1.0e-300_DP)
        out = q
        call check_equal(out, -1.0e-300_DP, 'real: roundtrip via serialize() preserves tiny values exactly')
    end subroutine

    subroutine test_real_array()
        type(Parameter):: p, q, r
        real(DP), dimension(3):: in = [1._DP, -2._DP, 0.125_DP]
        real(DP), dimension(3):: out
        real(DP), dimension(:), allocatable:: dyn
        integer, dimension(2):: s

        p = in
        call check_equal(type_of(p), TYPE_REAL, 'real array: type_of')
        call check_equal(shape_of(p), [3, 1], 'real array: shape_of')
        out = 0._DP
        out = p
        call check_equal(out, in, 'real array: roundtrip via assignment')

        q = serialize(in)
        s = shape_of(q)
        allocate(dyn(s(1)))
        dyn = q
        call check_equal(dyn, in, 'real array: roundtrip via serialize() into allocated buffer')

        r = serialize([real(DP)::])
        deallocate(dyn)
        allocate(dyn(0))
        dyn = r
        call check_equal(size(dyn), 0, 'real array: empty roundtrip')
    end subroutine

    subroutine test_real_matrix()
        type(Parameter):: p, q, r
        real(DP), dimension(2,3):: in, out
        real(DP), dimension(:,:), allocatable:: dyn
        integer, dimension(2):: s
        integer:: i, j

        do j = 1, 3
            do i = 1, 2
                in(i,j) = 10._DP * i + j
            end do
        end do

        p = in
        call check_equal(type_of(p), TYPE_REAL, 'real matrix: type_of')
        call check_equal(shape_of(p), [2, 3], 'real matrix: shape_of preserves extent order')
        out = 0._DP
        out = p
        call check_equal(out, in, 'real matrix: roundtrip via assignment')

        ! A receiving buffer of unknown shape is allocated from shape_of before reading.
        q = serialize(in)
        s = shape_of(q)
        allocate(dyn(s(1), s(2)))
        dyn = q
        call check_equal(dyn, in, 'real matrix: roundtrip into buffer allocated from shape_of')

        ! A zero-extent matrix must survive as well (models with no parameters may produce these).
        r = serialize(reshape([real(DP)::], [0, 4]))
        s = shape_of(r)
        call check_equal(s, [0, 4], 'real matrix: zero-extent shape preserved')
        deallocate(dyn)
        allocate(dyn(s(1), s(2)))
        dyn = r
        call check_equal(size(dyn), 0, 'real matrix: zero-extent roundtrip')
    end subroutine

    subroutine test_string()
        type(Parameter):: p, q
        character(:), allocatable:: out

        p = 'Boundaries'
        call check_equal(type_of(p), TYPE_STRING, 'string: type_of')
        call check_equal(shape_of(p), [1, 1], 'string: shape_of reports a single element')
        out = p
        call check_equal(out, 'Boundaries', 'string: roundtrip via assignment')

        q = serialize('')
        out = 'x'
        out = q
        call check_equal(len(out), 0, 'string: empty string roundtrip')
    end subroutine

    !> Reading a parameter leaves it intact, so it can be read again.
    subroutine test_read_does_not_consume()
        type(Parameter):: p
        real(DP), dimension(2):: first, second

        p = serialize([1._DP, 2._DP])
        first = p
        second = p
        call check_equal(second, first, 'read: a second read yields the same value')
    end subroutine

    !> Copying a parameter copies its value: overwriting the source afterwards leaves the copy untouched.
    subroutine test_copy_is_deep()
        type(Parameter):: p, q
        real(DP), dimension(2,2):: in = reshape([1._DP, 2._DP, 3._DP, 4._DP], [2, 2])
        real(DP), dimension(2,2):: out

        p = in
        q = p
        p = serialize(0._DP)
        out = q
        call check_equal(out, in, 'copy: survives overwriting the source')
    end subroutine

    !> IFX as of 2026.1.1.19 duplicated an element in an array constructor of Value-based serialize() results. Guards against a
    !> regression now that the storage is different.
    subroutine test_array_constructor()
        type(Parameter), dimension(:), allocatable:: params
        integer:: a
        integer, dimension(2):: b
        real(DP):: c
        character(:), allocatable:: d

        params = [serialize(1), serialize([2, 3]), serialize(4._DP), serialize('five')]
        call check_equal(size(params), 4, 'array constructor: size')
        a = params(1)
        b = params(2)
        c = params(3)
        d = params(4)
        call check_equal(a, 1, 'array constructor: integer')
        call check_equal(b, [2, 3], 'array constructor: int array')
        call check_equal(c, 4._DP, 'array constructor: real')
        call check_equal(d, 'five', 'array constructor: string')
    end subroutine

    !> Negative test. Expected to end the program via `error stop`; reaching the print means the check was skipped.
    subroutine test_shape_mismatch_aborts()
        type(Parameter):: p
        real(DP), dimension(3,3):: source = 0._DP
        real(DP), dimension(2,2):: too_small

        p = source
        too_small = p

        print '(A)', '  FAILED: real matrix: shape mismatch was not detected'
    end subroutine
end program
