!> Minimal assertion helpers for the libCrysP unit tests.
!>
!> Each test is a stand-alone program registered with CTest (see CMakeLists.txt in this directory). A test program calls
!> `check(...)` any number of times and ends with `call finish()`, which prints a summary and exits with a non-zero status if any
!> check failed. CTest interprets the exit status as pass/fail.
!>
!> Deliberately kept free of dependencies on libCrysP itself so that it can never be the cause of a test failure.
module testing
    use base_defs, only: DP

    implicit none

    private
    public:: check, &
             check_equal, &
             finish

    integer:: n_checks = 0
    integer:: n_failed = 0

    !> Assert that two values are equal. Reals are compared to within an optional absolute tolerance (default: exact).
    interface check_equal
        module procedure check_equal_int, &
                         check_equal_int_array, &
                         check_equal_real, &
                         check_equal_real_array, &
                         check_equal_real_matrix, &
                         check_equal_string
    end interface

contains

    !> Assert that a condition holds.
    subroutine check(condition, message)
        logical, intent(in):: condition
        character(*), intent(in):: message

        n_checks = n_checks + 1
        if (.not. condition) then
            n_failed = n_failed + 1
            print '(A,A)', '  FAILED: ', message
        end if
    end subroutine

    subroutine check_equal_int(actual, expected, message)
        integer, intent(in):: actual, expected
        character(*), intent(in):: message

        call check(actual == expected, message)
        if (actual /= expected) print '(A,I0,A,I0)', '          actual = ', actual, ', expected = ', expected
    end subroutine

    subroutine check_equal_int_array(actual, expected, message)
        integer, dimension(:), intent(in):: actual, expected
        character(*), intent(in):: message

        if (size(actual) /= size(expected)) then
            call check(.false., message)
            print '(A,I0,A,I0)', '          size(actual) = ', size(actual), ', size(expected) = ', size(expected)
        else
            call check(all(actual == expected), message)
        end if
    end subroutine

    subroutine check_equal_real(actual, expected, message, tolerance)
        real(DP), intent(in):: actual, expected
        character(*), intent(in):: message
        real(DP), intent(in), optional:: tolerance

        logical:: ok

        ok = abs(actual - expected) <= tol(tolerance)
        call check(ok, message)
        if (.not. ok) print '(A,ES23.15,A,ES23.15)', '          actual = ', actual, ', expected = ', expected
    end subroutine

    subroutine check_equal_real_array(actual, expected, message, tolerance)
        real(DP), dimension(:), intent(in):: actual, expected
        character(*), intent(in):: message
        real(DP), intent(in), optional:: tolerance

        if (size(actual) /= size(expected)) then
            call check(.false., message)
            print '(A,I0,A,I0)', '          size(actual) = ', size(actual), ', size(expected) = ', size(expected)
        else
            call check(all(abs(actual - expected) <= tol(tolerance)), message)
        end if
    end subroutine

    subroutine check_equal_real_matrix(actual, expected, message, tolerance)
        real(DP), dimension(:,:), intent(in):: actual, expected
        character(*), intent(in):: message
        real(DP), intent(in), optional:: tolerance

        if (any(shape(actual) /= shape(expected))) then
            call check(.false., message)
            print '(A,2I4,A,2I4)', '          shape(actual) = ', shape(actual), ', shape(expected) = ', shape(expected)
        else
            call check(all(abs(actual - expected) <= tol(tolerance)), message)
        end if
    end subroutine

    subroutine check_equal_string(actual, expected, message)
        character(*), intent(in):: actual, expected
        character(*), intent(in):: message

        call check(actual == expected, message)
        if (actual /= expected) print '(A,A,A,A,A)', '          actual = "', actual, '", expected = "', expected, '"'
    end subroutine

    pure function tol(tolerance) result(t)
        real(DP), intent(in), optional:: tolerance
        real(DP):: t

        t = 0._DP
        if (present(tolerance)) t = tolerance
    end function

    !> Print a summary and terminate with a non-zero exit status if any check failed.
    subroutine finish()
        print '(A,I0,A,I0,A)', 'Ran ', n_checks, ' checks, ', n_failed, ' failed.'
        if (n_failed > 0) error stop 1
    end subroutine
end module
