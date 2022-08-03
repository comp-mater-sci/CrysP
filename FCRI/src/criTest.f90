!
!> Simplistic stand-alone unit testing module
!>
!> This module, together with macros in criTest.fpp, implements
!> a very basic unit testing framework. While it neither offers
!> much features of xUnit, nor follows its architecture, it remains
!> lightweight and easy to use. No external dependencies are involved.
module criTest
use criErrcodes
implicit none

      private

      !> Collected data from testing tracking
      type :: TestTrack
            integer :: n_failed = 0 !< Counter of failed tests
            integer :: n_successful = 0 !< Counter of successful tests
      end type


      type(TestTrack),save :: test_track
      integer,save :: iounit = 6

      public :: testInit, testSummary, testReport

contains


      !> Initialize the module
      subroutine testInit(outunit)
      !> IO unit where the output will be printed to
      integer,intent(in),optional :: outunit
      !
            if (present(outunit)) iounit = outunit
            test_track = TestTrack()
      !
      end subroutine


      !> Print summary of the test
      subroutine testSummary()
      !
            write(iounit, fmt=100)
            write(iounit, fmt=200) test_track%n_failed + test_track%n_successful
            write(iounit, fmt=201) test_track%n_successful
            write(iounit, fmt=202) test_track%n_failed
            write(iounit, fmt=100)
            !
            100 format(40('='))
            200 format('Number of tests executed:', T30, I0)
            201 format('Successful:', T30, I0)
            202 format('Failed:', T30, I0)
      !
      end subroutine


      !> Report the outcome of an individual unit test.
      subroutine testReport(name, outcome, line, file)
      character(len=*),intent(in)               :: name !< Name of the test
      logical,intent(in)                        :: outcome !< Result: .true. for success
      integer,intent(in)                        :: line !< Line in the source file
      character(len=*),intent(in)               :: file !< Source file
      !
            if (outcome) then
                  write(iounit, fmt=201) trim(name)
                  test_track%n_successful = test_track%n_successful + 1
            else
                  write(iounit,fmt=202) trim(name), line, trim(file)
                  test_track%n_failed = test_track%n_failed + 1
            endif
            !
            201 format('Test ',A,': PASS')
            202 format('Test ',A,': FAILED in line ', I0 ' of ', A)
      !
      end subroutine

end module
