!> Various utility subroutines and functions
module dmcUtils
use,intrinsic :: iso_fortran_env, only: output_unit
implicit none

      integer,parameter       :: display_unit = output_unit

      character,parameter     :: default_comment_sign = '#'

contains

      double precision pure function average(a)
      double precision,dimension(:),intent(in) :: a
      integer :: n
      !
            n = size(a)
            if (n >= 1) average = sum(a) / dble(n)
            ! Undefined for empty array
      end function

end module
