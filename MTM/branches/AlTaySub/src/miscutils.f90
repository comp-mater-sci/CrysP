!> Container for miscellaneous utility routines.
module miscutils

contains

      !> This subroutine extracts the first word from str, fills
      !> the remaining part with spaces and removes all leading blanks.
      subroutine stripComment(str)
      implicit none
      character(len=*),intent(inout) :: str
      !
      integer :: iblank
      !
            str = adjustl(str)
            ! Scan for the first blank
            iblank = index(str,' ')
            if (iblank.GT.0) then
                 str(iblank:)=' '
            end if
      !
      end subroutine



end module