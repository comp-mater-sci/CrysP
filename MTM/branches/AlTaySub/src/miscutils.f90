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


      subroutine writeMSSHeader(ounit)
      implicit none
      integer,intent(in)      :: ounit
      !
            write(ounit,fmt=554) 
      554   format(T5,'Eps_vM',T21,'Eps_vM^Tot',T37,'Eps_HvM',T53,'Eps_HvM^Tot',T69,'Sigma_HvM', &
                   T90,'Sigma_11',T106,'Sigma_22',T122,'Sigma_33',T138,'Sigma_23',T154,'Sigma_31',T170,'Sigma_12')
      !
      end subroutine
end module