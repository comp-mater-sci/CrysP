!
!> Helper algorithms for processing config files
module criConfigReader
use criLinearMap
use criUncomment
implicit none

contains

      !> Read a keyword value and checks it against the map.
      !>
      !> \return
      !> If the keyword appears in the map, .true. is returned and the parameter value is set
      !> to the value associated to the keyword. Otherwise .false. is returned and value becomes undefined.
      logical function readKeyword(cnfunit,map,value) result(res)
      integer,intent(in)                        :: cnfunit
      type(MapItem),dimension(:),intent(in)     :: map
      integer,intent(out)                       :: value
      !
      character(len=max_line_len) :: buffer
      !
            value = 0
            res = .false.
            buffer = ''
            if (.not. readValue(cnfunit, buffer, '(A)')) return
            res = resolveName(map, buffer, value)
      !
      end function

end module
