!
!> Various algorithms
module criAlgorithm
implicit none

      !> tostring function converts from an intrisic data type into character string.
      interface tostring
            module procedure tostring_int, tostring_real, tostring_double
      end interface

      !> centered function centers a character string.
      interface centered
            module procedure  centered_int, centered_string
      end interface

      !> Find the first element in array not before val, using operator <
      interface lower_bound
            module procedure lower_bound_int, lower_bound_double
      end interface


      !> Test the presence of optional value, and return a default if the optional
      !> is not present.
      !>
      !> The function provides a simplified access pattern to optional parameters.
      !> The first formal argument is declared as optional parameter, butit must always
      !> appear as the actual parameter in a context where the actual parameter is
      !> declared itself as "optional".
      interface optionalDefault
            module procedure optionalDefault_logical, optionalDefault_integer
      end interface

contains
      ! Instantiate parametrized functions
      !
#define _TYPE_NAME integer
#define _LOWER_BOUND_FX_NAME lower_bound_int
#include "criAlgorithmTemplates.fpp"


#define _TYPE_NAME double precision
#define _LOWER_BOUND_FX_NAME lower_bound_double
#include "criAlgorithmTemplates.fpp"

      !> Check if the string value val is present in the list of strings.
      logical function isPresent(val, list,index)
      character(len=*),intent(in)               :: val      !< Value to be looked up
      character(len=*),dimension(:),intent(in)  :: list     !< Array of strings
      !> Index of the element that was found. It is set only if isPresent returns .true.
      integer,intent(out),optional              :: index
      !
      integer :: i
            isPresent = .false.
            do i = lbound(list,dim=1), ubound(list,dim=1)
                  if (val == list(i)) then
                        isPresent = .true.
                        exit
                  endif
            enddo
            if (present(index) .and. isPresent) index = i
      !
      end function


      !> Returns a string that stems from "str", but all instances of the character "from"
      !> are replaced by the character "to".
      function replaceAll(str,from,to) result(ustr)
      character(len=*),intent(in)         :: str
      character,intent(in)                :: from
      character,intent(in)                :: to
      character(len=len(str))             :: ustr
      integer :: i,n
      !
            ustr = str
            n = len(ustr)
            if (n > 0) then
                  do i = 1, n
                        if (str(i:i) == from) ustr(i:i) = to
                  enddo
            endif
      end function




      pure function tostring_int(val,strlen,fmt) result(str)
      integer,intent(in)                        :: val
      integer,intent(in)                        :: strlen
      character(len=*),intent(in),optional      :: fmt
      character(len=strlen)                     :: str
      !
      integer :: ierr
      !
            str = ''
            if (present(fmt)) then
                  write(str,fmt,iostat=ierr) val
            else
                  write(str,'(I0)',iostat=ierr) val
            endif
      !
      end function

      pure function tostring_real(val,strlen,fmt) result(str)
      real,intent(in)                           :: val
      integer,intent(in)                        :: strlen
      character(len=*),intent(in)               :: fmt
      character(len=strlen)                     :: str
      !
      integer :: ierr
      !
            str = ''
            write(str,fmt,iostat=ierr) val
      !
      end function

      pure function tostring_double(val,strlen,fmt) result(str)
      double precision,intent(in)               :: val
      integer,intent(in)                        :: strlen
      character(len=*),intent(in)               :: fmt
      character(len=strlen)                     :: str
      !
      integer :: ierr
      !
            str = ''
            write(str,fmt,iostat=ierr) val
      !
      end function


      pure function centered_int(val,strlen,fmt) result(str)
      integer,intent(in)                        :: val
      integer,intent(in)                        :: strlen
      character(len=*),intent(in),optional      :: fmt
      character(len=strlen)                     :: str
      !
            str = centered_string(tostring_int(val,strlen,fmt))
      !
      end function


      pure function centered_string(val) result(str)
      character(len=*),intent(in)               :: val
      character(len=len(val))                   :: str
      !
      integer :: ns
      !
            str = ''
            ns = max(1,(len(val) - len_trim(adjustl(val))) / 2)
            str(ns:) = adjustl(val)
      !
      end function

#define OPTIONALDEFAULT_TEST_EXPRESSION if (present(value))then;res=value;else;res=default;endif

      !> Test the presence of optional logical value, and return a default if the optional
      !> is not present.
      pure logical function optionalDefault_logical(value, default) result(res)
      !> The parameter to be tested for presence. The actual parameter MUST have optional attribute.
      logical,intent(in),optional   :: value
      logical,intent(in)            :: default  !< Default value
      !
            OPTIONALDEFAULT_TEST_EXPRESSION
      !
      end function

      !> Test the presence of optional integer value, and return a default if the optional
      !> is not present.
      pure integer function optionalDefault_integer(value, default) result(res)
      !> The parameter to be tested for presence. The actual parameter MUST have optional attribute.
      integer,intent(in),optional   :: value
      integer,intent(in)            :: default  !< Default value
      !
            OPTIONALDEFAULT_TEST_EXPRESSION
      !
      end function

#undef OPTIONALDEFAULT_TEST_EXPRESSION

end module
