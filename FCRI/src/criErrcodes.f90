#include "criStdDefs.fpp"
!
!> Definition of several symbolic error codes and its corresponding numerical values.
!>
!> The user is strongly advised against any direct usage of numerical error codes.
!> Instead of this, one should use the symbolic names.
!> \todo Extend the list of specific error conditions
!> \todo Append iso_binding enum that represents the same error codes
!> \todo Define string-output subroutine to emmit messages
!> \todo Define IO output subroutine to emmit messages
module criErrcodes
implicit none
      !>@{ \name General error codes

      !> Success
      integer,parameter :: criSuccess = 0

      !> General error
      integer,parameter :: criError   = -1

      !> General failure
      integer,parameter :: criFailure = 1

      !>@}

      !>@{ \name Specific error condition codes

      integer,parameter :: criErr_NumNaN     = -10    !< Error that led to NaN or Inf

      integer,parameter :: criErr_BadArgs    = -20    !< Inconsistent of incorrect procedure arguments
      integer,parameter :: criErr_BadDims    = -21    !< Dimension mismatch between parameters
      integer,parameter :: criErr_NullPtr    = -22    !< Null pointer would be referenced
      integer,parameter :: criErr_NonAlloc   = -23    !< Non-allocated allocatable would be referenced
      integer,parameter :: criErr_BadValue   = -24    !< Incorrect value is encountered
      integer,parameter :: criErr_BadIndex   = -25    !< Subscript is out of the allowed range

      integer,parameter :: criErr_Mem        = -30    !< Error on memory allocation/delallocation attempt
      integer,parameter :: criErr_MemAlloc   = -31    !< Error on memory allocation attempt
      integer,parameter :: criErr_MemDealloc = -32    !< Error on memory delallocation attempt

      integer,parameter :: criErr_IO         = -40    !< Input/Output general error
      integer,parameter :: criErr_IOOpen     = -41    !< Unsuccessful I/O open operation
      integer,parameter :: criErr_IORead     = -42    !< Unsuccessful I/O read operation
      integer,parameter :: criErr_IOWrite    = -43    !< Unsuccessful I/O write operation
      integer,parameter :: criErr_IOFrmt     = -44    !< Unsuccessful I/O format operation

      integer,parameter :: criErr_BadType    = -60    !< Incorrect type
      integer,parameter :: criErr_BadCast    = -61    !< Cannot statically cast one type into another
      integer,parameter :: criErr_DynCast    = -62    !< Cannot dynamically cast  one type into another
      integer,parameter :: criErr_TypeSel    = -63    !< No suitable match in "select type" construct.

      integer,parameter :: criErr_NotImplemented = -90 !< Feature not implemented
      !>@}

contains


      !> Returns .true. if errcode represents an error condition, otherwise .false.
      elemental logical function is_error(errcode)
      integer,value,intent(in)    :: errcode
      !
            is_error = (errcode /= criSuccess) .and. (errcode /= criFailure)
      !
      end function

end module

