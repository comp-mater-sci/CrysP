!> Provides access to config files with bash-style comments
module criUncomment
    use base_defs

    implicit none
    private

      !> Maximal length of a line
      integer, parameter, public       :: max_line_len = 512

      character, parameter     :: comment_sign = '#'

      !> Read value from iounit and strip comments
      !> Arguments:
      !> \param[in] inunit The IO unit (type: integer)
      !> \param[out] val   The value being retrieved (type: one of the supported types
      !>                   (integer, logical, string, real(DP)) OR a vector of
      !>                   elements of supported types)
      !> \param[in] frmt  The format to be used in the read operation (type: character(len=*), optional)
      interface readValue
        module procedure read_scalar, read_array, read_tensor
      end interface

      public:: readValue
contains
      logical function skipComment(nunit, buffer)
      integer, intent(in)            :: nunit
      character(512), intent(out)  :: buffer
      !
      integer  :: ios, hashidx
      logical  :: next
      !

            next = .true.
            do while (next)
                  read(nunit, fmt = 500, iostat = ios) buffer
                  if (ios /= 0) then
                        skipComment = .false.
                        next = .false.
                  endif
                  if (.not. isComment(trim(adjustl(buffer))) ) then
                        skipComment = .true.
                        next = .false.
                        ! sanitize output by removing '#'
                        hashidx = index(buffer, comment_sign)
                        if (hashidx /= 0) buffer(hashidx:) = ' '
                  endif
            enddo
            500 format(A512)
      !
      contains

      logical function isComment(buffer)
      character(len=*), intent(in)  :: buffer
      !
            isComment = .false.
            if (len(buffer) > 0) then
                  if (buffer(1:1) == comment_sign) isComment = .true.
            endif
      !
      end function
    end function

    logical function read_scalar(inunit, val) result(isOK)
        integer, intent(in):: inunit
        class(*), intent(out):: val
        character(max_line_len)   :: buffer
        integer:: ierr

        isOK = .false.
        if (skipComment(inunit, buffer)) then
            select type(val)
                type is (integer)
                    read(buffer, fmt=*,iostat = ierr) val
                type is (logical)
                    read(buffer, fmt=*,iostat = ierr) val
                type is (character(*))
                    read(buffer, fmt=*,iostat = ierr) val
                type is (real(DP))
                    read(buffer, fmt=*,iostat = ierr) val
            end select
        endif
        if (ierr == 0) isOK = .true.
    end function

    logical function read_array(inunit, val) result(isOK)
        integer, intent(in):: inunit
        real(DP), dimension(:), intent(out):: val
        character(max_line_len)   :: buffer
        integer:: ierr

        isOK = .false.
        if (skipComment(inunit, buffer)) read(buffer, fmt=*,iostat = ierr) val
        if (ierr == 0) isOK = .true.
    end function
    logical function read_tensor(inunit, val) result(isOK)
        integer, intent(in):: inunit
        real(DP), dimension(3,3), intent(out):: val
        character(max_line_len)   :: buffer
        integer:: ierr

        if (skipComment(inunit, buffer)) read(buffer, fmt=*,iostat = ierr) val
        isOK = ierr == 0
    end function

end module


