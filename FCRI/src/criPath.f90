!
!> Provides named constants and procedures for dealing with filesystem paths.
module criPath
implicit none

#if defined(_WIN32) || defined(_WIN64)
      !> Path separator
      character,parameter     :: pathsep = '\'
#else
      !> Path separator
      character,parameter     :: pathsep = '/'
#endif

      !> Maximal length of path acceptable by the filesystem
      integer,parameter       :: max_pathlen = 2048

contains

      !> Return basename of the pathname `path`, which is the name
      !> of the file or directory `path` with any leading directory
      !> components removed.
      !>
      !> The function is modelled after Unix command `basename` and
      !> Python os.path.basename()
      pure function basename(path)
      character(len=*),intent(in)   :: path
      character(len=len(path))       :: basename
      !
      integer :: lb,l,u
      !
            lb = len_trim(path)
            if (lb > 0) then
                  l = 1
                  u = index(path, pathsep, back=.true.)
                  ! (u == 0)  : basename is the entire path
                  ! (u == lb) : dir, search for another dirsep if u > 1; otherwise return root path
                  ! (u > 0) && (u < lb): there is a path separator inside the path
                  if (u > 0) then
                        if (u < lb) then
                              l = u + 1
                              u = lb
                        else
                              ! (u == lb)
                              if (u > 1) then
                                    u = u - 1
                                    l = index(path(1:u), pathsep, back=.true.)! pathsep at the end of path
                                    l = merge(1,l+1,l == 0)
                              endif
                        endif
                  else
                        u = lb
                  endif
                  ! finally: shift to the left
                  basename = adjustl(path(l:u))
            else
                  basename = ''
            endif
      !
      end function

      !> Returns the path without file exension (if there is any)
      pure function stripExt(path)
      character(len=*),intent(in)   :: path
      character(len=len(path))      :: stripExt
      !
      character(len=len(path))   :: ext ! temporary
      !
            call splitExt(path, stripExt, ext)
      !
      end function

      !> Split the pathname path into a pair (root, ext).
      !>
      !> Ext is empty or begins with a period and contains at most one period.
      !> Leading periods on the basename are ignored.
      !> Semantically it should hold that (root // ext) == path, but the effect of
      !> leading and trailing blanks must be also considered. It is safer to assume
      !> that:
      !> adjustl(trim(root)) // adjustl(trim(ext)) == adjustl(path)
      !> The procedure removes the leading blanks from root and ext, so:
      !> len_trim(root) // len_trim(ext) == adjustl(path)
      pure subroutine splitExt(path, root, ext)
      character(len=*),intent(in)   :: path
      character(len=*),intent(out)  :: root
      character(len=*),intent(out)  :: ext
      !
      integer :: lb,l,u,ups
      character,parameter :: dot = '.'
      !
            lb = len_trim(path)
            root = ''
            ext = ''
            if (lb > 0) then
                  l = 1
                  u = index(path, dot, back=.true.)
                  ! (u == 0)  : entire path
                  ! (u > 0): there is an extension separator
                  if (u > 0) then
                        ! Consider extension separator inside the path - it does not count as an extension separator.
                        ! Consider file name that starts with dot (dot after pathsep or at beginning of unrooted path)
                        ups = index(path, pathsep, back=.true.) ! Right-most pathsep or zero for unrooted path
                        if (ups + 1 >= u) then ! Dot appears inside the path, skipping
                              u = lb
                        else
                              u = u - 1
                        endif
                  else
                        u = lb
                  endif
                  if (l <= u) then
                        ! finally: shift to the left
                        root = adjustl(path(l:u))
                        ups = u + 1 ! Supposed position of the dot
                        if (ups <= lb) ext = adjustl(path(ups:lb))

                  endif
            endif

      !
      end subroutine


      !> Join two path components, inserting directory separator
      !> as needed.
      !>
      !> Notable special case:
      !> - 2nd path begins with root path, e.g.:
      !>   pathjoin('path1','/path2') returns '/path2'
      pure function pathjoin(path_a, path_b) result(path)
      character(len=*),intent(in)               :: path_a, path_b
      character(len=len(path_a)+len(path_b))    :: path
      !
      logical :: a_sep, b_sep
      integer :: a_end, b_start
      character :: sep
      !
            sep = ''
            a_sep = .false. ! does path_a _end_ with pathsep?
            b_sep = .false. ! does path_b _begin_ with pathsep?
            ! Get the index of first non-blank character in path_b
            b_start = verify(path_b, ' ')
            if (b_start > 0) b_sep = path_b(b_start:b_start) == pathsep
            if (b_sep) then
                  ! path_b begins with pathsep, so it is an absolute
                  ! path starting at root. We neglect path_a in this case.
                  path = trim(adjustl(path_b))
                  return
            endif
            ! ... and the last non-blank in path_a
            a_end = verify(path_a, ' ', back=.true.)
            if (a_end > 0) a_sep = path_a(a_end:a_end) == pathsep
            ! Add separator if the first path has no separator
            if ((a_end > 0) .and. .not. a_sep) sep = pathsep
            ! We have to trim sep to get empty string for '', otherwise it is
            ! a one-character string.
            path = trim(adjustl(path_a)) // trim(sep) // trim(adjustl(path_b))
      !
      end function


end module

