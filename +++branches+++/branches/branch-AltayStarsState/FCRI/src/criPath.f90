!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2012-11-02
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criPath.f90 
!
#include "criStdDefs.fpp"
!
!> Provides named constants and procedures for dealing with filesystem paths.
module criPath
use criAssert
implicit none

#if defined(_WIN32) || defined(_WIN64)
      !> Path separator
      character,parameter     :: pathsep = '\'
#else
      !> Path separator
      character,parameter     :: pathsep = '/'
#endif

      !> Maximal length of path acceptable by the filesystem
      integer,parameter       :: max_pathlen = 512

contains

            
      function basename(path)
      use ifport
      implicit none
      character(len=*),intent(in)   :: path
      character(len=len(path))       :: basename
      !
      integer :: lb,l,u
      !
            lb = lnblnk(path)
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
                  ASSERT(l <= u)
                  ! finally: shift to the left
                  basename = adjustl(path(l:u))
            else
                  basename = ''
            endif
      !
      end function
      
      !> Returns the path without file exension (if there is any)
      function stripExt(path)
      use ifport
      implicit none
      character(len=*),intent(in)   :: path
      character(len=len(path))      :: stripExt
      !
      integer :: lb,l,u,ups
      character,parameter :: dot = '.'
      !
            lb = lnblnk(path)
            stripExt = ''
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
                        stripExt = adjustl(path(l:u))
                  endif
            endif
      !
      end function      
      
      
      !> Construct a filename by stitching together prefix and suffix.
      !>
      !> The leading whitespaces in the suffix are not preserved in the resulting string.
      pure function  mkfilename(prefix,suffix)
      implicit none
      character(len=*),intent(in)               :: prefix, suffix
      character(len=len(prefix)+len(suffix))    :: mkfilename
      !
            mkfilename = trim(adjustl(prefix))//trim(adjustl(suffix))
      !
      end function
      
end module
      
