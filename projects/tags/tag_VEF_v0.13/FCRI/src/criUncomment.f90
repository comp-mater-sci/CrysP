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
!>    \date Date of first release: 2010-11-13
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criUncomment.f90 
!
#include "criStdDefs.fpp"
!
!> Provides access to config files with bash-style comments
module criUncomment

      !> Maximal length of a line
      integer,parameter       :: max_line_len = 512

      character,parameter     :: comment_sign = '#'

      !> Read value from iounit and strip comments
      !> Arguments:
      !> \param[in] inunit The IO unit (type: integer)
      !> \param[out] val   The value being retrieved (type: one of the supported types 
      !>                   (integer, logical, string, double precision) OR a vector of 
      !>                   elements of supported types)
      !> \param[in] frmt  The format to be used in the read operation (type: character(len=*),optional)
      interface readValue
            
            module procedure read_integer,   read_vector_integer, &
                             read_logical,   read_vector_logical, &
                             read_string,    read_vector_string,  &
                             read_double,    read_vector_double 
      
      end interface

      integer,private         :: line_count = 0

contains
      !> \name Various procedures on the theme "removing comment from a string"      
      !>@{ 
      !> Removes a comment from the string.
      elemental subroutine stripComment(line,comment_mark)
      implicit none
      character(len=*),intent(inout)      :: line
      character,intent(in),optional       :: comment_mark
      !
      integer     :: idx
      character   :: comment_delim
      !
            if (len(line) == 0) return  ! nothing to do
            ! Set default comment sign, override if comment_mark is provided by user
            comment_delim = comment_sign 
            if (present(comment_mark)) comment_delim = comment_mark
            !
            idx = index(line,comment_delim)
            if (idx /= 0) line(idx:) = ' '
      end subroutine
      
      !> Returns the string with a comment removed. 
      elemental function uncommentedString(string,comment_mark) 
      character(len=*),intent(in)         :: string
      character,intent(in),optional       :: comment_mark
      character(len=len(string))          :: uncommentedString
      !
      integer     :: idx
      character   :: comment_delim
      !
            uncommentedString = ''
            if (len(string) == 0) return
            ! Set default comment sign, override if comment_mark is provided by user
            comment_delim = comment_sign 
            if (present(comment_mark)) comment_delim = comment_mark
            idx = index(string,comment_delim)
            if (idx /= 0) then
                  uncommentedString = string(:idx-1)
            else
                  uncommentedString = string
            endif
      !
      end function
      
      !>@}
      
      !> Functions for processing input files with comments
      !>@{
      
      integer function getLineCount()
      implicit none
      !
            getLineCount = line_count
      !
      end function

      subroutine initLineCount(initval)
      implicit none
      integer,intent(in),optional :: initval
      !
            if (present(initval)) then
                  line_count = initval
            else
                  line_count = 0
            endif
      !
      end subroutine      
      

      logical function isComment(buffer)
      implicit none
      character(len=*),intent(in)  :: buffer
      !
            isComment = .false.
            if (len(buffer) > 0) then
                  if (buffer(1:1) == comment_sign) isComment = .true.
            endif
      !
      end function

      logical function skipComment(nunit,buffer)
      implicit none
      integer,intent(in)            :: nunit
      character(len=*),intent(out)  :: buffer
      !
      integer  :: ios, hashidx
      logical  :: next
      !
            next = .true.
            do while (next)
                  read(nunit,fmt=500,iostat=ios)  buffer
                  if (ios /= 0) then
                        skipComment = .false.
                        next = .false.
                  endif
                  line_count = line_count + 1
                  if (.not. isComment(trim(adjustl(buffer))) ) then
                        skipComment = .true.
                        next = .false.
                        ! sanitize output by removing '#'
                        hashidx = index(buffer,comment_sign)
                        if (hashidx /= 0) buffer(hashidx:) = ' '
                  endif                  
            enddo
            500 format(A512)
      !
      end function
!
! Instantization of template for integer
!      
#define TMPL_UNCOMMENT_FX read_integer
#define TMPL_UNCOMMENT_TYPE integer
#include "criUncommentTemplates.fpp"
#undef TMPL_UNCOMMENT_FX
#undef TMPL_UNCOMMENT_TYPE
!
#define TMPL_UNCOMMENT_FX read_vector_integer
#define TMPL_UNCOMMENT_TYPE integer,dimension(:)
#include "criUncommentTemplates.fpp"
#undef TMPL_UNCOMMENT_FX
#undef TMPL_UNCOMMENT_TYPE
!
! Instantization of template for logical
!      
#define TMPL_UNCOMMENT_FX read_logical
#define TMPL_UNCOMMENT_TYPE logical
#include "criUncommentTemplates.fpp"
#undef TMPL_UNCOMMENT_FX
#undef TMPL_UNCOMMENT_TYPE
!
#define TMPL_UNCOMMENT_FX read_vector_logical
#define TMPL_UNCOMMENT_TYPE logical,dimension(:)
#include "criUncommentTemplates.fpp"
#undef TMPL_UNCOMMENT_FX
#undef TMPL_UNCOMMENT_TYPE
!
! Instantization of template for string
!      
#define TMPL_UNCOMMENT_FX read_string
#define TMPL_UNCOMMENT_TYPE character(len=*)
#include "criUncommentTemplates.fpp"
#undef TMPL_UNCOMMENT_FX
#undef TMPL_UNCOMMENT_TYPE
!
#define TMPL_UNCOMMENT_FX read_vector_string
#define TMPL_UNCOMMENT_TYPE character(len=*),dimension(:)
#include "criUncommentTemplates.fpp"
#undef TMPL_UNCOMMENT_FX
#undef TMPL_UNCOMMENT_TYPE
!
! Instantization of template for double
!      
#define TMPL_UNCOMMENT_FX read_double
#define TMPL_UNCOMMENT_TYPE double precision
#include "criUncommentTemplates.fpp"
#undef TMPL_UNCOMMENT_FX
#undef TMPL_UNCOMMENT_TYPE
!
#define TMPL_UNCOMMENT_FX read_vector_double
#define TMPL_UNCOMMENT_TYPE double precision,dimension(:)
#include "criUncommentTemplates.fpp"
#undef TMPL_UNCOMMENT_FX
#undef TMPL_UNCOMMENT_TYPE

      !>@{

end module
