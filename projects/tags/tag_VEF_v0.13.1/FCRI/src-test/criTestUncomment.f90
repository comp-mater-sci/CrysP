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
!>    \date Date of the initial release: 2013-07-09
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criTestUncomment.f90 

#include "criTest.fpp"

module criTestUncomment
use criUncomment
use criTest
implicit none
private

public criTestUncomment_main

contains
      
      !> main driver routine of criTestUncomment
      logical function criTestUncomment_main() result(stat)
      implicit none
      
            stat = test_uncommentedString() 

            stat = test_stripComment() 

      end function


      
      logical function test_uncommentedString()
      implicit none
      character(len=max_line_len)    :: string, res_string
      !
            test_uncommentedString = .false.
            ! Test:
            string = ''
            _TEST('empty string',uncommentedString(string) == '')

            ! Test:
            _TEST('empty literal string',uncommentedString('') == '')
            
            ! Test:
            string = '   '
            _TEST('empty string, spaces',uncommentedString(string) == string)

            ! Test:
            _TEST('blank string, comment at the end',uncommentedString('   #') == '   ')

            ! Test:
            string = '#     '
            _TEST('blank string, comment at the beginning',uncommentedString(string) == '')

            ! Test:
            string = 'sample string with no comment'
            _TEST('sample string, no comment',uncommentedString(string) == string)

            ! Test:
            string = 'sample string with a trailing comment#'
            res_string = 'sample string with a trailing comment'
            _TEST('sample string, no comment',uncommentedString(string) == res_string)

            ! Test:
            string = 'sample string # with a comment'
            res_string = 'sample string '
            _TEST('sample string with a comment',uncommentedString(string) == res_string)

            ! Test:
            string = '#sample comment string with a comment char#'
            res_string = ''
            _TEST('sample comment string with a comment',uncommentedString(string) == res_string)

            
      !
      end function

      
      logical function test_stripComment()
      implicit none
      character(len=max_line_len)    :: string, res_string
      !
            test_stripComment = .false.
            ! Test:
            string = ''
            res_string = ''
            call stripComment(string)
            _TEST('empty string', string == res_string)

            ! Test: We cannot do this test: literal cannot have intent inout
            ! res_string = ''
            ! call stripComment('')
            ! _TEST('empty literal string', string == res_string)
            
            ! Test:
            string = '   '
            res_string = string
            call stripComment(string)
            _TEST('empty string, spaces', string == res_string)

            ! Test:
            string = '   #'
            res_string = '   '
            call stripComment(string)
            _TEST('blank string, comment at the end', string == res_string)

            ! Test:
            string = '#     '
            res_string = ''
            call stripComment(string)
            _TEST('blank string, comment at the beginning',string == res_string)

            ! Test:
            string = 'sample string with no comment'
            res_string = string
            call stripComment(string)
            _TEST('sample string, no comment', string == res_string)

            ! Test:
            string = 'sample string with a trailing comment#'
            res_string = 'sample string with a trailing comment'
            call stripComment(string)
            _TEST('sample string, no comment', string == res_string)

            ! Test:
            string = 'sample string # with a comment'
            res_string = 'sample string '
            call stripComment(string)
            _TEST('sample string with a comment', string == res_string)

            ! Test:
            string = '#sample comment string with a comment char#'
            res_string = ''
            call stripComment(string)
            _TEST('sample comment string with a comment', string == res_string)
      !
      end function

      
end module 