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
!>    \date Date of first release: 2012-04-11
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criAssert.f90 
!
#include "criStdDefs.fpp"
!
!> Run-time functions for checking assertions.
!>
!> Counterpart preprocessor definitions are defined in criStdDefs.fpp
module criAssert

contains

      subroutine assert_sub(line,file,terminate,message)
      integer,intent(in)                        :: line
      character(len=*),intent(in)               :: file
      logical,intent(in),optional               :: terminate
      character(len=*),intent(in),optional      :: message
      !
      logical :: do_stop
      !            
            write(*,fmt=100) trim(file), line
            if (present(message)) write(*,fmt=101) trim(message)
             !
            do_stop = .true.
            if(present(terminate)) do_stop = terminate
            !
            if (do_stop) then
                  write(*,fmt=200) 
                  stop
            endif
      !
            100 format('ASSERTION FAILED:',1X,A,' line:',1X,I0)
            101 format('MESSAGE:',1X,A)
            200 format('EXECUTION TERMINATED')
      !
      end subroutine

end module