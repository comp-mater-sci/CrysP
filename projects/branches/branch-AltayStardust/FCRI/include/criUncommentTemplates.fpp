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
!>    \date Date of first release: 2012-03-27
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criUncommentTemplates.fpp 
!
!> Templates of read functions 
!> Parametrization by pre-processor macros:
!> *   TMPL_UNCOMMENT_FX  - function name (it typically includes type name)
!> *   TMPL_UNCOMMENT_TYPE - real type name

      function TMPL_UNCOMMENT_FX(inunit,val,frmt) result(isOK)
      implicit none
      integer,intent(in)                     :: inunit
      TMPL_UNCOMMENT_TYPE,intent(out)        :: val
      character(len=*),intent(in),optional   :: frmt
      logical                                :: isOK
      !
      character(len=max_line_len)   :: buffer
      integer :: ierr
      !
            isOK = .false.
            if (skipComment(inunit,buffer)) then
                  if (present(frmt)) then
                        read(buffer,fmt=frmt,iostat=ierr) val
                  else
                        read(buffer,fmt=*,iostat=ierr) val 
                  endif
                  if (ierr == 0) isOK = .true.
            endif
      end function

