!
! $Id: criConfigReader.f90 1933 2014-07-11 14:04:49Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of the initial release: 2014-04-07
!>    $Revision: 1933 $
!>    $Date: 2014-07-11 16:04:49 +0200 (Fri, 11 Jul 2014) $
!>
!>    History of modifications: (see svn log)
!>
!
#include "criStdDefs.fpp"
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
      implicit none
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

      !> Read logical value
      logical function readFlag(cnfunit)
      implicit none
      integer,intent(in) :: cnfunit
      !
            readFlag = .false.
            if (.not. readValue(cnfunit,readFlag,'(L)')) readFlag = .false.
      !
      end function

end module
