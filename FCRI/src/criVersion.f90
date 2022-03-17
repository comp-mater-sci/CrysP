!
! $Id: criVersion.f90 1933 2014-07-11 14:04:49Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2011-11-05
!>    $Revision: 1933 $
!>    $Date: 2014-07-11 16:04:49 +0200 (Fri, 11 Jul 2014) $
!>
!>    History of modifications: (see svn log)
!>
!>    \file criVersion.f90
!
#include "criStdDefs.fpp"
!
!> Basic functions for displaying and manipulating version numbers.
module criVersion
      implicit none

      integer,parameter       :: criVersion_max_string_len = 32


contains

      function versionString(feature,major,minor,subminor,msg,rev)
      character(len=*),intent(in)               :: feature
      integer,intent(in)                        :: major,minor,subminor
      character(len=*),intent(in),optional      :: msg,rev
      character(len=criVersion_max_string_len)  :: versionString
      !
      character(len=criVersion_max_string_len) :: tmp
      !
            write(tmp,fmt=1) feature,major,minor,subminor
            versionString = trim(adjustl(tmp))
            if (present(msg)) versionString = trim(versionString) // ' ' // trim(adjustl(msg))
            if (present(rev)) versionString = trim(versionString) // ' ' // trim(adjustl(rev))
            !
            1 format(A12,1X,I2.2,'.',I3.3,'.',I3.3)
      !
      end function
end module
