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
!>    \date Date of first release: 2012-06-11
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criLibCRIVersion.f90 
!
#include "criStdDefs.fpp"
!
!> Version information about criLibCRI
module criLibCRIVersion
use criVersion
implicit none

      integer,parameter :: criLibCRIVersion_Major    = 0 
      
      integer,parameter :: criLibCRIVersion_Minor    = 1
      
      integer,parameter :: criLibCRIVersion_SubMinor = 21

contains

      character(len=criVersion_max_string_len) function criLibCRIVersionString()
      implicit none
      !
            criLibCRIVersionString =  versionString('criLibCRI',  criLibCRIVersion_Major,       &
                                                                  criLibCRIVersion_Minor,       &
                                                                  criLibCRIVersion_SubMinor)
      !
      end function

end module
