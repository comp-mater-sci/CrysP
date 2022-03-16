!
! $Id: criLibCRIVersion.f90 1980 2014-08-17 10:43:23Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2012-06-11
!>    $Revision: 1980 $
!>    $Date: 2014-08-17 12:43:23 +0200 (Sun, 17 Aug 2014) $
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
      
      integer,parameter :: criLibCRIVersion_SubMinor = 26

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
