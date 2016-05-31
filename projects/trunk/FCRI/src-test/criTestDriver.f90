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
!>    \date Date of first release: 2010-08-05
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criTest.f90 
!
!
#include "criStdDefs.fpp"
#include "criTest.fpp"
!
#ifdef WINDOWS
#define DIRSEP '\'
#else
#define DIRSEP '/'
#endif

program criTestDriver
use criTest
use criTestAlgorithm
use criTestRange
use criTestPath
use criTestUncomment
use criTestIterUtils
use criTestNumerics
use criTestExpandableVector
implicit none

integer  :: info
logical  :: l

      call testInit()

      l = test_replaceAll()
      
      l = criTestPath_main()
      
      l = criTestRange_main()
      
      l = criTestUncomment_main()
      
      l = criTestAlgorithm_main()
      
      l = criTestIterUtils_main()

      l = criTestNumerics_main()

      l = criTestExpandableVector_main()
      
      call testSummary()

end program


