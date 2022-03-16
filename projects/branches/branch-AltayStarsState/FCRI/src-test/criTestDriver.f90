!
! $Id: criTestDriver.f90 2824 2017-02-15 10:27:24Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2010-08-05
!>    $Revision: 2824 $
!>    $Date: 2017-02-15 11:27:24 +0100 (Wed, 15 Feb 2017) $
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
use criTestMathUtils
implicit none

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
      
      l = criTestMathUtils_main()
      
      call testSummary()

end program


