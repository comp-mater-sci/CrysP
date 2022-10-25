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
use criTestNumerics
use criTestMathUtils
implicit none

logical  :: l

      call testInit()

      l = test_replaceAll()

      l = criTestPath_main()

      l = criTestRange_main()

      l = criTestUncomment_main()

      l = criTestAlgorithm_main()

      l = criTestNumerics_main()

      l = criTestMathUtils_main()

      call testSummary()

end program


