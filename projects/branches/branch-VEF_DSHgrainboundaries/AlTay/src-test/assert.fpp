!
! $Id$
!

! NDEBUG macro disables the assertions 
#ifndef NDEBUG

#ifndef ASSERT
#define ASSERT(i) if(.not. (i))then;write(*,'(A,A,1X,A,1X,I0)') 'Assertion failed: ',__FILE__,'line',__LINE__;stop;endif
#endif

#else

#ifndef ASSERT
#define ASSERT(i) continue;
#endif

#endif
