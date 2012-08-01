!
! $Id$
!
!> RCM: Runtime Control Module preprocessor definitions
!> The module contains FPP-based components og the RCM.

! The macros are defined only in the instrumented mode.
#ifdef RCM_ENABLED


!> Macro for raising exceptions.
#ifndef RCM_RAISE
! c - exit code; f- function name, m - message; a - action
#define RCM_RAISE(c,f,m,a) call RCM_throw(c,f,m,a); return;
#endif

!> Macro for checking the state of the exception stack, but without 
!> any attempt to handle the exception.
!>
!> The RCM_GUARD should be placed directly after a call to a subroutine/function
!> that may raise an exception.
#ifndef RCM_GUARD
#define RCM_GUARD if(RCM_signal())return
#endif

!> Macro for intercepting all the exceptions on the stack.
!> The stack is cleaned.
#ifndef RCM_CATCHALL
#define RCM_CATCHALL call RCM_cleanup();
#endif

!> Macro for trivial handling of the exceptions.
!> In essence, it intercepts all the exceptions and returns control to the caller
!> of the present subroutine.
#ifndef RCM_HANDLE(i)
#define RCM_HANDLE(i) if(RCM_signal())then;i=RCM_topError();call RCM_clean();return;endif;
#endif


! End of: "RCM_ENABLED" is set
#else
! "RCM_ENABLED" is not set

! Simply define dummy macros.

#ifndef RCM_RAISE
#define RCM_RAISE(c,m,a) continue
#endif

#ifndef RCM_GUARD
#define RCM_GUARD continue
#endif

#ifndef RCM_CATCHALL
#define RCM_CATCHALL continue
#endif

#ifndef RCM_HANDLE(i)
#define RCM_HANDLE(i) continue
#endif

! End of: "RCM_ENABLED" is not set
#endif

