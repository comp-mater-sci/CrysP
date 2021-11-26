!
! $Id$
!
!> \file criMacros.fpp  Collection of useful generic preprocessor macros
!
#ifndef criMacros_88CBCFDF_F697_4929_A18A_A346111C60C3
#define criMacros_88CBCFDF_F697_4929_A18A_A346111C60C3

!> Assign either `TVAL` or `FVAL` to output variable `VAR`
!> depending on logical condition `X`.
!>
!> The macro `CHOOSE` is modelled after C/C++ ?: operator.
!> CHOOSE provides lazy evaluation of either TVAL or FVAL parameter,
!> based on a logical test on the parameter X. This lazy evaluation
!> is semantically equivalent to C/C++ ?: operator or R ifelse function.
!> The main technical difference between `CHOOSE` and the intrinsic function 
!> `merge` is that in `CHOOSE` there is no function call involved. Therefore,
!> there is no need to evaluate *both* TVAL and FVAL expressions to get the
!> result, as it is the case in `merge`
#define CHOOSE(VAR,X,TVAL,FVAL) if(X)then;VAR=TVAL;else;VAR=FVAL;endif

!> Execute STATEMENT and return if CONDITION is met.
!>
!> Typical use: testing post-conditions.
#define RETURN_IF(CONDITON,STATEMENT) STATEMENT;if(CONDITON)return;

!> Provided CONDITON is met, execute STATEMENT and return.
!>
!> Typical use: testing pre-conditions and setting exit code in STATEMENT
#define RETURN_IF_WITH(CONDITON,STATEMENT) if(CONDITON)then;STATEMENT;return;endif

!> Execute ACTION and check CONDITION. If CONDITON is met, execute STATEMENT and return.
!>
!> Typical use: testing post-conditions of action
#define RETURN_ON_WITH(ACTION,CONDITION,STATEMENT) ACTION;if(CONDITION)then;STATEMENT;return;endif

!> Cast pointer _P into type _T . If the type dynamically conforms, set the
!> result in _CP, otherwise nullify _CP.
#define DYNAMIC_CAST(_T,_P,_CP) select type(_P);class is(_T);_CP=>_P;class default;nullify(_CP);endselect

!> Set V to the size of allocated array X, or 0 if it is either not allocated 
!> or its size is zero.
!>
!> Rationale: size() cannot be directly called without a check if its argument
!> is allocated (see ] ISO/IEC JTC 1/SC 22/WG 5/N1830, 13.7.156. 
#define ALLOCATED_SIZE(V,X) if(allocated(X))then;V=size(X);else;V=0;endif

#endif
