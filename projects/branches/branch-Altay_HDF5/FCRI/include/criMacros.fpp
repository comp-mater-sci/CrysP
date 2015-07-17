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

#define RETURN_IF(CONDITON, STATEMENT) STATEMENT; if (CONDITON) return;

#define RETURN_IF_WITH(CONDITON, STATEMENT) if(CONDITON)then;STATEMENT;return;endif

#endif
