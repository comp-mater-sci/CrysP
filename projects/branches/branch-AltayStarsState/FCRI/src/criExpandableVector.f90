!
! $Id: criExpandableVector.f90 2595 2016-05-31 14:18:06Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2013-07-02
!>    $Revision: 2595 $
!>    $Date: 2016-05-31 16:18:06 +0200 (Tue, 31 May 2016) $
!>
!>    History of modifications: (see svn log)
!>
!>    \file criExpandableVector.f90 
!
#include "criStdDefs.fpp"
!
!> Implementation of a simple, expandable vector with constant amortized insertion time.
!>
!> Selected instantizations of the vector are provided:
!> - xVector_integer
!> - xVector_double
!>
!> \note This module uses a special feature of instantiating xVector_<valuename> typpe
!>       for several value types at once. Normal use is much simpler, for example:
!>            module xVectorMyType  
!>            #define _VALUE_TYPE type(MyType)
!>            #define _VALUE_NAME MyType
!>            #include "criExpandableVectorTemplates.fpp"
!>            #undef _VALUE_TYPE
!>            #undef _VALUE_NAME
!>            end module
module criExpandableVector
use criErrcodes
implicit none

! Prevent function definitions from being included here
#define DECLARATIONS_ONLY

    !
    ! Declarations for integer
    !
#define _VALUE_TYPE integer
#define _VALUE_NAME integer
#include "criExpandableVectorTemplates.fpp"
#undef _VALUE_TYPE
#undef _VALUE_NAME


    !
    ! Declarations for double precision
    !
#define _VALUE_TYPE double precision
#define _VALUE_NAME double
#include "criExpandableVectorTemplates.fpp"
#undef _VALUE_TYPE
#undef _VALUE_NAME

#undef DECLARATIONS_ONLY

contains


! Prevent all declarations from being included here
#define DEFINITIONS_ONLY
    !
    ! Definitons for integer
    !
#define _VALUE_TYPE integer
#define _VALUE_NAME integer
#include "criExpandableVectorTemplates.fpp"
#undef _VALUE_TYPE
#undef _VALUE_NAME

    !
    ! Definitons for double precision
    !
#define _VALUE_TYPE double precision
#define _VALUE_NAME double
#include "criExpandableVectorTemplates.fpp"
#undef _VALUE_TYPE
#undef _VALUE_NAME

#undef DEFINITIONS_ONLY

end module
