!
! $Id: criToolset.fpp 2087 2015-03-04 15:03:24Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2010-08-18
!>    $Revision: 2087 $
!>    $Date: 2015-03-04 16:03:24 +0100 (Wed, 04 Mar 2015) $
!>
!>    History of modifications: (see svn log)
!>
#ifndef criToolset_C748BE15_2C7D_409d_A6AF_5B462E119076
#define criToolset_C748BE15_2C7D_409d_A6AF_5B462E119076

!
! Speculations on Fortran features available.
!
#if __INTEL_COMPILER > 1200
!> If the "do concurrent" parallelization construct is available
#define FORT_HAS_DO_CONCURRENT
!> If CAF is available
#define FORT_HAS_COARRAYS
!> If the "associate" keyword is available
#define FORT_HAS_ASSOCIATE
!> If the intrinsic function "norm2" is available
#define FORT_HAS_NORM2
!> If the "newunit" specifier is available in the "open" statement
#define FORT_HAS_NEWUNIT
!> If the F2003 semantics can be used for the left-hand-side
!> reallocation
#define FORT_HAS_LHS_REALLOCATION
!> If the ALLOCATE statement accepts SOURCE parameter (which complies with F2003) 
#define FORT_HAS_ALLOCATE_F03SOURCE
!> If the ALLOCATE statement accepts SOURCE parameter (which complies with F2008)
!> Note the ALLOCATE in F2003 and F2008 are significantly different, see e.g.
!> ISO/IEC JTC1/SC22/WG5 N1828 Section 6.1 - 6.3.
#define FORT_HAS_ALLOCATE_SOURCE
!> If the ALLOCATE statement accepts MOLD parameter (which complies with F2008) 
#define FORT_HAS_ALLOCATE_MOLD
!> If a derived type can have generic initialization procedures 
!> (i.e. a generic name can be the same as a derived-type name)
#define FORT_HAS_DERIVED_TYPE_INTERFACE
!> If the attribute "abstract" can be attached to a derived type
#define FORT_HAS_ABSTRACT_TYPES
!> If the attribute "abstract" can be attached to an interface
#define FORT_HAS_ABSTRACT_INTERFACES
!> If the atribute "extends" can be used in definition of a derived type
#define FORT_HAS_INHERITANCE
!> If the keyword "class" can be used in declarations of dummy arguments
#define FORT_HAS_POLYMORPHIC_CALLS
!
#endif

#if __INTEL_COMPILER >= 1310
! Multi-module OOP bug is fixed
#define IFORT_OOP_FLAW_FIXED
#endif
!
! Macro definitions derived from the speculations on the 
! compiler's capabilities
! 
#if defined(FORT_HAS_ABSTRACT_INTERFACES) && defined(FORT_HAS_ABSTRACT_TYPES) && defined(FORT_HAS_INHERITANCE) && defined(FORT_HAS_POLYMORPHIC_CALLS)
!> If the Fortran compiler supports object-oriented programming.
#define FORT_IMPLEMENTS_OOP
#endif


#endif

