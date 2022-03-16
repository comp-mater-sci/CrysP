!
! $Id: criAssert.fpp 1933 2014-07-11 14:04:49Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of the first release: 2010-08-18
!>    $Revision: 1933 $
!>    $Date: 2014-07-11 16:04:49 +0200 (Fri, 11 Jul 2014) $
!>
!>    History of modifications: (see svn log)
!>
#ifndef criAssert_577784E4_0763_4bab_8D40_B950228283AD
#define criAssert_577784E4_0763_4bab_8D40_B950228283AD

!
! Support for assertions
!
#ifdef DEBUG
!
#ifndef ASSERT
#define ASSERT(test) if (.not.(test)) call assert_sub(__LINE__,__FILE__)
#endif
!
#else
! In a non-debug mode, we can define ASSERT either as ";" or "continue"
#ifndef ASSERT
#define ASSERT(test) continue
#endif
!
#endif

#endif

