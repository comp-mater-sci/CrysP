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
!>    \date Date of first release: 2012-11-01
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criTest.fpp 
!
#ifndef CRITESTFRAMEWORK

! Configuration of termination type:
! - CRITESTFRAMEWORK_TERMINATE : failure of test terminates the program
! - CRITESTFRAMEWORK_HANDLE the failure of test is handled by the caller (useful in most cases)
! - otherwise: an error message is written out on failure of the test, but the execution carries on 
#ifdef CRITESTFRAMEWORK_TERMINATE
#define _TEST_STOP stop
#else
#ifdef CRITESTFRAMEWORK_HANDLE
#define _TEST_STOP return
#else
#define _TEST_STOP continue
#endif
#endif

! _TEST macro: testing of a condition "boolval"
#ifndef _TEST
#define _TEST(name,boolval) if (boolval) then; write(*,'(A)') 'Test "'//trim(name)//'": pass'; else; write(*,'(A,1X,I0)') 'Test "'//trim(name)//'" failed, line:',__LINE__; _TEST_STOP; endif;
#endif

#endif
