!
! $Id: criModules.fpp 2407 2015-11-30 19:34:46Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of the first release: 2010-08-18
!>    $Revision: 2407 $
!>    $Date: 2015-11-30 20:34:46 +0100 (Mon, 30 Nov 2015) $
!>
!>    History of modifications: (see svn log)
!>
!
#ifndef criModules_5FA6616F_B913_4b20_8F28_725F76E95C11
#define criModules_5FA6616F_B913_4b20_8F28_725F76E95C11

!> Renaming policy for the modules
#ifdef MOD_RENAME_ABAQUS
! 
! Add appropriate macro to rename module
#define criAlgorithm kcriAlgorithm
#define criArray kcriArray
#define criAssert kcriAssert
#define criConfigReader kcriConfigReader
#define criErrcodes kcriErrcodes
#define criExpandableVector kcriExpandableVector
#define criLibCRIVersion kcriLibCRIVersion
#define criLinearMap kcriLinearMap
#define criLog kcriLog
#define criMathUtils kcriMathUtils
#define criMkTemp kcriMkTemp
#define criNumerics kcriNumerics
#define criPath kcriPath
#define criRange kcriRange
#define criRuntime kcriRuntime
#define criTimers kcriTimers
#define criUncomment kcriUncomment
#define criVersion kcriVersion
!
#endif

#endif

