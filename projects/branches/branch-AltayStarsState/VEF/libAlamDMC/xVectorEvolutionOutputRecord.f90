!
! $Id: xVectorEvolutionOutputRecord.f90 2851 2017-03-07 14:07:47Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of the initial release: 2016-05-31
!>    $Revision: 2851 $
!>    $Date: 2017-03-07 15:07:47 +0100 (Tue, 07 Mar 2017) $
!>
!>    History of modifications: (see svn log)

!> Helper data type for storing stress evolution outputs
module xVectorIncrementOutputRecord
use criErrcodes
use dmcEvolutionOutputRecord, only: IncrementOutputRecord
implicit none
!
! Instantiate xVector_IncrementOutputRecord
!
#define _VALUE_TYPE type(IncrementOutputRecord)
#define _VALUE_NAME IncrementOutputRecord
#include "criExpandableVectorTemplates.fpp"
#undef _VALUE_TYPE
#undef _VALUE_NAME

end module
