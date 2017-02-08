!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of the initial release: 2016-05-31
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

!> Helper data type for storing stress evolution outputs
module xVectorIncrementOutputRecord
use criErrcodes
use dmcEvolutionOutputRecord, only: IncrementOutputRecord

!
! Instantiate xVector_IncrementOutputRecord
!
#define _VALUE_TYPE type(IncrementOutputRecord)
#define _VALUE_NAME IncrementOutputRecord
#include "criExpandableVectorTemplates.fpp"
#undef _VALUE_TYPE
#undef _VALUE_NAME

end module
