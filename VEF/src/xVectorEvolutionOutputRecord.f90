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
