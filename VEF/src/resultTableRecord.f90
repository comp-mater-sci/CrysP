!> Datatype for storing essential results from the multi-level model.
module dmcResultTableRecord
use altay_definitions
use alamYLPConstants
implicit none

    public :: ResultTableRecord
    public :: xVector_ResultTableRecord, xVector_push, xVector_size, &
              xVector_capacity, xVector_expand, size
    private

    type :: ResultTableRecord

        double precision,dimension(alamEval_vSD_dim) :: vA = 0.D0
        double precision,dimension(alamEval_vSD_dim) :: vSonA = 0.D0

    end type

!
! Instantiate xVector_ResultTableRecord
!
#define _VALUE_TYPE type(ResultTableRecord)
#define _VALUE_NAME ResultTableRecord
#include "criExpandableVectorTemplates.fpp"
#undef _VALUE_TYPE
#undef _VALUE_NAME

end module
