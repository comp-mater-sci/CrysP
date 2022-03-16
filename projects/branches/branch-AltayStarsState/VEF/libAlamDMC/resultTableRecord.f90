!
! $Id: resultTableRecord.f90 2964 2017-04-20 07:56:02Z jgawad $
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2017-04-07
!>    $Revision: 2964 $
!>    $Date: 2017-04-20 09:56:02 +0200 (Thu, 20 Apr 2017) $
!>
!>    History of modifications: (see svn log)

!> Datatype for storing essential results from the multi-level model.
module dmcResultTableRecord
use criErrcodes
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
