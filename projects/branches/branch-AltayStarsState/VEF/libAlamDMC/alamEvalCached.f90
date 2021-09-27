!
! $Id: alamEvalCached.f90 2964 2017-04-20 07:56:02Z jgawad $
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

#include "criMacros.fpp"

!> Subclass of NormalizedV5DComp that stores intermediate points the table of cached results
module dmcAlamEvalCached
use criErrcodes
use alamYLPConstants
use alamEval
use dmcResultTable
implicit none

    public :: NormalizedV5DCompCached
    private

    type,extends(NormalizedV5DComp) :: NormalizedV5DCompCached
        
        type(ResultTable),pointer :: ptr_db => null()
        
    contains
        !> Implementation of virtual method defined in ObjectiveFunction
        procedure,pass(this)           :: objectiveEval => objectiveEval_NormalizedV5DCompCached
        
    end type
   
contains

    subroutine objectiveEval_NormalizedV5DCompCached(this,vX,info)
    implicit none
    class(NormalizedV5DCompCached),intent(inout)    :: this
    double precision,dimension(:),intent(in)    :: vX       !< Dimension must be: 5
    integer,intent(out)                         :: info
    !
    double precision,dimension(size(vX)):: vX_n
    double precision :: vX_norm
    !
        ! Assert the size and the norm
        RETURN_IF_WITH(size(vX) /= alamEval_vSD_dim, info = -1)
        RETURN_ON_WITH(vX_norm = norm2(vX), vX_norm < epsilon(0.D0), info = -1)
        ! Let the superclass to do its job: run the fine-scale model
        call this%NormalizedV5DComp%objectiveEval(vX, info)
        if (info == 0 .and. associated(this%ptr_db)) then 
            ! We wouldn't reach this point if ||vX|| is zero
            ! Normalize vX before storing it. It is also done by objectiveEval
            ! in the superclass.
            vX_n = vX / vX_norm
            CHOOSE(info, this%ptr_db%put(vX_n, this%NormalizedV5DComp%vSml) == criSuccess, 0, 1)
        endif
    !
    end subroutine
    
end module
