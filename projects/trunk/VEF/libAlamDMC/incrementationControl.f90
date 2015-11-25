! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2015-11-25, based on contents of 'dmcEWC.f90'
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

!> Types and procedures that control incrementation in stress-driven 
!> evolution of material state.
module dmcIncrementationControl
use alamYLPConstants
use criErrcodes
implicit none

    integer,parameter :: scalingStrainTensor = 0, &
                         scalingStrainTensorComponent = 1, &
                         scalingPlasticWork = 2

    integer,parameter :: incrementFixed = 0, &
                         incrementAuto = 1

    !> Basic control variables used in evolution of material state.
    type :: IncrementationControlVariables
        
        integer :: increment = 0
        
        !> Plastic work in the current increment
        double precision    :: plastic_work_inc = 0.D0
        
        !> Total plastic work
        double precision    :: plastic_work_total = 0.D0
        
        !> Increment of plastic strain
        double precision,dimension(alamEval_vSD_dim)    :: vP_inc = 0.D0
        
        !> Total plastic strain:
        double precision,dimension(alamEval_vSD_dim)    :: vP = 0.D0
        
        
        !> Sum of absolute plastic strain increments:
        !> \f[
        !>    vP_{norms} = \sum \| vE_{inc} \|
        !> \f]
        double precision,dimension(alamEval_vSD_dim)    :: vP_norms
        
    end type
    
    
    !> 
    type,extends(IncrementationControlVariables) :: IncrementationControl
        
    contains
    
        procedure,pass(this)        :: update => IncrementationControl_update
        
    end type


    !> Basic settings for incrementation control.
    type :: IncrementationControlSettings
        
        integer         :: scaling_type = scalingStrainTensor
        
        integer         :: incrementation_type = incrementFixed
        
        double precision :: increment_size = 0.D0
        
        double precision :: step_size = 0.D0

    end type

    
contains
    
    subroutine IncrementationControl_update(this, vDe, vSe, info)
    implicit none
    class(IncrementationControl),intent(inout)      :: this
    double precision,dimension(alamEval_vSD_dim),intent(in) :: vDe, vSe
    integer,intent(out)                             :: info
    !
        ! Plastic work in the current increment
        this%plastic_work_inc = dot_product(vDe,vSe)
        ! Total plastic work
        this%plastic_work_total = this%plastic_work_total + this%plastic_work_inc
        ! Increment of plastic strain
        this%vP_inc = vDe
        ! Total plastic strain:
        this%vP = this%vP + this%vP_inc
        ! Sum of absolute plastic strain increments:
        this%vP_norms = this%vP_norms + abs(this%vP_inc)
        this%increment = this%increment  + 1
        info = criSuccess
    !
    end subroutine
    
end module
