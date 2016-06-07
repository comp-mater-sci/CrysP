!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of the initial release: 2016-05-31 (based on dmcUDSA.f90)
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

!> Data types for stress evolution outputs
module dmcEvolutionOutputRecord
use criErrcodes
use criMathUtils, only: SRTensor, root23
use dmcIncrementationControl, only: IncrementationControlVariables
use dmcBasicModule, only: YLPResult !> \fixme This dependency should be avoided by refactoring dmcBasicModule
use fngVec5D, only: vec5D2tens
implicit none

    !> Data outputed per increment of stress driven state evolution
    type :: IncrementOutputRecord
         
        double precision :: vm_strain = 0.D0
        double precision :: vm_strain_total = 0.D0
        
        double precision :: norm_P_abs = 0.D0
        
        double precision :: dotWonA = 0.D0
        double precision :: taylor_factor = 0.D0
        double precision :: scal_s = 0.D0
        double precision :: norm_SonA = 0.D0
        double precision :: R = 0.D0

        type(SRTensor) :: A
        type(SRTensor) :: SonA

        type(SRTensor)  :: P_inc_evol
        type(SRTensor)  :: S_evol
            
        type(IncrementationControlVariables) :: icv
        
    end type


    interface IncrementOutputRecord
        module procedure IncrementOutputRecord_init
    end interface
    
contains


    !> Make IncrementOutputRecord from increment data.
    function IncrementOutputRecord_init(icv, ylp, De, Se, taylor_factor, info) result(this)
    implicit none
    type(IncrementOutputRecord)                 :: this
    type(IncrementationControlVariables),intent(in) :: icv
    type(YLPResult),intent(in)                  :: ylp
    type(SRTensor),intent(in)                   :: De
    type(SRTensor),intent(in)                   :: Se
    double precision,intent(in)                 :: taylor_factor
    integer,intent(out)                         :: info
    !
        this%vm_strain = root23 * norm2(icv%vP_step)
        this%vm_strain_total = root23 * norm2(icv%vP_total)
        this%norm_P_abs = norm2(icv%vP_abs)
        !
        this%dotWonA = ylp%dotWonA
        this%scal_s = ylp%scal_s
        this%norm_SonA = norm2(ylp%vSonA)
        this%R = ylp%R
        
        this%taylor_factor = taylor_factor

        this%A%t = vec5D2tens(ylp%vA)
        this%SonA%t = vec5D2tens(ylp%vSonA)

        this%P_inc_evol = De
        this%S_evol = Se
        
        this%icv = icv
        
        info = criSuccess
        
    end function

end module
