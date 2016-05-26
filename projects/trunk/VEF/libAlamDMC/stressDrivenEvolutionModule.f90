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

!> Implementation of a DMC computiational module that allows stress-driven evolution of
!> material state.
module dmcStressDrivenEvolutionModule
use dmcBasicModule
use dmcIncrementationControl
use commonUtils
use criMathUtils
implicit none


        type :: OutputRecord
         
            double precision :: vm_strain = 0.D0
            double precision :: vm_strain_total = 0.D0
        
            double precision :: P_abs_sum = 0.D0
        
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
    
    
    
    type :: EvolutionOutput
        type(OutputRecord),dimension(:),allocatable      :: values
    end type
    
    
    
    type,extends(BasicModule),abstract :: StressDrivenEvolutionModule
        
        type(IncrementationControlSettings) :: control
        
    contains
        procedure,pass(this)    :: calculateStressPath => StressDrivenEvolutionModule_calculateStressPath
        
        procedure,pass(this)    :: onIncrementEnd => StressDrivenEvolutionModule_onIncrementEnd
        
    end type

contains
    
    
        
    !> \todo optional initial icv should be provided as a parameter
    integer function StressDrivenEvolutionModule_calculateStressPath(this, sigma, control, outputs, rotmat) result(info)
    implicit none
    class(StressDrivenEvolutionModule),intent(in) :: this
    type(SRTensor),intent(in)   :: sigma
    class(IncrementationControlSettings),intent(inout) :: control
    type(EvolutionOutput),intent(out)   :: outputs
    !> Rotation matrix. Relevant only if scalingStrainTensorComponent is used
    double precision,dimension(rot_matrix_dim,rot_matrix_dim),intent(in),optional  :: rotmat
    !
    type(SRTensor) :: D, De, Se, X_tmp
    double precision :: scaling_factor, control_variable, taylor_factor
    type(YLPResult) :: ylp
    double precision,dimension(alamEval_vSD_dim) :: vDe, vSe
    type(IncrementationControl) :: icv
    double precision,dimension(sr_symm_voigt_dim) :: X_tmp_voigt
    !
    !> \fixme Shortcut: array that is "lage enough" to keep the outputs. 
    !>        To be replaced by a list, deque or another dynamic storage.
    integer,parameter :: max_records = 100
    type(OutputRecord),dimension(max_records) :: tmp_records
    integer :: increment
    
    !
        !
        ! Follow the evolution line along S
        !
        Se%t = 0.D0
        De%t = 0.D0
        !
        ! main loop over deformation increments
        increment = 0
        do
            ! Calculate the strain rate mode
            info = this%findSolution(sigma, D, ylp)
            if (info /= criSuccess) then
                ! re-attempt, try D from the previous increment as the starting point
                if (increment > 0) info = this%findSolution(sigma, D, ylp, vM_guess=.false.)
            endif
            if (info /= 0) exit
            !
            ! Calculate incrementation control variables
            !
            ! Calculate increment of plastic strain to be imposed for texture evolution: 
            select case(control%scaling_type)
            case(scalingStrainTensor)
                !! -> Scale the vA in order to get ||vA|| = NormIter
                control_variable = norm2(ylp%vA)
                !
            case(scalingPlasticWork)
                control_variable = ylp%dotWonA
            !
            case(scalingStrainTensorComponent)
                if (present(rotmat)) then
                    X_tmp = rotateSRTensorFrom(D,rotmat)
                else
                    X_tmp = D
                endif
                X_tmp_voigt = Mat33ToVec6(X_tmp%t)
                control_variable = abs(X_tmp_voigt(control%selected_tensor_component))
            !
            case default
                info = criErr_BadArgs
                exit
            end select
            !
            if (control_variable < epsilon(0.D0)) then
                  info = criError
                  exit
            endif
            scaling_factor = (control%increment_size / control_variable)
            
            !
            ! Calculate strain increment for material state evolution
            vDe = ylp%vA * scaling_factor
            De%t = vec5D2tens(vDe)
            ! Update material state
            call makeTextureUpdateStep(De%t,Se%t,taylor_factor,this%output%outputRequest,info)
            if (info /= 0) exit !< \fixme Literal constant in makeTextureUpdateStep
            vSe = tens2vec5D(Se%t)
            !
            increment = increment + 1
            ! Add output record to the list
            !> \fixme Get rid of the big array
            ! -->>
            call setOutputRecord(tmp_records(increment), &
                                 icv%IncrementationControlVariables, &
                                 ylp, &
                                 De, Se, &
                                 taylor_factor, &   
                                 info)
            if ((increment >= max_records) .or. (info /= criSuccess)) exit
            ! <<--
            !
            info = criSuccess
            select case(control%scaling_type)
            case(scalingStrainTensor)
                if (norm2(icv%vP) > control%step_size) exit
            !
            case(scalingPlasticWork)
                if (icv%plastic_work_total > control%step_size) exit
            !    
            case(scalingStrainTensorComponent)
                ! Get total plastic strain in appropriate reference frame
                ! and check the tensor component of interest.
                X_tmp%t = vec5D2tens(icv%vP)
                if (present(rotmat)) X_tmp = rotateSRTensorFrom(X_tmp ,rotmat)
                X_tmp_voigt = Mat33ToVec6(X_tmp%t)
                if (X_tmp_voigt(control%selected_tensor_component) > control%step_size) exit
            !
            end select
            !
            ! Update icv
            !
            call icv%update(vDe, vSe, info)
            !
            call this%onIncrementEnd(tmp_records(increment)) 
        !
        enddo
        if (info /= criSuccess) return

        !> \fixme Get rid of the big array. It should be asArray on list.
        ! -->>
        outputs%values = tmp_records(1:increment)
        ! <<--
    !
    end function
    
        
    subroutine setOutputRecord(this, icv, ylp, De, Se, taylor_factor, info)
    implicit none
    type(OutputRecord),intent(out)              :: this
    type(IncrementationControlVariables),intent(in) :: icv
    type(YLPResult),intent(in)                  :: ylp
    type(SRTensor),intent(in)                   :: De
    type(SRTensor),intent(in)                   :: Se
    double precision,intent(in)                 :: taylor_factor
    integer,intent(out)                         :: info
    !
        this%vm_strain = root23 * norm2(icv%vP_inc)
        this%vm_strain_total = root23 * norm2(icv%vP)
        this%P_abs_sum = sum(icv%vP_norms)
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
        
    end subroutine
    
    subroutine StressDrivenEvolutionModule_onIncrementEnd(this, output_record)
    implicit none
    class(StressDrivenEvolutionModule),intent(in) :: this
    type(OutputRecord),intent(in)                 :: output_record
    !

    !
    end subroutine
    
end module
    
