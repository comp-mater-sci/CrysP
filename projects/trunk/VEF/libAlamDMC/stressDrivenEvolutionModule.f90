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
use dmcEvolutionOutputRecord
use xVectorIncrementOutputRecord
use commonUtils
use criMathUtils
use criAlgorithm, only: optionalDefault
implicit none

    
    type :: EvolutionOutput
        type(IncrementOutputRecord),dimension(:),allocatable      :: values
    end type
    
    
    
    type,extends(BasicModule),abstract :: StressDrivenEvolutionModule
        
        type(IncrementationControlSettings) :: control
        
    contains
        procedure,pass(this)    :: calculateStressPath => StressDrivenEvolutionModule_calculateStressPath
        
        procedure,pass(this)    :: onIncrementEnd => StressDrivenEvolutionModule_onIncrementEnd
        
    end type

contains
    
    
        
    
    integer function StressDrivenEvolutionModule_calculateStressPath(this, sigma, control, outputs, rotmat, &
                                                                     incrementation_control, use_icv_as_is) result(info)
    implicit none
    class(StressDrivenEvolutionModule),intent(in) :: this
    type(SRTensor),intent(in)   :: sigma
    class(IncrementationControlSettings),intent(inout) :: control
    type(EvolutionOutput),intent(out)   :: outputs
    !> Rotation matrix. Relevant only if scalingStrainTensorComponent is used
    double precision,dimension(rot_matrix_dim,rot_matrix_dim),intent(in),optional  :: rotmat
    !> Incrementation control variables to override the defaults.
    !>
    !> Typical use is to inherit some control variables (the totals) from a previous
    !> call to this function.
    !> On exit, the parameter will contain updated control variables.
    type(IncrementationControl),intent(inout),optional  :: incrementation_control
    !> Suppress re-initialization of step-wide and increment-wide incrementation control variables
    !> (default: false)
    logical,intent(in),optional                         :: use_icv_as_is
    !
    type(SRTensor) :: D, De, Se, X_tmp, D_retry
    double precision :: scaling_factor, control_variable, taylor_factor
    type(YLPResult) :: ylp, ylp_retry
    double precision,dimension(alamEval_vSD_dim) :: vDe, vSe
    type(IncrementationControl) :: icv
    double precision,dimension(sr_symm_voigt_dim) :: X_tmp_voigt
    !
    type(xVector_IncrementOutputRecord) :: tmp_output
    type(IncrementOutputRecord)         :: tmp_record
    
    integer :: increment, i
    !
        ! Prepare non-default incrementation controls if requested
        if (present(incrementation_control)) then
            icv = incrementation_control
            if (optionalDefault(use_icv_as_is, .false.)) call icv%initStep(info)
        endif
        !
        ! Follow the evolution line along S
        !
        Se%t = 0.D0
        De%t = 0.D0
        !
        ! main loop over deformation increments
        increment = 0
        do
            !
            increment = increment + 1
            !
            ! Calculate the strain rate mode
            info = this%findSolution(sigma, D, ylp)
            if (info /= criSuccess) then
                ! Re-attempt, try A from the previous increment as the starting point
                !
                ! Pick the most recent converged solution
                do i = increment-1, 1, -1
                    if (tmp_output%values(i)%R < this%ylp%obj_func_eps) then
                        D_retry = tmp_output%values(i)%A
                        exit
                    endif
                enddo
                ! Check post-condition of the loop: i > 0 means
                ! we have such a solution:
                if (i > 0) then
                    info = this%findSolution(sigma, D_retry, ylp_retry, vM_guess=.false.)
                    ! Accept the solution only if it is better than the original one
                    if ((info == criSuccess) .and. (ylp_retry%R < ylp%R)) then 
                        D = D_retry
                        ylp = ylp_retry
                    endif
                endif

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
            ! Add output record to the list
            call setOutputRecord(tmp_record, &
                                 icv%IncrementationControlVariables, &
                                 ylp, &
                                 De, Se, &
                                 taylor_factor, &   
                                 info)
            info = xVector_push(tmp_output, tmp_record)
            if (info /= criSuccess) exit
            !
            select case(control%scaling_type)
            case(scalingStrainTensor)
                if (norm2(icv%vP_step) > control%step_size) exit
            !
            case(scalingPlasticWork)
                if (icv%plastic_work_total > control%step_size) exit
            !    
            case(scalingStrainTensorComponent)
                ! Get total plastic strain in appropriate reference frame
                ! and check the tensor component of interest.
                X_tmp%t = vec5D2tens(icv%vP_step)
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
            call this%onIncrementEnd(tmp_record) 
        !
        enddo
        if (info /= criSuccess) return

        outputs%values = tmp_output%values
        ! Report back the incrementation control variables if requested
        if (present(incrementation_control)) incrementation_control = icv
    !
    end function
    
        
    subroutine setOutputRecord(this, icv, ylp, De, Se, taylor_factor, info)
    implicit none
    type(IncrementOutputRecord),intent(out)         :: this
    type(IncrementationControlVariables),intent(in) :: icv
    type(YLPResult),intent(in)                  :: ylp
    type(SRTensor),intent(in)                   :: De
    type(SRTensor),intent(in)                   :: Se
    double precision,intent(in)                 :: taylor_factor
    integer,intent(out)                         :: info
    !
        this%vm_strain = root23 * norm2(icv%vP_step)
        this%vm_strain_total = root23 * norm2(icv%vP_total)
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
    type(IncrementOutputRecord),intent(in)                 :: output_record
    !

    !
    end subroutine
    
end module
    
