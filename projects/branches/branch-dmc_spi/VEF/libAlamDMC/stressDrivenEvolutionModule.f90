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
use dmcStressDrivenModule
use dmcIncrementationControl
use dmcEvolutionOutputRecord
use xVectorIncrementOutputRecord
use commonUtils
use criMathUtils
use criLog
use criAlgorithm, only: optionalDefault
implicit none

    
    type :: EvolutionOutput
        type(IncrementOutputRecord),dimension(:),allocatable      :: values
    end type
    
    
    
    type,extends(StressDrivenModule) :: StressDrivenEvolutionModule
        
        type(IncrementationControlSettings) :: control

    contains

        !> Main loop of incremental stress driven state evolution
        !>
        !> Under normal circumstances the subclasses do not need to override this method.
        !> Event handlers should be used to get info and/or control how the main loop
        !> advances.
        procedure,pass(this)    :: calculateStressPath => StressDrivenEvolutionModule_calculateStressPath

        !> Event handler invoked on increment start.
        !>
        !> A subclass can overload this to get informed about the incrementation and 
        !> influence the incrementation.
        procedure,pass(this)    :: onIncrementStart => StressDrivenEvolutionModule_onIncrementStart

        !> Event handler invoked on increment end
        !>
        !> A subclass can overload this to get informed about the incrementation and 
        !> influence the incrementation.
        procedure,pass(this)    :: onIncrementEnd => StressDrivenEvolutionModule_onIncrementEnd
        
    end type

contains


    !> Main loop of incremental stress driven state evolution
    integer function StressDrivenEvolutionModule_calculateStressPath(this, sigma, control, outputs, rotmat, &
                                                                     incrementation_control, use_icv_as_is) result(info)
    implicit none
    class(StressDrivenEvolutionModule),intent(inout):: this
    type(SRTensor),intent(in)                       :: sigma !< Imposed stress tensor
    !> Settings that control the incrementation process
    class(IncrementationControlSettings),intent(inout) :: control
    !> Results if the incrementation procedure. 
    !>
    !> On successful exit it will include  n+1 entries, where n is the number of
    !> increments needed to reach the end of the step.
    !> The leading n contain complete results (search for strain rate
    !> AND strain incrementation), while the last one just the result of the search for
    !> the strain rate. Therefore, the last entry corresponds to the state of 
    !> the material at the end of the step.
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
    type(SRTensor) :: D, X_tmp, D_retry
    double precision :: scaling_factor, control_variable, stop_control_variable, taylor_factor, stretch
    type(YLPResult) :: ylp, ylp_retry
    double precision,dimension(alamEval_vSD_dim) :: vDe, vSe
    type(IncrementationControl) :: icv
    double precision,dimension(sr_symm_voigt_dim) :: X_tmp_voigt
    !
    type(xVector_IncrementOutputRecord) :: tmp_output
    type(IncrementOutputRecord)         :: tmp_record
    integer :: i, n_roots
    double precision,dimension(2) :: xi
    logical :: stop_flag
    double precision,parameter :: stretch_ratio = 1e-3
    !
        ! Prepare non-default incrementation controls if requested
        if (present(incrementation_control)) then
            icv = incrementation_control
            if (.not. optionalDefault(use_icv_as_is, .false.)) call icv%initStep(info)
        endif
        !
        ! Trick: allow the increment to "stretch" a bit.
        ! The trick is used in the stop condition of the loop to prevent starting
        ! a new increment because stop_control_variable - control%step_size gives some
        ! small positive value. The trick does not eliminate the main cause of that
        ! drift, which is the accumulation of increment tensor components of different sign.
        stretch = stretch_ratio * control%increment_size
        !
        ! Follow the evolution line along S
        ! in the main loop over deformation increments
        do
            call this%onIncrementStart(control, icv, info)
            if (info /= criSuccess) exit
            !
            ! Calculate the strain rate mode
            info = this%findSolution(sigma, D, ylp)
            if ((info /= criSuccess) .or. (ylp%R > this%ylp%obj_func_eps)) then
                ! Re-attempt, try A from the previous increment as the starting point
                !
                ! Pick the most recent converged solution
                do i = icv%increment, 1, -1
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
            ! Nasty hack: drilling a hole to libaltay to get the Taylor factor
            call getTaylorFactor(1,taylor_factor,info)
            !
            ! Make output record and prepare variables for updating icv
            tmp_record = IncrementOutputRecord(icv%IncrementationControlVariables, &
                                               ylp, &
                                               SRTensor(), SRTensor(), &
                                               taylor_factor)
            !
            ! Check if we start a/another increment
            stop_flag = .false.
            select case(control%scaling_type)
            case(scalingStrainTensor, scalingStrainTensorIncrement)
                stop_control_variable = norm2(icv%vP_step)
            !
            case(scalingPlasticWork)
                stop_control_variable = icv%plastic_work_total
            !    
            case(scalingStrainTensorComponent)
                ! Get total plastic strain in appropriate reference frame
                ! and check the tensor component of interest.
                X_tmp%t = vec5D2tens(icv%vP_step)
                if (present(rotmat)) X_tmp = rotateSRTensorFrom(X_tmp ,rotmat)
                X_tmp_voigt = Mat33ToVec6(X_tmp%t)
                stop_control_variable = abs(X_tmp_voigt(control%selected_tensor_component))
            case default
                ! Make sure it stops immediately
                stop_flag = .true.
                stop_control_variable = control%step_size + control%increment_size
            !
            end select
            !
            stop_flag = stop_flag &
                        .or.(stop_control_variable + stretch > control%step_size)
            !
            if (.not. stop_flag) then
                !
                ! Calculate incrementation control variables
                !
                ! Calculate increment of plastic strain to be imposed for texture evolution: 
                select case(control%scaling_type)
                case(scalingStrainTensorIncrement)
                    control_variable = norm2(ylp%vA)
                !
                case(scalingStrainTensor)
                    ! Find scaling factor x such as 
                    ! ||vP_step - x vA|| - ||vP_step|| = increment_size   (*)
                    n_roots = solveQuadraticPolynomial(a=dot_product(ylp%vA, ylp%vA), &
                                                       b=2*dot_product(ylp%vA, icv%vP_step), &
                                                       c=dot_product(icv%vP_step, icv%vP_step) - &
                                                         (control%increment_size + norm2(icv%vP_step))**2, &
                                                       x=xi)
                    ! Up to two roots; we pick the largest one;
                    if (n_roots > 0) control_variable = control%increment_size / maxval(xi(1:n_roots))
                    ! If control variable is negative (the only way to satisfy (*) is 
                    ! to decrease the strain), fall back to a less accurate scheme.
                    if (control_variable < 0.D0) control_variable = norm2(ylp%vA)
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
                tmp_record%P_inc_evol%t = vec5D2tens(vDe)
                ! Update material state
                call makeTextureUpdateStep(tmp_record%P_inc_evol%t, &
                                           tmp_record%S_evol%t, &
                                           taylor_factor,&
                                           this%output%outputRequest, info)
                if (info /= 0) exit !< \fixme Literal constant in makeTextureUpdateStep
                vSe = tens2vec5D(tmp_record%S_evol%t)
            else
                vDe = 0.D0
                vSe = 0.D0
            endif
            !
            ! Append the output record
            info = xVector_push(tmp_output, tmp_record)
            if (info /= criSuccess) exit
            !
            ! Update icv
            !
            call icv%update(vDe, vSe, info)
            if (info /= criSuccess) exit
            !
            call this%onIncrementEnd(control, icv, tmp_record, info)
            if (stop_flag .or. (info /= criSuccess)) exit
        !
        enddo
        if (info /= criSuccess) return

        outputs%values = tmp_output%values
        ! Report back the incrementation control variables if requested
        if (present(incrementation_control)) incrementation_control = icv
    !
    end function



    !> Event handler in calculateStressPath: invoked at the begining of each
    !> increment
    subroutine StressDrivenEvolutionModule_onIncrementStart(this, control, icv, info)
    implicit none
    class(StressDrivenEvolutionModule),intent(inout)    :: this
    class(IncrementationControlSettings),intent(inout)  :: control
    class(IncrementationControl),intent(inout)          :: icv
    integer,intent(out)                                 :: info
    !
        if (doLogging(criLogDebug,this%output%verbosity)) then
            write(display_unit,fmt=801)
            write(display_unit,fmt=300) icv%increment
            ! Formats
            300 format(/,'Increment ', I0,/, 'TR search progress:')
        endif
        info = criSuccess
#define MSG_GROUP_RULERS
#include "msgFormats.inc"
#undef MSG_GROUP_RULERS
    !
    end subroutine


    !> Event handler in calculateStressPath: invoked at the end of each
    !> increment
    subroutine StressDrivenEvolutionModule_onIncrementEnd(this, control, icv, output_record, info)
    implicit none
    class(StressDrivenEvolutionModule),intent(inout)    :: this
    class(IncrementationControlSettings),intent(inout)  :: control
    class(IncrementationControl),intent(inout)          :: icv
    type(IncrementOutputRecord),intent(in)              :: output_record
    integer,intent(out)                                 :: info
    !
    integer :: j
    !
        if (doLogging(criLogInfo,this%output%verbosity)) then
    
            write(display_unit,fmt=300) output_record%icv%increment, output_record%R
            
            if (doLogging(criLogDebug,this%output%verbosity)) then
                write(display_unit,400) 'A^star', 'S(A^star)', 'Delta eps'
                do j=1,3
                    write(display_unit,411) output_record%A%t(:,j), output_record%SonA%t(:,j), &
                                            output_record%P_inc_evol%t(:,j)
                enddo
            endif
            ! Formats
            300 format('Increment ', I0, 1X, 'finished, residual error: ', E10.3)
            400 format(T15,A,T54,A,T85,A)
            411 format(3(E10.3,1X),' | ',3(E10.3,1X),' | ',3(E10.3,1X))
        endif
        info = criSuccess
    !
    end subroutine

end module
    
