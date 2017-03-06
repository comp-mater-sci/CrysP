!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2017-03-06
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

#include "criMacros.fpp"

!> Strain-(rate) driven step
module dmcStrainDrivenStep
use criRange
use criLog
use criMathUtils
use criErrcodes
use dmcUtils, only: display_unit
use dmcSubsteppingConfig
implicit none

public StrainDrivenStep, StrainDrivenStepConfig, StrainDrivenFixedStep
public StepOutput, IncrementOutput


    !> Base class for deformation rate driven steps
    type :: StrainDrivenStepConfig
        type(SRTensor)  :: deformation_rate
        logical         :: update_state = .false.
        logical         :: output_state = .false.
    end type


    !> A strain-(rate) driven step
    type :: StrainDrivenStep
        
        type(logData)                       :: log
        
        type(StrainDrivenStepConfig)        :: config
        
        !> Volumetric strain
        type(SRTensor)                      :: volumetric_strain ! FIXME: no need to store SRTensor
        
        !> Step strain
        type(SRTensor)                      :: deviatoric_strain
        
    contains
        procedure,pass(this)    :: setUp => StrainDrivenStep_setUp
        procedure,pass(this)    :: execute => StrainDrivenStep_execute
        
        procedure,pass(this)    :: readConfig => StrainDrivenStep_readConfig
        
    end type


    type,extends(StrainDrivenStep) :: StrainDrivenFixedStep
        class(FixedSubsteppingConfig),pointer    :: substepping_config => null()
    contains
        procedure,pass(this)    :: setUp => StrainDrivenFixedStep_setUp
        procedure,pass(this)    :: execute => StrainDrivenFixedStep_execute
        
        procedure,pass(this)    :: readConfig => StrainDrivenFixedStep_readConfig
        
    end type

    !> Constructors of StrainDrivenFixedStep
    interface StrainDrivenFixedStep
        module procedure StrainDrivenFixedStep_init_nincrements
    end interface


    !> Outputs collected per increment
    type :: IncrementOutput
        
        type(SRTensor)  :: L
        
        type(SRTensor)  :: D
        
        type(SRTensor)  :: S
        
        double precision :: vm_stress = 0.D0
        
        double precision :: plastic_work_inc = 0.D0 !< Plastic work during the increment, i.e. dotW = (D : S)
        
        double precision :: taylor_factor = 0.D0
        
    end type


    !> Outputs collected per step
    type :: StepOutput
        type(IncrementOutput),dimension(:),allocatable :: increments
    contains
        procedure,pass(this)        :: collect => StepOutput_collect
    end type


contains


    !> Constructor of StrainDrivenFixedStep. It takes number of increments
    !> as argument
    pure function StrainDrivenFixedStep_init_nincrements(n_increments) result(this)
    type(StrainDrivenFixedStep) :: this
    integer,intent(in)          :: n_increments !< Number of increments.
    !
        allocate(this%substepping_config, source=FixedSubsteppingConfig(n_increments))
    !
    end function


    !> Set up StrainDrivenStep 
    integer function StrainDrivenStep_setUp(this) result(info)
    implicit none
    class(StrainDrivenStep),intent(inout)   :: this
    !
    double precision :: step_strain_norm, volumetric_strain_norm, volumetric_strain_fraction
    integer :: n_increments
    !
    ! Volumetric strain fraction that triggers a warning (0.1%)
    double precision,parameter :: volumetric_strain_fraction_threshold = 0.001
    double precision,parameter :: auto_increment_norm = 0.02
    !
        info = criErr_BadArgs
        associate(config => this%config)
            ! Make the step traceless: decompose into volumetric strain rate
            ! and strain rate deviator
            this%volumetric_strain%t = trace(config%deformation_rate) / 3.D0 * unit_sr_tensor%t
            this%deviatoric_strain%t = config%deformation_rate%t - this%volumetric_strain%t
            step_strain_norm = norm2(this%deviatoric_strain%t)
            if (step_strain_norm < epsilon(0.D0)) then
                write(display_unit, 900) 'Norm of the deviatoric part of prescribed deformation is too small.'
                return
            endif
            !
            if (doLogging(criLogInfo, this%log%level)) then
                ! Report the discrepancy if the substracted volumetric part is larger than a given
                ! fraction of the total.
                volumetric_strain_norm = norm2(this%volumetric_strain%t)
                volumetric_strain_fraction = volumetric_strain_norm / norm2(config%deformation_rate%t)
                if (volumetric_strain_fraction > volumetric_strain_fraction_threshold ) then
                    write(display_unit, 600) volumetric_strain_norm, volumetric_strain_fraction * 100
                    600 format(/, 'Note: volumetric deformation of magnitude ', G0.2, 1X, &
                                ', which makes ', G0.2, 1X, &
                                'percent of the prescribed deformation in this step, was substracted.', /)
                endif
            endif
        end associate
        info = criSuccess
    !
#define MSG_GROUP_ERRORS
#define MSG_GROUP_RULERS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS
    !
    end function


    !> Execute StrainDrivenStep. 
    !>
    !> It is a placeholder method, it always returns criError.
    integer function StrainDrivenStep_execute(this, step_output) result(info)
    class(StrainDrivenStep),intent(inout)   :: this
    class(StepOutput),intent(out)           :: step_output
    !
        info = criError
    !
    end function
    
    !> Read configuration of StrainDrivenStep from config IO unit
    !>
    !> It is a placeholder method, it always returns criSuccess
    integer function StrainDrivenStep_readConfig(this, cnfunit) result(info)
    class(StrainDrivenStep),intent(inout)   :: this
    integer,intent(in)                      :: cnfunit !< IO unit
    !
        info = criSuccess
    !
    end function
    
    
    !> Set up strain driven fixed step.
    !>
    !> Returns criSuccess on success.
    integer function StrainDrivenFixedStep_setUp(this) result(info)
    implicit none
    class(StrainDrivenFixedStep),intent(inout)   :: this
    !
    double precision :: step_strain_norm, volumetric_strain_norm, volumetric_strain_fraction
    integer :: n_increments
    ! Volumetric strain fraction that triggers a warning (0.1%)
    double precision,parameter :: volumetric_strain_fraction_threshold = 0.001
    double precision,parameter :: auto_increment_norm = 0.02
    !
        info = this%StrainDrivenStep%setUp()
        if (info /= criSuccess) return
        !
        ! Set up automatic substepping with fixed number of increments
        !
        if (.not. associated(this%substepping_config)) then
            ! Automatic incrementation to be used. Set it up:
            step_strain_norm = norm2(this%deviatoric_strain%t)
            if (step_strain_norm >= auto_increment_norm) then
                n_increments = floor(step_strain_norm / auto_increment_norm)
                allocate(this%substepping_config, &
                            source=FixedSubsteppingConfig(UniformRange(0.D0, 1.D0, npoints=n_increments)))
                if (doLogging(criLogInfo, this%log%level)) then
                    write(display_unit, 601)
                    601 format('Note: automatic substepping will be used in this step.')
                endif
            else
                allocate(this%substepping_config, source=FixedSubsteppingConfig())
            endif
        endif
    !
#define MSG_GROUP_ERRORS
#define MSG_GROUP_RULERS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS
    !
    end function


    !> Execute step and store the output in step_output
    !>
    !> Returns criSuccess on success.
    integer function StrainDrivenFixedStep_execute(this, step_output) result(info)
    use altaySub
    use altayConfig
    implicit none
    class(StrainDrivenFixedStep),intent(inout)  :: this
    class(StepOutput),intent(out)               :: step_output
    !
    integer :: n_increments, i_incr
    double precision :: increment_size, x, x_prev, increment_size_tot
    type(SRTensor) :: increment_strain,  step_strain_total
    !
        ! Precondition
        RETURN_IF_WITH(.not. associated(this%substepping_config), info = criErr_BadArgs)
        RETURN_IF_WITH(.not. associated(this%substepping_config%ptr_range), info = criErr_BadArgs)
        !
        n_increments = this%substepping_config%getNumberOfIncrements()
        
        if (doLogging(criLogInfo, this%log%level)) then
            if (n_increments > 1) then
                write(display_unit,fmt=400) n_increments
                400 format('The step will be subdivided into ', I0, ' increments.')
            else
                write(display_unit,fmt=401)
                401 format('The step deformation will be instantly reached in one increment.')
            endif
        endif
        !
        ! Initialize AlTay structures
        RETURN_ON_WITH(call initStepData(n_increments, astate,info), &
                        info /= 0, &
                        info = criError)
        !
        associate(increment_range => this%substepping_config%ptr_range)
            !
            ! Get the lower boundary, it should be zero.
            RETURN_IF_WITH(.not. increment_range%next(x_prev), info = criErr_BadArgs)
            
            increment_size_tot = 0.D0
            
            ! Set-up the substeps
            step_strain_total%t = 0.D0
            i_incr = 0
            do while(increment_range%next(x))
                i_incr = i_incr + 1
                increment_size = x - x_prev
                increment_strain%t = increment_size * this%deviatoric_strain%t
                step_strain_total%t = step_strain_total%t + increment_strain%t
                x_prev = x
                !
                ! increment_size_tot = increment_size_tot + increment_size ! FIXME
                ! write(*,*) increment_size_tot, norm2(increment_strain%t) ! FIXME
                !
                if (norm2(increment_strain%t) < epsilon(0.D0)) then
                    write(display_unit, 900) 'Norm of the prescribed incremental deformation is too small.'
                    info = criErr_BadArgs
                    return
                endif
                !
                ! Set input data for AlTay
                associate (input => astate%simulCalls(i_incr)%input)
                        input%dgf = increment_strain%t
                        input%keep_texture = .not. this%config%update_state
                        input%keep_state = .not. this%config%update_state
                        input%full_model = .true.
                        input%do_output_init = .false.
                        input%do_output_final = this%config%output_state
                        call setStepType(input, acnf%model_id, info)
                end associate
            enddo
            ! Check if the loop had at least one iteration
            RETURN_IF_WITH(i_incr == 0, info = criErr_BadArgs)
        end associate
        !
        ! Call the AlTay
        RETURN_ON_WITH(call runSteps(astate,info), &
                        info /= 0, &
                        info = criError)
        !
        RETURN_IF(info /= criSuccess, info = step_output%collect(n_increments))
    !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    !
    end function

    !> Read configuration of substepping from the config IO unit
    integer function StrainDrivenFixedStep_readConfig(this, cnfunit) result(info)
    class(StrainDrivenFixedStep),intent(inout)  :: this
    integer,intent(in)                          :: cnfunit !< IO unit
    !
        if (.not. associated(this%substepping_config)) allocate(FixedSubsteppingConfig :: this%substepping_config)
        info = this%substepping_config%readConfig(cnfunit)
    !
    end function
    

    !> Collect the outputs from the AlTay simulation
    integer function StepOutput_collect(this, n_increments) result(info)
    use altayConfig
    use altayMacroKinematic
    implicit none
    class(StepOutput),intent(inout)     :: this
    integer,intent(in)                  :: n_increments
    !
    integer :: i, ierr, n_simulcalls
    type(DeformationRate) :: deformation_rate ! defined in altayMacroKinematic
    !
        ! Check if the input and the state of libaltay correspond.
        ALLOCATED_SIZE(n_simulcalls, astate%simulCalls)
        RETURN_IF(n_simulcalls < n_increments .or. n_simulcalls /= astate%nSimulCalls, &
                  info = criErr_BadArgs)
        !
        ! Allocate storage for output
        RETURN_ON_WITH(allocate(this%increments(n_increments), stat=ierr), &
                       ierr /= 0, info = criErr_MemAlloc)
        !
        ! collect the results
        do i = 1, n_increments
            associate (increment_output =>  this%increments(i))
                increment_output%L%t = astate%simulCalls(i)%input%dgf
                ! Let libaltay calculate the strain rates etc.
                call Set_DeformationRate(increment_output%L%t, deformation_rate)
                increment_output%D%t = deformation_rate%StrainRate
                increment_output%S%t = astate%simulCalls(i)%output%stress_tensor
                increment_output%vm_stress = astate%simulCalls(i)%output%effective_stress
                ! D : S
                increment_output%plastic_work_inc = sum(increment_output%D%t * increment_output%S%t)
                !
                increment_output%taylor_factor = astate%simulCalls(i)%output%taylor_factor
            end associate
        enddo
        info = criSuccess
    !
    end function

end module
