!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2017-02-13
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

#include "criMacros.fpp"

!> Arbitrary Deformation Path strain-(rate) driven simulations
module dmcADP
use criErrcodes
use criConfigReader
use dmcSD
use altayMacroKinematic, only: DeformationRate, Set_DeformationRate
use dmcResultFileOutput
implicit none

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
    end type

    !> Outputs collected by the simulation run
    type :: ADPOutputData
        type(StepOutput),dimension(:),allocatable :: steps
    end type

    
    !> Arbitrary Strain Mode
    type,extends(SDModule) :: ADPModule
        
    contains
        
        !>@{ \name Interface methods of AbstractModule
        
        procedure,pass(this) :: initialize => ADPModule_initialize
        
        procedure,pass(this) :: printConfig => ADPModule_printConfig
        
        procedure,pass(this) :: readConfig => ADPModule_readConfig
        
        procedure,pass(this) :: run => ADPModule_run
        
        !>@}
        
        procedure,pass(this) :: execute => ADPModule_execute
        procedure,pass(this) :: fileOutput => ADPModule_fileOutput
        
        
    end type

contains


    !> Initialization of the module
    integer function ADPModule_initialize(this) result(info)
    implicit none
    class(ADPModule),intent(inout) :: this
    !
        info = this%SDModule%initialize()
    !
    end function


    !> Print configuration to IO unit
    integer function ADPModule_printConfig(this, outunit) result(info)
    class(ADPModule),intent(in)      :: this
    integer,intent(in)              :: outunit !< IO unit for output
    !
        info = this%SDModule%printConfig(outunit)

    !
    end function


    !> Read configuration from IO unit
    integer function ADPModule_readConfig(this, cnfunit) result(info)
    implicit none
    class(ADPModule),intent(inout)   :: this
    integer,intent(in)              :: cnfunit !< IO input unit
    !
    integer     :: n_steps, ierr, i, deformation, incrementation
    !
    integer,parameter :: n_incrementation_types = 2
    integer,parameter :: auto_incrementation_id = 1, fixed_incrementation_id = 2
    type(MapItem),dimension(n_incrementation_types) :: incrementation_type_names = [&
        MapItem('Auto', auto_incrementation_id), &
        MapItem('Fixed', fixed_incrementation_id)]
    !
    integer,parameter :: n_deformation_types = 3
    integer,parameter :: deformation_id = 1, strainmode_id = 2, strain_id = 3
    type(MapItem),dimension(n_deformation_types) :: deformation_type_names = [&
        MapItem('Deformation', deformation_id),&
        MapItem('StrainMode', strainmode_id),&
        MapItem('Strain', strain_id)]
    !
    double precision,dimension(sr_voigt_dim) :: tmp_deformation
    double precision,dimension(sr_symm_voigt_dim) :: tmp_strain
    
    double precision :: step_size, tmp
    !
        RETURN_IF(info /= criSuccess, info = this%SDModule%readConfig(cnfunit))
        info = criErr_IORead
        !
        ! Read the module-specific config
        if (.not. readValue(cnfunit, n_steps)) return
        !
        RETURN_IF_WITH(n_steps < 1, info = criErr_BadArgs)
        RETURN_ON_WITH(allocate(this%steps(n_steps), stat=ierr), &
                       ierr /= 0, &
                       info = criErr_MemAlloc)
        !
        do i = 1, n_steps
            associate(step => this%steps(i))
                !
                ! Read the step definition and convert it into
                !  StrainDrivenStep object step
                !
                ! Read the step input type
                if (.not. readKeyword(cnfunit, deformation_type_names, deformation)) return
                select case(deformation)
                case(deformation_id)
                    if (.not. readValue(cnfunit, tmp_deformation)) return
                    step%deformation_rate%t = Vec9ToMat33(tmp_deformation)
                !
                case(strainmode_id)
                    if (.not. readValue(cnfunit, tmp_strain)) return
                    if (.not. readValue(cnfunit, step_size)) return
                    !
                    step%deformation_rate%t = Vec6ToMat33(tmp_strain)
                    ! Normalize the deformation
                    tmp = norm2(step%deformation_rate%t)
                    if (tmp < epsilon(0.D0)) then
                        write(display_unit,fmt=900) 'Norm of the strain mode must not be zero'
                        return
                    endif
                    step%deformation_rate%t = step%deformation_rate%t / tmp * step_size
                !   
                case(strain_id)
                    if (.not. readValue(cnfunit, tmp_strain)) return
                    step%deformation_rate%t = Vec6ToMat33(tmp_strain)
                !
                case default
                    return
                end select
                !
                if (.not. readValue(cnfunit, step%update_state)) return
                ! Read the incrementation type
                if (.not. readKeyword(cnfunit, incrementation_type_names, incrementation)) return
            end associate
        enddo
        info = criSuccess
    !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    !
    end function


    !> Run the simulation
    subroutine ADPModule_run(this, info)
    implicit none
    class(ADPModule),intent(inout)  :: this
    integer,intent(out)             :: info
    !
    type(ADPOutputData) :: output
    integer :: iounit
    !
        ! Introduce youself ;-)
        write(display_unit,'(A)') 'ADP, $Rev$'
        ! Open output file
        RETURN_IF(info /= criSuccess, info = this%openOutputFile('.adp',iounit))
        ! Run the simulation
        RETURN_IF(info /= criSuccess, info = this%execute(output))
        !
        ! Output the results
        RETURN_IF(info /= criSuccess, info = this%fileOutput(iounit, output, header=.true.))
    !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    !
    end subroutine
    
    
    
    !> Execute the simulation and gather output
    integer function ADPModule_execute(this, output) result(info)
    use altaySub
    use altayConfig
    implicit none
    class(ADPModule),intent(inout)  :: this
    type(ADPOutputData),intent(out) :: output
    integer :: i_step, n_steps, j, n_increments
    double precision :: volumetric_strain_fraction, volumetric_strain_norm
    ! Volumetric strain fraction that triggers a warning (0.1%)
    double precision,parameter :: volumetric_strain_fraction_threshold = 0.001
    type(SRTensor) :: step_strain, increment_strain, volumetric_strain
    type(DeformationRate) :: deformation_rate ! defined in altayMacroKinematic
    !
        info = criErr_BadArgs
        ALLOCATED_SIZE(n_steps, this%steps)
        if (n_steps < 1) return
        !
        ! Storage for the calculated output
        allocate(output%steps(n_steps))
        !
        ! Main loop over the steps
        do i_step = 1, n_steps
            associate(step => this%steps(i_step), &
                      step_output => output%steps(i_step))
                !
                ! Befing step
                !
                if (doLogging(criLogInfo, this%output%verbosity)) then
                    write(display_unit,800)
                    write(display_unit,fmt=300) i_step, n_steps
                    300 format(/, 'Step ', I0, ' out of ', I0, /)
                endif
                !
                ! Make the step traceless: decompose into volumetric strain rate
                ! and strain rate deviator
                volumetric_strain%t = trace(step%deformation_rate) / 3.D0 * unit_sr_tensor%t
                step_strain%t = step%deformation_rate%t - volumetric_strain%t
                if (norm2(step_strain%t) < epsilon(0.D0)) then
                    write(display_unit, 900) 'Norm of the deviatoric part of prescribed deformation is too small.'
                    return
                endif
                !
                if (doLogging(criLogInfo, this%output%verbosity)) then
                    ! Report the discrepancy if the substracted volumetric part is larger than a given
                    ! fraction of the total.
                    volumetric_strain_norm = norm2(volumetric_strain%t)
                    volumetric_strain_fraction = volumetric_strain_norm / norm2(step%deformation_rate%t)
                    if (volumetric_strain_fraction > volumetric_strain_fraction_threshold ) then
                        write(display_unit, 600) volumetric_strain_norm, volumetric_strain_fraction * 100
                        600 format(/, 'Note: volumetric deformation of magnitude ', G0.2, 1X, &
                                   ', which makes ', G0.2, 1X, &
                                   'percent of the prescribed deformation in this step, was substracted.', /)
                    endif
                endif
                !
                ! Calculate number of increments
                if (associated(step%substepping_config)) then
                    ! FIXME To be implemented
                    info = criErr_BadArgs
                    return
                else
                    n_increments = 1
                endif

                !
                ! Initialize AlTay structures
                RETURN_ON_WITH(call initStepData(n_increments, astate,info), &
                               info /= 0, &
                               info = criError)
                ! Set-up the substeps
                do j = 1, n_increments
                    ! FIXME Actual substepping: to be implemented
                    increment_strain%t = step_strain%t / dble(n_increments)
                    !
                    ! Set input data for AlTay
                    associate (input => astate%simulCalls(j)%input)
                          input%dgf = increment_strain%t
                          input%keep_texture = .not. step%update_state
                          input%keep_state = .not. step%update_state
                          input%full_model = .true.
                          input%do_output_init = .false.
                          input%do_output_final = this%output%outputRequest
                          call setStepType(input, acnf%model_id, info)
                    end associate
                enddo
                !
                ! Call the AlTay
                RETURN_ON_WITH(call runSteps(astate,info), &
                               info /= 0, &
                               info = criError)
                !
                ! Allocate storage for output
                allocate(step_output%increments(n_increments))
                !
                ! Get the results

                do j = 1, n_increments
                    associate (increment_output =>  step_output%increments(j))
                        increment_output%L%t = astate%simulCalls(j)%input%dgf
                        ! Let libaltay calculate the strain rates etc.
                        call Set_DeformationRate(increment_output%L%t, deformation_rate)
                        increment_output%D%t = deformation_rate%StrainRate
                        increment_output%S%t = astate%simulCalls(j)%output%stress_tensor
                        increment_output%vm_stress = astate%simulCalls(j)%output%effective_stress
                        ! D : S
                        increment_output%plastic_work_inc = sum(increment_output%D%t * increment_output%S%t)
                        !
                        increment_output%taylor_factor = astate%simulCalls(j)%output%taylor_factor
                    end associate
                enddo
                
            end associate
        enddo
    
    !
#define MSG_GROUP_ERRORS
#define MSG_GROUP_RULERS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS
    !
    end function


    !> Write out results to the output file
    integer function ADPModule_fileOutput(this, iounit, data_record, header) result(info)
    implicit none
    class(ADPModule),intent(in)                 :: this
    integer,intent(in)                          :: iounit !< Output IO unit
    type(ADPOutputData),intent(in),optional     :: data_record !< Data to be written out
    logical,intent(in),optional                 :: header !< Header to be written out
    !
    integer :: step, increment, ierr, n_steps, n_increments
    !      
    integer,parameter :: ncolumn_labels = 26, column_width = 18, short_column_width = 9
    character(len=column_width),dimension(ncolumn_labels) :: column_names = [character(len=column_width) :: &
        'step', 'increment', & ! 2 fields
        ! 'eps_vM', 'Pnorm','eps_xx', 'sigma_xx', 'S_xx','W','dotW', 'M-factor'&
        'L_11','L_22','L_33','L_12','L_23','L_31', 'L_21', 'L_32', 'L_13',  & ! 9 fields  (I)
        'D_11','D_22','D_33','D_12','D_23','D_13', & ! 6 fields  (I)
        'S_11','S_22','S_33','S_12','S_23','S_13', & ! 6 fields  (I)
        'S_vM', 'dW', 'M-factor' & ! 3 fields
        ]
    ! integer,dimension(ncolumn_labels),parameter :: column_widths = [ &
    !    short_column_width, short_column_width, & ! step, increment
    !    (column_width, i=1,ncolumn_labels-2) ]
    !
        info = criErr_BadArgs
        if (optionalDefault(header,.false.)) then
            ! Write column numbers
            info = writeColumnNumbers(iounit, size(column_names), [column_width] )
            if (info /= criSuccess) return
            ! Write column labels
            info = writeColumnNames(iounit, column_names, [column_width] )
            if (info /= criSuccess) return
        endif
        !
        if (present(data_record)) then
            ALLOCATED_SIZE(n_steps, data_record%steps)
            ! Write the data
            do step = 1, n_steps
                associate (step_output => data_record%steps(step))
                    ALLOCATED_SIZE(n_increments, step_output%increments)
                    do increment = 1, n_increments
                        associate(increment_output => step_output%increments(increment))
                            write(iounit,fmt=710,iostat=ierr) step, increment, &
                                                              Mat33ToVec9(increment_output%L%t), &
                                                              Mat33ToVec6(increment_output%D%t), &
                                                              Mat33ToVec6(increment_output%S%t), &
                                                              increment_output%vm_stress, &
                                                              increment_output%plastic_work_inc, &
                                                              increment_output%taylor_factor
                        end associate
                        if (ierr /= 0) return
                    enddo
                end associate
            enddo
            info = criSuccess
        endif

        !
        ! Formats for the output file
        710 format(1X, 2(I18,1X),24(E18.9,1X))
    !
    end function
    
    
    
end module
