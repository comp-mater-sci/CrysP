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
use dmcStrainDrivenStep
implicit none


    !> Arbitrary Strain Mode
    type,extends(SDModule) :: ADPModule
    contains
        
        !>@{ \name Interface methods of AbstractModule
        
        procedure,pass(this) :: printConfig => ADPModule_printConfig
        
        procedure,pass(this) :: readConfig => ADPModule_readConfig
        
        procedure,pass(this) :: run => ADPModule_run
        
        !>@}

        procedure,pass(this) :: fileOutput => ADPModule_fileOutput
        
    end type

    !> Outputs collected by the simulation run
    type :: ADPOutputData
        type(StepOutput),dimension(:),allocatable :: steps
    end type
    
contains


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
    integer,parameter :: n_incrementation_types = 3
    integer,parameter :: none_incrementation_id = 0, auto_incrementation_id = 1, fixed_incrementation_id = 2
    type(MapItem),dimension(n_incrementation_types) :: incrementation_type_names = [&
        MapItem('none', none_incrementation_id), &
        MapItem('auto', auto_incrementation_id), &
        MapItem('fixed', fixed_incrementation_id)]
    !
    integer,parameter :: n_deformation_types = 3
    integer,parameter :: deformation_id = 1, strainmode_id = 2, strain_id = 3
    type(MapItem),dimension(n_deformation_types) :: deformation_type_names = [&
        MapItem('deformation', deformation_id),&
        MapItem('strainmode', strainmode_id),&
        MapItem('strain', strain_id)]
    !
    double precision,dimension(sr_voigt_dim) :: tmp_deformation
    double precision,dimension(sr_symm_voigt_dim) :: tmp_strain
    
    double precision :: step_size, tmp
    type(StrainDrivenStepConfig) :: tmp_step_config
    type(SRTensor) :: tmp_deformation_rate
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
                    tmp_deformation_rate%t = Vec9ToMat33(tmp_deformation)
                !
                case(strainmode_id)
                    if (.not. readValue(cnfunit, tmp_strain)) return
                    if (.not. readValue(cnfunit, step_size)) return
                    !
                    tmp_deformation_rate%t = Vec6ToMat33(tmp_strain)
                    ! Normalize the deformation
                    tmp = norm2(tmp_deformation_rate%t)
                    if (tmp < epsilon(0.D0)) then
                        write(display_unit,fmt=900) 'Norm of the strain mode must not be zero'
                        return
                    endif
                    tmp_deformation_rate%t = tmp_deformation_rate%t / tmp * step_size
                !   
                case(strain_id)
                    if (.not. readValue(cnfunit, tmp_strain)) return
                    tmp_deformation_rate%t = Vec6ToMat33(tmp_strain)
                !
                case default
                    return
                end select
                !
                if (.not. readValue(cnfunit, tmp_step_config%update_state)) return
                !
                tmp_step_config%deformation_rate = tmp_deformation_rate
                tmp_step_config%output_state = this%output%outputRequest
                !
                ! Read the incrementation type
                if (.not. readKeyword(cnfunit, incrementation_type_names, incrementation)) return
                !
                ! Phase 1: allocate right step type
                select case(incrementation)
                case(none_incrementation_id)
                    ! Allocate step with one fixed increment
                    allocate(step%step, source=StrainDrivenFixedStep(1))
                !
                case(auto_incrementation_id)
                    allocate(StrainDrivenFixedStep :: step%step)

                case(fixed_incrementation_id)
                    allocate(StrainDrivenFixedStep :: step%step)
                    info = step%step%readConfig(cnfunit)
                    if (info /= criSuccess) return
                !
                case default
                    info = criErr_BadArgs
                    return
                end select
                !
                ! Phase 2: set the config
                step%step%config = tmp_step_config
                step%step%log = logData(this%output%verbosity, display_unit)
                
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
    integer :: iounit, i_step, n_steps
    !
        ! Introduce youself ;-)
        write(display_unit,'(A)') 'ADP, $Rev$'
        ! Open output file
        RETURN_IF(info /= criSuccess, info = this%openOutputFile('.adp',iounit))
        !
        ! Run the simulation
        info = criErr_BadArgs
        ALLOCATED_SIZE(n_steps, this%steps)
        if (n_steps < 1) return
        !
        ! Storage for the calculated output
        allocate(output%steps(n_steps))
        !
        ! Main loop over the steps
        do i_step = 1, n_steps
            associate(step => this%steps(i_step)%step, &
                      step_output => output%steps(i_step))
                !
                ! Begin step
                !
                if (doLogging(criLogInfo, this%output%verbosity)) then
                    write(display_unit,800)
                    write(display_unit,fmt=300) i_step, n_steps
                    300 format(/, 'Step ', I0, ' out of ', I0, /)
                endif
                ! Set up the step
                RETURN_IF(info /= criSuccess, info = step%setUp())
                ! Execute the step
                RETURN_IF(info /= criSuccess, info = step%execute(step_output))
                
                ! Output the results
                RETURN_IF(info /= criSuccess, info = this%fileOutput(iounit, output, header=(i_step==1), step_id=i_step))
                
                if (info /= criSuccess) return
                
            end associate
        enddo
        !
    !
#define MSG_GROUP_ERRORS
#define MSG_GROUP_RULERS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS
    !
    end subroutine



    !> Write out results to the output file
    integer function ADPModule_fileOutput(this, iounit, data_record, header, step_id) result(info)
    implicit none
    class(ADPModule),intent(in)                 :: this
    integer,intent(in)                          :: iounit !< Output IO unit
    type(ADPOutputData),intent(in),optional     :: data_record !< Data to be written out
    logical,intent(in),optional                 :: header !< Header to be written out
    integer,intent(in),optional                 :: step_id
    !
    integer :: step, increment, ierr, n_steps, first_step, last_step, n_increments 
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
            first_step = optionalDefault(step_id, 1)
            last_step = optionalDefault(step_id, n_steps)
            RETURN_IF_WITH(first_step < 1 .or. last_step > n_steps, info = criErr_BadArgs)
            ! Write the data
            do step = first_step, last_step
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
