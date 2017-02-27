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
    contains
        procedure,pass(this)        :: collect => StepOutput_collect
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
                select case(incrementation)
                case(none_incrementation_id)
                    allocate(step%substepping_config, &
                            source=FixedSubsteppingConfig([0.D0, 1.D0]))
                !
                case(auto_incrementation_id)
                    continue ! Nothing to be done here.
                !
                case(fixed_incrementation_id)
                    allocate(FixedSubsteppingConfig :: step%substepping_config)
                    if (associated(step%substepping_config)) info = step%substepping_config%readConfig(cnfunit)
                    if (info /= criSuccess) return
                !
                case default
                    info = criErr_BadArgs
                    return
                end select
                
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
        !
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
    integer :: i_step, n_steps, i_incr, n_increments
    double precision :: volumetric_strain_fraction, volumetric_strain_norm, step_strain_norm
    double precision :: increment_size, x, x_prev, increment_size_tot
    ! Volumetric strain fraction that triggers a warning (0.1%)
    double precision,parameter :: volumetric_strain_fraction_threshold = 0.001
    double precision,parameter :: auto_increment_norm = 0.02
    type(SRTensor) :: step_strain, increment_strain, volumetric_strain, step_strain_total
    class(FixedSubsteppingConfig),pointer :: ptr_substepping
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
                ! Begin step
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
                step_strain_norm = norm2(step_strain%t)
                if (step_strain_norm < epsilon(0.D0)) then
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
                ! Basic procedure: fixed number of increments
                !

                if (.not. associated(step%substepping_config)) then
                    ! Automatic incrementation to be used. Set it up:
                    if (step_strain_norm >= auto_increment_norm) then
                        n_increments = floor(step_strain_norm / auto_increment_norm)
                        allocate(step%substepping_config, &
                                 source=FixedSubsteppingConfig(UniformRange(0.D0, 1.D0, npoints=n_increments)))
                        if (doLogging(criLogInfo, this%output%verbosity)) then
                            write(display_unit, 601)
                            601 format('Note: automatic substepping will be used in this step.')
                        endif
                    else
                        allocate(step%substepping_config, &
                                 source=FixedSubsteppingConfig())
                    endif
                endif

                ! FIXME: awful stuff: we need dynamic cast event though there is no other 
                ! branch for non-fixed substepping. To be refactored. Separate class for
                ! fixed and non-fixed substepping (not configurations, but executors?)?
                select type(ptr_ss => step%substepping_config)
                !
                class is (FixedSubsteppingConfig)
                    ! Precondition
                    RETURN_IF_WITH(.not. associated(ptr_ss%ptr_range), info = criErr_BadArgs)
                    n_increments = ptr_ss%getNumberOfIncrements()
                    
                    if (doLogging(criLogInfo, this%output%verbosity)) then
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
                    ! Get the lower boundary, it should be zero.
                    RETURN_IF_WITH(.not. ptr_ss%ptr_range%next(x_prev), info = criErr_BadArgs)
                    
                    increment_size_tot = 0.D0
                    
                    ! Set-up the substeps
                    step_strain_total%t = 0.D0
                    i_incr = 0
                    do while(ptr_ss%ptr_range%next(x))
                        i_incr = i_incr + 1
                        increment_size = x - x_prev
                        increment_strain%t = increment_size * step_strain%t
                        step_strain_total%t = step_strain_total%t + increment_strain%t
                        x_prev = x
                        !
                        increment_size_tot = increment_size_tot + increment_size ! FIXME
                        write(*,*) increment_size_tot, norm2(increment_strain%t) ! FIXME
                        !
                        if (norm2(increment_strain%t) < epsilon(0.D0)) then
                            write(display_unit, 900) 'Norm of the prescribed incremental deformation is too small.'
                            return
                        endif
                        !
                        ! Set input data for AlTay
                        associate (input => astate%simulCalls(i_incr)%input)
                              input%dgf = increment_strain%t
                              input%keep_texture = .not. step%update_state
                              input%keep_state = .not. step%update_state
                              input%full_model = .true.
                              input%do_output_init = .false.
                              input%do_output_final = this%output%outputRequest
                              call setStepType(input, acnf%model_id, info)
                        end associate
                    enddo
                    ! Check if the loop had at least one iteration
                    RETURN_IF_WITH(i_incr == 0, info = criErr_BadArgs)
                    !
                    ! Call the AlTay
                    RETURN_ON_WITH(call runSteps(astate,info), &
                                   info /= 0, &
                                   info = criError)
                    !
                    RETURN_IF(info /= criSuccess, info = step_output%collect(n_increments))
                    
                class default
                    nullify(ptr_substepping)
                    info = criErr_BadCast
                    return
                endselect
                
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

    
    integer function StepOutput_collect(this, n_increments) result(info)
    use altayConfig
    use altayMacroKinematic
    implicit none
    class(StepOutput),intent(inout)     :: this
    integer,intent(in)                  :: n_increments
    !
    integer :: i, ierr
    type(DeformationRate) :: deformation_rate ! defined in altayMacroKinematic
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
