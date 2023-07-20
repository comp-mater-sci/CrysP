#include "criMacros.fpp"

!> Arbitrary Deformation Path strain-(rate) driven simulations
module dmcADP
use definitions
use criConfigReader
use criMathUtils
use altayMacroKinematic, only: DeformationRate, Set_DeformationRate
use dmcDeformationDrivenModule
use dmcResultFileOutput
use dmcStrainDrivenStep
implicit none

    public :: ADPModule
    private

    !> Arbitrary Strain Mode (extends DeformationDrivenModule by 4 procedures)
    type,extends(DeformationDrivenModule) :: ADPModule
    contains ! type-bound procedures; pass(this) passes object itself, through which procedure referenced, as first argument to procedure
        procedure,pass(this) :: readConfig => ADPModule_readConfig
        procedure,pass(this) :: run => ADPModule_run
        procedure,pass(this) :: fileOutput => ADPModule_fileOutput
    end type

    !> Outputs collected by the simulation run
    type :: ADPOutputData
        type(StepOutput),dimension(:),allocatable :: steps
    end type

contains

    !> Read configuration from IO unit (type-bound function)
    integer function ADPModule_readConfig(this, cnfunit) result(info) ! call with 1 argument (cnfunit) when referenced through object
    implicit none
    class(ADPModule),intent(inout)   :: this !< passed implicitly
    integer,intent(in)              :: cnfunit !< IO input unit; pass explicitly
    !
    integer     :: n_steps, ierr, i, deformation, incrementation
    !
    integer,parameter :: n_incrementation_types = 3 !< number of supported incrementation types
    integer,parameter :: none_incrementation_id = 0, auto_incrementation_id = 1,  fixed_incrementation_id = 2
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
    real(DP),dimension(sr_voigt_dim) :: tmp_deformation
    real(DP),dimension(sr_symm_voigt_dim) :: tmp_strain

    real(DP) :: step_size, tmp
    type(StrainDrivenStepConfig) :: tmp_step_config
    type(SRTensor) :: tmp_deformation_rate
        !
        ! Read generic configuration section (output settings, AlTay (texture, microstructure, hardening), solver settings
        RETURN_IF(info /= VEF_OK, info = this%DeformationDrivenModule%readConfig(cnfunit))
        info = VEF_ERROR
        !
        ! Read the module-specific config
        if (.not. readValue(cnfunit, n_steps)) return
        !
        RETURN_IF_WITH(n_steps < 1, info = VEF_ERROR)

        RETURN_ON_WITH(allocate(this%steps(n_steps), stat=ierr), ierr /= 0, info = VEF_ERROR)
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
                    if (info /= VEF_OK) return
                !
                case default
                    info = VEF_ERROR
                    return
                end select
                !
                ! Phase 2: set the config
                step%step%config = tmp_step_config

            end associate
        enddo
        info = VEF_OK
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
        ! Super-class first
        RETURN_IF(info /= VEF_OK, call this%DeformationDrivenModule%run(info))
        !
        ! Open output file
        RETURN_IF(info /= VEF_OK, info = this%openOutputFile('.adp',iounit))
        !
        ! Run the simulation
        info = VEF_ERROR
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
                ! Set up the step
                RETURN_IF(info /= VEF_OK, info = step%setUp())
                ! Execute the step
                RETURN_IF(info /= VEF_OK, info = step%execute(step_output))
                ! Output the results
                RETURN_IF(info /= VEF_OK, info = this%fileOutput(iounit, output, header=(i_step==1), step_id=i_step))
                if (info /= VEF_OK) return
            end associate
        enddo
    !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    !
    end subroutine



    !> Write out results to the output file
    integer function ADPModule_fileOutput(this, iounit, output, header, step_id) result(info)
    implicit none
    class(ADPModule),intent(in)                 :: this
    integer,intent(in)                          :: iounit !< Output IO unit
    type(ADPOutputData),intent(in),optional     :: output !< Data to be written out
    logical,intent(in),optional                 :: header !< Header to be written out
    integer,intent(in),optional                 :: step_id
    !
    integer :: step, increment, ierr, n_steps, first_step, last_step, n_increments
    !
    integer,parameter :: ncolumn_labels = 2+9+3*6+3+7, column_width = 18
    character(len=column_width),dimension(ncolumn_labels) :: column_names = [character(len=column_width) :: &
        'step', 'increment', & ! 2 fields
        'L_11','L_22','L_33','L_12','L_23','L_31','L_21','L_32','L_13',  & ! 9 fields  (I)
        'D_11','D_22','D_33','D_12','D_23','D_13', & ! 6 fields  (I)
        'O_12','O_23','O_13', & ! 3 fields  (I)
        'A_11','A_22','A_33','A_12','A_23','A_13', & ! 6 fields  (I)
        'S_11','S_22','S_33','S_12','S_23','S_13', & ! 6 fields  (I)
        'eps_vM_begin', 'eps_vM_end', 'D_vM', 'S_vM', 'dW', 'M-factor', 'gamma' & ! 7 fields
        ]
        !
        info = VEF_ERROR
        if (optionalDefault(header,.false.)) then
            ! Write column numbers
            info = writeColumnNumbers(iounit, size(column_names), [column_width] )
            if (info /= VEF_OK) return
            ! Write column labels
            info = writeColumnNames(iounit, column_names, [column_width] )
            if (info /= VEF_OK) return
        endif
        !
        if (present(output)) then
            ALLOCATED_SIZE(n_steps, output%steps)
            first_step = optionalDefault(step_id, 1)
            last_step = optionalDefault(step_id, n_steps)
            RETURN_IF_WITH(first_step < 1 .or. last_step > n_steps, info = VEF_ERROR)
            !
            info = VEF_ERROR
            !
            ! Write the data
            do step = first_step, last_step
                associate(step_output => output%steps(step))
                    !
                    ALLOCATED_SIZE(n_increments, step_output%increments)
                    !
                    do increment = 1, n_increments
                          associate(v => step_output%increments(increment))
                              write(iounit,fmt=710,iostat=ierr) &
                                          step, increment, &            ! 2 fields
                                          Mat33ToVec9(v%L%t), &         ! 9 fields: velocity gradient
                                          Mat33ToVec6(v%D%t), &         ! 6 fields: rate for deformation tensor (strain rate)
                                          Mat33ToVec3(v%O%t), &         ! 3 fields: spin tensor
                                          Mat33ToVec6(v%A%t), &         ! 6 fields: strain mode
                                          Mat33ToVec6(v%S%t), &         ! 6 fields: deviatoric stress
                                          v%vm_strain_begin, &
                                          v%vm_strain_end, &
                                          v%vMeqStrainRate, &
                                          v%vm_stress, &
                                          v%plastic_work_inc, &
                                          v%taylor_factor, &
                                          v%plastic_slip_tot
                          end associate
                          if (ierr /= 0) return
                    enddo
                end associate
            enddo
            info = VEF_OK
        endif

        !
        ! Formats for the output file
        710 format(1X, 2(I18,1X),39(ES18.9E3,1X))
    !
    end function



end module
