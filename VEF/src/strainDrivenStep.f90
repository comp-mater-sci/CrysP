#include "criMacros.fpp"

!> Strain-(rate) driven step
module dmcStrainDrivenStep
use criRange
use criMathUtils
use definitions
use dmcUtils, only: display_unit
use dmcSubsteppingConfig
implicit none



    public :: StrainDrivenStep, StrainDrivenStepConfig, StrainDrivenFixedStep
    public :: StepOutput, IncrementOutput
    private


    !> Base class for deformation rate driven steps
    type :: StrainDrivenStepConfig
        type(SRTensor)  :: deformation_rate
        logical         :: update_state = .false.
        logical         :: output_state = .false.
    end type


    !> A strain-(rate) driven step
    type :: StrainDrivenStep

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

        type(SRTensor)  :: L !< Velocity gradient

        type(SRTensor)  :: D !< Rate of deformation tensor (symmetric part of L) (strain rate)

        type(SRTensor)  :: O !< Spin tensor (antisymmetric part of L)

        type(SRTensor)  :: A !< Strain mode

        type(SRTensor)  :: S !< Deviatoric stress tensor

        double precision :: vm_strain_begin = 0.D0 !< Von Mises strain at the beginning of the increment

        double precision :: vm_strain_end = 0.D0 !< Von Mises strain at the end of the increment

        double precision :: vm_stress = 0.D0 !< Von Mises equivalent stress

        double precision :: plastic_work_inc = 0.D0 !< Plastic work during the increment, i.e. dotW = (D : S)

        double precision :: taylor_factor = 0.D0

        double precision :: plastic_slip_tot = 0.D0 !< total accumulated plastic slip

        double precision :: vMeqStrainRate = 0.D0 !< von Mises equivalent strain rate (= sqrt(2/3)*||D||)

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
    class(StrainDrivenStep),intent(inout)   :: this
    !
    double precision :: step_strain_norm, volumetric_strain_norm, volumetric_strain_fraction
    !
    ! Volumetric strain fraction that triggers a warning (0.1%)
    double precision,parameter :: volumetric_strain_fraction_threshold = 0.001
    double precision,parameter :: auto_increment_norm = 0.02
    !
        info = VEF_ERROR
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
        end associate
        info = VEF_OK
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
    !> It is a placeholder method, it always returns VEF_ERROR.
    integer function StrainDrivenStep_execute(this, step_output) result(info)
    class(StrainDrivenStep),intent(inout)   :: this
    class(StepOutput),intent(out)           :: step_output
    !
        info = VEF_ERROR
    !
    end function

    !> Read configuration of StrainDrivenStep from config IO unit
    !>
    !> It is a placeholder method, it always returns VEF_OK
    integer function StrainDrivenStep_readConfig(this, cnfunit) result(info)
    class(StrainDrivenStep),intent(inout)   :: this
    integer,intent(in)                      :: cnfunit !< IO unit
    !
        info = VEF_OK
    !
    end function


    !> Set up strain driven fixed step.
    !>
    !> Returns VEF_OK on success.
    integer function StrainDrivenFixedStep_setUp(this) result(info)
    class(StrainDrivenFixedStep),intent(inout)   :: this
    !
    double precision :: step_strain_norm
    integer :: n_increments
    ! Volumetric strain fraction that triggers a warning (0.1%)
    double precision,parameter :: volumetric_strain_fraction_threshold = 0.001
    double precision,parameter :: auto_increment_norm = 0.02
    !
        info = this%StrainDrivenStep%setUp()
        if (info /= VEF_OK) return
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
    !> Returns VEF_OK on success.
    integer function StrainDrivenFixedStep_execute(this, step_output) result(info)
    use altaySub
    use altayConfig
    class(StrainDrivenFixedStep),intent(inout)  :: this
    class(StepOutput),intent(out)               :: step_output
    !
    integer :: n_increments, i_incr
    double precision :: increment_size, x, x_prev, increment_size_tot
    type(SRTensor) :: increment_strain,  step_strain_total
    !
        ! Precondition
        RETURN_IF_WITH(.not. associated(this%substepping_config), info = VEF_ERROR)
        RETURN_IF_WITH(.not. associated(this%substepping_config%ptr_range), info = VEF_ERROR)
        !
        n_increments = this%substepping_config%getNumberOfIncrements()

        ! Initialize AlTay structures
        RETURN_ON_WITH(call initStepData(n_increments, astate,info),info /= 0,info = VEF_ERROR)
        !
        associate(increment_range => this%substepping_config%ptr_range)
            !
            ! Get the lower boundary, it should be zero.
            RETURN_IF_WITH(.not. increment_range%next(x_prev), info = VEF_ERROR)

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
                    info = VEF_ERROR
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
                end associate
            enddo
            ! Check if the loop had at least one iteration
            RETURN_IF_WITH(i_incr == 0, info = VEF_ERROR)
        end associate
        !
        ! Call the AlTay
        RETURN_ON_WITH(call runSteps(astate,info), info /= 0, info = VEF_ERROR)
        !
        RETURN_IF(info /= VEF_OK, info = step_output%collect(n_increments))
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
    class(StepOutput),intent(inout)     :: this
    integer,intent(in)                  :: n_increments
    !
    integer :: i, ierr, n_simulcalls
    type(DeformationRate) :: deformation_rate ! defined in altayMacroKinematic
    !
        ! Check if the input and the state of libaltay correspond.
        ALLOCATED_SIZE(n_simulcalls, astate%simulCalls)
        RETURN_IF(n_simulcalls < n_increments .or. n_simulcalls /= astate%nSimulCalls, info = VEF_ERROR)
        !
        ! Allocate storage for output
        RETURN_ON_WITH(allocate(this%increments(n_increments), stat=ierr), ierr /= 0, info = VEF_ERROR)
        !
        ! collect the results
        do i = 1, n_increments
            associate (increment_output =>  this%increments(i), &
                       altay_state => astate%simulCalls(i), &
                       altay_output => astate%simulCalls(i)%output)   ! HGH: originally altay_output => altay_state%output
                !
                increment_output%L%t = altay_state%input%dgf
                ! Let libaltay calculate the strain rates etc. from velocity gradient
                call Set_DeformationRate(increment_output%L%t, deformation_rate)
                increment_output%D%t = deformation_rate%StrainRate
                increment_output%O%t = deformation_rate%Spin
                increment_output%A%t = deformation_rate%StrainMode
                increment_output%S%t = altay_output%stress_tensor
                increment_output%vm_strain_begin = altay_output%effective_macro_strain_tot
                increment_output%vm_strain_end = altay_output%effective_macro_strain_tot_end
                increment_output%vm_stress = altay_output%effective_stress
                ! D : S
                increment_output%plastic_work_inc = sum(increment_output%D%t * increment_output%S%t)
                !
                increment_output%taylor_factor = altay_output%taylor_factor
                increment_output%plastic_slip_tot = altay_output%homogenised_slip_tot
                increment_output%vMeqStrainRate = deformation_rate%vMeqStrainRate

            end associate
        enddo
        info = VEF_OK
    !
    end function

end module
