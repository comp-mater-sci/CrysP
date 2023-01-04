!> Types and procedures that control incrementation in stress-driven
!> evolution of material state.
module dmcIncrementationControl
use criErrcodes
use criLinearMap, only: MapItem
use alamYLPConstants
implicit none

    integer,parameter :: scalingNone = 0, &
                         scalingStrainTensor = 1, &
                         scalingStrainTensorComponent = 2, &
                         scalingPlasticWork = 3, &
                         scalingStrainTensorIncrement = 4

    integer,parameter :: nscaling_types = 4
    type(MapItem),dimension(nscaling_types) :: scaling_type_names = [&
                                                MapItem('StrainTensor', scalingStrainTensor), &
                                                MapItem('StrainTensorIncrement', scalingStrainTensorIncrement), &
                                                MapItem('StrainTensorComponent', scalingStrainTensorComponent), &
                                                MapItem('PlasticWork', scalingPlasticWork)]

    integer,parameter :: incrementFixed = 0, &
                         incrementAuto = 1

    !> Default maximal number of increments along a deformation path.
    integer,parameter :: dmcIC_max_increments = 1000

    !> Basic control variables used in evolution of material state.
    type :: IncrementationControlVariables

        integer :: increment = 0

        !> Plastic work in the current increment
        double precision    :: plastic_work_inc = 0.D0

        !> Plastic work in the current step
        double precision    :: plastic_work_step = 0.D0

        !> Total plastic work
        double precision    :: plastic_work_total = 0.D0

        !> Increment of plastic strain
        double precision,dimension(alamEval_vSD_dim)    :: vP_inc = 0.D0

        !> Total plastic strain in the current step
        double precision,dimension(alamEval_vSD_dim)    :: vP_step = 0.D0

        !> Total plastic strain:
        double precision,dimension(alamEval_vSD_dim)    :: vP_total = 0.D0


        !> Sum of absolute plastic strain increments:
        !> \f[
        !>    vP_{abs} = \sum | vP_{inc} |
        !> \f]
        double precision,dimension(alamEval_vSD_dim)    :: vP_abs

    end type


    !>
    type,extends(IncrementationControlVariables) :: IncrementationControl

    contains

        procedure,pass(this)        :: initStep => IncrementationControl_initStep

        procedure,pass(this)        :: update => IncrementationControl_update

    end type


    !> Basic settings for incrementation control.
    type :: IncrementationControlSettings

        integer         :: scaling_type = scalingNone

        integer         :: incrementation_type = incrementFixed

        !> Maximal number of increments. If set to positive value, it limits
        !> the number of increments independently of other criteria.
        !> It is ignored if set to a negative value.
        integer         :: max_increment_count = dmcIC_max_increments

        double precision :: increment_size = 0.D0

        double precision :: step_size = 0.D0

        !> If scaling_type StrainTensorComponent is used, this contains
        !> the index of the tensor component of interest in Voigt notation.
        !> The default corresponds to X_11 in SR tensor X.
        integer         :: selected_tensor_component = 1

    end type


contains

    subroutine IncrementationControl_update(this, vDe, vSe, info)
    class(IncrementationControl),intent(inout)      :: this
    double precision,dimension(alamEval_vSD_dim),intent(in) :: vDe, vSe
    integer,intent(out)                             :: info
    !
        ! Plastic work in the current increment
        this%plastic_work_inc = dot_product(vDe,vSe)
        ! Plastic work in the current step
        this%plastic_work_step = this%plastic_work_step + this%plastic_work_inc
        ! Total plastic work
        this%plastic_work_total = this%plastic_work_total + this%plastic_work_inc
        ! Increment of plastic strain
        this%vP_inc = vDe
        ! Total plastic strain in the current step
        this%vP_step = this%vP_step + this%vP_inc
        ! Total plastic strain:
        this%vP_total = this%vP_total + this%vP_inc
        ! Sum of absolute plastic strain increments:
        this%vP_abs = this%vP_abs + abs(this%vP_inc)
        this%increment = this%increment  + 1
        info = criSuccess
    !
    end subroutine


    !> Clean increment-wise and step-wise control fields.
    !>
    !> All fields except for the ones representing 'totals' are set to zero.
    subroutine IncrementationControl_initStep(this, info)
    class(IncrementationControl),intent(inout)      :: this
    integer,intent(out)                             :: info
    !
        ! Plastic work in the current increment
        this%plastic_work_inc = 0.D0
        ! Plastic work in the current step
        this%plastic_work_step = 0.D0
        ! Increment of plastic strain
        this%vP_inc = 0.D0
        ! Total plastic strain in the current step
        this%vP_step = 0.D0
        this%increment = 0
        info = criSuccess
    !
    end subroutine

    !> Read IncrementationControlSettings from configuration file
    subroutine IncrementationControlSettings_read(this, cnfunit, info, allowed)
    use criConfigReader
    use criUncomment
    type(IncrementationControlSettings),intent(out)   :: this
    integer,intent(in)                                :: cnfunit
    integer,intent(out)                               :: info
    integer,dimension(:),optional                     :: allowed
    !
    integer :: id, i
    logical :: is_allowed
    !
        info = criErr_IORead
        if (.not. readKeyword(cnfunit, scaling_type_names, id)) return
        ! Check for additional constraints on the scaling type
        if (present(allowed)) then
            ! check: if id not in allowed: return
            info = criErr_BadArgs
            is_allowed = .false.
            do i = 1, size(allowed)
                if (id == allowed(i)) then
                    is_allowed = .true.
                    exit
                endif
            enddo
            if (.not. is_allowed) return
        endif
        this%scaling_type = id
        !
        info = criErr_IORead
        if (.not. readValue(cnfunit, this%step_size)) return
        if (.not. readValue(cnfunit, this%increment_size)) return
        info = criSuccess
    !
    end subroutine

end module
