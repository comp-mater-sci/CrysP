#include "criMacros.fpp"

!
!> Yield locus calculations
module dmcYld
use definitions
use criAlgorithm
use criRange
use criMathUtils
use criUncomment, only: readValue
use dmcYLPResult
use dmcUtils
use commonConfig
use dmcStressDrivenModule
implicit none

    public YldModule
    private


    integer,parameter                               :: nbase = 3


    !> Class responsible for calculations of yield locus sections
    type,extends(StressDrivenModule) :: YldModule

        class(range_type),pointer                 :: ptr_theta_range

        class(range_type),pointer                 :: ptr_w_range

        double precision,dimension(sr_symm_voigt_dim,nbase) :: base_vectors = reshape( &
                                            [1., 0., 0., 0., 0., 0., & ! First base vector
                                             0., 1., 0., 0., 0., 0., & ! second base vector
                                             0., 0., 0., 0., 0., 0.], & ! offset vector (zeros)
                                            [sr_symm_voigt_dim,nbase])

        logical                                   :: do_scaling = .true.

        double precision,dimension(sr_symm_voigt_dim) :: scaling_vector = &
                                            [1., 0., 0., 0., 0., 0.]

        logical                                   :: normalizeSm = .false.

    contains

        !>@{ \name Interface methods of AbstractModule

        procedure,pass(this)    :: readConfig => YldModule_readConfig

        procedure,pass(this)    :: printConfig => YldModule_printConfig

        procedure,pass(this)    :: run => YldModule_run
        !>@}

    end type

    !> Data that describe a single yield locus point
    type :: yldResult
        double precision :: theta = 0.D0
        double precision :: w = 0.D0
        double precision :: scal_s = 0.D0
        double precision :: scal_s_rel = 0.D0
        double precision :: norm_sona = 0.D0
        double precision :: dotWonA = 0.D0
        type(pair_double) :: scal_s_rel_cart = pair_double(0.D0,0.D0)
        type(pair_double) :: normal_cart = pair_double(0.D0,0.D0)
        double precision :: beta = 0.D0
        double precision :: residual = 0.D0
    end type

contains


    integer function YldModule_readConfig(this,cnfunit) result(info)
    implicit none
    class(YldModule),intent(inout)            :: this
    integer,intent(in)                        :: cnfunit
    !
    integer :: i
    double precision :: norm
    logical :: normalize, use_default_settings
    !
        info = this%StressDrivenModule%ReadConfig(cnfunit)
        if (info /= VEF_OK) return
        ! Read parameters specific for the dmcYld program
        this%ptr_theta_range => rangeFromConfig(cnfunit,info)
        if ( (info /= VEF_OK) .or. (.not. associated(this%ptr_theta_range)) ) return
        if (.not. readValue(cnfunit, use_default_settings)) return
        if (use_default_settings) then
            ! use the defaults:
            allocate(uniformRange :: this%ptr_w_range)
        else
            info = VEF_ERROR
            this%base_vectors = 0.D0
            if (.not. readValue(cnfunit, normalize)) return
            do i=1,nbase
                if (.not. readValue(cnfunit, this%base_vectors(:,i))) return
                if (normalize) then
                    norm = norm2(this%base_vectors(:,i))
                    if (norm > 0.D0) this%base_vectors(:,i)  = this%base_vectors(:,i) / norm
                endif
            enddo
            !
            if (.not. readValue(cnfunit, this%normalizeSm)) return
            this%ptr_w_range => rangeFromConfig(cnfunit,info)
            if ( (info /= 0) .or. (.not. associated(this%ptr_w_range)) ) return
            if (.not. readValue(cnfunit, this%do_scaling)) return
            if (this%do_scaling) then
                if (.not. readValue(cnfunit, this%scaling_vector)) return
            endif
        endif
        !
        ! Override the requests for outputs:
        this%altay%output_config%nfile = 0   ! texture
        this%altay%output_config%npebp = 0   ! KOST1x state
        this%output%outputRequest = .false.       ! idem.
        !
        info = VEF_OK
    !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    end function


    integer function YldModule_printConfig(this, outunit) result (info)
    implicit none
    class(YldModule),intent(in)         :: this
    integer,intent(in)                  :: outunit
    !
    character(len=6),dimension(nbase)    :: veclabels = [ character(len=6) :: 'base','base','offset' ]
    integer :: i
    !
        info = this%StressDrivenModule%printConfig(outunit)
        if (info /= VEF_OK) return
        info = VEF_OK
    end function


    subroutine YldModule_run(this,info)
    implicit none
    class(YldModule),intent(inout)            :: this
    integer,intent(out)                       :: info
    !
    double precision                          :: theta, w
    double precision                          :: iunilen ! Inverse of the length of the deviatoric part of uniaxial tensile stress

    type(SRTensor)                            :: Sm, D
    type(YLPResult)                           :: ylp_result !< Results of the interative search
    double precision                          :: scal_s_rel
    type(yldResult),dimension(:),allocatable  :: yldRes
    double precision,dimension(sr_symm_voigt_dim) :: sigma_vector
    class(range_type),allocatable             :: theta_range
    !
    integer                 :: i,npoints, ofunit
    !
    integer :: posA, posB
    logical :: first_run, acceptable_point
    double precision,parameter :: beta = 0.D0
    !
        ! Super-class first
        RETURN_IF(info /= VEF_OK, call this%StressDrivenModule%run(info))
        !
        info = VEF_ERROR
        if (.not. (associated(this%ptr_theta_range) .and. associated(this%ptr_w_range)))  return
        !
        npoints = this%ptr_theta_range%size()
        if (npoints <= 0) then
                write(display_unit,fmt='(A)') 'Cannot run using empty range of theta angles.'
                return
        endif
        !
        ! Open the main output file
        RETURN_IF(info /= VEF_OK, info = this%openOutputFile('.xyld', ofunit))
        !
        ! Fix the configuration: no need for anything except for the stresses.
        this%ylp%evaluate_full_model = .false.
        !
        iunilen = 1.D0
        if (this%do_scaling) then
            Sm%t =  Vec6ToMat33(this%scaling_vector)
            if (norm2(Sm%t) < epsilon(0.D0)) then
                write(display_unit,fmt=900) 'Norm of the input stress for scaling cannot be zero'
                return
            endif
            ! Run the identification
            info = this%findSolution(Sm, D, ylp_result)
            if (info /= VEF_OK) then
                write(display_unit,fmt=900) 'Cannot find solution for the scaling stress'
                return
            endif

            if (abs(ylp_result%scal_s) < epsilon(0.D0)) then
                write(display_unit,fmt=900) 'Identification results in zero-length stress tensor.'
                return
            endif
            iunilen = 1.D0 / ylp_result%scal_s
        endif
        !
        allocate(yldRes(npoints))
        !
        first_run = .true.
        do while (this%ptr_w_range%next(w))
            ! Clone theta range
            allocate(theta_range, source=this%ptr_theta_range)
            !
            ! Loop over the range of theta angles
            i = 1
            do while (theta_range%next(theta))

                theta = deg2rad(theta)
                ! Combine the base vectors
                ! Note: explicit temporary sigma_vector prevents runtime warning about
                !       a temporary created in a call to Vec6ToMat33
                sigma_vector = this%base_vectors(:,1)*cos(theta) + this%base_vectors(:,2)*sin(theta) &
                                + w*this%base_vectors(:,3)
                Sm%t = Vec6ToMat33(sigma_vector)
                !
                info = this%findSolution(Sm, D, ylp_result, is_acceptable=acceptable_point)
                ! Consider what to do with unsuccessful search
                if (info == VEF_ERROR .or. ((info == VEF_FAIL) .and. (.not. acceptable_point))) then
                    write(display_unit,fmt=860) 'Cannot find solution, datapoint dropped'
                    cycle
                endif
                scal_s_rel = ylp_result%scal_s * iunilen
                
                yldRes(i) = yldResult(rad2deg(theta), w, ylp_result%scal_s, scal_s_rel, &
                                      norm2(ylp_result%vSonA), ylp_result%dotWonA, &
                                      pair_double(scal_s_rel * cos(theta), scal_s_rel * sin(theta)),&
                                      pair_double(0.D0,0.D0), beta, ylp_result%R)

                i = i + 1
            enddo
            deallocate(theta_range)
            !
            ! Post-process the results. Get the lower bound of container
            ! size and iterator - some points may have been dropped.
            npoints = min(size(yldRes), i-1)
            do i = 1, npoints
                ! Get the positions of the bracketing points:
                posA = merge(npoints-1,i - 1,i == 1)
                posB = merge(2,i + 1, i == npoints)
                ! write(display_unit,*) posA,i,posB
                call getNormalVector2D(yldRes(posA)%scal_s_rel_cart, yldRes(posB)%scal_s_rel_cart, &
                                       1.D0, yldRes(i)%normal_cart, yldRes(i)%beta)
                yldRes(i)%beta = rad2deg(yldRes(i)%beta)
            enddo
            !
            call writeYldResults(ofunit,yldRes(:npoints),info,write_header=first_run)
            first_run = .false.
        enddo
        !
        close(ofunit)
        info = VEF_OK
    !
    3200 format(28('-'))
    3201 format('Theta angle =',T20,F8.3)
    2500 format(1X, A10,    '|',4(A12,'|'))
    2501 format(1X, F10.3,  1X, 4(E12.5,1X))
    2510 format('|',10('-'),'|',4(12('-'),'|'))
    1600 format('Yield locus point at theta:',1X, F0.2, 1X, 'computed, residual error: ', E10.3)
    !
#define MSG_GROUP_RULERS
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS
    !
    end subroutine


    subroutine writeYldResults(ounit,res,info,write_header)
    implicit none
    integer,intent(in)                        :: ounit
    type(yldResult),dimension(:),intent(in)   :: res
    integer,intent(out)                       :: info
    logical,intent(in),optional               :: write_header
    !
    integer :: i,ierr
    integer,parameter :: column_width = 18, ncolumns = 12
    character(len=column_width),dimension(ncolumns),parameter  :: column_labels = [ character(len=column_width) :: &
        'theta', 'w', 'sigma', 'sigma_scaled', 'S','dotW', 'sigma_x', 'sigma_y', 'dsigma_x', 'dsigma_y', 'beta', 'residual']
    !
        info = VEF_ERROR
        ! Write the header
        if (optionalDefault(write_header,.false.)) then
            write(ounit,fmt=700,iostat=ierr) (centered(i,column_width),  i = 1, ncolumns)
            if (ierr /= 0) return
            write(ounit,fmt=701,iostat=ierr) (centered(column_labels(i)),i = 1, ncolumns)
            if (ierr /= 0) return
        endif
        !
        do i = 1, size(res)
            write(ounit,fmt=710,iostat=ierr) res(i)
            if (ierr /= 0) exit
        enddo
        write(ounit,fmt=720)
        if (ierr == 0) info = VEF_OK
        !
        ! Formats for output file
        700 format('#',12(A15,1X))
        701 format(1X, 12(A15,1X))
        710 format(1X, 12(E15.8,1X),4(F15.8,1X))
        720 format(/) ! Double empty line
    !
    end subroutine

end module
