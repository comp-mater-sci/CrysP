#include "criMacros.fpp"

!
!> Yield locus calculations
module dmcYld
    use utils
    use criRange
    use criUncomment, only: readValue
    use dmcYLPResult
    use commonConfig
    use dmcStressDrivenModule
    use commonUtils
    
    implicit none

    public YldModule
    private


    integer, parameter                               :: nbase = 3


    !> Class responsible for calculations of yield locus sections
    type, extends(StressDrivenModule):: YldModule

        class(range_type), pointer                 :: ptr_theta_range

        class(range_type), pointer                 :: ptr_w_range

        real(DP), dimension(6, nbase):: base_vectors = real(reshape( &
                                            [1, 0, 0, 0, 0, 0, & ! First base vector
                                             0, 1, 0, 0, 0, 0, & ! second base vector
                                             0, 0, 0, 0, 0, 0], & ! offset vector (zeros)
                                            [6, nbase]), DP)

        logical                                   :: do_scaling = .true.

        real(DP), dimension(6):: scaling_vector = [1._DP, 0._DP, 0._DP, 0._DP, 0._DP, 0._DP]

        logical                                   :: normalizeSm = .false.

    contains

        !>@{ \name Interface methods of AbstractModule

        procedure, pass(this)    :: readConfig => YldModule_readConfig

        procedure, pass(this)    :: run => YldModule_run
        !>@}

    end type

    !> Data that describe a single yield locus point
    type:: yldResult
        real(DP):: theta = 0.D0
        real(DP):: w = 0.D0
        real(DP):: scal_s = 0.D0
        real(DP):: scal_s_rel = 0.D0
        real(DP):: norm_sona = 0.D0
        real(DP):: dotWonA = 0.D0
        real(DP), dimension(2) ::   scal_s_rel_cart = 0._DP, &
                                    normal_cart = 0._DP
        real(DP):: beta = 0.D0
        real(DP):: residual = 0.D0
    end type

contains


    integer function YldModule_readConfig(this, cnfunit) result(info)
    implicit none
    class(YldModule), intent(inout)            :: this
    integer, intent(in)                        :: cnfunit
    !
    integer:: i
    real(DP):: norm
    logical:: normalize, use_default_settings
    !
        info = this%StressDrivenModule%ReadConfig(cnfunit)
        if (info /= VEF_OK) return
        ! Read parameters specific for the dmcYld program
        this%ptr_theta_range => rangeFromConfig(cnfunit, info)
        if ( (info /= VEF_OK) .or. (.not. associated(this%ptr_theta_range)) ) return
        if (.not. readValue(cnfunit, use_default_settings)) return
        if (use_default_settings) then
            ! use the defaults:
            allocate(uniformRange:: this%ptr_w_range)
        else
            info = VEF_ERROR
            this%base_vectors = 0.D0
            if (.not. readValue(cnfunit, normalize)) return
            do i = 1, nbase
                if (.not. readValue(cnfunit, this%base_vectors(:,i))) return
                if (normalize) then
                    norm = norm2(this%base_vectors(:,i))
                    if (norm > 0.D0) this%base_vectors(:,i)  = this%base_vectors(:,i) / norm
                endif
            enddo
            !
            if (.not. readValue(cnfunit, this%normalizeSm)) return
            this%ptr_w_range => rangeFromConfig(cnfunit, info)
            if ( (info /= 0) .or. (.not. associated(this%ptr_w_range)) ) return
            if (.not. readValue(cnfunit, this%do_scaling)) return
            if (this%do_scaling) then
                if (.not. readValue(cnfunit, this%scaling_vector)) return
            endif
        endif
        !
        ! Override the requests for outputs:
        this%altay%output_config%nfile = 0   ! texture
        this%output%outputRequest = .false.       ! idem.
        !
        info = VEF_OK
    !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    end function


    subroutine YldModule_run(this, info)
        class(YldModule), intent(inout)            :: this
        integer, intent(out)                       :: info
        real(DP):: theta, &
                    w,  &
                    iunilen, &
                    Sm(3, 3), &
                    D(3, 3), &
                    scal_s_rel, &
                    sigma_vector(6)
        type(YLPResult)                           :: ylp_result !< Results of the interative search
        type(yldResult), dimension(:), allocatable  :: yldRes
        class(range_type), allocatable             :: theta_range
        integer                 :: i, npoints, ofunit
        integer:: posA, posB
        logical:: first_run, acceptable_point
        real(DP), parameter:: beta = 0._DP

        ! Super-class first
        RETURN_IF(info /= VEF_OK, call this%StressDrivenModule%run(info))
        !
        info = VEF_ERROR
        if (.not. (associated(this%ptr_theta_range) .and. associated(this%ptr_w_range)))  return
        !
        npoints = this%ptr_theta_range%size()
        if (npoints <= 0) then
                write(display_unit, fmt='(A)') 'Cannot run using empty range of theta angles.'
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
            Sm =  convert_voigt(this%scaling_vector)
            if (norm2(Sm) < epsilon(0.D0)) then
                write(display_unit, fmt = 900) 'Norm of the input stress for scaling cannot be zero'
                return
            endif
            ! Run the identification
            info = this%findSolution(Sm, D, ylp_result)
            if (info /= VEF_OK) then
                write(display_unit, fmt = 900) 'Cannot find solution for the scaling stress'
                return
            endif

            if (abs(ylp_result%scal_s) < epsilon(0.D0)) then
                write(display_unit, fmt = 900) 'Identification results in zero-length stress tensor.'
                return
            endif
            iunilen = 1.D0/ylp_result%scal_s
        endif
        !
        allocate(yldRes(npoints))
        !
        first_run = .true.
        do while (this%ptr_w_range%next(w))
            ! Clone theta range
            allocate(theta_range, source = this%ptr_theta_range)
            !
            ! Loop over the range of theta angles
            i = 1
            do while (theta_range%next(theta))

                theta = theta/RAD_TO_DEG
                ! Combine the base vectors
                ! Note: explicit temporary sigma_vector prevents runtime warning about
                !       a temporary created in a call to convert_voigt
                sigma_vector = this%base_vectors(:,1)*cos(theta) + this%base_vectors(:,2)*sin(theta) &
                                + w*this%base_vectors(:,3)
                Sm = convert_voigt(sigma_vector)
                !
                info = this%findSolution(Sm, D, ylp_result, is_acceptable = acceptable_point)
                ! Consider what to do with unsuccessful search
                if (info == VEF_ERROR .or. ((info == VEF_FAIL) .and. (.not. acceptable_point))) then
                    write(display_unit, fmt = 860) 'Cannot find solution, datapoint dropped'
                    cycle
                endif
                scal_s_rel = ylp_result%scal_s*iunilen

                yldRes(i) = yldResult(theta*RAD_TO_DEG, w, ylp_result%scal_s, scal_s_rel, &
                                      norm2(ylp_result%vSonA), ylp_result%dotWonA, &
                                      [scal_s_rel*cos(theta), scal_s_rel*sin(theta)], &
                                      [0._DP, 0._DP], beta, ylp_result%R)

                i = i+1
            enddo
            deallocate(theta_range)
            !
            ! Post-process the results. Get the lower bound of container
            ! size and iterator-some points may have been dropped.
            npoints = min(size(yldRes), i-1)
            do i = 1, npoints
                ! Get the positions of the bracketing points:
                posA = merge(npoints-1, i-1, i == 1)
                posB = merge(2, i+1, i == npoints)
                ! write(display_unit, *) posA, i, posB
                call getNormalVector2D(yldRes(posA)%scal_s_rel_cart, yldRes(posB)%scal_s_rel_cart, &
                                       1.D0, yldRes(i)%normal_cart, yldRes(i)%beta)
                yldRes(i)%beta = yldRes(i)%beta*RAD_TO_DEG
            enddo
            !
            call writeYldResults(ofunit, yldRes(:npoints), info, write_header = first_run)
            first_run = .false.
        enddo
        !
        close(ofunit)
        info = VEF_OK
    !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    !
    end subroutine


    subroutine writeYldResults(ounit, res, info, write_header)
    implicit none
    integer, intent(in)                        :: ounit
    type(yldResult), dimension(:), intent(in)   :: res
    integer, intent(out)                       :: info
    logical, intent(in), optional               :: write_header
    !
    integer:: i, ierr
    integer, parameter:: column_width = 18, ncolumns = 12
    character(len = column_width), dimension(ncolumns), parameter  :: column_labels = [ character(len = column_width) :: &
        'theta', 'w', 'sigma', 'sigma_scaled', 'S','dotW', 'sigma_x', 'sigma_y', 'dsigma_x', 'dsigma_y', 'beta', 'residual']
    !
        info = VEF_ERROR
        ! Write the header
        if (optionalDefault(write_header, .false.)) then
            write(ounit, fmt = 700, iostat = ierr) (toString(i), i = 1, ncolumns)
            if (ierr /= 0) return
            write(ounit, fmt = 701, iostat = ierr) (column_labels(i), i = 1, ncolumns)
            if (ierr /= 0) return
        endif
        !
        do i = 1, size(res)
            write(ounit, fmt = 710, iostat = ierr) res(i)
            if (ierr /= 0) exit
        enddo
        write(ounit, fmt = 720)
        if (ierr == 0) info = VEF_OK
        !
        ! Formats for output file
        700 format('#',12(A18, 1X))
        701 format(1X, 12(A18, 1X))
        !710 format(1X, 16(ES18.9E3, 1X))
        710 format(1X, 12(ES18.9E3, 1X))
        720 format(/)  ! Double empty line
    !
    end subroutine



    !> Calculate vector v that is normal to the vector AB (from point A to B).
    !> Provide the angle between the vector v and the x axis.
    !> v is obtained by a clockwise rotation by 90 degs applied to the AB vector.
    subroutine getNormalVector2D(A, B, length, v, beta)
        real(DP), intent(in):: A(2), B(2), length 
        real(DP), intent(out):: v(2)       
        !> Angle between the horizontal axis and the vector u [radians]
        !> The range of the angle is [0:2pi], thus it may vary from acute angle
        ! via obtuse angle to reflex angle.
        real(DP), intent(out)  :: beta
        
        real(DP), dimension(2):: u
        real(DP):: u_norm
    
        ! Build the secant vector
        u = b-a
        u_norm = norm2(u)
        if (u_norm > epsilon(0._DP)) then
            ! Build the normal vector. Anticlockwise rotation by 90degs
            ! gives [-u_y, u_x]. Apply the clockwise rotation by 90degs:
            u = [u(2), -u(1)]
            beta = acos(u(1) / u_norm)
            ! Let the vectors that point "downwards" have beta angle > 180deg
            if (u(2) < 0._DP) beta = 2._DP*pi-beta
            v = u/u_norm*length
        else
            ! ouups, the points C and A overlap!
            beta = 0._DP
            v = 0._DP 
        endif
    end subroutine
end module
