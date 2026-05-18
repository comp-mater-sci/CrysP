!> Yield locus calculations
module dmcYld
    use conversions
    use commonUtils
    use logging
    use file_io
    use altay
    use dmcBasicModule

    implicit none

    public YldModule
    private

    integer, parameter                               :: nbase = 3
    character(*), parameter:: MOD_NAME = 'dmvYld'


    !> Class responsible for calculations of yield locus sections
    type, extends(BasicModule):: YldModule

        real(DP):: angular_resolution

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
    character(*), parameter:: PROC_NAME = 'yldmodule_readconfig'

    integer:: i
    real(DP):: norm
    logical:: normalize, use_default_settings
    !
        info = this%BasicModule%ReadConfig(cnfunit)
        if (info /= VEF_OK) return
        ! Read parameters specific for the dmcYld program
        if (.not. readValue(cnfunit, this%angular_resolution)) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read angular resolution')
        this%angular_resolution = deg_to_rad(this%angular_resolution)
        if (.not. readValue(cnfunit, use_default_settings)) return
        if (.not. use_default_settings) then
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
            if (.not. readValue(cnfunit, this%do_scaling)) return
            if (this%do_scaling) then
                if (.not. readValue(cnfunit, this%scaling_vector)) return
            endif
        endif
        !
        ! Override the requests for outputs:
        this%altay%nfile = 0   ! texture
        this%output%outputRequest = .false.       ! idem.
        !
        info = VEF_OK
    end function

    subroutine YldModule_run(this, info)
        class(YldModule), intent(inout)            :: this
        integer, intent(out)                       :: info
        real(DP):: theta, &
                    iunilen, &
                    scal_s_rel, &
                    sigma_vector(6)
        type(yldResult), dimension(:), allocatable  :: yldRes
        integer                 :: i, npoints, ofunit
        integer:: posA, posB
        real(DP), parameter:: beta = 0._DP

        real(DP):: target_stress_mode(5), &
                   target_stress_norm, &
                   strain_mode(5), &
                   stress(5), &
                   residual(5)

        info = VEF_ERROR

        npoints = ceiling(2*PI / this%angular_resolution - TOLERANCE)

        ! Open the main output file
        info = this%openOutputFile('.xyld', ofunit)
        if (info /= VEF_OK) return
        !
        ! Fix the configuration: no need for anything except for the stresses.
        iunilen = 1.D0
        if (this%do_scaling) then
            target_stress_mode = unscaled_voigt_to_deviatoric(this%scaling_vector)
            iunilen = norm2(target_stress_mode)
            target_stress_mode = target_stress_mode / iunilen
            strain_mode = target_stress_mode
            call altay_simulate_stress_mode(target_stress_mode, strain_mode, stress, residual)
            iunilen = iunilen / norm2(stress)
        endif
        !
        allocate(yldRes(npoints))
        !
        theta = 0._DP
        do i=1,npoints

            ! Combine the base vectors
            ! Note: explicit temporary sigma_vector prevents runtime warning about
            !       a temporary created in a call to convert_voigt
            sigma_vector = this%base_vectors(:,1)*cos(theta) + this%base_vectors(:,2)*sin(theta) &
                            + this%base_vectors(:,3)
            target_stress_mode = unscaled_voigt_to_deviatoric(sigma_vector)
            target_stress_norm = norm2(target_stress_mode)
            target_stress_mode = target_stress_mode / target_stress_norm

            !Initial guess for strain mode is stress mode
            if (i==1) &
                strain_mode = target_stress_mode

            call altay_simulate_stress_mode(target_stress_mode, strain_mode, stress, residual)

            scal_s_rel = norm2(stress)/target_stress_norm*iunilen

            yldRes(i) = yldResult(rad_to_deg(theta), norm2(stress)/target_stress_norm, scal_s_rel, &
                                  norm2(stress), &
                                  strain_mode .dot. stress, &
                                  [scal_s_rel*cos(theta), scal_s_rel*sin(theta)], &
                                  [0._DP, 0._DP], beta, norm2(deviatoric_to_unscaled_voigt(residual)))


            theta = theta + this%angular_resolution
        end do

        do i=1,npoints
            ! Get the positions of the bracketing points:
            posA = merge(npoints, i-1, i == 1)
            posB = merge(1, i+1, i == npoints)
            call getNormalVector2D(yldRes(posA)%scal_s_rel_cart, yldRes(posB)%scal_s_rel_cart, &
                                   1.D0, yldRes(i)%normal_cart, yldRes(i)%beta)
            yldRes(i)%beta = rad_to_deg(yldRes(i)%beta)
        end do

        call writeYldResults(ofunit, yldRes(:npoints), info, write_header = .true.)
        close(ofunit)
        info = VEF_OK
    end subroutine


    subroutine writeYldResults(ounit, res, info, write_header)
    implicit none
    integer, intent(in)                        :: ounit
    type(yldResult), dimension(:), intent(in)   :: res
    integer, intent(out)                       :: info
    logical, intent(in), optional               :: write_header
    !
    integer:: i, ierr
    integer, parameter:: column_width = 18, ncolumns = 11
    character(len = column_width), dimension(ncolumns), parameter  :: column_labels = [ character(len = column_width) :: &
        'theta', 'sigma', 'sigma_scaled', 'S','dotW', 'sigma_x', 'sigma_y', 'dsigma_x', 'dsigma_y', 'beta', 'residual']
    !
        info = VEF_ERROR
        ! Write the header
        if (optionalDefault(write_header, .false.)) then
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
        700 format('#',11(A18, 1X))
        701 format(1X, 11(A18, 1X))
        !710 format(1X, 16(ES18.9E3, 1X))
        710 format(1X, 11(ES18.9E3, 1X))
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
