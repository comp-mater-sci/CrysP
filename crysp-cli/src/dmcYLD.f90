!> Yield locus calculations
module dmcYLD
    use conversions
    use commonUtils
    use logging
    use file_io
    use libcrysp
    use dmcBasicModule

    implicit none

    private
    public:: YLDModule

    character(*), parameter:: MOD_NAME = 'dmvYld'
    !Knowing the base vectors, we can reconstruct the stress state from the angle and norm.
    !The strain mode can strictly be represented by 5 components (deviatoric, no rotation), but use 6 for easy interpretation.
    character(9), dimension(9), parameter:: OUTPUT_HEADER = [character(9):: 'angle', &
                                                             'stress', &
                                                             'strain_11', 'strain_22', 'strain_33', 'strain_23', 'strain_13','strain_12', &
                                                             'residual']

    !> Class responsible for calculations of yield locus sections
    type, extends(BasicModule):: YLDModule
        real(DP):: angular_resolution
        real(DP), dimension(6, 2):: base
    contains
        procedure:: initialize => yld_initialize
        procedure:: run => yld_run
    end type

contains

    subroutine yld_initialize(this, cnfunit)
        class(YLDModule), intent(inout):: this
        integer, intent(in)::             cnfunit

        integer:: i

        call this%BasicModule%initialize(cnfunit)

        ! Read parameters specific for the dmcYld program
        call read_value(cnfunit, this%angular_resolution)
        this%angular_resolution = deg_to_rad(this%angular_resolution)

        do i = 1, 2
            call read_value(cnfunit, this%base(:,i))
            !Normalize for easier processing later
            this%base(:,i) = this%base(:,i) / norm2(this%base(:,i))
        enddo

        if (abs(this%base(:,1) .dot. this%base(:,2)) > TOLERANCE) &
            call log_error(MOD_NAME, 'yld)initialize', ERR_VAL, 'Base vectors must be orthogonal!')
    end subroutine

    subroutine yld_run(this)
        class(YldModule), intent(inout):: this

        integer:: i, &
                  n_points, &
                  out_unit

        real(DP):: angle, &
                   target_stress_mode(5), &
                   target_stress_norm, &
                   strain_mode(5), &
                   stress_norm, &
                   dev_stress(5), &
                   residual(5)

        out_unit = open_output_file(this%output_prefix, OUTPUT_HEADER)

        n_points = ceiling(2*PI / this%angular_resolution - TOLERANCE)

        angle = 0._DP
        do i=1,n_points
            !Target stress mode is combination of basis vectors according to current angle
            target_stress_mode = unscaled_voigt_to_deviatoric(this%base(:,1)*cos(angle) + this%base(:,2)*sin(angle))
            target_stress_norm = norm2(target_stress_mode)
            target_stress_mode = target_stress_mode / target_stress_norm

            !Initial guess for strain mode is stress mode
            !If first iteration or anguler resolution is too large, stick to von mises guess
            if (i==1 .or. this%angular_resolution > 0.1_DP) &
                strain_mode = target_stress_mode

            call altay_simulate_stress_mode(this%material, target_stress_mode, strain_mode, dev_stress, residual)

            !Because the direcion remains identical, ||stress||/||dev(stress)|| == ||stress_mode||/||dev(stress_mode)||
            !Therefore, scaling ||dev_stress|| by ||stress_mode||/||dev(stress_mode)|| yields ||stress||.
            !Since ||stress_mode|| == 1, ||stress|| == ||dev(stress)|| / ||dev(stress_mode)||
            stress_norm = norm2(dev_stress)/target_stress_norm

            call write_output_increment(out_unit, [rad_to_deg(angle), &
                                                   stress_norm, &
                                                   deviatoric_to_unscaled_voigt(strain_mode), &
                                                   norm2(residual)])

            angle = angle + this%angular_resolution
        end do

        close(out_unit)
    end subroutine
end module
