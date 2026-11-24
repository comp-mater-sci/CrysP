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
                                                             'stress_mode_deviation']

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
        class(YLDModule), target, intent(inout):: this
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
            call log_error(MOD_NAME, 'yld)initialize', CRYSP_ERR_VAL, 'Base vectors must be orthogonal!')
    end subroutine

    subroutine yld_run(this)
        class(YldModule), target, intent(inout):: this

        integer:: i, &
                  n_points, &
                  out_unit

        real(DP):: angle, &
                   stress_norm
        real(DP), allocatable:: angles(:), &
                                target_stress_norms(:), &
                                target_stress_modes(:,:), &
                                strain_modes(:,:), &
                                dev_stresses(:,:), &
                                stress_mode_deviations(:,:)

        out_unit = open_output_file(this%output_prefix, OUTPUT_HEADER)

        n_points = ceiling(2*PI / this%angular_resolution - TOLERANCE)
        allocate(angles(n_points), target_stress_norms(n_points), target_stress_modes(5,n_points), strain_modes(5,n_points), &
                 dev_stresses(5,n_points), stress_mode_deviations(5,n_points))

        angle = 0._DP
        do i=1,n_points
            angles(i) = angle
            !Target stress mode is combination of basis vectors according to current angle
            target_stress_modes(:,i) = unscaled_voigt_to_deviatoric(this%base(:,1)*cos(angle) + this%base(:,2)*sin(angle))
            target_stress_norms(i) = norm2(target_stress_modes(:,i))
            target_stress_modes(:,i) = target_stress_modes(:,i) / target_stress_norms(i)

            angle = angle + this%angular_resolution
        end do

        call handle_crysp_status(crysp_simulate_stress_modes(this%material, target_stress_modes, strain_modes, dev_stresses, &
                                                             stress_mode_deviations))

        do i=1,n_points
            !Because the direcion remains identical, ||stress||/||dev(stress)|| == ||stress_mode||/||dev(stress_mode)||
            !Therefore, scaling ||dev_stress|| by ||stress_mode||/||dev(stress_mode)|| yields ||stress||.
            !Since ||stress_mode|| == 1, ||stress|| == ||dev(stress)|| / ||dev(stress_mode)||
            stress_norm = norm2(dev_stresses(:,i))/target_stress_norms(i)

            call write_output_increment(out_unit, [rad_to_deg(angles(i)), &
                                                   stress_norm, &
                                                   deviatoric_to_unscaled_voigt(strain_modes(:,i)), &
                                                   norm2(stress_mode_deviations(:,i))])
        end do

        close(out_unit)
    end subroutine
end module
