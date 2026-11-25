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
                  out_unit, &
                  points

        real(DP):: angle, &
                   angles(n_angles(this%angular_resolution)), &
                   target_stress_modes(3,3,n_angles(this%angular_resolution)), &
                   stresses(3,3,n_angles(this%angular_resolution)), &
                   strain_modes(3,3,n_angles(this%angular_resolution)), &
                   stress_mode_deviations(n_angles(this%angular_resolution))

        out_unit = open_output_file(this%output_prefix, OUTPUT_HEADER)
        points = n_angles(this%angular_resolution)

        angle = 0._DP
        do i=1,points
            angles(i) = angle
            !Target stress mode is combination of basis vectors (unscaled Voigt) according to current angle
            target_stress_modes(:,:,i) = unscaled_voigt_to_tensor(this%base(:,1)*cos(angle) + this%base(:,2)*sin(angle))
            angle = angle + this%angular_resolution
        end do

        call handle_crysp_status(crysp_simulate_stress_modes(this%material, &
                                                             target_stress_modes, &
                                                             strain_modes, &
                                                             stresses, &
                                                             stress_mode_deviations))

        do i=1,points
            call write_output_increment(out_unit, [rad_to_deg(angles(i)), &
                                                   norm2(tensor_to_unscaled_voigt(stresses(:,:,i))), &
                                                   tensor_to_unscaled_voigt(strain_modes(:,:,i)), &
                                                   stress_mode_deviations(i)])
        end do

        close(out_unit)
    end subroutine
end module
