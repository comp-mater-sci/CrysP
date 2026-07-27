!> dmcQRS calculates plastic anisotropic properties, expressed in terms of q-values,
!> directly from texture data, presented in form of SMT, CUR or CUB files.
module dmcQRS
    use conversions
    use altay
    use file_io
    use commonUtils
    use logging
    use dmcbasicmodule

    implicit none

    private
    public:: QRSModule

    character(*), parameter:: MOD_NAME = 'dmcQRS'
    character(8), dimension(5):: OUTPUT_HEADER = [character(8):: 'angle', &
                                                  'q-value', &
                                                  'r-value', &
                                                  's-value', &
                                                  'residual']

    type, extends(BasicModule):: QRSModule
        real(DP):: angular_resolution
    contains
        procedure:: initialize => qrs_initialize
        procedure:: run => qrs_run
    end type

contains

    subroutine qrs_initialize(this, cnfunit)
        class(QRSModule), intent(inout):: this
        integer, intent(in)::             cnfunit

        character(*), parameter:: PROC_NAME = 'QRSModule_readconfig'

        logical:: use_default_settings

        use_default_settings = .false.
        call this%BasicModule%initialize(cnfunit)

        call read_value(cnfunit, this%angular_resolution)
        this%angular_resolution = deg_to_rad(this%angular_resolution)
    end subroutine

    subroutine qrs_run(this)
        class(QRSModule), intent(inout)      :: this

        integer:: i, &
                  out_unit, &
                  n_points
        real(DP):: fi2, &
                   target_stress_mode(5), &
                   strain_mode(5), &
                   stress(5), &
                   residual(5), &
                   Mrot(3,3), &
                   strain_tensile_frame(3,3), &
                   stress_tensile_frame(3,3), &
                   sigma(3,3), &
                   sigma_t(3,3), &
                   r_value

        out_unit = open_output_file(this%output_prefix, OUTPUT_HEADER)

        n_points = ceiling(2._DP*PI / this%angular_resolution - TOLERANCE)

        fi2 = 0._DP
        sigma_t = 0._DP
        sigma_t(1, 1) = 1._DP

        !Initial guess for strain mode is stress mode
        strain_mode = tensor_to_deviatoric(sigma_t)
        strain_mode = strain_mode / norm2(strain_mode)

        do i=1,n_points
            ! Calculate rotation matrix
            ! - due to passive rotation convention
            Mrot = euler_to_tensor([0._DP,0._DP, -fi2])

            ! Rotate from "tensile" to material coordinate system
            sigma = rotate_to(sigma_t, Mrot)

            target_stress_mode = tensor_to_deviatoric(sigma)
            target_stress_mode = target_stress_mode / norm2(target_stress_mode)

            !If the angle between the increments is small enough the strain mode for the previous increment is a good initial guess.
            !Otherwise, use von mises guess
            if (this%angular_resolution > 0.1_DP) &
                strain_mode = target_stress_mode

            call altay_simulate_stress_mode(this%material, target_stress_mode, strain_mode, stress, residual)

            !Rotate results such that the stress mode aligns with the virtual tensile test direction.
            strain_tensile_frame = rotate_from(deviatoric_to_tensor(strain_mode), Mrot)
            stress_tensile_frame = rotate_from(deviatoric_to_tensor(stress), Mrot)
            r_value = strain_tensile_frame(2,2) / strain_tensile_frame(3,3)

            call write_output_increment(out_unit, [rad_to_deg(fi2), &
                                                   r_value / (1._DP + r_value), &
                                                   r_value, &
                                                   norm2(stress_tensile_frame), &
                                                   norm2(residual)])

            fi2 = fi2 + this%angular_resolution
        enddo

        close(out_unit)
    end subroutine
end module
