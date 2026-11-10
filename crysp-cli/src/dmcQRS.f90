!> dmcQRS calculates plastic anisotropic properties, expressed in terms of q-values,
!> directly from texture data, presented in form of SMT, CUR or CUB files.
module dmcQRS
    use conversions
    use libcrysp
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
        class(QRSModule), target, intent(inout):: this
        integer, intent(in)::             cnfunit

        character(*), parameter:: PROC_NAME = 'QRSModule_readconfig'

        logical:: use_default_settings

        use_default_settings = .false.
        call this%BasicModule%initialize(cnfunit)

        call read_value(cnfunit, this%angular_resolution)
        this%angular_resolution = deg_to_rad(this%angular_resolution)
    end subroutine

    subroutine qrs_run(this)
        class(QRSModule), target, intent(inout)      :: this

        integer:: i, &
                  out_unit, &
                  n_points
        real(DP):: phi_2, &
                   strain_test_frame(3,3), &
                   stress_test_frame(3,3), &
                   sigma_t(3,3), &
                   r_value
        real(DP), allocatable:: phi_2s(:), &
                                test_frames(:,:,:), &
                                target_stress_modes(:,:), &
                                strain_modes(:,:), &
                                stresses(:,:), &
                                residuals(:,:)

        out_unit = open_output_file(this%output_prefix, OUTPUT_HEADER)

        n_points = ceiling(2._DP*PI / this%angular_resolution - TOLERANCE)
        allocate(phi_2s(n_points), test_frames(3,3,n_points), target_stress_modes(5,n_points), strain_modes(5,n_points), &
                 stresses(5,n_points), residuals(5,n_points))

        phi_2 = 0._DP
        sigma_t = 0._DP
        sigma_t(1, 1) = 1._DP

        do i=1,n_points
            phi_2s(i) = phi_2
            test_frames(:,:,i) = euler_to_rotation_matrix([0._DP,0._DP, phi_2])

            ! Rotate from "tensile" to material coordinate system
            target_stress_modes(:,i) = tensor_to_deviatoric(sigma_t .fromframe. test_frames(:,:,i))
            target_stress_modes(:,i) = target_stress_modes(:,i) / norm2(target_stress_modes(:,i))

            phi_2 = phi_2 + this%angular_resolution
        enddo

        call handle_crysp_status(crysp_simulate_stress_modes(this%material, target_stress_modes, strain_modes, stresses, residuals))

        do i=1,n_points
            !Rotate results such that the stress mode aligns with the virtual tensile test direction.
            strain_test_frame = deviatoric_to_tensor(strain_modes(:,i)) .toframe. test_frames(:,:,i)
            stress_test_frame = deviatoric_to_tensor(stresses(:,i))     .toframe. test_frames(:,:,i)
            r_value = strain_test_frame(2,2) / strain_test_frame(3,3)

            call write_output_increment(out_unit, [rad_to_deg(phi_2s(i)), &
                                                   r_value / (1._DP + r_value), &
                                                   r_value, &
                                                   norm2(stress_test_frame), &
                                                   norm2(residuals(:,i))])
        enddo

        close(out_unit)
    end subroutine
end module
