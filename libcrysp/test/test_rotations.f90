!> Rotation conventions of the conversion routines.
!>
!> All rotation matrices in libCrysP are active: their columns are the rotated basis vectors expressed in the reference frame.
!> Every conversion that produces a rotation matrix must return one, and every conversion that consumes one must expect one.
!> Composing a producer with a consumer is therefore required to be the identity, which is what most checks below verify.
!>
!> Euler angles are compared directly where they are unique. In the degenerate cases Phi = 0 and Phi = pi only phi1 + phi2
!> respectively phi1 - phi2 is defined, so there the orientations are compared as rotation matrices instead.
program test_rotations
    use base_defs, only: DP
    use math_utils, only: PI, det, polar_decomposition, UNIT_MATRIX_3X3
    use conversions, only: euler_to_rotation_matrix, deformation_gradient_to_euler, tensor_to_rotation
    use testing

    implicit none

    real(DP), parameter:: TOL = 1.e-12_DP

    !> Euler angles (Bunge, radians) away from the degenerate cases and from the 0 / 2*pi wrap-around, so that they are unique.
    real(DP), parameter:: GENERIC_ANGLES(3, 4) = reshape([0.3_DP, 0.7_DP, 1.9_DP, &
                                                          4.0_DP, 2.5_DP, 0.2_DP, &
                                                          5.5_DP, 1.5_DP, 3.0_DP, &
                                                          2.2_DP, 0.1_DP, 6.1_DP], [3, 4])

    !> Symmetric, positive definite and non-coaxial with any of the rotations above.
    real(DP), parameter:: STRETCH(3, 3) = reshape([1.2_DP,  0.1_DP,  0.0_DP, &
                                                   0.1_DP,  0.9_DP,  0.05_DP, &
                                                   0.0_DP,  0.05_DP, 1.0_DP], [3, 3])

    call test_euler_to_rotation_matrix_is_active()
    call test_rotation_matrix_is_proper()
    call test_euler_roundtrip()
    call test_euler_roundtrip_degenerate()
    call test_euler_from_deformation_gradient()
    call test_tensor_to_rotation()
    call test_polar_decomposition()

    call finish()

contains

    !> A rotation of phi1 = 90 degrees about z maps the x axis onto the y axis, and the y axis onto -x.
    subroutine test_euler_to_rotation_matrix_is_active()
        real(DP), dimension(3,3), parameter:: EXPECTED = reshape([0._DP, 1._DP, 0._DP, &
                                                                 -1._DP, 0._DP, 0._DP, &
                                                                  0._DP, 0._DP, 1._DP], [3, 3])

        call check_equal(euler_to_rotation_matrix([PI/2._DP, 0._DP, 0._DP]), EXPECTED, &
                         'euler_to_rotation_matrix: columns are the rotated basis vectors', tolerance=TOL)
    end subroutine

    subroutine test_rotation_matrix_is_proper()
        real(DP), dimension(3,3):: R
        integer:: i

        do i = 1, size(GENERIC_ANGLES, 2)
            R = euler_to_rotation_matrix(GENERIC_ANGLES(:,i))
            call check_equal(matmul(transpose(R), R), UNIT_MATRIX_3X3, 'euler_to_rotation_matrix: orthogonal', tolerance=TOL)
            call check_equal(det(R), 1._DP, 'euler_to_rotation_matrix: determinant 1', tolerance=TOL)
        end do
    end subroutine

    !> deformation_gradient_to_euler inverts euler_to_rotation_matrix.
    subroutine test_euler_roundtrip()
        integer:: i

        do i = 1, size(GENERIC_ANGLES, 2)
            call check_equal(deformation_gradient_to_euler(euler_to_rotation_matrix(GENERIC_ANGLES(:,i))), GENERIC_ANGLES(:,i), &
                             'deformation_gradient_to_euler: inverts euler_to_rotation_matrix', tolerance=TOL)
        end do
    end subroutine

    !> Phi = 0 and Phi = pi are handled separately by deformation_gradient_to_euler. Both must still yield the right orientation
    !> and the right Phi.
    subroutine test_euler_roundtrip_degenerate()
        real(DP), dimension(3,3):: R
        real(DP), dimension(3):: angles

        R = euler_to_rotation_matrix([1.1_DP, 0._DP, 0.4_DP])
        angles = deformation_gradient_to_euler(R)
        call check_equal(euler_to_rotation_matrix(angles), R, 'deformation_gradient_to_euler: orientation for Phi = 0', tolerance=TOL)
        call check_equal(angles(2), 0._DP, 'deformation_gradient_to_euler: Phi = 0', tolerance=TOL)

        R = euler_to_rotation_matrix([1.1_DP, PI, 0.4_DP])
        angles = deformation_gradient_to_euler(R)
        call check_equal(euler_to_rotation_matrix(angles), R, 'deformation_gradient_to_euler: orientation for Phi = pi', tolerance=TOL)
        call check_equal(angles(2), PI, 'deformation_gradient_to_euler: Phi = pi', tolerance=TOL)
    end subroutine

    !> Only the rotational component of a deformation gradient determines its Euler angles, whether the stretch is applied before
    !> (F = RU) or after (F = VR) the rotation.
    subroutine test_euler_from_deformation_gradient()
        real(DP), dimension(3,3):: R
        integer:: i

        do i = 1, size(GENERIC_ANGLES, 2)
            R = euler_to_rotation_matrix(GENERIC_ANGLES(:,i))
            call check_equal(deformation_gradient_to_euler(matmul(R, STRETCH)), GENERIC_ANGLES(:,i), &
                             'deformation_gradient_to_euler: ignores right stretch', tolerance=TOL)
            call check_equal(deformation_gradient_to_euler(matmul(STRETCH, R)), GENERIC_ANGLES(:,i), &
                             'deformation_gradient_to_euler: ignores left stretch', tolerance=TOL)
        end do
    end subroutine

    subroutine test_tensor_to_rotation()
        real(DP), dimension(3,3):: R
        integer:: i

        do i = 1, size(GENERIC_ANGLES, 2)
            R = euler_to_rotation_matrix(GENERIC_ANGLES(:,i))
            call check_equal(tensor_to_rotation(matmul(R, STRETCH)), R, 'tensor_to_rotation: active rotation of F = RU', &
                             tolerance=TOL)
        end do
    end subroutine

    subroutine test_polar_decomposition()
        real(DP), dimension(3,3):: R, rotation, right_stretch
        integer:: i

        do i = 1, size(GENERIC_ANGLES, 2)
            R = euler_to_rotation_matrix(GENERIC_ANGLES(:,i))
            call polar_decomposition(matmul(R, STRETCH), stretch=right_stretch, rotation=rotation)
            call check_equal(rotation, R, 'polar_decomposition: active rotation of F = RU', tolerance=TOL)
            call check_equal(right_stretch, STRETCH, 'polar_decomposition: right stretch of F = RU', tolerance=TOL)
        end do
    end subroutine
end program
