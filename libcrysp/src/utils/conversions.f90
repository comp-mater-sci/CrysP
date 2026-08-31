!> All conversions used in VEF.

module conversions
    use math_utils

    implicit none
    public


    interface

        !> Transform a fortran string to a fixed-length C string
        module pure function to_c_string(fortran_string, length) result(c_string)
            character(*), intent(in):: fortran_string
            integer, intent(in):: length
            character(C_CHAR), dimension(length):: c_string
        end function

        !> Convert an angle in radians to an angle in degrees.
        module pure elemental function rad_to_deg(rad) result(deg)
            real(DP), intent(in):: rad  !! Angle in radians
            real(DP):: deg              !! Angle in degrees
        end function

        !> Convert an angle in degrees to an angle in radians.
        module pure elemental function deg_to_rad(deg) result(rad)
            real(DP), intent(in):: deg  !! Angle in degrees
            real(DP):: rad              !! Angle in radians
        end function

        !> Convvert a unit vector expressed using spherical angles to cartesian coordinates.
        module pure function spherical_to_cartesian(spherical) result(cartesian)
            real(DP), dimension(2), intent(in):: spherical !! Standard spherical angles: polar angle w.r.t. the z+ axis and azimuth
                                                           !! with respect to x+ axis in radians.
            real(DP), dimension(3):: cartesian !! Unit vector in cartesian coordinates
        end function

        !> Convert an Euler angle triplet to a rotation matrix.
        !>
        !> The resulting rotation matrix represents an active rotation.
        !> Its columns represent the rotated basis vectors in the reference frame.
        !> It can also be interpreted as a deformation gradient with no stretch.
        module pure function euler_to_rotation_matrix(euler) result(rotation_matrix)
            real(DP), dimension(3), intent(in):: euler !! Euler angles in radians using passive Bunge convention.
            real(DP), dimension(3,3):: rotation_matrix !! Pure finite strain active rotation matrix.
        end function

        !> Convert a spin from vector to tensor representation.
        !>
        !> Uses small strain theory.
        module pure function spin_to_tensor(spin) result(tensor)
            real(DP), dimension(3), intent(in):: spin !! Vector representing the spin component of a small strain velocity gradient.
                                                      !! I.e. the top-right corner of the tensor. Components are ordered following
                                                      !! Voigt convention.
            real(DP), dimension(3,3):: tensor         !! Antisymmetric spin tensor.
        end function

        !> Calculate the Von Mises equivalent of a deviatoric strain.
        module pure function deviatoric_strain_to_von_mises(deviatoric) result(von_mises)
            real(DP), dimension(5), intent(in):: deviatoric !! Deviatoric strain following the later Paul Van Houtte convention.
            real(DP):: von_mises                            !! Von Mises equivalent stress/strain.
        end function

        !> Convert a deviatoric stress/strain to unscaled voigt representation.
        module pure function deviatoric_to_unscaled_voigt(deviatoric) result(voigt)
            real(DP), dimension(5), intent(in):: deviatoric !! Deviatoric stress/strain following the later Paul Van Houtte convention.
            real(DP), dimension(6):: voigt                  !! Stress/strain following the standard voigt component order but without any scaling.
        end function

        !> Convert a deviatoric stress/strain to a tensor.
        module pure function deviatoric_to_tensor(deviatoric) result(tensor)
            real(DP), dimension(5), intent(in):: deviatoric !! Deviatoric stress/strain following the later Paul Van Houtte convention.
            real(DP), dimension(3,3):: tensor               !! Symmetric tensor with 0 trace
        end function

        !> Extract the deviatoric component from a stress or strain in unscaled voigt notation.
        !>
        !> Eliminates any hydrostatic component.
        module pure function unscaled_voigt_to_deviatoric(voigt) result(deviatoric)
            real(DP), dimension(6), intent(in):: voigt !! Stress/strain following the standard voigt component order but without any scaling.
            real(DP), dimension(5):: deviatoric        !! Deviatoric component following the later Van Houtte convention.
        end function

        !> Convert a stress/strain in unscaled voigt notation to a tensor.
        module pure function unscaled_voigt_to_tensor(voigt) result(tensor)
            real(DP), dimension(6), intent(in):: voigt !! Stress/strain following the standard voigt component order but without any scaling.
            real(DP), dimension(3,3):: tensor          !! Symmetric tensor.
        end function

        !> Calculate the Von Mises equivalent of a tensor.
        !>
        !> Symmitrizes the tensor. Even useful for proper stress/strain tensors to get rid of roundoff errors during the simulation.
        !> Only valid in small strain settings.
        module pure function strain_tensor_to_von_mises(tensor) result(von_mises)
            real(DP), dimension(3,3), intent(in):: tensor   !! Tensor
            real(DP):: von_mises                            !! Von mises equivalent stress/strain of the tensor.
        end function

        !> Calculate Euler angles from a deformation gradient.
        !>
        !> Follows finite strain convention.
        !> Performs polar decomposition on the input tensor.
        !> Even useful for proper rotation tensors to compensate for roundoff errors during the simulation.
        module function deformation_gradient_to_euler(deformation_gradient) result(euler_angles)
            real(DP), dimension(3,3), intent(in):: deformation_gradient
            real(DP), dimension(3):: euler_angles                       !! Euler angles in passive Bunge convention in radians.
        end function

        !> Extract the spin from a tensor.
        !>
        !> Follows small strain theory.
        module pure function tensor_to_spin(tensor) result(spin)
            real(DP), dimension(3,3), intent(in):: tensor !! Tensor representing a deformation in small-strain setting.
            real(DP), dimension(3):: spin                 !! Spin component of the tensor as the upper right corner of the
                                                          !! antisymmetric component of the tensor, with component order following Voigt convention.
        end function

        !> Extract the deviatoric component of a tensor.
        !>
        !> Applicable to stresses, small strains and velocity gradients.
        !> Eliminates rotational and hydrostatic components.
        module pure function tensor_to_deviatoric(tensor) result(deviatoric)
            real(DP), dimension(3,3), intent(in):: tensor !! Tensor in small strain setting.
            real(DP), dimension(5):: deviatoric           !! Deviatoric component following the later Van Houtte convention.
        end function

        !> Extract the stress/strain component from a tensor.
        !>
        !> Only valid in small strain settings.
        !> Eliminates the rotational component.
        module pure function tensor_to_unscaled_voigt(tensor) result(voigt)
            real(DP), dimension(3,3), intent(in):: tensor !! Tensor in small strain setting.
            real(DP), dimension(6):: voigt                !! Stress/strain following the standard voigt component order but without any scaling.
        end function

        !> Extract rotational component of a large deformation gradient
        !>
        !> Follows finite strain theory
        module function tensor_to_rotation(F) result(R)
            real(DP), dimension(3,3), intent(in):: F
            real(DP), dimension(3,3):: R  !Rotational component of the deformation gradient in passive convention
        end function

        !> Calculate true strain from a given stretch tensor
        !>
        !> Useful when we know the velocity gradient is purely deviatoric because there U = V = F
        !> When used with the left stretch tensor, it yields the spatial/Eulerian true strain.
        !> When used with the right stretch tensor, it yields the material/Lagrangian true strain.
        module function stretch_to_true_strain(stretch) result(strain)
            real(DP), dimension(3,3), intent(in):: stretch !! Left or right stretch tensor
            real(DP), dimension(3,3):: strain
        end function


        !> Calculate true strain from a given stretch tensor
        !>
        !> Useful when we know the velocity gradient is purely deviatoric because there U = V = F
        !> When used with the left stretch tensor, it yields the spatial/Eulerian true strain.
        !> When used with the right stretch tensor, it yields the material/Lagrangian true strain.
        module function stretch_to_von_mises_true_strain(stretch) result(vm_strain)
            real(DP), dimension(3,3), intent(in):: stretch
            real(DP):: vm_strain
        end function

        !> Extract the true (logarithmic) strain from any deformation gradient.
        !>
        !> Uses finite strain kenematics.
        module function deformation_gradient_to_true_strain(deformation_gradient) result(true_strain)
            real(DP), dimension(3,3), intent(in):: deformation_gradient  !! Deformation gradient
            real(DP), dimension(3,3)::             true_strain !! Von mises equivalent strain of the input.
        end function

        !> Extract the von mises equivalent true (logarithmic) strain from any deformation gradient.
        !>
        !> Uses finite strain kenematics.
        module function deformation_gradient_to_von_mises_true_strain(deformation_gradient) result(von_mises_true_strain)
            real(DP), dimension(3,3), intent(in):: deformation_gradient  !! Deformation gradient
            real(DP)::                             von_mises_true_strain !! Von mises equivalent strain of the input.
        end function

    end interface
end module

submodule(conversions) conversions_imp
    implicit none

    real(DP), parameter:: SQR0P5     = sqrt(0.5_DP)         !! Square root of 1/2.
    real(DP), parameter:: SQR0P67    = sqrt(2._DP/3._DP)    !! Square root of 2/3.
    real(DP), parameter:: SQR1P5     = sqrt(1.5_DP)         !! Square root of 3/2.
    real(DP), parameter:: SQR2       = sqrt(2._DP)          !! Square root of 2.
    real(DP), parameter:: ROOT6I     = 1.D0/sqrt(6.D0)      !! 1/sqr(6)

contains

    module procedure rad_to_deg
        deg = rad * 180._DP / PI
    end procedure

    module procedure deg_to_rad
        rad = deg / 180._DP * PI
    end procedure

    module procedure spherical_to_cartesian
        cartesian(1) = sin(spherical(1))*cos(spherical(2))
        cartesian(2) = sin(spherical(1))*sin(spherical(2))
        cartesian(3) = cos(spherical(1))
    end procedure

    module procedure to_c_string
        integer:: i

        do i=1,len(fortran_string)
            c_string(i) = char(iachar(fortran_string(i:i)),kind=C_CHAR)
        end do
        c_string(i) = C_NULL_CHAR
    end procedure

    module procedure euler_to_rotation_matrix
        real(DP):: sins(3), &
                   coss(3)

        !Bunge convention: phi1, Phi, phi2
        sins = sin(euler)
        coss = cos(euler)

        rotation_matrix(1, 1) = coss(1)*coss(3) - (sins(1)*sins(3)*coss(2))
        rotation_matrix(2, 1) = sins(1)*coss(3) + (coss(1)*sins(3)*coss(2))
        rotation_matrix(3, 1) = sins(3)*sins(2)
        rotation_matrix(1, 2) = -coss(1)*sins(3) - (sins(1)*coss(3)*coss(2))
        rotation_matrix(2, 2) = -sins(1)*sins(3) + (coss(1)*coss(3)*coss(2))
        rotation_matrix(3, 2) = coss(3)*sins(2)
        rotation_matrix(1, 3) = sins(1)*sins(2)
        rotation_matrix(2, 3) = -coss(1)*sins(2)
        rotation_matrix(3, 3) = coss(2)
    end procedure

    module procedure spin_to_tensor
        tensor = 0._DP
        tensor(2, 3) = spin(1)
        tensor(1, 3) = spin(2)
        tensor(1, 2) = spin(3)
        tensor(2, 1) = -tensor(1, 2)
        tensor(3, 1) = -tensor(1, 3)
        tensor(3, 2) = -tensor(2, 3)
    end procedure

    module procedure deviatoric_strain_to_von_mises
        von_mises = SQR0P67 * norm2(deviatoric)
    end procedure

    module procedure deviatoric_to_unscaled_voigt
        voigt(1) = SQR0P5*deviatoric(1) + ROOT6I*deviatoric(2)
        voigt(2) = -SQR0P5*deviatoric(1) + ROOT6I*deviatoric(2)
        voigt(3) = -SQR0P67*deviatoric(2)
        voigt(4) =  SQR0P5*deviatoric(3)
        voigt(5) =  SQR0P5*deviatoric(4)
        voigt(6) =  SQR0P5*deviatoric(5)
    end procedure

    module procedure deviatoric_to_tensor
        tensor(1, 1) =  SQR0P5*deviatoric(1) + root6i*deviatoric(2)
        tensor(2, 2) = -SQR0P5*deviatoric(1) + root6i*deviatoric(2)
        tensor(3, 3) = -SQR0P67*deviatoric(2)
        tensor(2, 3) =  SQR0P5*deviatoric(3)
        tensor(3, 1) =  SQR0P5*deviatoric(4)
        tensor(1, 2) =  SQR0P5*deviatoric(5)
        tensor(3, 2) = tensor(2, 3)
        tensor(1, 3) = tensor(3, 1)
        tensor(2, 1) = tensor(1, 2)
    end procedure

    module procedure unscaled_voigt_to_deviatoric
        deviatoric(1) =  SQR0P5*(voigt(1) - voigt(2))
        deviatoric(2) = -SQR1P5*(voigt(3) - (sum(voigt(1:3)) / 3._DP))
        deviatoric(3) = SQR2 * voigt(4)
        deviatoric(4) = SQR2 * voigt(5)
        deviatoric(5) = SQR2 * voigt(6)
    end procedure

    module procedure unscaled_voigt_to_tensor
        tensor(1, 1) = voigt(1)
        tensor(2, 2) = voigt(2)
        tensor(3, 3) = voigt(3)
        tensor(2, 3) = voigt(4)
        tensor(1, 3) = voigt(5)
        tensor(1, 2) = voigt(6)
        tensor(3, 1) = tensor(1, 3)
        tensor(2, 1) = tensor(1, 2)
        tensor(3, 2) = tensor(2, 3)
    end procedure

    module procedure strain_tensor_to_von_mises
        real(DP):: trace, &
                   isochoric(3,3)
        integer:: i

        trace = math_trace33(tensor)
        isochoric = tensor

        do i=1,3
            isochoric(i,i) = isochoric(i,i) - trace / 3._DP
        end do
        von_mises = SQR0P67 * norm2(isochoric)
    end procedure

    module procedure deformation_gradient_to_euler
        real(DP) :: R(3,3)

        R = tensor_to_rotation(deformation_gradient)

        if (R(3,3) + TOLERANCE > 1._DP) then !Phi is very close to being out of bounds
            euler_angles(1) = atan2(-R(2, 1), R(2, 2))  ! range: [-pi, pi[
            euler_angles(2) = 0._DP
            euler_angles(3) = 0._DP
        else if (R(3,3) - TOLERANCE < -1._DP) then !Phi is very close to being out of bounds
            euler_angles(1) = atan2(R(2, 1), -R(2, 2))  ! range: [-pi, pi[
            euler_angles(2) = 0._DP
            euler_angles(3) = 0._DP
        else
            euler_angles(1) = atan2(R(3, 1), -R(3, 2))  ! range: [-pi, pi[
            euler_angles(2) = acos(R(3,3))
            euler_angles(3) = atan2(R(1, 3), R(2, 3))  ! range: [-pi, pi[
        end if

        !No need to check angles(2) because acos(-1+TOLERANCE) << (PI - TOLERANCE)
        if (euler_angles(1) < 0._DP) euler_angles(1) = euler_angles(1)+2._DP*PI   ![-pi, pi[ -> [0, 2*pi[
        if (euler_angles(3) < 0._DP) euler_angles(3) = euler_angles(3)+2._DP*PI   ![-pi, pi[ -> [0, 2*pi[
    end procedure

    module procedure tensor_to_spin
        real(DP), dimension(3, 3):: antisym

        antisym = (tensor-transpose(tensor)) / 2._DP
        spin = [antisym(2, 3), antisym(1, 3), antisym(1, 2)]
    end procedure

    module procedure tensor_to_deviatoric
        deviatoric(1) =  SQR0P5*(tensor(1, 1) - tensor(2, 2))
        deviatoric(2) = -SQR1P5*(tensor(3, 3) - (tensor(1, 1) + tensor(2, 2) + tensor(3, 3)) / 3._DP)
        deviatoric(3) =  SQR0P5*(tensor(2, 3) + tensor(3, 2))
        deviatoric(4) =  SQR0P5*(tensor(3, 1) + tensor(1, 3))
        deviatoric(5) =  SQR0P5*(tensor(1, 2) + tensor(2, 1))
    end procedure

    module procedure tensor_to_unscaled_voigt
        voigt(1) = tensor(1, 1)
        voigt(2) = tensor(2, 2)
        voigt(3) = tensor(3, 3)
        voigt(4) = (tensor(2, 3) + tensor(3,2)) / 2._DP
        voigt(5) = (tensor(1, 3) + tensor(3,1)) / 2._DP
        voigt(6) = (tensor(1, 2) + tensor(2,1)) / 2._DP
    end procedure

    !> Imported from DAMASK
    !> https://damask2.mpie.de/bin/view/Home/WebHome.html
    module procedure tensor_to_rotation
      real(DP), dimension(3,3) ::  C                 ! right Cauchy-Green tensor
      real(DP), dimension(3) :: &
        lambda, &                                     ! principal stretches
        I_C, &                                        ! invariants of C
        I_U                                           ! invariants of U
      real(DP), dimension(2) :: &
        I_F                                           ! first two invariants of F
      real(DP) :: x,Phi


      C = matmul(transpose(F),F)
      I_C = math_invariantsSym33(C)
      I_F = [math_trace33(F), 0.5_DP*(math_trace33(F)**2 - math_trace33(matmul(F,F)))]

      x = math_clip(I_C(1)**2 -3.0_DP*I_C(2),0.0_DP)**(3.0_DP/2.0_DP)
      if (x /= 0._DP) then
        Phi = acos(math_clip((I_C(1)**3 -4.5_DP*I_C(1)*I_C(2) +13.5_DP*I_C(3))/x,-1.0_DP,1.0_DP))
        lambda = I_C(1) +(2.0_DP * sqrt(math_clip(I_C(1)**2-3.0_DP*I_C(2),0.0_DP))) &
                        *cos((Phi-2._DP*PI*[1.0_DP,2.0_DP,3.0_DP])/3.0_DP)
        lambda = sqrt(math_clip(lambda,0.0_DP)/3.0_DP)
      else
        lambda = sqrt(I_C(1)/3.0_DP)
      end if

      I_U = [sum(lambda), lambda(1)*lambda(2)+lambda(2)*lambda(3)+lambda(3)*lambda(1), product(lambda)]

      R = I_U(1)*I_F(2) * UNIT_MATRIX_3X3 &
        +(I_U(1)**2-I_U(2)) * F &
        - I_U(1)*I_F(1) * transpose(F) &
        + I_U(1) * transpose(matmul(F,F)) &
        - matmul(F,C)
      R = R*det(R)**(-1.0_DP/3.0_DP)
    end procedure

    module procedure stretch_to_true_strain
        strain = matrix_log(stretch)
    end procedure

    module procedure stretch_to_von_mises_true_strain
        vm_strain = strain_tensor_to_von_mises(stretch_to_true_strain(stretch))
    end procedure

    module procedure deformation_gradient_to_true_strain
        real(DP):: right_stretch(3,3)

        call polar_decomposition(deformation_gradient, stretch=right_stretch)
        true_strain = stretch_to_true_strain(right_stretch)
    end procedure

    module procedure deformation_gradient_to_von_mises_true_strain
        von_mises_true_strain = strain_tensor_to_von_mises(deformation_gradient_to_true_strain(deformation_gradient))
    end procedure
end submodule
