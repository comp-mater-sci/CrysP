module conversions
    use base_defs
    use math_utils

    implicit none
    public

    interface
        module pure function euler_to_tensor(euler) result(tensor)
            real(DP), dimension(3), intent(in):: euler
            real(DP), dimension(3,3):: tensor
        end function

        module pure function spin_to_tensor(spin) result(tensor)
            real(DP), dimension(3), intent(in):: spin
            real(DP), dimension(3,3):: tensor
        end function

        module pure function deviatoric_to_von_mises(deviatoric) result(von_mises)
            real(DP), dimension(5), intent(in):: deviatoric
            real(DP):: von_mises
        end function
        module pure function deviatoric_to_unscaled_voigt(deviatoric) result(voigt)
            real(DP), dimension(5), intent(in):: deviatoric
            real(DP), dimension(6):: voigt
        end function
        module pure function deviatoric_to_tensor(deviatoric) result(tensor)
            real(DP), dimension(5), intent(in):: deviatoric
            real(DP), dimension(3,3):: tensor
        end function

        module pure function unscaled_voigt_to_deviatoric(voigt) result(deviatoric)
            real(DP), dimension(6), intent(in):: voigt
            real(DP), dimension(5):: deviatoric
        end function
        module pure function unscaled_voigt_to_tensor(voigt) result(tensor)
            real(DP), dimension(6), intent(in):: voigt
            real(DP), dimension(3,3):: tensor
        end function

        module pure function tensor_to_von_mises(tensor) result(von_mises)
            real(DP), dimension(3,3), intent(in):: tensor
            real(DP):: von_mises
        end function
        module function tensor_to_euler(tensor) result(euler)
            real(DP), dimension(3,3), intent(in):: tensor
            real(DP), dimension(3):: euler
        end function
        module pure function tensor_to_spin(tensor) result(spin)
            real(DP), dimension(3,3), intent(in):: tensor
            real(DP), dimension(3):: spin
        end function
        module pure function tensor_to_deviatoric(tensor) result(deviatoric)
            real(DP), dimension(3,3), intent(in):: tensor
            real(DP), dimension(5):: deviatoric
        end function
        module pure function tensor_to_unscaled_voigt(tensor) result(voigt)
            real(DP), dimension(3,3), intent(in):: tensor
            real(DP), dimension(6):: voigt
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

    module procedure euler_to_tensor
        real(DP):: sins(3), &
                   coss(3)

        !Bunge convention: phi1, Phi, phi2
        sins = sin(euler)
        coss = cos(euler)

        tensor(1, 1) = coss(1)*coss(3) - (sins(1)*sins(3)*coss(2))
        tensor(1, 2) = sins(1)*coss(3) + (coss(1)*sins(3)*coss(2))
        tensor(1, 3) = sins(3)*sins(2)
        tensor(2, 1) = -coss(1)*sins(3) - (sins(1)*coss(3)*coss(2))
        tensor(2, 2) = -sins(1)*sins(3) + (coss(1)*coss(3)*coss(2))
        tensor(2, 3) = coss(3)*sins(2)
        tensor(3, 1) = sins(1)*sins(2)
        tensor(3, 2) = -coss(1)*sins(2)
        tensor(3, 3) = coss(2)
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

    module procedure deviatoric_to_von_mises
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

    module procedure tensor_to_von_mises
        von_mises = SQR0P67 * norm2((tensor + transpose(tensor))/2._DP)
    end procedure

    module procedure tensor_to_euler
        real(DP) :: U(3,3), VT(3,3), S(3), R(3,3)
        real(DP) :: work(15)
        integer :: info

        R = tensor

        !Perform polar decomposition to isolate rotational component of input matrix.
        !Useful even if the input matrix is a rotation matrix due to accumulation of roundoff errors during the simulation.

        ! Perform SVD: M = U * S * VT
        call dgesvd('A', 'A', 3, 3, R, 3, S, U, 3, VT, 3, work, size(work), info)

        ! Compute orthonormal rotation matrix R = U * VT
        R = matmul(U, VT)

        ! Ensure R is proper rotation (det = +1)
        if (det(R) < 0._DP) then
            U(:,3) = -U(:,3)
            R = matmul(U, VT)
        end if

        if (R(3,3) + TOLERANCE > 1._DP) then !Phi is very close to being out of bounds
            euler(1) = atan2(-R(2, 1), R(2, 2))  ! range: [-pi, pi[
            euler(2) = 0._DP
            euler(3) = 0._DP
        else if (R(3,3) - TOLERANCE < -1._DP) then !Phi is very close to being out of bounds
            euler(1) = atan2(R(2, 1), -R(2, 2))  ! range: [-pi, pi[
            euler(2) = 0._DP
            euler(3) = 0._DP
        else
            euler(1) = atan2(R(3, 1), -R(3, 2))  ! range: [-pi, pi[
            euler(2) = acos(R(3,3))
            euler(3) = atan2(R(1, 3), R(2, 3))  ! range: [-pi, pi[
        end if

        !No need to check angles(2) because acos(-1+TOLERANCE) << (PI - TOLERANCE)
        if (euler(1) < 0._DP) euler(1) = euler(1)+2._DP*PI   ![-pi, pi[ -> [0, 2*pi[
        if (euler(3) < 0._DP) euler(3) = euler(3)+2._DP*PI   ![-pi, pi[ -> [0, 2*pi[
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
end submodule

