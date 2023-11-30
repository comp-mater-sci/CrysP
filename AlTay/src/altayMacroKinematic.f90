module altayMacroKinematic
    use utils

    implicit none
    private

    type, public ::  DeformationRate
        real(DP), dimension(3, 3):: VelGrad = 0.0D0        !< Velocity Gradient
        real(DP), dimension(3, 3):: StrainRate = 0.0D0     !< Strain Rate, i.e. symmetric part of the velocity gradient
        real(DP), dimension(3, 3):: StrainMode = 0.0D0     !< Strain Mode normalized by the norm of strain rate
        real(DP)                 :: vMeqStrainRate = 0.0D0 !< von Mises equivalent strain rate
        real(DP), dimension(3, 3):: StrainModevM = 0.0D0   !< Strain Mode normalized by von Mises equivalent strain rate
        real(DP), dimension(3, 3):: Spin = 0.0D0           !< Spin, i.e. anti-symmetric part of the velocity gradient
    end type DeformationRate

    type, public ::  DeformationState
        real(DP), dimension(3, 3):: TotalDefGrad = UNIT_MATRIX_3X3            !< Total Deformation Gradient (from undeformed state to the end of current increment)
        real(DP), dimension(3, 3):: IncrDefGrad = UNIT_MATRIX_3X3             !< Incremental Deformation Gradient (from start to end of current increment)
        real(DP), dimension(3, 3):: IncrDefGrad_inverse = UNIT_MATRIX_3X3     !< Inverse of Incremental Deformation Gradient
        real(DP)                 :: IncrvMeqStrain = 0.0D0               !< Incremental von Mises equivalent strain (from start to end of current increment)
        real(DP)                 :: AccumvMeqStrain_ToStartOfInc = 0.0D0 !< Accumulated von Mises equivalent strain, up to the start of current inc.
                                                                         !< (note: reference state might be different than that of TotalDefGrad)
        real(DP)                 :: AccumvMeqStrain_ToEndOfInc = 0.0D0   !< Accumulated von Mises equivalent strain, up to the end of current inc.
                                                                         !< (note: reference state might be different than that of TotalDefGrad)
    end type DeformationState

    public :: &
        Set_DeformationRate, &
        Update_deformationState

    contains

    !Calculate strain rate, spin etc. from velocity gradient
    subroutine Set_DeformationRate(VelGrad, this)
        real(DP), dimension(3, 3), intent(in)    :: VelGrad
        type(DeformationRate)           , intent(out)   :: this

        !Explicitly make the velocity gradient traceless
        this%VelGrad = VelGrad-UNIT_MATRIX_3X3 * (VelGrad(1, 1)+VelGrad(2, 2)+VelGrad(3, 3))/3.D0

        this%StrainRate = (this%VelGrad+transpose(this%VelGrad))/2.D0
        this%Spin       = (this%VelGrad-transpose(this%VelGrad))/2.D0

        this%StrainMode     = this%StrainRate/norm2(this%StrainRate)
        this%vMeqStrainRate = sqrt(2.0D0/3.0D0) * norm2(this%StrainRate)
        this%StrainModevM   = this%StrainRate/this%vMeqStrainRate

    end subroutine

    subroutine Update_DeformationState(thisRate, thisState, info)
        type(DeformationRate), intent(in)    :: thisRate
        type(DeformationState), intent(inout):: thisState
        integer,                    intent(out):: info
        integer:: i

        real(DP), dimension(3, 3):: Ldt

        Ldt = thisRate%VelGrad

        thisState%IncrDefGrad = matrix_exponential_small_norm(ldt)
        thisState%incrdefgrad_inverse = invert(thisstate%incrdefgrad)
        !if (info /= 0) error stop  ! MD: needs further investigations, should not happen
        thisState%TotalDefGrad = matmul(thisState%IncrDefGrad, thisState%TotalDefGrad)
        thisState%IncrvMeqStrain = thisRate%vMeqStrainRate
        thisState%AccumvMeqStrain_ToStartOfInc = thisState%AccumvMeqStrain_ToEndOfInc
        thisState%AccumvMeqStrain_ToEndOfInc   = thisState%AccumvMeqStrain_ToEndOfInc+thisState%IncrvMeqStrain
    end subroutine

end module
