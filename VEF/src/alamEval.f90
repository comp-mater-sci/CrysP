!> Objective function for minimization of difference between requested stress tensor
!> and stresses obtained from the ALAMEL
module alamEval
    use nllsTR
    use dmcResultTable
    use altay

    implicit none

    !> Objective function: difference between the searched-for normalized stress and the normalized
    !> stress given by the multilevel model.
    type, extends(ObjectiveFunction):: NormalizedV5DComp

        !> Multilevel prediction of stress from the previous call
        real(DP), dimension(5)        :: vSml = 0.D0
        type(ResultTable), pointer:: ptr_db => null()
    contains
        !> Implementation of virtual method defined in ObjectiveFunction
        procedure, pass(this)           :: objectiveEval => objectiveEval_NV5DComp
    end type

contains

    subroutine objectiveEval_NV5DComp(this, vX, info)
        class(NormalizedV5DComp), intent(inout):: this
        real(DP), dimension(5), intent(in):: vX
        integer, intent(out):: info

        real(DP):: vS(5)

        !Round to TOLERANCE to get rid of numerical instability due to scheduling. The underlying model is much less accurate
        !anyway.
        vs = anint(convert_stress_strain_space(altay_get_stress_state(convert_stress_strain_space(vX)))/TOLERANCE) * TOLERANCE

        ! Retrieve output stress into 5D vector
        !vS = convert_stress_strain_space(astate%simulCalls(istp)%output%stress_tensor)
        ! Transfer vS to vSml
        this%vSml = vS
        vS = vS/norm2(vs)
        this%state%vF = this%vSn-vS

        if (info == 0 .and. associated(this%ptr_db)) &
            call this%ptr_db%put(vX/norm2(vx), this%vSml)  ! Normalize because the magnitude has no impact on the response.

        info = VEF_OK
    end subroutine
end module
