!> Data types for stress evolution outputs
module dmcEvolutionOutputRecord
    use base_defs
    use conversions
    use dmcIncrementationControl, only: IncrementationControlVariables
    use dmcStressDrivenModule

    implicit none

    public:: IncrementOutputRecord
    private

    !> Data outputed per increment of stress driven state evolution
    type:: IncrementOutputRecord
        real(DP):: vm_strain = 0.D0          !< von Mises equivalent of the strain in current step
        real(DP):: vm_strain_total = 0.D0    !< von Mises equivalent of the total strain across all steps
        real(DP):: norm_P_abs = 0.D0         !< norm of accumulated absolute plastic strain increment tensors
        real(DP):: dotWonA = 0.D0
        real(DP):: scal_s = 0.D0
        real(DP):: norm_SonA = 0.D0
        real(DP):: R = 0.D0                  !< residual of search procedure
        real(DP), dimension(3, 3):: A, &
                                    sonA, &
                                    p_inc_evol, &
                                    s_evol
        type(IncrementationControlVariables):: icv
    end type

    interface IncrementOutputRecord
        module procedure IncrementOutputRecord_init
    end interface

contains


    !> Make IncrementOutputRecord from increment data.
    function IncrementOutputRecord_init(icv, De, Se, target_stress_mode, strain_mode, stress_mode, stress_norm, residual_norm) result(this)
    type(IncrementOutputRecord)                 :: this
    type(IncrementationControlVariables), intent(in):: icv
    real(DP), dimension(3, 3), intent(in)        ::  de, &
                                                    se
    real(DP), dimension(5), intent(in):: target_stress_mode
    real(DP), dimension(5), intent(in):: strain_mode
    real(DP), dimension(5), intent(in):: stress_mode
    real(DP), intent(in):: stress_norm
    real(DP), intent(in):: residual_norm

        this%vm_strain = deviatoric_strain_to_von_mises(icv%vP_step)
        this%vm_strain_total = deviatoric_strain_to_von_mises(icv%vP_total)
        this%norm_P_abs = norm2(icv%vP_abs)
        !
        this%dotWonA = strain_mode .dot. stress_mode * stress_norm
        this%scal_s = stress_norm
        this%norm_SonA = stress_norm
        this%R = residual_norm

        this%A = deviatoric_to_tensor(strain_mode)
        this%SonA = deviatoric_to_tensor(stress_mode)

        this%P_inc_evol = De
        this%S_evol = Se

        this%icv = icv
    end function
end module
