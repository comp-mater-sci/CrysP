!> Data types for stress evolution outputs
module dmcEvolutionOutputRecord
use utils
use dmcIncrementationControl, only: IncrementationControlVariables
use dmcYLPResult, only: YLPResult
implicit none

    public:: IncrementOutputRecord
    private

    !> Data outputed per increment of stress driven state evolution
    type:: IncrementOutputRecord
        real(DP):: vm_strain = 0.D0          !< von Mises equivalent of the strain in current step
        real(DP):: vm_strain_total = 0.D0    !< von Mises equivalent of the total strain across all steps
        real(DP):: norm_P_abs = 0.D0         !< norm of accumulated absolute plastic strain increment tensors
        real(DP):: dotWonA = 0.D0
        real(DP):: taylor_factor = 0.D0
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
    function IncrementOutputRecord_init(icv, ylp, De, Se, taylor_factor) result(this)
    type(IncrementOutputRecord)                 :: this
    type(IncrementationControlVariables), intent(in):: icv
    type(YLPResult), intent(in)                  :: ylp
    real(DP), dimension(3, 3), intent(in)        ::  de, &
                                                    se
    real(DP), intent(in)                 :: taylor_factor
    
        this%vm_strain = SQR0P67*norm2(icv%vP_step)
        this%vm_strain_total = SQR0P67*norm2(icv%vP_total)
        this%norm_P_abs = norm2(icv%vP_abs)
        !
        this%dotWonA = ylp%dotWonA
        this%scal_s = ylp%scal_s
        this%norm_SonA = norm2(ylp%vSonA)
        this%R = ylp%R

        this%taylor_factor = taylor_factor

        this%A = convert_stress_strain_space(ylp%vA)
        this%SonA = convert_stress_strain_space(ylp%vSonA)

        this%P_inc_evol = De
        this%S_evol = Se

        this%icv = icv
    end function
end module
