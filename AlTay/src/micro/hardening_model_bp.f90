module hardening_model_bp
    use utils
    use parameters
    use hardening_model_dsh
    use slip_systems

    implicit none
    private

    !Normalized movement vector of EDGE disl. on slip system s
    !== normalized burgers vector of slip system s
    type, extends(HardeningModelDSH) :: HardeningModelBP
    contains
        procedure :: init => bp_init
    end type

    public :: HardeningModelBP

contains

    subroutine bp_init(this, params)
        class(HardeningModelBP), intent(inout) :: this
        type(Parameter), allocatable, intent(in) :: params(:)

        call dsh_init(this, params)
        this%eff = transpose(matmul(CBBNORMAL, normalize(BCC24(:,2,:))))
        call this%initstate()
    end subroutine bp_init
end module hardening_model_bp


