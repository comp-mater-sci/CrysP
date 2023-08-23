module hardening_model_bp
    use utils
    use parameters
    use hardening_model_dsh

    implicit none
    private

    !Normalized movement vector of EDGE disl. on slip system s
    !== normalized burgers vector of slip system s

    real(DP), dimension(24,3), parameter :: EDGEDIR = transpose(reshape([ 1,  1,  1, &
                                                                               1,  1,  1, &
                                                                               1,  1,  1, &
                                                                             ! ---------
                                                                              -1, -1,  1, &
                                                                              -1, -1,  1, &
                                                                              -1, -1,  1, &
                                                                             ! ---------
                                                                              -1,  1,  1, &
                                                                              -1,  1,  1, &
                                                                              -1,  1,  1, &
                                                                             ! ---------
                                                                               1, -1,  1, &
                                                                               1, -1,  1, &
                                                                               1, -1,  1, &
                                                                             ! ---------
                                                                               1,  1,  1, &
                                                                               1,  1,  1, &
                                                                               1,  1,  1, &
                                                                             ! ---------
                                                                              -1, -1,  1, &
                                                                              -1, -1,  1, &
                                                                              -1, -1,  1, &
                                                                             ! ---------
                                                                              -1,  1,  1, &
                                                                              -1,  1,  1, &
                                                                              -1,  1,  1, &
                                                                             ! ---------
                                                                               1, -1,  1, &
                                                                               1, -1,  1, &
                                                                               1, -1,  1], [3,24]),DP)/sqrt(3._DP)

    !DSH model assuming all slip is carried by edge dislocations
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
        this%eff = matmul(EDGEDIR, transpose(CBBNORMAL))
        call this%initstate()
    end subroutine bp_init
end module hardening_model_bp
