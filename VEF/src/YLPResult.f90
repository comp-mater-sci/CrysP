!> Datatypes that simplify work with results of multilevelYLP and procedures
!> that operate on these datatypes.
module dmcYLPResult
use utils

implicit none

    !> Datatype to store results of iterative search
    type:: YLPResult
        real(DP), dimension(5):: vA = 0.D0       !< Strain rate mode on yield locus
        real(DP), dimension(5):: vS = 0.D0       !< Imposed stress
        real(DP), dimension(5):: vSonA = 0.D0    !< Stress corresponding to A
        real(DP), dimension(5):: vSonAn = 0.D0   !< Stress mode corresponding to A
        real(DP):: R = 0.D0                  !< Norm of stress residual
        real(DP):: dotWonA = 0.D0            !< Work rate corresponding to vA and vSonA
        real(DP):: scal_s = 0.D0             !< norm2(vSonA) / vS_length
        real(DP):: vS_length = 0.D0          !< norm2(vS)
    end type


    !> Constructors of YLPResult type
    interface YLPResult
        module procedure YLPResult_init
    end interface

    real(DP), parameter, private:: default_residual_tolerance_factor = 5.D0

    !> Default angular tolerance (given in degrees)
    real(DP), parameter, private:: default_angular_tolerance = 0.25D0

    !> Datatype for commonly used tolerances that the YLPResult should meet to be
    !> an acceptable solution. The defaults are
    type:: YLPResultTolerance
        !> Tolerance in terms of residual norm
        real(DP)    :: residual_tolerance_factor = default_residual_tolerance_factor
        !> Tolerance in terms of angle between requested stess and identified stress (in degrees)
        real(DP)    :: angular_tolerance = default_angular_tolerance
    end type

contains


    !> Create YLPResult from arbitrary vS and performs normalization.
    !>
    !> \post A correctly initialized result has non-zero vS_length field.
    pure function YLPResult_init(vS) result(res)
    type(YLPResult):: res
    real(DP), dimension(5), intent(in):: vS
    !
        res%vS_length = norm2(vS)
        if (res%vS_length > 0.D0) res%vS = vS/res%vS_length
    !
    end function


    !> Derive dependant fields from properly initialized and evaluated YLPResult;
    !>
    !> This requires fields: vS, vA and vS_length.
    !> \return VEF_ERROR if input ylp_result contains wrong data.
    integer function deriveYLPResult(ylp_result) result(info)
    type(YLPResult), intent(inout)   :: ylp_result
    !
    real(DP):: SonA_norm
    !
        info = VEF_ERROR
        SonA_norm = norm2(ylp_result%vSonA)
        if ((ylp_result%vS_length < epsilon(0.D0)) .or. (SonA_norm < epsilon(0.D0))) return
        !
        ylp_result%dotWonA = dot_product(ylp_result%vA, ylp_result%vSonA)
        ylp_result%scal_s = SonA_norm/ylp_result%vS_length
        ! Calculate normalized stess
        ylp_result%vSonAn = ylp_result%vSonA/SonA_norm
        info = VEF_OK
    !
    end function
end module
