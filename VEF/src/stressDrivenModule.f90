#include "criMacros.fpp"
include 'mkl_rci.f90'

!> Implementation of a altay-based DMC computiational module.
module dmcStressDrivenModule
    use, intrinsic:: iso_fortran_env, only: error_unit
    use iso_c_binding
    use utils
    use criUncomment, only: readValue
    use dmcResultTable
    use dmcBasicModule
    use logging
    use commonUtils
    use mkl_rci

    implicit none

    private
    public:: StressDrivenModule, &
             OBJECTIVE_THRESHOLD, &
             YLPResult, &
             deriveylpresult

    character(*), parameter:: MOD_NAME = 'stressDrivnModule'
    real(DP), parameter:: OBJECTIVE_THRESHOLD = 1.E-2_DP

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

    !> Abstract class implementing basic subset of operations that are shared by all
    !> stress-drien computational modules
    type, extends(BasicModule):: StressDrivenModule
        class(ResultTable), pointer  :: ptr_db => null()
    contains
        procedure, pass(this)     :: initialize => StressDrivenModule_initialize
        procedure, pass(this)     :: finalize => StressDrivenModule_finalize
        procedure, pass(this)     :: findSolution => StressDrivenModule_findSolution
    end type

    interface
        integer(C_INT) function trust_region_solve(stress_target, stress_mode, strain_mode, jacobi, residual) bind(C) result(mkl_result_code)
            import C_INT, &
                   C_DOUBLE

            real(C_DOUBLE), dimension(5), intent(in):: stress_target
            real(C_DOUBLE), dimension(5), intent(out):: stress_mode
            real(C_DOUBLE), dimension(5), intent(out):: strain_mode
            real(C_DOUBLE), dimension(5, 5), intent(out):: jacobi
            real(C_DOUBLE), dimension(5), intent(out):: residual
        end function
    end interface

contains

    !> Create YLPResult from arbitrary vS and performs normalization.
    !>
    !> \post A correctly initialized result has non-zero vS_length field.
    pure function YLPResult_init(vS) result(res)
        type(YLPResult):: res
        real(DP), dimension(5), intent(in):: vS

        res%vS_length = norm2(vS)
        if (res%vS_length > 0.D0) res%vS = vS/res%vS_length
    end function

    !> Derive dependant fields from properly initialized and evaluated YLPResult;
    !>
    !> This requires fields: vS, vA and vS_length.
    !> \return VEF_ERROR if input ylp_result contains wrong data.
    integer function deriveYLPResult(ylp_result) result(info)
        type(YLPResult), intent(inout)   :: ylp_result

        real(DP):: SonA_norm

        info = VEF_ERROR
        SonA_norm = norm2(ylp_result%vSonA)
        if ((ylp_result%vS_length < epsilon(0.D0)) .or. (SonA_norm < epsilon(0.D0))) return
        !
        ylp_result%dotWonA = dot_product(ylp_result%vA, ylp_result%vSonA)
        ylp_result%scal_s = SonA_norm/ylp_result%vS_length
        ! Calculate normalized stess
        ylp_result%vSonAn = ylp_result%vSonA/SonA_norm
        info = VEF_OK
    end function

    !> Initialize a configured StressDrivenModule object
    integer function StressDrivenModule_initialize(this) result(info)
        class(StressDrivenModule), intent(inout)          :: this

        integer:: ierr

        RETURN_IF(info /= VEF_OK, info = this%BasicModule%initialize())
        allocate(this%ptr_db, stat = ierr)
    end function

    !> Finalization of the module
    integer function StressDrivenModule_finalize(this) result(info)
        class(StressDrivenModule), intent(inout):: this

        if (associated(this%ptr_db)) then
            deallocate(this%ptr_db)
        endif
        info = this%BasicModule%finalize()
    end function

    !> Calculate plastic strain rate D that corresponds to the superimposed input stress `sigma`
    !> by performing an iterative search.
    !>
    !> The results of the iterative search are placed in ylp_results.
    !> \return VEF_ERROR on lack of convergence. ylp_result and D are set to the best solution found
    !> \return VEF_ERROR or any criErr_*on severe error conditions. ylp_result and D are undefined
    !> \return VEF_OK on success
    integer function StressDrivenModule_findSolution(this, sigma, D, ylp_result, vM_guess) result(info)
        class(StressDrivenModule), intent(in)   :: this
        real(DP), dimension(3, 3), intent(in):: sigma
        real(DP), dimension(3, 3), intent(inout):: D
        type(YLPResult), intent(out)     :: ylp_result !< Results of the iterative search
        !> Flag: use von Mises inital guess (default: .true.). If false, D will be used as the
        !> starting point for the iterative search.
        logical, intent(in), optional     :: vM_guess

        real(DP):: vA_norm
        real(DP), dimension(5):: vS            !< Input stress in 5D deviatoric stress space
        logical:: use_vM_guess
        real(DP), parameter:: pretry_search_angle = 2._DP/RAD_TO_DEG
        real(DP):: r(5), &
                   jacobi(5,5)


        info = VEF_ERROR
        use_vM_guess = optionalDefault(vM_guess, .true.)

        ! Convert input to the 5D space and make the unit vector(s).
        ! This also makes sure it is deviatoric.
        vS = convert_stress_strain_space(sigma)
        ylp_result = YLPResult(vS)

        if (.not. use_vM_guess) then
            ylp_result%vA = convert_stress_strain_space(D)
            vA_norm = norm2(ylp_result%vA)
            if (vA_norm < epsilon(0.D0)) return
        else if (this%ptr_db%get(ylp_result%vS, ylp_result%vA, max_angle = pretry_search_angle) /= VEF_OK) then
            ylp_result%va = ylp_result%vs
        endif

        if(trust_region_solve(ylp_result%vs/norm2(ylp_result%vs), ylp_result%vsona, ylp_result%va, jacobi, r) /= TR_SUCCESS) &
            call log_error(MOD_NAME, 'multilevelYlp', ERR, 'Error in MKL')


        info = merge(VEF_FAIL, VEF_OK, norm2(r) > OBJECTIVE_THRESHOLD)

        if (associated(this%ptr_db)) &
            call this%ptr_db%put(ylp_result%va, ylp_result%vsona)

        if (info == VEF_ERROR .or. deriveYLPResult(ylp_result) /= VEF_OK) then
            info = VEF_ERROR
            return
        end if

        D = convert_stress_strain_space(ylp_result%vA)
    end function
end module
