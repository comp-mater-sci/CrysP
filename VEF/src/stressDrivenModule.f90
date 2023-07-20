#include "criMacros.fpp"

!> Implementation of a altay-based DMC computiational module.
module dmcStressDrivenModule
use,intrinsic :: iso_fortran_env, only: error_unit
use definitions
use criUncomment, only: readValue
use criMathUtils, only: vec5D2tens,tens2vec5D
use alamYLP
use alamEval, only: NormalizedV5DComp, alamEval_objFx_call_count
use dmcYLPResult
use dmcAlamEvalCached
use dmcResultTable
use dmcBasicModule
use logging
use commonUtils

implicit none

    public :: StressDrivenModule
    private

    !> Abstract class implementing basic subset of operations that are shared by all
    !> stress-drien computational modules
    type,extends(BasicModule) :: StressDrivenModule

        type(multilevelYLPConfig)   :: ylp

        type(YLPResultTolerance)    :: solution_tolerance

        class(ResultTable),pointer  :: ptr_db => null()

    contains
        procedure,pass(this)     :: initialize => StressDrivenModule_initialize

        procedure,pass(this)     :: readConfig => StressDrivenModule_readConfig

        procedure,pass(this)     :: finalize => StressDrivenModule_finalize

        procedure,pass(this)     :: findSolution => StressDrivenModule_findSolution

        procedure,pass(this)     :: search => StressDrivenModule_search

    end type


contains


    !> Initialize a configured StressDrivenModule object
    integer function StressDrivenModule_initialize(this) result(info)
    class(StressDrivenModule),intent(inout)          :: this
    !
    integer :: ierr
    !
        !
        ! Let the superclass do its initialization first ...
        !
        RETURN_IF(info /= VEF_OK, info = this%BasicModule%initialize())
        !
        ! ... and then do your own initialization
        !
        ! Allocate and possibly populate the result cache
        allocate(this%ptr_db, stat = ierr)
    end function



    integer function StressDrivenModule_readConfig(this,cnfunit) result(info)
    class(StressDrivenModule),intent(inout)          :: this
    integer,intent(in)                        :: cnfunit
    !
        info = this%BasicModule%readConfig(cnfunit)
        if (info /= VEF_OK) return
        !
        ! Read multilevelYLP configuration
        call readYLPConfigSection(cnfunit,this%ylp,info)
        if (info /= VEF_OK) &
            call log_error('StressDrivenModule', 'readConfig', ERR_VAL, 'Check YLP config section.')
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    !
    end function


    !> Finalization of the module
    integer function StressDrivenModule_finalize(this) result(info)
    class(StressDrivenModule),intent(inout) :: this
    !

        ! Save the result cache and delete the object
        if (associated(this%ptr_db)) then
            deallocate(this%ptr_db)
        endif
        !
        ! ... and then finalize the superclass.
        !
        info = this%BasicModule%finalize()
    !
    end function

    !> Calculate plastic strain rate D that corresponds to the superimposed input stress `sigma`
    !> by performing an iterative search.
    !>
    !> The results of the iterative search are placed in ylp_results.
    !> \return VEF_ERROR on lack of convergence. ylp_result and D are set to the best solution found
    !> \return VEF_ERROR or any criErr_* on severe error conditions. ylp_result and D are undefined
    !> \return VEF_OK on success
    integer function StressDrivenModule_findSolution(this,sigma, D, ylp_result, vM_guess, is_acceptable, pretry) result(info)
    class(StressDrivenModule),intent(in)   :: this
    real(DP), dimension(3,3), intent(in) :: sigma
    real(DP), dimension(3,3), intent(inout) :: D
    type(YLPResult),intent(out)     :: ylp_result !< Results of the iterative search
    !> Flag: use von Mises inital guess (default: .true.). If false, D will be used as the
    !> starting point for the iterative search.
    logical,intent(in),optional     :: vM_guess
    logical,intent(out),optional    :: is_acceptable
    logical,intent(in),optional     :: pretry
    !
    real(DP) :: vA_norm
    real(DP),dimension(alamEval_vSD_dim) :: vS            !< Input stress in 5D deviatoric stress space
    logical :: use_vM_guess, use_pretry, is_pretry_acceptable
    type(NormalizedV5DCompCached),target :: obj_func
    !
    type(YLPResult)  :: ylp_result_retry, ylp_result_pretry
    type(multilevelYLPConfig)   :: ylp_pretry
    real(DP), parameter :: pretry_search_angle = pi_deg * 2.0D0
    !
        info = VEF_ERROR

        obj_func%ptr_db => this%ptr_db
        !
        if (present(is_acceptable)) is_acceptable = .false.
        !
        use_vM_guess = optionalDefault(vM_guess, .true.)
        ! Pre-try if requested and no explicit initial quess is provided
        use_pretry = optionalDefault(pretry, .true.) .and. use_vM_guess
        !
        ! Convert input to the 5D space and make the unit vector(s).
        ! This also makes sure it is deviatoric.
        vS = tens2vec5D(sigma)
        ylp_result = YLPResult(vS)
        if (ylp_result%vS_length < epsilon(0.D0)) return
        !
        is_pretry_acceptable = .false.
        if (use_pretry) then
            !
            ylp_result_pretry = ylp_result
            !
            if (this%ptr_db%get(ylp_result_pretry%vS, &
                                ylp_result_pretry%vA, &
                                max_angle=pretry_search_angle) == VEF_OK) then
                ! Use special settings for pre-try
                ylp_pretry = this%ylp
                ylp_pretry%linearize = .true.
                ylp_pretry%nonlinear = .false.
                !
                ! get the solution
                info = this%search(ylp_pretry, ylp_result_pretry, .false., obj_func)
                ! Accept the solution only if it reached the requested quality
                if (info == VEF_OK .and. (ylp_result_pretry%R < this%ylp%obj_func_eps)) then
                    is_pretry_acceptable = .true.
                    ylp_result = ylp_result_pretry
                endif
            endif
            !
        endif
        !
        if (.not. is_pretry_acceptable) then
            !
            if (.not. use_vM_guess) then
                ylp_result%vA = tens2vec5D(D)
                vA_norm = norm2(ylp_result%vA)
                if (vA_norm < epsilon(0.D0)) return
            endif
            ! Calculate the corresponding strain rate vA
            info = this%search(this%ylp, ylp_result, use_vM_guess, obj_func)
            if (info == VEF_FAIL .and. associated(this%ptr_db)) then
                !
                ! Try another starting point
                !
                ! Set the re-try point
                ylp_result_retry = ylp_result
                !
                if (this%ptr_db%get(ylp_result_retry%vS, ylp_result_retry%vA) == VEF_OK) then
                    ! get new solution
                    info = this%search(this%ylp, ylp_result_retry, .false., obj_func)
                    ! Use the better of the two
                    if (ylp_result_retry%R < ylp_result%R) ylp_result = ylp_result_retry
                endif
            endif
            RETURN_IF_WITH(info == VEF_ERROR, info = VEF_ERROR)
            ! Rare case: normal search and re-try cannot improve over pre-try
            if (info /= VEF_OK .and. use_pretry) then
                if (ylp_result_pretry%R < ylp_result%R) ylp_result = ylp_result_pretry
            endif
        endif
        !
        if (present(is_acceptable)) then
            is_acceptable = checkYLPResult(ylp_result, this%solution_tolerance, this%ylp%obj_func_eps)
        endif
        !
        D = vec5D2tens(ylp_result%vA)
        ! Return the info from the last call to 'search'
        !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    !
    end function


    !> Wrapper around multilevelYLP that uses YLPResult for communicating with the caller.
    !>
    !> The wrapper applies settings provided as members of StressDrivenModule.
    !> It provides a ready-to-use ylp_result on non-error info code.
    !> \return Exit code from multilevelYLP, unless an error condition occurs
    !> at later stage. In such case VEF_ERROR is returned.
    !> In such case
    integer function StressDrivenModule_search(this, ylp_config, ylp_result, use_vM_guess, obj_func) result(info)
    class(StressDrivenModule),intent(in):: this
    type(multilevelYLPConfig),intent(in)    :: ylp_config
    type(YLPResult),intent(inout)           :: ylp_result
    logical,intent(in)                      :: use_vM_guess
    class(NormalizedV5DComp),intent(inout)  :: obj_func
    !
        call multilevelYLP(ylp_result%vS,    &
                           ylp_result%vA,    &
                           ylp_result%vSonA, &
                           ylp_result%R,     &
                           info,             &
                           useVMGuess=use_vM_guess, &
                           YLPconfig=ylp_config, &
                           verbose=this%output%verbosity, &
                           objective_function=obj_func)
        if (info == VEF_ERROR) return
        if (deriveYLPResult(ylp_result) /= VEF_OK) info = VEF_ERROR
    !
    end function


    !> Read configuration of the solver (libalamylp)
    subroutine readYLPConfigSection(cnfunit,cnf,info)
    integer,intent(in)                        :: cnfunit
    type(multilevelYLPConfig),intent(out)     :: cnf
    integer,intent(out)                       :: info
    !
    real(DP),dimension(2) :: tmp
    logical :: use_default_solver_settings, use_advanced_settings
    !
        info = VEF_ERROR
        use_default_solver_settings = .true.
        use_advanced_settings = .false.
        if (.not. readValue(cnfunit, use_default_solver_settings)) return
        if (.not. use_default_solver_settings) then
            if (.not. readValue(cnfunit, cnf%jacobi_eps)) then
                 write(error_unit,fmt=900) 'Check epsilon controlling numerical estimation over Jacobian.'
                 return
            endif
            if (.not. readValue(cnfunit, cnf%linearize)) return
            ! read default_eps and obj_func_eps
            if (.not. readValue(cnfunit,tmp)) then
                 write(error_unit,fmt=900) 'Check the linearization parameters.'
                 return
            endif
            cnf%default_eps = tmp(1)
            cnf%obj_func_eps = tmp(2)
            ! read flag for advanced settings (placeholder at the moment)
            if (.not. readValue(cnfunit, use_advanced_settings)) return
        endif
        info = VEF_OK

#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    end subroutine

end module
