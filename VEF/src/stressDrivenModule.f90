#include "criMacros.fpp"

!> Implementation of a altay-based DMC computiational module.
module dmcStressDrivenModule
use, intrinsic:: iso_fortran_env, only: error_unit
use utils
use criUncomment, only: readValue
use alamYLP
use dmcYLPResult
use dmcResultTable
use dmcBasicModule
use logging
use commonUtils
use nllstr

implicit none

    public:: StressDrivenModule
    private

    !> Abstract class implementing basic subset of operations that are shared by all
    !> stress-drien computational modules
    type, extends(BasicModule):: StressDrivenModule

        type(YLPResultTolerance)    :: solution_tolerance

        class(ResultTable), pointer  :: ptr_db => null()

    contains
        procedure, pass(this)     :: initialize => StressDrivenModule_initialize

        procedure, pass(this)     :: readConfig => StressDrivenModule_readConfig

        procedure, pass(this)     :: finalize => StressDrivenModule_finalize

        procedure, pass(this)     :: findSolution => StressDrivenModule_findSolution

        procedure, pass(this)     :: search => StressDrivenModule_search

    end type


contains


    !> Initialize a configured StressDrivenModule object
    integer function StressDrivenModule_initialize(this) result(info)
    class(StressDrivenModule), intent(inout)          :: this
    !
    integer:: ierr
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



    integer function StressDrivenModule_readConfig(this, cnfunit) result(info)
    class(StressDrivenModule), intent(inout)          :: this
    integer, intent(in)                        :: cnfunit
    !
        info = this%BasicModule%readConfig(cnfunit)
        if (info /= VEF_OK) return
        !
        ! Read multilevelYLP configuration
        !User-provided connfiguration is no longer supported and thus ignored in the rest of the program. A call to  this procedure
        !must however remain to not mess up the existing configuration file format.
        call readYLPConfigSection(cnfunit, info)

        if (info /= VEF_OK) &
            call log_error('StressDrivenModule', 'readConfig', ERR_VAL, 'Check YLP config section.')
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    !
    end function


    !> Finalization of the module
    integer function StressDrivenModule_finalize(this) result(info)
    class(StressDrivenModule), intent(inout):: this
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
    !> \return VEF_ERROR or any criErr_*on severe error conditions. ylp_result and D are undefined
    !> \return VEF_OK on success
    integer function StressDrivenModule_findSolution(this, sigma, D, ylp_result, vM_guess, is_acceptable) result(info)
        class(StressDrivenModule), intent(in)   :: this
        real(DP), dimension(3, 3), intent(in):: sigma
        real(DP), dimension(3, 3), intent(inout):: D
        type(YLPResult), intent(out)     :: ylp_result !< Results of the iterative search
        !> Flag: use von Mises inital guess (default: .true.). If false, D will be used as the
        !> starting point for the iterative search.
        logical, intent(in), optional     :: vM_guess
        logical, intent(out), optional    :: is_acceptable

        real(DP):: vA_norm
        real(DP), dimension(5):: vS            !< Input stress in 5D deviatoric stress space
        logical:: use_vM_guess
        type(ObjectiveFunction), target:: obj_func
        type(YLPResult)  :: ylp_result_retry, ylp_result_pretry
        real(DP), parameter:: pretry_search_angle = 2._DP/RAD_TO_DEG

        info = VEF_ERROR
        obj_func%ptr_db => this%ptr_db
        if (present(is_acceptable)) is_acceptable = .false.
        use_vM_guess = optionalDefault(vM_guess, .true.)

        ! Convert input to the 5D space and make the unit vector(s).
        ! This also makes sure it is deviatoric.
        vS = convert_stress_strain_space(sigma)
        ylp_result = YLPResult(vS)

        if (.not. use_vM_guess) then
            ylp_result%vA = convert_stress_strain_space(D)
            vA_norm = norm2(ylp_result%vA)
            if (vA_norm < epsilon(0.D0)) return
        else if (this%ptr_db%get(ylp_result%vS, ylp_result%vA, max_angle = pretry_search_angle) == VEF_OK) then
                use_vm_guess = .false.
        endif

        !Calculate the corresponding strain rate vA
        info = this%search(ylp_result, use_vM_guess, obj_func)

        !If the search fails, we try again with the end point of the previous search as the new starting point. This leads to
        !convergence after all in the majority of cases. Note that experiments have shown that increasing the number of retries does not
        !notably improve convergence further.
        if (info == VEF_FAIL .and. associated(this%ptr_db)) then
            !
            ! Try another starting point
            !
            ! Set the re-try point
            ylp_result_retry = ylp_result
            !
            if (this%ptr_db%get(ylp_result_retry%vS, ylp_result_retry%vA) == VEF_OK) then
                ! get new solution
                info = this%search(ylp_result_retry, .false., obj_func)
                ! Use the better of the two
                if (ylp_result_retry%R < ylp_result%R) ylp_result = ylp_result_retry
            endif
        endif
        RETURN_IF_WITH(info == VEF_ERROR, info = VEF_ERROR)
        !
        if (present(is_acceptable)) then
            is_acceptable = checkYLPResult(ylp_result, this%solution_tolerance, OBJECTIVE_THRESHOLD)
        endif
        !
        D = convert_stress_strain_space(ylp_result%vA)
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
    integer function StressDrivenModule_search(this, ylp_result, use_vM_guess, obj_func) result(info)
    class(StressDrivenModule), intent(in):: this
    type(YLPResult), intent(inout)           :: ylp_result
    logical, intent(in)                      :: use_vM_guess
    class(ObjectiveFunction), intent(inout)  :: obj_func
    !
        call multilevelYLP(ylp_result%vS,    &
                           ylp_result%vA,    &
                           ylp_result%vSonA, &
                           ylp_result%R,     &
                           info,             &
                           useVMGuess = use_vM_guess, &
                           verbose = this%output%verbosity, &
                           objective_function = obj_func)
        if (info == VEF_ERROR) return
        if (deriveYLPResult(ylp_result) /= VEF_OK) info = VEF_ERROR
    !
    end function


    !> Read configuration of the solver (libalamylp)
    subroutine readYLPConfigSection(cnfunit, info)
    integer, intent(in)                        :: cnfunit
    integer, intent(out)                       :: info
    !
    real(DP), dimension(2):: tmp
    real(DP):: dummy
    logical:: dummy_2
    logical:: use_default_solver_settings, use_advanced_settings
    !
        info = VEF_ERROR
        use_default_solver_settings = .true.
        use_advanced_settings = .false.
        if (.not. readValue(cnfunit, use_default_solver_settings)) return
        if (.not. use_default_solver_settings) then
            if (.not. readValue(cnfunit, dummy)) then
                 write(error_unit, fmt = 900) 'Check epsilon controlling numerical estimation over Jacobian.'
                 return
            endif
            if (.not. readValue(cnfunit, dummy_2)) return
            ! read default_eps and obj_func_eps
            if (.not. readValue(cnfunit, tmp)) then
                 write(error_unit, fmt = 900) 'Check the linearization parameters.'
                 return
            endif
            ! read flag for advanced settings (placeholder at the moment)
            if (.not. readValue(cnfunit, use_advanced_settings)) return
        endif
        info = VEF_OK

#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    end subroutine

end module
