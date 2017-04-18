! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of the initial release: 2017-02-08, as a result of refactoring 'stressDrivenModule.f90'
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

#include "criMacros.fpp"

!> Implementation of a altay-based DMC computiational module.
module dmcStressDrivenModule
use alamYLP
use dmcYLPResult
use dmcAlamEvalCached
use dmcResultTable
use dmcBasicModule
implicit none


    !> Abstract class implementing basic subset of operations that are shared by all 
    !> stress-drien computational modules
    type,extends(BasicModule) :: StressDrivenModule

        type(multilevelYLPConfig)   :: ylp

        type(YLPResultTolerance)    :: solution_tolerance

        class(ResultTable),pointer  :: ptr_db => null()

    contains
        procedure,pass(this)     :: initialize => StressDrivenModule_initialize

        procedure,pass(this)     :: readConfig => StressDrivenModule_readConfig

        procedure,pass(this)     :: printConfig => StressDrivenModule_printConfig

        procedure,pass(this)     :: finalize => StressDrivenModule_finalize

        procedure,pass(this)     :: findSolution => StressDrivenModule_findSolution

        procedure,pass(this)     :: search => StressDrivenModule_search

    end type


contains


    !> Initialize a configured StressDrivenModule object
    integer function StressDrivenModule_initialize(this) result(info)
    implicit none
    class(StressDrivenModule),intent(inout)          :: this
    !
    integer :: ierr
    !
        !
        ! Let the superclass do its initialization first ...
        !
        info = this%BasicModule%initialize()
        !
        ! ... and then do your own initialization
        !
        ! Allocate and possibly populate the result cache
        allocate(this%ptr_db, stat = ierr)
        RETURN_IF_WITH(ierr /= 0, info=criErr_MemAlloc)
        ! Try to load data
        ierr = this%ptr_db%load(trim(this%output%outputPrefix)//'.rtdb')
    !
    end function


    
    integer function StressDrivenModule_readConfig(this,cnfunit) result(info)
    implicit none
    class(StressDrivenModule),intent(inout)          :: this
    integer,intent(in)                        :: cnfunit
    !
        info = this%BasicModule%readConfig(cnfunit)
        if (info /= criSuccess) return 
        !
        ! Read multilevelYLP configuration
        call readYLPConfigSection(cnfunit,this%ylp,info)
        if (info /= criSuccess) then
            write(errmsg,fmt=901) 'check YLP config section' 
            return
        endif
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    !
    end function
    
    
    !> Print out configuration of the module
    integer function StressDrivenModule_printConfig(this,outunit) result(info)
    use altayConfig
    implicit none
    class(StressDrivenModule),intent(in):: this
    integer,intent(in)                  :: outunit
    !
        info = this%BasicModule%printConfig(outunit)
        if (doLogging(criLogInfo,this%output%verbosity)) then
            !
            if (this%ylp%linearize) then
                write(display_unit,100) 'Info: the program will first attempt to linearize the identification problems.'
            else
                write(display_unit,100) 'Info: The program will attempt to solve the nonlinear problems.'
            endif
        endif
        !
        info = criSuccess
        !
        100 format(/,A,/)
    !
    end function
    
    
    !> Finalization of the module
    integer function StressDrivenModule_finalize(this) result(info)
    implicit none
    class(StressDrivenModule),intent(inout) :: this
    !
        !
        ! Do your own finalization first ...
        !
        if (doLogging(criLogDebug, this%output%verbosity)) then
            write(display_unit,'(A,1X,I8,1X,A)') 'Objective function was called', alamEval_objFx_call_count, 'times'
        endif
        !
        ! Save the result cache and delete the object
        if (associated(this%ptr_db)) then
            info = this%ptr_db%store(trim(this%output%outputPrefix)//'.rtdb')
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
    !> \return criFailure on lack of convergence. ylp_result and D are set to the best solution found
    !> \return criError or any criErr_* on severe error conditions. ylp_result and D are undefined
    !> \return criSuccess on success
    integer function StressDrivenModule_findSolution(this,sigma, D, ylp_result, vM_guess, is_acceptable) result(info)
    implicit none
    class(StressDrivenModule),intent(in)   :: this
    type(SRTensor),intent(in)       :: sigma !< Input stress
    type(SRTEnsor),intent(inout)    :: D     !< Plastic strain rate
    type(YLPResult),intent(out)     :: ylp_result !< Results of the interative search
    !> Flag: use von Mises inital guess (default: .true.). If false, D will be used as the
    !> starting point for the iterative search.
    logical,intent(in),optional     :: vM_guess
    logical,intent(out),optional    :: is_acceptable
    !
    double precision :: vS_norm, vA_norm, SonA_norm
    logical :: use_vM_guess
    type(NormalizedV5DCompCached),target :: obj_func
    !
    type(YLPResult)  :: ylp_result_retry
    !
        info = criErr_BadArgs

        obj_func%ptr_db => this%ptr_db
        !
        if (present(is_acceptable)) is_acceptable = .false.
        !
        ! Convert input to the 5D space and make the unit vector(s).
        ! This also makes sure it is deviatoric.
        ylp_result%vS = tens2vec5D(sigma%t)
        vS_norm = norm2(ylp_result%vS)
        if (vS_norm < epsilon(0.D0)) return
        ylp_result%vS = ylp_result%vS / vS_norm
        !
        use_vM_guess = optionalDefault(vM_guess, .true.)
        if (.not. use_vM_guess) then
            ylp_result%vA = tens2vec5D(D%t)
            vA_norm = norm2(ylp_result%vA)
            if (vA_norm < epsilon(0.D0)) return
        endif
        ! Calculate the corresponding strain rate vA
        info = this%search(ylp_result, use_vM_guess, obj_func)
        if (info == criFailure .and. associated(this%ptr_db)) then
            ! Try another starting point
            if (this%ptr_db%get(ylp_result%vS, ylp_result_retry%vA) == criSuccess) then
                if (doLogging(criLogDebug,this%output%verbosity)) then
                     write(display_unit,860) 'Poor convergence, re-attempting to find the solution.'
                endif
                ! Set the re-try point
                ylp_result_retry%vS = ylp_result%vS
                !
                ! get new solution
                info = this%search(ylp_result_retry, .false., obj_func)
                ! Use the better of the two
                if (ylp_result_retry%R < ylp_result%R) ylp_result = ylp_result_retry
            endif
        endif
        SonA_norm = norm2(ylp_result%vSonA)
        if (is_error(info) .or. (SonA_norm < epsilon(0.D0))) then
            info = criError
            return
        endif
        !
        ylp_result%dotWonA = dot_product(ylp_result%vA, ylp_result%vSonA)
        ylp_result%scal_s = SonA_norm / vS_norm
        ! Calculate normalized stess
        ylp_result%vSonAn = ylp_result%vSonA / SonA_norm
        if (present(is_acceptable)) then
            is_acceptable = checkYLPResult(ylp_result, this%solution_tolerance, this%ylp%obj_func_eps)
        endif
        !
        D%t = vec5D2tens(ylp_result%vA)
        ! Return the info from the last call to 'search'
        !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    !
    end function


    !> Wrapper around multilevelYLP that uses YLPResult for communicating with the caller.
    !> 
    !> It applies settings provided as members of StressDrivenModule.
    integer function StressDrivenModule_search(this, ylp_result, use_vM_guess, obj_func) result(info)
    implicit none
    class(StressDrivenModule),intent(in):: this
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
                           YLPconfig=this%ylp, &
                           verbose=this%output%verbosity, &
                           objective_function=obj_func)
    !
    end function
    
    
    !> Read configuration of the solver (libalamylp)
    subroutine readYLPConfigSection(cnfunit,cnf,info)
    implicit none
    integer,intent(in)                        :: cnfunit
    type(multilevelYLPConfig),intent(out)     :: cnf
    integer,intent(out)                       :: info
    !
    double precision,dimension(2) :: tmp
    logical :: use_default_solver_settings, use_advanced_settings
    !
        info = criErr_IORead
        use_default_solver_settings = .true.
        use_advanced_settings = .false.
        if (.not. readValue(cnfunit, use_default_solver_settings)) return
        if (.not. use_default_solver_settings) then
            if (.not. readValue(cnfunit, cnf%jacobi_eps)) return
            if (.not. readValue(cnfunit, cnf%linearize)) return
            ! read default_eps and obj_func_eps
            if (.not. readValue(cnfunit,tmp)) return
            cnf%default_eps = tmp(1)
            cnf%obj_func_eps = tmp(2)
            ! read flag for advanced settings (placeholder at the moment)
            if (.not. readValue(cnfunit, use_advanced_settings)) return
        endif
        info = criSuccess
    !
    end subroutine


end module