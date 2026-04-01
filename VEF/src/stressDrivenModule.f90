!> Implementation of a altay-based DMC computiational module.
module dmcStressDrivenModule
    use, intrinsic:: iso_fortran_env, only: error_unit
    use iso_c_binding
    use conversions
    use criUncomment, only: readValue
    use dmcResultTable
    use dmcBasicModule
    use logging
    use commonUtils
    use mkl_rci

    implicit none

    private
    public:: StressDrivenModule


    character(*), parameter:: MOD_NAME = 'stressDrivenModule'

    !> Abstract class implementing basic subset of operations that are shared by all
    !> stress-drien computational modules
    type, extends(BasicModule):: StressDrivenModule
        class(ResultTable), pointer  :: ptr_db => null()
    contains
        procedure, pass(this):: initialize => StressDrivenModule_initialize
        procedure, pass(this):: finalize => StressDrivenModule_finalize
        procedure, pass(this):: findSolution => StressDrivenModule_findSolution
    end type

contains

    !> Initialize a configured StressDrivenModule object
    integer function StressDrivenModule_initialize(this) result(info)
        class(StressDrivenModule), intent(inout)          :: this

        integer:: ierr

        info = this%BasicModule%initialize()
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
    subroutine stressdrivenmodule_findSolution(this, target_stress_mode, strain_mode, stress, residual)
        class(StressDrivenModule), intent(in)   :: this
        real(DP), dimension(5), intent(in):: target_stress_mode
        real(DP), dimension(5), intent(out):: strain_mode
        real(DP), dimension(5), intent(out):: stress
        real(DP), dimension(5), intent(out):: residual

        real(DP), parameter:: pretry_search_angle = 0.035_DP !! Approx. 2 degrees

        if (associated(this%ptr_db)) then
            if (this%ptr_db%get(target_stress_mode, strain_mode, pretry_search_angle) /= VEF_OK) &
                strain_mode = target_stress_mode
        else
            strain_mode = target_stress_mode
        end if

        call altay_simulate_stress_mode(target_stress_mode, strain_mode, stress, residual)

        if (associated(this%ptr_db)) &
            call this%ptr_db%put(strain_mode, stress/norm2(stress))
    end subroutine
end module
