module incrementation
    use base_defs
    use math_utils

    implicit none

    private
    public:: Increment, &
             IncrementFactory

    character(*), parameter:: MOD_NAME = 'incrementation'

    type:: Increment
        real(DP), dimension(3,3):: velocity_gradient = 0._DP
        real(DP), dimension(6):: stress_mode_deviation = 0._DP     !! Deviation of stress mode from target stress mode, if applicable.
        real(DP):: duration = 0._DP
        real(DP), dimension(3,3):: deformation_gradient = UNIT_MATRIX_3X3 !! Technically redundant but saves a lot of computation
        real(DP):: vm_strain = 0._DP                           !! Von mises true strain. Technically redundant but saves a lot of computation
        real(DP), dimension(3,3):: stress = 0._DP
        real(DP):: taylor_factor = 0._DP
        integer, dimension(:,:), allocatable:: texture
    end type

    type:: IncrementFactory
        integer, private:: size = 0
        type(Increment), dimension(:), allocatable, private:: increments
    contains
        procedure:: add => add_increments
        procedure:: get => get_increments
    end type

contains

    subroutine add_increments(this, increments, padding)
        class(IncrementFactory), intent(inout):: this
        type(Increment), dimension(:), intent(in):: increments
        integer, intent(in):: padding

        integer:: n_new_incs
        type(Increment), allocatable:: buffer(:)

        n_new_incs = size(increments)
        if (this%size == 0) then
            allocate(this%increments(n_new_incs + padding))
        else if (this%size + n_new_incs > size(this%increments)) then
            allocate(buffer(this%size + n_new_incs + padding))
            buffer(:this%size) = this%increments(:this%size)
            call move_alloc(buffer, this%increments)
        end if

        this%increments(this%size+1:this%size + n_new_incs) = increments
        this%size = this%size + n_new_incs
    end subroutine

    function get_increments(this) result(increments)
        class(IncrementFactory), intent(in):: this
        type(Increment), dimension(this%size):: increments

        increments = this%increments(:this%size)
    end function
end module
