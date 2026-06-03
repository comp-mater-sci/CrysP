module incrementation
    use base_defs
    use logging
    use math_utils

    implicit none

    private
    public:: StrainIncrement, &
             StressIncrement, &
             StrainIncrementFactory, &
             StressIncrementFactory

    character(*), parameter:: MOD_NAME = 'incrementation'

    type:: StrainIncrement
        real(DP):: duration = 0._DP
        real(DP), dimension(3,3):: deformation_gradient = UNIT_MATRIX_3X3 !! Technically redundant but saves a lot of computation
        real(DP):: vm_strain = 0._DP                           !! Von mises true strain. Technically redundant but saves a lot of computation
        real(DP), dimension(3,3):: stress = 0._DP
        real(DP):: taylor_factor = 0._DP
    end type

    type:: StressIncrement
        real(DP), dimension(5):: strain_rate
        real(DP), dimension(5):: residual
        type(StrainIncrement), dimension(:), allocatable:: strain_increments
    end type

    type, abstract:: IncrementFactory
        integer, private:: size = 0
    end type

    type, extends(IncrementFactory):: StrainIncrementFactory
        type(StrainIncrement), dimension(:), allocatable, private:: increments
    contains
        procedure:: add => add_strain_increment
        procedure:: get => get_strain_increments
    end type

    type, extends(IncrementFactory):: StressIncrementFactory
        type(StressIncrement), dimension(:), allocatable, private:: increments
    contains
        procedure:: add => add_stress_increment
        procedure:: get => get_stress_increments
    end type

contains

    subroutine add_strain_increment(this, increment, buffer)
        class(StrainIncrementFactory), intent(inout):: this
        type(StrainIncrement), intent(in):: increment
        integer, intent(in):: buffer

        type(StrainIncrement), allocatable:: inc_buffer(:)

        if (this%size == 0) then
            allocate(this%increments(buffer))
        else if (this%size == size(this%increments)) then
            allocate(inc_buffer(this%size+buffer))
            inc_buffer(:this%size) = this%increments
            call move_alloc(inc_buffer, this%increments)
        end if

        this%size = this%size + 1
        this%increments(this%size) = increment
    end subroutine
    subroutine add_stress_increment(this, increment, buffer)
        class(StressIncrementFactory), intent(inout):: this
        type(StressIncrement), intent(in):: increment
        integer, intent(in):: buffer

        type(StressIncrement), allocatable:: inc_buffer(:)

        if (this%size == 0) then
            allocate(this%increments(buffer))
        else if (this%size == size(this%increments)) then
            allocate(inc_buffer(this%size+buffer))
            inc_buffer(:this%size) = this%increments
            call move_alloc(inc_buffer, this%increments)
        end if

        this%size = this%size + 1
        this%increments(this%size) = increment
    end subroutine

    function get_strain_increments(this) result(increments)
        class(StrainIncrementFactory), intent(in):: this
        type(StrainIncrement), dimension(this%size):: increments

        increments = this%increments(:this%size)
    end function

    function get_stress_increments(this) result(increments)
        class(StressIncrementFactory), intent(in):: this
        type(StressIncrement), dimension(this%size):: increments

        increments = this%increments(:this%size)
    end function



end module


