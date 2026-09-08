module incrementation
    use base_defs
    use logging
    use math_utils
    use iso_c_binding

    implicit none

    private
    public:: StrainIncrement, &
             StressIncrement, &
             StrainIncrementFactory, &
             StressIncrementFactory

    character(*), parameter:: MOD_NAME = 'incrementation'

    type, bind(C):: StrainIncrement
        real(C_DOUBLE):: duration = 0._DP
        real(C_DOUBLE), dimension(3,3):: deformation_gradient = UNIT_MATRIX_3X3 !! Technically redundant but saves a lot of computation
        real(C_DOUBLE):: vm_strain = 0._DP                           !! Von mises true strain. Technically redundant but saves a lot of computation
        real(C_DOUBLE), dimension(3,3):: stress = 0._DP
        real(C_DOUBLE):: taylor_factor = 0._DP
    end type

    type, bind(C):: StressIncrement
        real(C_DOUBLE), dimension(5):: strain_rate
        real(C_DOUBLE), dimension(5):: residual
        integer(C_INT):: n_strain_increments
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

    subroutine add_strain_increment(this, increment, padding)
        class(StrainIncrementFactory), intent(inout):: this
        type(StrainIncrement), intent(in):: increment
        integer, intent(in):: padding

        type(StrainIncrement), allocatable:: buffer(:)

        if (this%size == 0) then
            allocate(this%increments(padding))
        else if (this%size == size(this%increments)) then
            allocate(buffer(this%size+padding))
            buffer(:this%size) = this%increments
            call move_alloc(buffer, this%increments)
        end if

        this%size = this%size + 1
        this%increments(this%size) = increment
    end subroutine
    subroutine add_stress_increment(this, increment, padding)
        class(StressIncrementFactory), intent(inout):: this
        type(StressIncrement), intent(in):: increment
        integer, intent(in):: padding

        type(StressIncrement), allocatable:: buffer(:)

        if (this%size == 0) then
            allocate(this%increments(padding))
        else if (this%size == size(this%increments)) then
            allocate(buffer(this%size+padding))
            buffer(:this%size) = this%increments
            call move_alloc(buffer, this%increments)
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
