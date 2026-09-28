module incrementation
    use base_defs
    use math_utils

    implicit none

    private
    public:: StrainIncrement, &
             StrainIncrement_C, &
             StressIncrement, &
             StressIncrement_C, &
             StrainIncrementFactory, &
             StressIncrementFactory, &
             assignment(=)

    character(*), parameter:: MOD_NAME = 'incrementation'

    type:: StrainIncrement
        real(DP):: duration = 0._DP
        real(DP), dimension(3,3):: deformation_gradient = UNIT_MATRIX_3X3 !! Technically redundant but saves a lot of computation
        real(DP):: vm_strain = 0._DP                           !! Von mises true strain. Technically redundant but saves a lot of computation
        real(DP), dimension(3,3):: stress = 0._DP
        real(DP):: taylor_factor = 0._DP
        integer, dimension(:,:), allocatable:: texture
    end type

    !> C-compatible counterpart of [[StrainIncrement]].
    type, bind(C):: StrainIncrement_C
        real(C_DOUBLE):: duration
        real(C_DOUBLE), dimension(3,3):: deformation_gradient
        real(C_DOUBLE):: vm_strain
        real(C_DOUBLE), dimension(3,3):: stress
        real(C_DOUBLE):: taylor_factor
        integer(C_INT):: n_grains
        type(C_PTR):: texture                   !! `3*n_grains` ints: the Euler angles of each grain in degrees.
    end type

    type:: StressIncrement
        real(DP), dimension(5):: strain_rate
        real(DP), dimension(5):: residual
        type(StrainIncrement), dimension(:), allocatable:: strain_increments
    end type

    !> C-compatible counterpart of [[StressIncrement]].
    type, bind(C):: StressIncrement_C
        real(C_DOUBLE), dimension(5):: strain_rate
        real(C_DOUBLE), dimension(5):: residual
        integer(C_INT):: n_strain_increments
        type(C_PTR):: strain_increments !! n_strain_increments * type(StrainIncrement_C)
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

    interface assignment(=)
        module procedure strain_increment_to_c, stress_increment_to_c
    end interface

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

    !> Convert a strain increment to its C-compatible counterpart. The caller takes ownership of `inc_c%texture`.
    !>
    !> Not `pure`: the texture is malloc'ed.
    subroutine strain_increment_to_c(inc_c, inc)
        type(StrainIncrement_C), intent(out):: inc_c
        type(StrainIncrement), intent(in):: inc

        integer:: n_grains
        integer(C_INT), dimension(:,:), pointer:: texture

        inc_c%duration = real(inc%duration, C_DOUBLE)
        inc_c%deformation_gradient = real(inc%deformation_gradient, C_DOUBLE)
        inc_c%vm_strain = real(inc%vm_strain, C_DOUBLE)
        inc_c%stress = real(inc%stress, C_DOUBLE)
        inc_c%taylor_factor = real(inc%taylor_factor, C_DOUBLE)

        n_grains = size(inc%texture, 2)
        inc_c%n_grains = int(n_grains, C_INT)

        inc_c%texture = malloc(int(3*n_grains, C_SIZE_T) * c_sizeof(1_C_INT))
        call c_f_pointer(inc_c%texture, texture, [3, n_grains])
        texture = int(inc%texture, C_INT)
    end subroutine

    !> Convert a stress increment to its C-compatible counterpart. The caller takes ownership of `inc_c%strain_increments` and of
    !> the texture of each of them.
    !>
    !> Not `pure`: the strain increments and their textures are malloc'ed.
    subroutine stress_increment_to_c(inc_c, inc)
        type(StressIncrement_C), intent(out):: inc_c
        type(StressIncrement), intent(in):: inc

        integer:: i, n
        type(StrainIncrement_C):: mold
        type(StrainIncrement_C), dimension(:), pointer:: strain_increments

        inc_c%strain_rate = real(inc%strain_rate, C_DOUBLE)
        inc_c%residual = real(inc%residual, C_DOUBLE)

        n = size(inc%strain_increments)
        inc_c%n_strain_increments = int(n, C_INT)

        inc_c%strain_increments = malloc(int(n, C_SIZE_T) * c_sizeof(mold))
        call c_f_pointer(inc_c%strain_increments, strain_increments, [n])
        do i = 1, n
            strain_increments(i) = inc%strain_increments(i)
        end do
    end subroutine
end module
