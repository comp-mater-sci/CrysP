module incrementation
    use base_defs
    use logging
    use math_utils

    implicit none

    private
    public:: StrainIncrement, &
             StressIncrement, &
             IncrementListBuilder

    character(*), parameter:: MOD_NAME = 'incrementation'

    type, abstract:: Increment
    end type

    type, extends(Increment):: StrainIncrement
        real(DP):: duration = 0._DP
        real(DP), dimension(3,3):: deformation_gradient = UNIT_MATRIX_3X3 !! Technically redundant but saves a lot of computation
        real(DP):: vm_strain = 0._DP                           !! Von mises true strain. Technically redundant but saves a lot of computation
        real(DP), dimension(3,3):: stress = 0._DP
        real(DP):: taylor_factor = 0._DP
    end type

    type, extends(Increment):: StressIncrement
        real(DP), dimension(5):: strain_rate
        real(DP), dimension(5):: residual
        type(StrainIncrement), dimension(:), allocatable:: strain_increments
    end type

    type:: IncrementWrapper
        class(Increment), allocatable:: increment
    end type

    type:: IncrementListBuilder
        integer:: size = 0
        type(IncrementWrapper), dimension(:), allocatable:: data
    contains
        procedure:: add => incrementbuilder_add
        procedure:: get_strain_increments => incrementbuilder_get_strain
        procedure:: get_stress_increments => incrementbuilder_get_stress
    end type

contains

    subroutine incrementbuilder_add(this, inc)
        class(IncrementListBuilder), intent(inout):: this
        class(Increment), intent(in):: inc

        type(IncrementWrapper), allocatable:: buffer(:)

        if (this%size == 0) then
            allocate(this%data(4))
        else if (this%size == size(this%data)) then
            allocate(buffer(2*this%size))
            buffer(:this%size) = this%data
            call move_alloc(buffer, this%data)
        end if

        this%size = this%size + 1
        allocate(this%data(this%size)%increment,source=inc)
    end subroutine

    function incrementbuilder_get_strain(this) result(strain_increments)
        class(IncrementListBuilder), intent(in):: this
        type(StrainIncrement), dimension(:), allocatable:: strain_increments

        integer:: i

        allocate(strain_increments(this%size))
        if (.not. same_type_as(strain_increments, this%data(1)%increment)) &
            call log_error(MOD_NAME, 'get_strain', ERR_TYPE, 'List does not contain strain increments.')

        do i=1,this%size
            strain_increments(i) = transfer(this%data(i)%increment, strain_increments(i))
        end do
    end function

    function incrementbuilder_get_stress(this) result(stress_increments)
        class(IncrementListBuilder), intent(in):: this
        type(StressIncrement), dimension(:), allocatable:: stress_increments

        integer:: i

        allocate(stress_increments(this%size))
        if (.not. same_type_as(stress_increments, this%data(1)%increment)) &
            call log_error(MOD_NAME, 'get_stress', ERR_TYPE, 'List does not contain stress increments.')

        do i=1,this%size
            stress_increments(i) = transfer(this%data(i)%increment, stress_increments(i))
        end do
    end function
end module
