module crysp_parameters
    use base_defs
    implicit none

    private
    public:: Parameter, assignment(=), serialize

    character(*), parameter:: MOD_NAME = 'parameters'

    !> Type-safe container for a value of any supported type and rank.
    !>
    !> Values are stored as a rank-2 buffer: scalars and strings as (1,1), arrays as (n,1) and matrices as (m,n). `rank` records
    !> the original rank, so that reading into a destination of a different rank is caught even when the extents are compatible.
    !> Its default of -1 makes reading a parameter that was never assigned fail the rank check.
    !>
    !> @note
    !> Arrays and matrices are stored by a typed `allocate` followed by a copy under `select type`, not by
    !> `allocate(..., source=data)`. GFortran (as of 16.2) does not set the dynamic type of an unlimited polymorphic object when
    !> the `source=` expression is a zero-sized array, so an empty array would later fail `select type`, or segfault. `mold=` and
    !> typed allocation are unaffected, as is a non-empty `source=`, which is why scalars and strings still use it. IFX (as of
    !> 2026.1) handles all forms correctly.
    !> @endnote
    type:: Parameter
        integer:: rank = -1
        class(*), dimension(:,:), allocatable:: buffer
    end type

    interface assignment(=)
        module procedure int_to_param, int_array_to_param, real_to_param, real_array_to_param, real_matrix_to_param, string_to_param, &
                         param_to_int, param_to_int_array, param_to_real, param_to_real_array, param_to_real_matrix, param_to_string
    end interface

    interface serialize
        module procedure serialize_int, serialize_int_array, serialize_real, serialize_real_array, serialize_real_matrix, serialize_string
    end interface

contains

    subroutine int_to_param(param, data)
        type(Parameter), intent(out):: param
        integer, intent(in):: data

        param%rank = 0
        allocate(param%buffer(1,1), source=data)
    end subroutine

    subroutine int_array_to_param(param, data)
        type(Parameter), intent(out):: param
        integer, dimension(:), intent(in):: data

        param%rank = 1
        !Typed allocation, not source=data: GFortran 16.2 loses the type of an empty source= array. See note on Parameter.
        allocate(integer:: param%buffer(size(data),1))
        select type (buffer => param%buffer)
            type is (integer)
                buffer(:,1) = data
        end select
    end subroutine
    subroutine real_to_param(param, data)
        type(Parameter), intent(out):: param
        real(DP), intent(in):: data

        param%rank = 0
        allocate(param%buffer(1,1), source=data)
    end subroutine
    subroutine real_array_to_param(param, data)
        type(Parameter), intent(out):: param
        real(DP), dimension(:), intent(in):: data

        param%rank = 1
        !Typed allocation, not source=data: GFortran 16.2 loses the type of an empty source= array. See note on Parameter.
        allocate(real(DP):: param%buffer(size(data),1))
        select type (buffer => param%buffer)
            type is (real(DP))
                buffer(:,1) = data
        end select
    end subroutine
    subroutine real_matrix_to_param(param, data)
        type(Parameter), intent(out):: param
        real(DP), dimension(:,:), intent(in):: data

        param%rank = 2
        !Typed allocation, not source=data: GFortran 16.2 loses the type of an empty source= array. See note on Parameter.
        allocate(real(DP):: param%buffer(size(data,1),size(data,2)))
        select type (buffer => param%buffer)
            type is (real(DP))
                buffer = data
        end select
    end subroutine
    subroutine string_to_param(param, data)
        type(Parameter), intent(out):: param
        character(*), intent(in):: data

        param%rank = 0
        allocate(param%buffer(1,1), source=data)
    end subroutine

    subroutine param_to_int(data, param)
        integer, intent(out):: data
        type(Parameter), intent(in):: param

        if (param%rank /= 0) error stop 'parameters::param_to_int: Parameter is not a scalar'
        select type (value => param%buffer)
            type is (integer)
                data = value(1,1)
            class default
                error stop 'parameters::param_to_int: Parameter is not of correct type'
        end select
    end subroutine
    subroutine param_to_int_array(data, param)
        integer, dimension(:), intent(out):: data
        type(Parameter), intent(in):: param

        if (param%rank /= 1) error stop 'parameters::param_to_int_array: Parameter is not an array'
        select type (value => param%buffer)
            type is (integer)
                if (size(data) /= size(value,1)) &
                    error stop 'parameters::param_to_int_array: Parameter size does not match destination'
                data = value(:,1)
            class default
                error stop 'parameters::param_to_int_array: Parameter is not of correct type'
        end select
    end subroutine
    subroutine param_to_real(data, param)
        real(DP), intent(out):: data
        type(Parameter), intent(in):: param

        if (param%rank /= 0) error stop 'parameters::param_to_real: Parameter is not a scalar'
        select type (value => param%buffer)
            type is (real(DP))
                data = value(1,1)
            class default
                error stop 'parameters::param_to_real: Parameter is not of correct type'
        end select
    end subroutine
    subroutine param_to_real_array(data, param)
        real(DP), dimension(:), intent(out):: data
        type(Parameter), intent(in):: param

        if (param%rank /= 1) error stop 'parameters::param_to_real_array: Parameter is not an array'
        select type (value => param%buffer)
            type is (real(DP))
                if (size(data) /= size(value,1)) &
                    error stop 'parameters::param_to_real_array: Parameter size does not match destination'
                data = value(:,1)
            class default
                error stop 'parameters::param_to_real_array: Parameter is not of correct type'
        end select
    end subroutine
    subroutine param_to_real_matrix(data, param)
        real(DP), dimension(:,:), intent(out):: data
        type(Parameter), intent(in):: param

        if (param%rank /= 2) error stop 'parameters::param_to_real_matrix: Parameter is not a matrix'
        select type (value => param%buffer)
            type is (real(DP))
                if (any(shape(data) /= shape(value))) &
                    error stop 'parameters::param_to_real_matrix: Parameter shape does not match destination'
                data = value
            class default
                error stop 'parameters::param_to_real_matrix: Parameter is not of correct type'
        end select
    end subroutine
    subroutine param_to_string(data, param)
        character(len=:), allocatable, intent(out):: data
        type(Parameter), intent(in):: param

        if (param%rank /= 0) error stop 'parameters::param_to_string: Parameter is not a scalar'
        select type (value => param%buffer)
            type is (character(*))
                data = value(1,1)
            class default
                error stop 'parameters::param_to_string: Parameter is not of correct type'
        end select
    end subroutine

    function serialize_int(data) result(param)
        integer, intent(in):: data
        type(Parameter):: param

        param = data
    end function

    function serialize_int_array(data) result(param)
        integer, dimension(:), intent(in):: data
        type(Parameter):: param

        param = data
    end function
    function serialize_real(data) result(param)
        real(DP), intent(in):: data
        type(Parameter):: param

        param = data
    end function
    function serialize_real_array(data) result(param)
        real(DP), dimension(:), intent(in):: data
        type(Parameter):: param

        param = data
    end function
    function serialize_real_matrix(data) result(param)
        real(DP), dimension(:,:), intent(in):: data
        type(Parameter):: param

        param = data
    end function
    function serialize_string(data) result(param)
        character(*), intent(in):: data
        type(Parameter):: param

        param = data
    end function
end module
