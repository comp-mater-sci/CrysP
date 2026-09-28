module crysp_parameters
    use base_defs
    use iso_c_binding
    implicit none

    private
    public:: TYPE_INTEGER, &
             TYPE_REAL, &
             TYPE_STRING, &
             Parameter, &
             Parameter_C, &
             assignment(=), &
             serialize, &
             type_of, &
             shape_of

    character(*), parameter:: MOD_NAME = 'parameters'

    !> Element types a parameter can hold. The rank is reported separately, by `shape_of`.
    enum, bind(C)
        enumerator:: TYPE_INTEGER   !! integer
        enumerator:: TYPE_REAL      !! real(DP)
        enumerator:: TYPE_STRING    !! character(*)
    end enum

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

    !> C-compatible counterpart of [[crysp_parameters:Parameter]].
    !>
    !> `data` points to the elements in column-major order: `extents(1)*extents(2)` values of `int` (`TYPE_INTEGER`) or `double`
    !> (`TYPE_REAL`). Scalars have extents (1,1), arrays (n,1) and matrices (m,n). A string (`TYPE_STRING`, rank 0) is
    !> `extents(1)` characters. A parameter that was never set has rank -1 and a null `data`, as does any parameter without
    !> elements.
    !>
    !> A `Parameter_C` produced by libCrysP owns its `data`, malloc'ed so that C can release it with `free()`. Strings produced by
    !> libCrysP are NUL-terminated for convenience, but the terminator is not counted in `extents` and is not required on input.
    !> Converting from C copies the data, so the C caller keeps ownership of what it passed in.
    type, bind(C):: Parameter_C
        integer(C_INT):: type
        integer(C_INT):: rank
        integer(C_INT), dimension(2):: extents !! A pointer allocated and freed based on the value of rank would be more idiomatic but is not worth the malloc/free overhead.
        type(C_PTR):: data
    end type


    interface assignment(=)
        module procedure int_to_param, int_array_to_param, real_to_param, real_array_to_param, real_matrix_to_param, string_to_param, &
                         param_to_int, param_to_int_array, param_to_real, param_to_real_array, param_to_real_matrix, param_to_string, &
                         parameter_to_c, parameter_from_c
    end interface

    interface serialize
        module procedure serialize_int, serialize_int_array, serialize_real, serialize_real_array, serialize_real_matrix, serialize_string
    end interface

contains

    !> Element type of the stored value, as one of the `TYPE_*` enumerators above.
    pure function type_of(param) result(t)
        type(Parameter), intent(in):: param
        integer:: t

        select type (buffer => param%buffer)
            type is (integer)
                t = TYPE_INTEGER
            type is (real(DP))
                t = TYPE_REAL
            type is (character(*))
                t = TYPE_STRING
            class default
                error stop 'parameters::type_of: Parameter holds an unsupported type'
        end select
    end function

    !> Extents of the stored value, as stored: scalars and strings (1,1), arrays (n,1), matrices (m,n).
    !>
    !> Useful for allocating a receiving buffer of the right size before reading the value.
    pure function shape_of(param) result(s)
        type(Parameter), intent(in):: param
        integer, dimension(2):: s

        s = shape(param%buffer)
    end function

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

    !> Convert a parameter to its C-compatible counterpart. The caller takes ownership of `param_c%data`.
    subroutine parameter_to_c(param_c, param)
        type(Parameter_C), intent(out):: param_c
        type(Parameter), intent(in):: param

        param_c%rank = param%rank
        param_c%data = C_NULL_PTR
        if (param%rank < 0) then
            param_c%type = -1
            param_c%extents = 0
            return
        end if

        param_c%type = type_of(param)
        param_c%extents = shape_of(param)

        select type (buffer => param%buffer)
            type is (integer)
                call ints_to_c(buffer)
            type is (real(DP))
                call reals_to_c(buffer)
            type is (character(*))
                call string_to_c(buffer(1,1))
        end select

    contains

        subroutine ints_to_c(buffer)
            integer, dimension(:,:), intent(in):: buffer

            integer(C_INT):: mold
            integer(C_INT), dimension(:,:), pointer:: ints

            if (size(buffer) == 0) return
            param_c%data = malloc(int(size(buffer), C_SIZE_T) * c_sizeof(mold))
            call c_f_pointer(param_c%data, ints, shape(buffer))
            ints = int(buffer, C_INT)
        end subroutine

        subroutine reals_to_c(buffer)
            real(DP), dimension(:,:), intent(in):: buffer

            real(C_DOUBLE):: mold
            real(C_DOUBLE), dimension(:,:), pointer:: reals

            if (size(buffer) == 0) return
            param_c%data = malloc(int(size(buffer), C_SIZE_T) * c_sizeof(mold))
            call c_f_pointer(param_c%data, reals, shape(buffer))
            reals = real(buffer, C_DOUBLE)
        end subroutine

        subroutine string_to_c(str)
            character(*), intent(in):: str

            character(C_CHAR), dimension(:), pointer:: chars
            integer:: i, n

            n = len(str)
            param_c%extents = [n, 1]
            param_c%data = malloc(int(n + 1, C_SIZE_T))
            call c_f_pointer(param_c%data, chars, [n + 1])
            do i = 1, n
                chars(i) = str(i:i)
            end do
            chars(n + 1) = C_NULL_CHAR
        end subroutine
    end subroutine

    !> Convert a C-compatible parameter to a parameter. The data is copied: `param_c` is left untouched and still owned by C.
    !>
    !> Terminates the program if `param_c` does not describe a value a [[crysp_parameters:Parameter]] can hold, for the same reason
    !> the readers in [[crysp_parameters]] do: a defined assignment has no room for a [[crysp_status:Status]].
    subroutine parameter_from_c(param, param_c)
        type(Parameter), intent(out):: param
        type(Parameter_C), intent(in):: param_c

        if (param_c%rank < 0) return
        if (any(param_c%extents < 0)) error stop 'crysp_parameters::parameter_from_c: Negative extent'

        select case (param_c%type)
            case (TYPE_INTEGER)
                call check_extents(1)
                call ints_from_c()
            case (TYPE_REAL)
                call check_extents(2)
                call reals_from_c()
            case (TYPE_STRING)
                if (param_c%rank /= 0) error stop 'crysp_parameters::parameter_from_c: A string must have rank 0'
                call string_from_c()
            case default
                error stop 'crysp_parameters::parameter_from_c: Unknown parameter type'
        end select
    contains
        !> Check that the rank of a numerical parameter is at most `max_rank` and consistent with its extents.
        subroutine check_extents(max_rank)
            integer, intent(in):: max_rank
            if (param_c%rank > max_rank) error stop 'crysp_parameters::parameter_from_c: Rank not supported for this type'
            select case (param_c%rank)
                case (0)
                    if (any(param_c%extents /= 1)) &
                        error stop 'crysp_parameters::parameter_from_c: A scalar must have extents (1,1)'
                case (1)
                    if (param_c%extents(2) /= 1) &
                        error stop 'crysp_parameters::parameter_from_c: An array must have extents (n,1)'
            end select
        end subroutine
        !> c_f_pointer cannot be given a null pointer, which C may pass for an empty array, hence the local empty buffers below.
        subroutine ints_from_c()
            integer(C_INT), dimension(:,:), pointer:: ints
            integer(C_INT), dimension(:,:), allocatable, target:: empty

            if (product(param_c%extents) == 0) then
                allocate(empty(param_c%extents(1), param_c%extents(2)))
                ints => empty
            else
                call c_f_pointer(param_c%data, ints, param_c%extents)
            end if
            select case (param_c%rank)
                case (0)
                    param = int(ints(1,1))
                case (1)
                    param = int(ints(:,1))
            end select
        end subroutine
        subroutine reals_from_c()
            real(C_DOUBLE), dimension(:,:), pointer:: reals
            real(C_DOUBLE), dimension(:,:), allocatable, target:: empty

            if (product(param_c%extents) == 0) then
                allocate(empty(param_c%extents(1), param_c%extents(2)))
                reals => empty
            else
                call c_f_pointer(param_c%data, reals, param_c%extents)
            end if
            select case (param_c%rank)
                case (0)
                    param = real(reals(1,1), DP)
                case (1)
                    param = real(reals(:,1), DP)
                case (2)
                    param = real(reals, DP)
            end select
        end subroutine
        subroutine string_from_c()
            character(C_CHAR), dimension(:), pointer:: chars
            character(:), allocatable:: str
            integer:: i, n

            n = param_c%extents(1)
            allocate(character(n):: str)
            if (n > 0) then
                call c_f_pointer(param_c%data, chars, [n])
                do i = 1, n
                    str(i:i) = chars(i)
                end do
            end if
            param = str
        end subroutine
    end subroutine
end module
