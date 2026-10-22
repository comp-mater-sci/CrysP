module crysp_utils_c
    use iso_c_binding
    use crysp_input
    use crysp_model
    use crysp_status

    implicit none

    public

    interface
        !> Lists referenced through a `C_PTR` are malloc'ed, so that C can release them with `free()`.
        function malloc(size) bind(C, name='malloc') result(ptr)
            import C_SIZE_T, C_PTR
            integer(C_SIZE_T), value:: size
            type(C_PTR):: ptr
        end function

        subroutine free(ptr) bind(C, name='free')
            import C_PTR
            type(C_PTR), value:: ptr
        end subroutine
    end interface

    !> C-compatible counterpart of [[crysp_parameters:Parameter]].
    !>
    !> `data` points to the elements in column-major order: `extents(1)*extents(2)` values of `int` (`TYPE_INTEGER`) or `double`
    !> (`TYPE_REAL`). Scalars have extents (1,1), arrays (n,1) and matrices (m,n). A string (`TYPE_STRING`, rank 0) is
    !> `extents(1)` characters. A parameter that was never set has rank -1 and a null `data`, as does any parameter without
    !> elements.
    !>
    !> A `Parameter_C` produced by libCrysP owns its `data`, malloc'ed so that C can release it with `free()`. Strings produced by
    !> libCrysP are NUL-terminated for convenience, but the terminator is not counted in `extents` and is not required on input.
    !> Converting from C copies the data and then frees it, so a C caller must pass malloc'ed data and not touch it afterwards.
    type, bind(C):: Parameter_C
        integer(C_INT):: type
        integer(C_INT):: rank
        integer(C_INT), dimension(2):: extents !! A pointer allocated and freed based on the value of rank would be more idiomatic but is not worth the malloc/free overhead.
        type(C_PTR):: data
    end type

    !> C-compatible counterpart of [[Input]].
    type, bind(C):: Input_C
        character(C_CHAR), dimension(NAME_LEN):: name
        character(C_CHAR), dimension(DESC_LEN):: description
        integer(C_INT):: type
        type(Parameter_C):: lower_bound
        logical(C_BOOL):: lower_bound_inclusive
        type(Parameter_C):: upper_bound
        logical(C_BOOL):: upper_bound_inclusive
        type(Parameter_C):: default_value
    end type

    !> C-compatible counterpart of [[StateDescriptor]]. Name and description are NUL-terminated and truncated to fit.
    type, bind(C):: StateDescriptor_C
        character(C_CHAR), dimension(NAME_LEN):: name
        character(C_CHAR), dimension(DESC_LEN):: description
        integer(C_INT):: type
        integer(C_INT):: rank
    end type

    !> C-compatible counterpart of [[ModelDescriptor]].
    !>
    !> Each list is a malloc'ed array of `n_...` elements. A `ModelDescriptor_C` produced by libCrysP owns these lists and, through
    !> the inputs, the data of their bounds and default values: C releases those with `free()`, then the lists themselves.
    type, bind(C):: ModelDescriptor_C
        integer(C_INT):: id
        character(C_CHAR), dimension(NAME_LEN):: name
        character(C_CHAR), dimension(DESC_LEN):: description
        integer(C_INT):: n_inputs
        type(C_PTR):: inputs             !! n_inputs * type(Input_C)
        integer(C_INT):: n_model_state
        type(C_PTR):: model_state        !! n_model_state * type(StateDescriptor_C)
        integer(C_INT):: n_element_state
        type(C_PTR):: element_state      !! n_element_state * type(StateDescriptor_C)
    end type

    !> C-compatible counterpart of [[ModelData]].
    type, bind(C):: ModelData_C
        integer(C_INT):: id
        integer(C_INT):: n_parameters
        type(C_PTR):: parameters         !! n_parameters * type(Parameter_C)
    end type

    !> C-compatible counterpart of [[Status]], returned by value by every entry point of [[libcrysp_c_binding]].
    !>
    !> `code` is `CRYSP_OK` on success. The three buffers are NUL-terminated C strings, truncated to fit, and all-NUL when `code` is
    !> `CRYSP_OK`.
    !>
    !> @note
    !> Returned by value, so keep it small.
    !> @endnote
    type, bind(C):: Status_C
        integer(C_INT)::         code                = CRYSP_OK    !! One of the status codes in [[crysp_status]].
        character(kind=C_CHAR):: mod_name(NAME_LEN)  = C_NULL_CHAR !! Module in which the failure was raised.
        character(kind=C_CHAR):: proc_name(NAME_LEN) = C_NULL_CHAR !! Routine in which the failure was raised.
        character(kind=C_CHAR):: message(MSG_LEN)    = C_NULL_CHAR !! Human-readable description of the failure.
    end type

    interface assignment(=)
        module procedure status_to_c, status_from_c, &
                         input_to_c, &
                         state_descriptor_to_c, &
                         model_descriptor_to_c, &
                         model_data_to_c, model_data_from_c, &
                         parameter_to_c, parameter_from_c
    end interface

contains
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

    !> Convert a C-compatible parameter to a parameter. The data is copied and then freed, so `param_c%data` must have been
    !> malloc'ed and is dangling afterwards.
    !>
    !> `param_c` is assumed to be well-formed: it is not validated here.
    subroutine parameter_from_c(param, param_c)
        type(Parameter), intent(out):: param
        type(Parameter_C), intent(in):: param_c

        if (param_c%rank < 0) return

        select case (param_c%type)
            case (TYPE_INTEGER)
                call ints_from_c()
            case (TYPE_REAL)
                call reals_from_c()
            case (TYPE_STRING)
                call string_from_c()
        end select
        call free(param_c%data)
    contains
        subroutine ints_from_c()
            integer(C_INT), dimension(:,:), pointer:: ints

            call c_f_pointer(param_c%data, ints, param_c%extents)
            select case (param_c%rank)
                case (0)
                    param = int(ints(1,1))
                case (1)
                    param = int(ints(:,1))
            end select
        end subroutine
        subroutine reals_from_c()
            real(C_DOUBLE), dimension(:,:), pointer:: reals

            call c_f_pointer(param_c%data, reals, param_c%extents)
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
            call c_f_pointer(param_c%data, chars, [n])
            do i = 1, n
                str(i:i) = chars(i)
            end do
            param = str
        end subroutine
    end subroutine

    !> Copy a string into a fixed-size C buffer of `length` characters.
    !>
    !> The string is truncated to `length - 1` characters if needed. The rest of the buffer is NUL, so the result is always
    !> NUL-terminated.
    pure function string_to_chars(string, length) result(chars)
        character(*), intent(in):: string
        integer, intent(in):: length                      !! Size of the buffer, including the NUL terminator. Must be positive.
        character(kind=C_CHAR), dimension(length):: chars

        integer:: i

        chars = C_NULL_CHAR
        do i = 1, min(len(string), length - 1)
            chars(i) = string(i:i)
        end do
    end function

    !> Recover a string from a C buffer.
    !>
    !> The string ends at the first NUL. If the buffer contains none, the whole buffer is used.
    pure function chars_to_string(chars) result(string)
        character(kind=C_CHAR), dimension(:), intent(in):: chars
        character(:), allocatable:: string

        integer:: i, n

        n = findloc(chars, C_NULL_CHAR, 1) - 1
        if (n < 0) n = size(chars)

        allocate(character(n):: string)
        do i = 1, n
            string(i:i) = chars(i)
        end do
    end function

    !> Convert an input to its C-compatible counterpart.
    !>
    !> The caller takes ownership of the data of the bounds and default value.
    !> String inputs are truncted to fit
    subroutine input_to_c(inp_c, inp)
        type(Input_C), intent(out):: inp_c
        type(Input), intent(in):: inp

        inp_c%name = string_to_chars(inp%name, size(inp_c%name))
        inp_c%description = string_to_chars(inp%description, size(inp_c%description))
        inp_c%type = int(inp%type, C_INT)
        inp_c%lower_bound = inp%lower_bound
        inp_c%lower_bound_inclusive = logical(inp%lower_bound_inclusive, C_BOOL)
        inp_c%upper_bound = inp%upper_bound
        inp_c%upper_bound_inclusive = logical(inp%upper_bound_inclusive, C_BOOL)
        inp_c%default_value = inp%default_value
    end subroutine

   !> Convert a state descriptor to its C-compatible counterpart.
    subroutine state_descriptor_to_c(desc_c, desc)
        type(StateDescriptor_C), intent(out):: desc_c
        type(StateDescriptor), intent(in):: desc

        desc_c%name = string_to_chars(desc%name, size(desc_c%name))
        desc_c%description = string_to_chars(desc%description, size(desc_c%description))
        desc_c%type = int(desc%type, C_INT)
        desc_c%rank = int(desc%rank, C_INT)
    end subroutine

    !> Convert a model descriptor to its C-compatible counterpart.
    !>
    !> The caller takes ownership of the pointers.
    subroutine model_descriptor_to_c(desc_c, desc)
        type(ModelDescriptor_C), intent(out):: desc_c
        type(ModelDescriptor), intent(in):: desc

        integer:: i
        type(Input_C):: input_mold
        type(StateDescriptor_C):: state_mold
        type(Input_C), dimension(:), pointer:: inputs
        type(StateDescriptor_C), dimension(:), pointer:: model_state, &
                                                         element_state

        desc_c%id = int(desc%id, C_INT)
        desc_c%name = string_to_chars(desc%name, size(desc_c%name))
        desc_c%description = string_to_chars(desc%description, size(desc_c%description))

        desc_c%n_inputs = int(size(desc%inputs), C_INT)
        desc_c%inputs = malloc(int(size(desc%inputs), C_SIZE_T) * c_sizeof(input_mold))
        call c_f_pointer(desc_c%inputs, inputs, [size(desc%inputs)])
        do i = 1, size(desc%inputs)
            inputs(i) = desc%inputs(i)
        end do

        desc_c%n_model_state = int(size(desc%model_state), C_INT)
        desc_c%model_state = malloc(int(size(desc%model_state), C_SIZE_T) * c_sizeof(state_mold))
        call c_f_pointer(desc_c%model_state, model_state, [size(desc%model_state)])
        do i = 1, size(desc%model_state)
            model_state(i) = desc%model_state(i)
        end do

        desc_c%n_element_state = int(size(desc%element_state), C_INT)
        desc_c%element_state = malloc(int(size(desc%element_state), C_SIZE_T) * c_sizeof(state_mold))
        call c_f_pointer(desc_c%element_state, element_state, [size(desc%element_state)])
        do i = 1, size(desc%element_state)
            element_state(i) = desc%element_state(i)
        end do
    end subroutine

    !> Convert model data to its C-compatible counterpart. The caller takes ownership of the parameters, see [[ModelData_C]].
    subroutine model_data_to_c(model_data_c, model_data)
        type(ModelData_C), intent(out):: model_data_c
        type(ModelData), intent(in):: model_data

        integer:: i
        type(Parameter_C):: mold
        type(Parameter_C), dimension(:), pointer:: params

        model_data_c%id = int(model_data%id, C_INT)
        model_data_c%n_parameters = int(size(model_data%parameters), C_INT)
        model_data_c%parameters = malloc(int(size(model_data%parameters), C_SIZE_T) * c_sizeof(mold))
        call c_f_pointer(model_data_c%parameters, params, [size(model_data%parameters)])
        do i = 1, size(model_data%parameters)
            params(i) = model_data%parameters(i)
        end do
    end subroutine

    !> Convert C-compatible model data to model data. The parameters are copied and then freed, see [[parameter_from_c]], and so is
    !> the list holding them: `model_data_c%parameters` must have been malloc'ed and is dangling afterwards.
    !>
    !> `model_data_c` is assumed to be well-formed: it is not validated here.
    subroutine model_data_from_c(model_data, model_data_c)
        type(ModelData), intent(out):: model_data
        type(ModelData_C), intent(in):: model_data_c

        integer:: i
        type(Parameter_C), dimension(:), pointer:: params

        model_data%id = int(model_data_c%id)
        allocate(model_data%parameters(model_data_c%n_parameters))

        call c_f_pointer(model_data_c%parameters, params, [int(model_data_c%n_parameters)])
        do i = 1, size(params)
            model_data%parameters(i) = params(i)
        end do
        call free(model_data_c%parameters)
    end subroutine

    !> Convert a status to its C-compatible counterpart.
    subroutine status_to_c(stat_c, stat)
        type(Status_C), intent(out):: stat_c
        type(Status), intent(in):: stat

        stat_c%code = int(stat%code, C_INT)
        if (stat%code /= CRYSP_OK) then
            stat_c%mod_name = string_to_chars(trim(stat%mod_name), size(stat_c%mod_name))
            stat_c%proc_name = string_to_chars(trim(stat%proc_name), size(stat_c%proc_name))
            stat_c%message = string_to_chars(trim(stat%message), size(stat_c%message))
        end if
    end subroutine

    !> Convert a C-compatible status to a status. Strings end at the first NUL of their buffer.
    subroutine status_from_c(stat, stat_c)
        type(Status), intent(out):: stat
        type(Status_C), intent(in):: stat_c

        stat%code = int(stat_c%code)
        if (stat%code /= CRYSP_OK) then
            stat%mod_name = chars_to_string(stat_c%mod_name)
            stat%proc_name = chars_to_string(stat_c%proc_name)
            stat%message = chars_to_string(stat_c%message)
        end if
    end subroutine
end module
