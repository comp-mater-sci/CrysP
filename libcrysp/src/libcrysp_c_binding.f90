module libcrysp_c_binding
    use iso_c_binding
    use libcrysp

    implicit none

    private
    public:: crysp_get_meso_models_c, &
             crysp_get_micro_models_c, &
             crysp_new_material_c, &
             crysp_destroy_material_c, &
             crysp_serialize_material_c, &
             crysp_deserialize_material_c, &
             crysp_get_texture_c, &
             crysp_strain_driven_deformation_c, &
             crysp_stress_driven_deformation_c, &
             crysp_simulate_stress_mode_c, &
             crysp_simulate_strain_mode_c

    !> C-compatible counterpart of [[crysp_status:Status]], returned by value by every entry point below.
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

    type, bind(C):: StrainIncrement_C
        real(C_DOUBLE):: duration
        real(C_DOUBLE), dimension(3,3):: deformation_gradient
        real(C_DOUBLE):: vm_strain
        real(C_DOUBLE), dimension(3,3):: stress
        real(C_DOUBLE):: taylor_factor
        integer(C_INT):: n_grains
        type(C_PTR):: texture
    end type

    type, bind(C):: StressIncrement_C
        real(C_DOUBLE), dimension(5):: strain_rate
        real(C_DOUBLE), dimension(5):: residual
        integer(C_INT):: n_strain_increments
        type(C_PTR):: strain_increments
    end type

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

    type, bind(C):: StateDescriptor_C
        character(C_CHAR), dimension(NAME_LEN):: name
        character(C_CHAR), dimension(DESC_LEN):: description
        integer(C_INT):: type
    end type

    !> General information about a model supported by libCrysP, as reported to C callers.
    !>
    !> The C-compatible counterpart of [[crysp_model:ModelDescriptor]]. Each list is a malloc'ed array of `n_...` elements, or
    !> null if it is empty, and is released by the caller with `free()`. The bounds and default value of each input are owned by
    !> the caller as well: see [[Parameter_C]].
    type, bind(C):: ModelDescriptor_C
        integer(C_INT):: id
        character(C_CHAR), dimension(NAME_LEN):: name
        character(C_CHAR), dimension(DESC_LEN):: description
        integer(C_INT):: n_inputs
        type(C_PTR):: inputs
        integer(C_INT):: n_model_state
        type(C_PTR):: model_state
        integer(C_INT):: n_element_state
        type(C_PTR):: element_state
    end type

    interface
        !> Lists referenced through a `C_PTR` are malloc'ed, so that C can release them with `free()`.
        function malloc(size) bind(C, name='malloc') result(ptr)
            import C_SIZE_T, C_PTR
            integer(C_SIZE_T), value:: size
            type(C_PTR):: ptr
        end function
    end interface

    interface assignment(=)
        module procedure parameter_to_c, parameter_from_c
    end interface

contains

    !> C binding of [[libcrysp:crysp_get_meso_models]]. See [[ModelDescriptor_C]] for what the caller has to release.
    function crysp_get_meso_models_c(model_descs) bind(C, name='crysp_get_meso_models') result(stat)
        type(ModelDescriptor_C), dimension(:), allocatable, intent(out):: model_descs
        type(Status_C):: stat

        type(ModelDescriptor), dimension(:), allocatable:: model_descs_

        stat = status_to_c(crysp_get_meso_models(model_descs_))
        if (stat%code /= CRYSP_OK) return

        model_descs = to_model_descriptor_c(model_descs_)
    end function

    !> C binding of [[libcrysp:crysp_get_micro_models]]. See [[ModelDescriptor_C]] for what the caller has to release.
    function crysp_get_micro_models_c(model_descs) bind(C, name='crysp_get_micro_models') result(stat)
        type(ModelDescriptor_C), dimension(:), allocatable, intent(out):: model_descs
        type(Status_C):: stat

        type(ModelDescriptor), dimension(:), allocatable:: model_descs_

        stat = status_to_c(crysp_get_micro_models(model_descs_))
        if (stat%code /= CRYSP_OK) return

        model_descs = to_model_descriptor_c(model_descs_)
    end function

    !> C binding of [[libcrysp:crysp_new_material]].
    !>
    !> @note
    !> The arrays sized by `phase_sizes` are assumed-size rather than explicit-shape. IFX (as of 2026.1) evaluates specification
    !> expressions of a `bind(C)` procedure before its assumed-shape dummies are set up, so any expression involving `phase_sizes`
    !> there reads an undefined descriptor.
    !> @endnote
    function crysp_new_material_c(phase_sizes, orientations, deformation_mechanisms, hardening_model_ids, hardening_params, &
                                  meso_model_id, meso_params, mat) bind(C, name='crysp_new_material') result(stat)
        integer(C_INT), dimension(:), intent(in):: phase_sizes
        real(C_DOUBLE), dimension(3,*), intent(in):: orientations
        integer(C_INT), dimension(*), intent(in):: deformation_mechanisms
        integer(C_INT), dimension(*), intent(in):: hardening_model_ids
        type(Parameter_C), dimension(:), intent(in):: hardening_params
        integer(C_INT), intent(in):: meso_model_id
        type(Parameter_C), dimension(:), intent(in):: meso_params
        type(MaterialHandle), intent(out):: mat
        type(Status_C):: stat

        stat = status_to_c(crysp_new_material(phase_sizes, orientations, deformation_mechanisms, hardening_model_ids, &
                                              parameters_from_c(hardening_params), meso_model_id, &
                                              parameters_from_c(meso_params), mat))
    end function

    !> C binding of [[libcrysp:crysp_destroy_material]].
    subroutine crysp_destroy_material_c(mat) bind(C, name='crysp_destroy_material')
        type(MaterialHandle), intent(inout):: mat

        call crysp_destroy_material(mat)
    end subroutine

    !> C binding of [[libcrysp:crysp_serialize_material]]. The caller owns the data of the parameters: see [[Parameter_C]].
    function crysp_serialize_material_c(mat, params) bind(C, name='crysp_serialize_material') result(stat)
        type(MaterialHandle), intent(inout):: mat
        type(Parameter_C), dimension(:), allocatable, intent(out):: params
        type(Status_C):: stat

        integer:: i
        type(Parameter), dimension(:), allocatable:: params_

        stat = status_to_c(crysp_serialize_material(mat, params_))
        if (stat%code /= CRYSP_OK) return

        allocate(params(size(params_)))
        do i = 1, size(params_)
            params(i) = params_(i)
        end do
    end function

    !> C binding of [[libcrysp:crysp_deserialize_material]].
    function crysp_deserialize_material_c(params, mat) bind(C, name='crysp_deserialize_material') result(stat)
        type(Parameter_C), dimension(:), intent(in):: params
        type(MaterialHandle), intent(out):: mat
        type(Status_C):: stat

        stat = status_to_c(crysp_deserialize_material(parameters_from_c(params), mat))
    end function

    !> C binding of [[libcrysp:crysp_get_texture]].
    function crysp_get_texture_c(mat, texture) bind(C, name='crysp_get_texture') result(stat)
        type(MaterialHandle), intent(in):: mat
        type(TexturePoint), dimension(:), allocatable, intent(out):: texture
        type(Status_C):: stat

        stat = status_to_c(crysp_get_texture(mat, texture))
    end function

    !> C binding of [[libcrysp:crysp_strain_driven_deformation]].
    function crysp_strain_driven_deformation_c(mat, velocity_gradient, target_vm_strain, increments, textures) &
            bind(C, name='crysp_strain_driven_deformation') result(stat)
        type(MaterialHandle), intent(in):: mat
        real(C_DOUBLE), dimension(3,3), intent(in):: velocity_gradient
        real(C_DOUBLE), intent(in):: target_vm_strain
        type(StrainIncrementData), dimension(:), allocatable, intent(out):: increments
        type(TexturePoint), dimension(:,:), allocatable, intent(out):: textures
        type(Status_C):: stat

        stat = status_to_c(crysp_strain_driven_deformation(mat, velocity_gradient, target_vm_strain, increments, textures))
    end function

    !> C binding of [[libcrysp:crysp_stress_driven_deformation]].
    function crysp_stress_driven_deformation_c(mat, target_stress_mode, target_vm_strain, stress_increments, strain_increments, &
                                               textures) bind(C, name='crysp_stress_driven_deformation') result(stat)
        type(MaterialHandle), intent(in):: mat
        real(C_DOUBLE), dimension(5), intent(in):: target_stress_mode
        real(C_DOUBLE), intent(in):: target_vm_strain
        type(StressIncrementData), dimension(:), allocatable, intent(out):: stress_increments
        type(StrainIncrementData), dimension(:), allocatable, intent(out):: strain_increments
        type(TexturePoint), dimension(:,:), allocatable, intent(out):: textures
        type(Status_C):: stat

        stat = status_to_c(crysp_stress_driven_deformation(mat, target_stress_mode, target_vm_strain, stress_increments, &
                                                           strain_increments, textures))
    end function

    !> C binding of [[libcrysp:crysp_simulate_stress_mode]].
    function crysp_simulate_stress_mode_c(mat, target_stress_mode, strain_mode, stress, residual) &
            bind(C, name='crysp_simulate_stress_mode') result(stat)
        type(MaterialHandle), intent(in):: mat
        real(C_DOUBLE), dimension(5), intent(in):: target_stress_mode
        real(C_DOUBLE), dimension(5), intent(inout):: strain_mode
        real(C_DOUBLE), dimension(5), intent(out):: stress
        real(C_DOUBLE), dimension(5), intent(out):: residual
        type(Status_C):: stat

        stat = status_to_c(crysp_simulate_stress_mode(mat, target_stress_mode, strain_mode, stress, residual))
    end function

    !> C binding of [[libcrysp:crysp_simulate_strain_mode]].
    function crysp_simulate_strain_mode_c(mat, strain_mode, stress) bind(C, name='crysp_simulate_strain_mode') result(stat)
        type(MaterialHandle), intent(in):: mat
        real(C_DOUBLE), dimension(5), intent(in):: strain_mode
        real(C_DOUBLE), dimension(5), intent(out):: stress
        type(Status_C):: stat

        stat = status_to_c(crysp_simulate_strain_mode(mat, strain_mode, stress))
    end function

    !> Convert a status to its C-compatible counterpart.
    function status_to_c(stat) result(stat_c)
        type(Status), intent(in):: stat
        type(Status_C):: stat_c

        stat_c%code = int(stat%code, C_INT)
        if (allocated(stat%mod_name)) call to_c_chars(stat%mod_name, stat_c%mod_name)
        if (allocated(stat%proc_name)) call to_c_chars(stat%proc_name, stat_c%proc_name)
        if (allocated(stat%message)) call to_c_chars(stat%message, stat_c%message)
    end function

    !> Copy a string into a fixed-size C buffer, truncated to fit. The rest of the buffer is NUL, so it is always NUL-terminated.
    pure subroutine to_c_chars(str, chars)
        character(*), intent(in):: str
        character(kind=C_CHAR), dimension(:), intent(out):: chars

        integer:: i

        chars = C_NULL_CHAR
        do i = 1, min(len(str), size(chars) - 1)
            chars(i) = str(i:i)
        end do
    end subroutine

    !> Convert a list of C-compatible parameters to parameters. The data is copied: see [[parameter_from_c]].
    function parameters_from_c(params_c) result(params)
        type(Parameter_C), dimension(:), intent(in):: params_c
        type(Parameter), dimension(:), allocatable:: params

        integer:: i

        allocate(params(size(params_c)))
        do i = 1, size(params_c)
            params(i) = params_c(i)
        end do
    end function

    !> Convert a model descriptor to its C-compatible counterpart. See [[ModelDescriptor_C]] for who owns what.
    impure elemental function to_model_descriptor_c(desc) result(desc_c)
        type(ModelDescriptor), intent(in):: desc
        type(ModelDescriptor_C):: desc_c

        desc_c%id = int(desc%id, C_INT)
        call to_c_chars(desc%name, desc_c%name)
        call to_c_chars(desc%description, desc_c%description)
        desc_c%n_inputs = int(size(desc%inputs), C_INT)
        desc_c%inputs = to_inputs_c(desc%inputs)
        desc_c%n_model_state = int(size(desc%model_state), C_INT)
        desc_c%model_state = to_state_descriptors_c(desc%model_state)
        desc_c%n_element_state = int(size(desc%element_state), C_INT)
        desc_c%element_state = to_state_descriptors_c(desc%element_state)
    end function

    !> Copy a list of inputs into a malloc'ed array of their C-compatible counterparts. Null if the list is empty.
    !>
    !> The bounds and default values are copied as well: the result owns them.
    function to_inputs_c(inputs) result(ptr)
        type(Input), dimension(:), intent(in):: inputs
        type(C_PTR):: ptr

        integer:: i
        type(Input_C):: mold
        type(Input_C), dimension(:), pointer:: inputs_c

        ptr = C_NULL_PTR
        if (size(inputs) == 0) return

        ptr = malloc(int(size(inputs), C_SIZE_T) * c_sizeof(mold))
        call c_f_pointer(ptr, inputs_c, [size(inputs)])
        do i = 1, size(inputs)
            associate (input_ => inputs(i), input_c => inputs_c(i))
                call to_c_chars(input_%name, input_c%name)
                call to_c_chars(input_%description, input_c%description)
                input_c%type = int(input_%type, C_INT)
                input_c%lower_bound = input_%lower_bound
                input_c%lower_bound_inclusive = logical(input_%lower_bound_inclusive, C_BOOL)
                input_c%upper_bound = input_%upper_bound
                input_c%upper_bound_inclusive = logical(input_%upper_bound_inclusive, C_BOOL)
                input_c%default_value = input_%default_value
            end associate
        end do
    end function

    !> Copy a list of state descriptors into a malloc'ed array of their C-compatible counterparts. Null if the list is empty.
    function to_state_descriptors_c(states) result(ptr)
        type(StateDescriptor), dimension(:), intent(in):: states
        type(C_PTR):: ptr

        integer:: i
        type(StateDescriptor_C):: mold
        type(StateDescriptor_C), dimension(:), pointer:: states_c

        ptr = C_NULL_PTR
        if (size(states) == 0) return

        ptr = malloc(int(size(states), C_SIZE_T) * c_sizeof(mold))
        call c_f_pointer(ptr, states_c, [size(states)])
        do i = 1, size(states)
            call to_c_chars(states(i)%name, states_c(i)%name)
            call to_c_chars(states(i)%description, states_c(i)%description)
            states_c(i)%type = int(states(i)%type, C_INT)
        end do
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
        if (any(param_c%extents < 0)) error stop 'libcrysp_c_binding::parameter_from_c: Negative extent'

        select case (param_c%type)
            case (TYPE_INTEGER)
                call check_extents(1)
                call ints_from_c()
            case (TYPE_REAL)
                call check_extents(2)
                call reals_from_c()
            case (TYPE_STRING)
                if (param_c%rank /= 0) error stop 'libcrysp_c_binding::parameter_from_c: A string must have rank 0'
                call string_from_c()
            case default
                error stop 'libcrysp_c_binding::parameter_from_c: Unknown parameter type'
        end select
    contains
        !> Check that the rank of a numerical parameter is at most `max_rank` and consistent with its extents.
        subroutine check_extents(max_rank)
            integer, intent(in):: max_rank
            if (param_c%rank > max_rank) error stop 'libcrysp_c_binding::parameter_from_c: Rank not supported for this type'
            select case (param_c%rank)
                case (0)
                    if (any(param_c%extents /= 1)) &
                        error stop 'libcrysp_c_binding::parameter_from_c: A scalar must have extents (1,1)'
                case (1)
                    if (param_c%extents(2) /= 1) &
                        error stop 'libcrysp_c_binding::parameter_from_c: An array must have extents (n,1)'
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
