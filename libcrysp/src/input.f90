module crysp_input
    use base_defs
    use conversions, only: string_to_chars
    use crysp_parameters

    implicit none

    public

    !> Possible types of user input.
    !>
    !> Input types describe the kind of data expected, i.e. how a UI should go about collecting the data.
    !> Each input type has a particular type signature in terms of Parameter types (see [[crysp_parameters]]).
    !> This signature is added as a comment after each input type.
    !> A model providing an input parameter of a certain input type expects the UI to return a value corresponding to the linked
    !> parameter type. Note that for non-scalar parameter types, the shape of the parameter type is specified as well.
    enum, bind(C)
        enumerator:: INPUT_REAL         !! TYPE_REAL
        enumerator:: INPUT_ANGLES_LIST  !! TYPE_REAL_MATRIX(2,*)
    end enum

    !> Descriptor of an input field requested from the user.
    !>
    !> Contains all needed information to enforce sanitization at the front-end.
    !> Any parameters needed by models should be described using this type.
    !>
    !> @note
    !> This type is deliberately not `bind(C)`: it is the representation the core uses, so the text is held as ordinary
    !> deferred-length Fortran strings rather than fixed NUL-terminated buffers. [[Input_C]] is the interoperable
    !> representation used at the C API boundary. Because the type is not interoperable, it can carry default initialization, which is
    !> what lets the default structure constructor stand in for the hand-written one this type used to need.
    !> @endnote
    type:: Input
        character(:), allocatable:: name                       !! Name of the parameter
        character(:), allocatable:: description                !! Description of the parameter
        integer:: type                                         !! Type of the parameter. Must exist in enum list at the
                                                               !! top of this module
        type(Parameter):: lower_bound                          !! Lower bound. May be a numerical value or the name of another parameter.
        logical:: lower_bound_inclusive = .false.              !! Ignored if lower_bound is unset
        type(Parameter):: upper_bound                          !! Upper bound. May be a numerical value or the name of another parameter.
        logical:: upper_bound_inclusive = .false.              !! Ignored if upper_bound is unset
        type(Parameter):: default_value                        !! Value to use when the caller supplies none.
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

    interface assignment(=)
        module procedure input_to_c
    end interface

contains

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
end module
