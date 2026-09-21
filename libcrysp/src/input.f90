module crysp_input
    use iso_c_binding
    use base_defs, only: NAME_LEN, DESCRIPTION_LEN
    use crysp_serialization

    implicit none

    public

    !> Possible types of user input.
    !>
    !> Input types describe the kind of data expected, i.e. how a UI should go about collecting the data.
    !> Each input type has a particular type signature in terms of Parameter types (see [[crysp_serializaton]]).
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
    !> The text buffers are NUL-terminated.
    type, bind(C):: Input
        character(kind=C_CHAR), dimension(NAME_LEN):: name               !! Name of the parameter
        integer(C_INT):: type                                            !! Type of the parameter. Must exist in enum list at the
                                                                         !! top of this module
        character(kind=C_CHAR), dimension(DESCRIPTION_LEN):: description  !! Description of the parameter
        logical(C_BOOL):: optional                                       !! Indicate if the parameter is optional
        type(Parameter):: lower_bound                                    !! Lower bound. May be a numerical value or the name of another parameter.
        logical(C_BOOL):: lower_bound_inclusive                          !! Ignored if lower_bound is unset
        type(Parameter):: upper_bound                                    !! Upper bound. May be a numerical value or the name of another parameter.
        logical(C_BOOL):: upper_bound_inclusive                          !! Ignored if upper_bound is unset
        type(Parameter):: default_value                                  !! Value to use when the caller supplies none.
    end type

    !> Build an [[Input]], filling in the conventional value for every component that is omitted.
    !>
    !> Overloads the structure constructor.
    !> Needed because Inputs can not have default initialization when they are returned as allocatable lists from a bind(C)
    !> procedure, which is the whole point of their creation. Therefore, we create a constructor routine with optional arguments
    !> which behaves exactly the same.
    interface Input
        module procedure make_input
    end interface

contains

    !> See the interface declaration.
    pure function make_input(name, type, description, optional, lower_bound, lower_bound_inclusive, &
                             upper_bound, upper_bound_inclusive, default_value) result(inp)
        character(kind=C_CHAR), dimension(NAME_LEN), intent(in):: name
        integer(C_INT), intent(in):: type
        character(kind=C_CHAR), dimension(DESCRIPTION_LEN), intent(in), optional:: description
        logical, intent(in), optional:: optional
        type(Parameter), intent(in), optional:: lower_bound
        logical, intent(in), optional:: lower_bound_inclusive
        type(Parameter), intent(in), optional:: upper_bound
        logical, intent(in), optional:: upper_bound_inclusive
        type(Parameter), intent(in), optional:: default_value
        type(Input):: inp !! The described input, with every omitted component set to a default value.

        inp%name = name
        inp%type = type

        inp%description = C_NULL_CHAR
        if (present(description)) inp%description = description

        inp%optional = .false._C_BOOL
        if (present(optional)) inp%optional = logical(optional, C_BOOL)

        inp%lower_bound = Parameter(C_NULL_PTR)
        if (present(lower_bound)) inp%lower_bound = lower_bound

        inp%lower_bound_inclusive = .false._C_BOOL
        if (present(lower_bound_inclusive)) inp%lower_bound_inclusive = logical(lower_bound_inclusive, C_BOOL)

        inp%upper_bound = Parameter(C_NULL_PTR)
        if (present(upper_bound)) inp%upper_bound = upper_bound

        inp%upper_bound_inclusive = .false._C_BOOL
        if (present(upper_bound_inclusive)) inp%upper_bound_inclusive = logical(upper_bound_inclusive, C_BOOL)

        inp%default_value = Parameter(C_NULL_PTR)
        if (present(default_value)) inp%default_value = default_value
    end function
end module
