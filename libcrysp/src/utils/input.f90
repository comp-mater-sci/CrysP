module crysp_input
    use base_defs
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
        enumerator:: INPUT_REAL         !! TYPE_REAL, rank 0
        enumerator:: INPUT_ANGLES_LIST  !! TYPE_REAL, rank 2, shape (2,*)
    end enum

    !> Descriptor of an input field requested from the user.
    !>
    !> Contains all needed information to enforce sanitization at the front-end.
    !> Any parameters needed by models should be described using this type.
    !>
    !> @warning
    !> Only initialize using the Input constructor defined below. Refer to its documentation for why.
    !> @endwarning
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

    !> Overrides the default structure constructor to work around an ifx bug, see [[Input]].
    interface Input
        module procedure new_input
    end interface

contains

    !> Construct an input. Takes the same arguments as the default structure constructor; omitted components keep their default.
    !>
    !> Needed due to a bug in IFX as of 2026.1.1 where using the default type constructor leads to partially unitialized descriptors
    !> for default-intitialized Parameter fields in some build configs.
    function new_input(name, description, type, lower_bound, lower_bound_inclusive, upper_bound, upper_bound_inclusive, &
                       default_value) result(inp)
        character(*), intent(in):: name
        character(*), intent(in), optional:: description
        integer, intent(in):: type
        type(Parameter), intent(in), optional:: lower_bound
        logical, intent(in), optional:: lower_bound_inclusive
        type(Parameter), intent(in), optional:: upper_bound
        logical, intent(in), optional:: upper_bound_inclusive
        type(Parameter), intent(in), optional:: default_value
        type(Input):: inp

        inp%name = name
        if (present(description)) inp%description = description
        inp%type = type
        if (present(lower_bound)) inp%lower_bound = lower_bound
        if (present(lower_bound_inclusive)) inp%lower_bound_inclusive = lower_bound_inclusive
        if (present(upper_bound)) inp%upper_bound = upper_bound
        if (present(upper_bound_inclusive)) inp%upper_bound_inclusive = upper_bound_inclusive
        if (present(default_value)) inp%default_value = default_value
    end function
end module
