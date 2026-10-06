module crysp_input
    use iso_c_binding
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
    type, bind(C):: Input
        character(kind=C_CHAR), dimension(32)  :: name                    !! Name of the parameter
        integer(C_INT)                         :: type                    !! Type of the parameter. Must exist in enum list at the
                                                                          !! top of this module
        character(kind=C_CHAR), dimension(1024):: description           = ""      !! Description of the parameter
        logical(C_BOOL)                        :: optional              = .false. !! Indicate if the parameter is optional
        type(Parameter)       :: lower_bound  = Parameter(C_NULL_PTR)!! Lower bound. May be a numerical value or the name of another parameter.
        logical(C_BOOL)                        :: lower_bound_inclusive = .false. !! Ignored if lower_bound == ""
        type(Parameter)       :: upper_bound   = Parameter(C_NULL_PTR)!! Upper bound. May be a numerical value or the name of another parameter.
        logical(C_BOOL)                        :: upper_bound_inclusive = .false. !! Ignored if upper_bound == ""
        type(Parameter):: default_value = Parameter(C_NULL_PTR)
    end type
end module
