module crysp_input
    use iso_c_binding
    use crysp_serialization

    implicit none

    public

    !> Possible types of input. Maps to exactly one serialization type
    enum, bind(C)
        enumerator:: INPUT_REAL         !! real(C_DOUBLE)
        enumerator:: INPUT_ANGLES_LIST  !! real(C_DOUBLE)(2,*)
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
        type(Parameter)       :: lower_bound  !! Lower bound. May be a numerical value or the name of another parameter.
        logical(C_BOOL)                        :: lower_bound_inclusive = .false. !! Ignored if lower_bound == ""
        type(Parameter)       :: upper_bound   !! Upper bound. May be a numerical value or the name of another parameter.
        logical(C_BOOL)                        :: upper_bound_inclusive = .false. !! Ignored if upper_bound == ""
        type(Parameter):: default_value
    end type
end module
