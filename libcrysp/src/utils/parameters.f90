module parameters
    use iso_c_binding
    use serialization

    implicit none

    public

    !> Generic container for configuration settings that can be passed between program units in an opaque way.
    !>
    !> Fields can only be modified through dedicated procedures.
    type, bind(C):: Parameter
        character(kind=C_CHAR), dimension(32):: name                    !! Name of the parameter
        integer(C_INT):: type                                           !! Type of the parameter. Must exist in enum list in serialization.
        character(kind=C_CHAR), dimension(1024):: description = ""      !! Description of the parameter
        logical:: optional                                    = .false. !! Indicate if the parameter is optional
        character(C_CHAR), dimension(32):: lower_bound        = ""      !! Lower bound. Can be a JSON number or the name of another parameter. If
                                                                        !! empty string, no lower bound is assumed.
        logical(C_BOOL):: lower_bound_inclusive               = .false. !! Ignored if lower_bound == ""
        character(C_CHAR), dimension(32):: upper_bound        = ""      !! Upper bound. Can be a JSON number or the name of another parameter.
                                                                        !! If empty string, no upper bound is assumed.
        logical(C_BOOL):: upper_bound_inclusive               = .false. !! Ignored if upper_bound == ""
        character(C_CHAR), dimension(1024):: default_value    = ""      !! JSON string describing the default value of the parameter, in the format
                                                                        !! corresponding to the parmeter type.
    end type Parameter

end module
