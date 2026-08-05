module parameters
    use iso_c_binding
    use serialization

    implicit none

    public

    !> Generic container for configuration settings that can be passed between program units in an opaque way.
    !>
    !> Fields can only be modified through dedicated procedures.
    type, bind(C):: Parameter
        private
        character(kind=C_CHAR), dimension(32):: name  !! Name of the parameter
        character(kind=C_CHAR), dimension(512):: description  !! Description of the parameter
        integer(C_INT):: type        !! Type of the parameter. Must exist in enum list at the top of this module.
        character(C_CHAR), dimension(32):: lower_bound  !! Lower bound. Can be a JSON number or the name of another parameter. If
                                                        !! empty string, no lower bound is assumed.
        logical(C_BOOL):: lower_bound_inclusive         !! Ignored if lower_bound == ""
        character(C_CHAR), dimension(32):: upper_bound  !! Upper bound. Can be a JSON number or the name of another parameter.
                                                        !! If empty string, no upper bound is assumed.
        logical(C_BOOL):: upper_bound_inclusive         !! Ignored if upper_bound == ""
    end type Parameter

end module
