module mod_model
    use parameters

    implicit none

    type, abstract:: Model

    contains
        procedure(model_get_name), nopass, deferred:: get_name
        procedure(model_get_description), nopass, deferred:: get_description
        procedure(model_get_parameter_signature), nopass, deferred:: get_signature
        procedure(model_get_parameter_description), nopass, deferred:: get_parameters
    end type

    abstract interface
        pure function model_get_name() result(name)
            character(:), allocatable:: name
        end function
        pure function model_get_description() result(description)
            character(:), allocatable:: description
        end function
        pure function model_get_parameter_signature() result(signature)
            integer, dimension(:), allocatable:: signature
        end function
        pure function model_get_parameter_description() result(descriptors)
            import ParameterDescriptor

            type(ParameterDescriptor), dimension(:), allocatable:: descriptors
        end function
    end interface
end module
