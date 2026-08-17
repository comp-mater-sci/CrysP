module mod_model
    use crysp_serialization
    use crysp_input

    implicit none

    type, abstract:: Model

    contains
        procedure(model_get_name), nopass, deferred:: get_name
        procedure(model_get_description), nopass, deferred:: get_description
        procedure(model_get_signature), nopass, deferred:: get_signature
        procedure(model_get_input), nopass, deferred:: get_input
    end type

    abstract interface
        pure function model_get_name() result(name)
            character(:), allocatable:: name
        end function
        pure function model_get_description() result(description)
            character(:), allocatable:: description
        end function
        pure function model_get_signature() result(signature)
            integer, dimension(:), allocatable:: signature
        end function
        pure function model_get_input() result(inputs)
            import Input

            type(Input), dimension(:), allocatable:: inputs
        end function
    end interface
end module
