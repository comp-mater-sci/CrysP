module crysp_model
    use crysp_serialization
    use crysp_input

    implicit none

    type, extends(State), abstract:: Model
    contains
        procedure(model_get_name), nopass, deferred:: get_name
        procedure(model_get_description), nopass, deferred:: get_description
        procedure, nopass:: get_signature => model_get_signature
        procedure, nopass:: get_input => model_get_input
        procedure:: serialize => model_serialize
        procedure:: deserialize => model_deserialize
    end type

    abstract interface
        pure function model_get_name() result(name)
            character(:), allocatable:: name
        end function
        pure function model_get_description() result(description)
            character(:), allocatable:: description
        end function
    end interface
contains
    pure function model_serialize(this) result(params)
        class(Model), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        allocate(params(0))
    end function

    function model_deserialize(this, params) result(params_)
        class(Model), target, intent(out):: this
        type(Parameter), dimension(:), intent(in):: params
        type(Parameter), dimension(:), allocatable:: params_

        params_ = params
    end function

    pure function model_get_signature() result(signature)
        integer, dimension(:), allocatable:: signature

        allocate(signature(0))
    end function

    pure function model_get_input() result(inputs)
        type(Input), dimension(:), allocatable:: inputs

        allocate(inputs(0))
    end function
end module
