module hardening_model
    use utils, only: dp
    use altayConfig
    use parameters
    use logging
    use grain_module

    implicit none

    private
    public ::   HardeningModel

    !>Basic hardening model implementation.
    !>No hardening occurs.
    !>All other hardening models extend this type.
    type, abstract:: HardeningModel
    contains
        procedure(hardening_model_get_parameters), deferred, nopass::      get_parameters
        procedure(hardening_model_validate_parameters), deferred, nopass:: validate_parameters
        procedure(hardening_model_init), deferred::                        init
        procedure(hardening_model_deform), deferred::                      deform
    end type

    abstract interface
        !>@Brief Get the parameters associated with the hardening model.
        !>@Details The default implementation returns an empty list.
        !>@Return The list of initialized parameters.
        function hardening_model_get_parameters(this) result(params)
            type(Parameter), dimension(:), allocatable:: params !> List of parameters
        end function

        !>@Brief Validate a parameter set for the current hardening model.
        !>@Details Check if the parameter values provided by the caller lie within acceptable bounds. Crashes the program if not.
        subroutine hardening_model_validate_parameters(this, params)
            type(Parameter), dimension(:), target, intent(in):: params !> The parameter list with user-provided values.
        end subroutine

        !>@Brief Initialize the hardening model and the model-specific state data of the grains using this model.
        !>@Details If the parameters do not meet the constraints provided below, this routine crashes the program.
        subroutine hardening_model_init(this, grains, params)
            class(HardeningModel), intent(inout)::     this      !> Instance of the hardening model to be initialized
            type(Grain), dimension(:), intent(inout):: grains  !> The list of grains making use of this model.
            type(Parameter), dimension(:), target, intent(in):: params !> List of parameters to initialize the model with.
                                                                       !! Must pass this%validate_parameters(params)
        end subroutine

        !>@Brief Update the critical resolved shear stresses (CRSS) of a grain.
        !>@Details Updates CRSS based on the slip rates provided by the caller, assuming these slip rates remain constant over the time
        !!         interval provided by the caller. May update internal grain state accordingly. The default implementation returns 1 for all of
        !!         the CRSS values.
        subroutine hardening_model_deform(this, grain_, time, slip_rates)
            class(HardeningModel), intent(inout):: this     !> The hardening model
            type(Grain), intent(in)             :: grain_   !> The grain for which to update the CRSS.
            real(DP), intent(in)                :: time     !> Elapsed time since the last update of the CRSS of this grain.
            real(DP), dimension(size(grain_%slip_systems)), intent(in):: slip_rates  !> Slip rates on each of the slip systems of the grain in the time
                                                                                     !! interval since the last CRSS update for this grain. Size must equal
                                                                                     !! the number of slip systems of the grain.
        end subroutine
    end interface
end module hardening_model


