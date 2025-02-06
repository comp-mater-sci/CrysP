module constitutive_model
    use utils
    use parameters

    implicit none

    private
    public:: ConstitutiveModel, &
             HardeningState

    type, abstract:: HardeningState
        real(DP), dimension(:,:), allocatable:: crss
    end type

    type, abstract:: ConstitutiveModel
        real(DP), dimension(:,:), allocatable:: taylor_coeffs
        real(DP), dimension(:,:), allocatable:: spin_coeffs
    contains
        procedure(cm_get_parameters), deferred, nopass::      get_parameters
        procedure(cm_validate_parameters), deferred, nopass:: validate_parameters
        procedure(cm_init), deferred::                        init
        procedure(cm_deform), deferred::                      deform
        procedure:: base_init
    end type

    abstract interface
        !>@Brief Get the parameters associated with the hardening model.
        !>@Details The default implementation returns an empty list.
        !>@Return The list of initialized parameters.
        function cm_get_parameters() result(params)
            import Parameter

            type(Parameter), dimension(:), allocatable:: params !> List of parameters
        end function

        !>@Brief Validate a parameter set for the current hardening model.
        !>@Details Check if the parameter values provided by the caller lie within acceptable bounds. Crashes the program if not.
        subroutine cm_validate_parameters(params)
            import Parameter

            type(Parameter), dimension(:), target, intent(in):: params !> The parameter list with user-provided values.
        end subroutine

        !>@Brief Initialize the hardening model and the model-specific state data of the grains using this model.
        !>@Details If the parameters do not meet the constraints provided below, this routine crashes the program.
        function cm_init(this, miller_indices, params) result(initial_state)
            import ConstitutiveModel, &
                   Parameter, &
                   HardeningState

            class(ConstitutiveModel), intent(inout)::     this      !> Instance of the hardening model to be initialized
            integer, dimension(:,:,:), intent(in):: miller_indices
            type(Parameter), dimension(:), target, intent(in):: params !> List of parameters to initialize the model with.
                                                                       !! Must pass this%validate_parameters(params)
            class(HardeningState), allocatable:: initial_state
        end function

        !>@Brief Update the critical resolved shear stresses (CRSS) of a grain.
        !>@Details Updates CRSS based on the slip rates provided by the caller, assuming these slip rates remain constant over the time
        !!         interval provided by the caller. May update internal grain state accordingly. The default implementation returns 1 for all of
        !!         the CRSS values.
        subroutine cm_deform(this, state, time, slip_rates)
            import ConstitutiveModel, &
                   HardeningState, &
                   DP

            class(ConstitutiveModel), intent(inout):: this     !> The hardening model
            class(HardeningState), target, intent(inout)             :: state   !> The grain for which to update the CRSS.
            real(DP), intent(in)                :: time     !> Elapsed time since the last update of the CRSS of this grain.
            real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in):: slip_rates  !> Slip rates on each of the slip systems of the grain in the time
                                                                                     !! interval since the last CRSS update for this grain. Size must equal
                                                                                     !! the number of slip systems of the grain.
        end subroutine
    end interface

contains

    subroutine base_init(this, miller_indices, initial_state)
        class(ConstitutiveModel), intent(out):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        class(HardeningState), allocatable, intent(inout):: initial_state

        integer:: i, &
                  n_systems
        real(DP):: normalized(3, 2), &
                   schmid_matrix(3, 3)

        n_systems = size(miller_indices, 3)

        allocate(this%taylor_coeffs(5, n_systems))
        allocate(this%spin_coeffs(3, n_systems))
        allocate(initial_state%crss(2, n_systems))

        do i = 1, n_systems
            normalized = normalize(miller_indices(:,:,i))
            schmid_matrix = normalized(:,1) .outer. normalized(:,2)
            this%taylor_coeffs(:,i) = convert_stress_strain_space(schmid_matrix)
            this%spin_coeffs(:,i) = convert_spin(schmid_matrix)
        end do
    end subroutine
end module
