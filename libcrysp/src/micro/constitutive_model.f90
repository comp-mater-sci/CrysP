module constitutive_model
    use base_defs
    use math_utils
    use conversions
    use crysp_serialization
    use crysp_model
    use crysp_grain

    implicit none

    private
    public:: Phase

    !> Base constitutive model
    !>
    !> Each concrete constitutive model must extend this model and implement its deferred procedures.
    type, extends(Model), abstract:: ConstitutiveModel
        real(DP), dimension(:,:), allocatable:: taylor_coeffs  !! Taylor coefficients of the slip systems. I.e. the 5D vector representation of the symmetric component of the Schmidt matrix.
        real(DP), dimension(:,:), allocatable:: spin_coeffs    !! Spin coeffiecients of the slip systems. I.e. the 3D vector representation of the antisymmetric part of the Schmidt matrix.
        integer, dimension(5):: basis                          !! Indices of a set of independent columns of the Taylor coefficient matrix that form a basis in stress-strain space. Useful for many calculations.
    contains
        procedure(cm_init), deferred::                        init                  !! Initialize the model
        procedure(cm_deform), deferred::                      deform                !! Update the hardening state under a given deformation.
        procedure:: init => cm_init
        procedure:: serialize => cm_serialize
        procedure:: deserialize => cm_deserialize
    end type

    abstract interface
        !> Initialize the hardening model and the model-specific state data of the grains using this model.
        !>
        !> If the parameters do not meet the constraints provided below, this routine crashes the program.
        function cm_init(this, miller_indices, params) result(initial_state)
            import ConstitutiveModel, &
                   GrainState, &
                   Parameter

            class(ConstitutiveModel), intent(inout)::     this      !! Instance of the hardening model to be initialized
            integer, dimension(:,:,:), intent(in):: miller_indices  !! Miller indices of the deformation mechanism to be used.
            type(Parameter), dimension(:), intent(in):: params !! List of parameters to initialize the model with.
                                                                       !! Assumed to pass this%validate_parameters(params)
            class(GrainState), allocatable:: initial_state
        end function

        !> Update the critical resolved shear stresses (CRSS) of a grain.
        !>
        !> Updates CRSS based on the slip rates provided by the caller, assuming these slip rates remain constant over the time
        !> interval provided by the caller. May update internal grain state accordingly.
        subroutine cm_deform(this, state, time, slip_rates)
            import ConstitutiveModel, &
                   GrainState, &
                   DP

            class(ConstitutiveModel), intent(inout):: this
            class(GrainState), target, intent(inout):: state   !! Hardening state to update.
            real(DP), intent(in)                :: time            !! Elapsed time since the last update of the hardening state of this grain.
            real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in):: slip_rates  !! Slip rates on each of the slip systems of the grain in the time
                                                                                       !! interval since the last CRSS update for this grain. Size must equal
                                                                                       !! the number of slip systems of the grain.
        end subroutine
    end interface

contains

    !> Basic iniitialization common to all constitutive models.
    !>
    !> Initializes taylor and spin coefficients and allocates memory for the CRSS.
    subroutine base_init(this, miller_indices)
        class(ConstitutiveModel), intent(out):: this
        integer, dimension(:,:,:), intent(in):: miller_indices              !! Miller indices of all the slip systems.
        integer:: i, &
                  n_systems
        real(DP):: normalized(3, 2), &
                   schmid_matrix(3, 3)

        n_systems = size(miller_indices, 3)

        allocate(this%taylor_coeffs(5, n_systems))
        allocate(this%spin_coeffs(3, n_systems))

        do i = 1, n_systems
            normalized = normalize(miller_indices(:,:,i))
            schmid_matrix = normalized(:,2) .outer. normalized(:,1)
            this%taylor_coeffs(:,i) = tensor_to_deviatoric(schmid_matrix)
            this%spin_coeffs(:,i) = tensor_to_spin(schmid_matrix)
        end do
        this%basis = basis_indices(this%taylor_coeffs)
    end subroutine

    pure function phase_get_grain_state() result(state)
        class(GrainState), allocatable:: state

        allocate(GrainState::state)
    end function

    pure function serialize(this) result(params)
        class(ConstitutiveModel), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        params = params .add. [serialize(this%tayor_coeffs), &
                               serialize(this%spin_coeffs), &
                               serialize(this%basis)]
    end function

    function deserialize(this, params) result(remaining_params)
        class(ConstitutiveModel), target, intent(inout):: this
        type(Parameter), dimension(:), intent(in):: params
        type(Parameter), dimension(:), allocatable:: remaining_params

        this%taylor_coeffs = params(1)
        this%spin_coeffs = remaining_params(2)
        this%basis = remaining_params(3)

        remaining_params = remaining_params .pop. 3
    end function
end module
