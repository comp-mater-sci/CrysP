module constitutive_model
    use base_defs
    use math_utils
    use conversions
    use parameters

    implicit none

    private
    public:: ConstitutiveModel, &
             HardeningState

    !> Hardening state object specific to each grain.
    !>
    !> Concrete constitutive models are to extend this type to include fields for whatever grain-specific state they want to track.
    !> @note
    !> It may seem much nicer to simply create subtypes of [[Grain]] with additional fields for hardening state in the concrete constitutive models
    !> but this leads to problems at the meso level because a Cluster must keep a list of Grains that belong to it
    !> and Fortran does not allow lists of heterogeneous type.
    !> @endnote
    type, abstract:: HardeningState
        real(DP), dimension(:,:), allocatable:: crss  !! Critical resolved shear stress in the positive and negative direction for each slip system.
    end type

    !> Base constitutive model
    !>
    !> Each concrete constitutive model must extend this model and implement its deferred procedures.
    type, abstract:: ConstitutiveModel
        real(DP), dimension(:,:), allocatable:: taylor_coeffs  !! Taylor coefficients of the slip systems. I.e. the 5D vector representation of the symmetric component of the Schmidt matrix.
        real(DP), dimension(:,:), allocatable:: spin_coeffs    !! Spin coeffiecients of the slip systems. I.e. the 3D vector representation of the antisymmetric part of the Schmidt matrix.
        integer, dimension(5):: basis                          !! Indices of a set of independent columns of the Taylor coefficient matrix that form a basis in stress-strain space. Useful for many calculations.
    contains
        procedure(cm_get_signature), deferred, nopass::      get_signature          !! Get the type signature of the parameters of this model
        procedure(cm_get_parameters), deferred, nopass::      get_parameters        !! Get the parameters for this model
        procedure(cm_init), deferred::                        init                  !! Initialize the model
        procedure(cm_deform), deferred::                      deform                !! Update the hardening state under a given deformation.
        procedure:: base_init                                                       !! Basic initializtion common to all constitutive models.
    end type

    abstract interface
        !> Get the type signature of the input parameters of this model
        function cm_get_signature() result(signature)
            integer, dimension(:), allocatable:: signature !! Type signature. Each element represents 1 input parameter. The value
                                                           !! of the element represents its type, encoded as a TYPE enum.
        end function

        !> Get the parameters associated with the hardening model.
        function cm_get_parameters() result(params)
            import ParameterDescriptor

            type(ParameterDescriptor), dimension(:), allocatable:: params
        end function

        !> Initialize the hardening model and the model-specific state data of the grains using this model.
        !>
        !> If the parameters do not meet the constraints provided below, this routine crashes the program.
        function cm_init(this, miller_indices, params) result(initial_state)
            import ConstitutiveModel, &
                   HardeningState, &
                   Parameter

            class(ConstitutiveModel), intent(inout)::     this      !! Instance of the hardening model to be initialized
            integer, dimension(:,:,:), intent(in):: miller_indices  !! Miller indices of the deformation mechanism to be used.
            type(Parameter), dimension(:), intent(in):: params !! List of parameters to initialize the model with.
                                                                       !! Assumed to pass this%validate_parameters(params)
            class(HardeningState), allocatable:: initial_state
        end function

        !> Update the critical resolved shear stresses (CRSS) of a grain.
        !>
        !> Updates CRSS based on the slip rates provided by the caller, assuming these slip rates remain constant over the time
        !> interval provided by the caller. May update internal grain state accordingly.
        subroutine cm_deform(this, state, time, slip_rates)
            import ConstitutiveModel, &
                   HardeningState, &
                   DP

            class(ConstitutiveModel), intent(inout):: this
            class(HardeningState), target, intent(inout):: state   !! Hardening state to update.
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
    subroutine base_init(this, miller_indices, initial_state)
        class(ConstitutiveModel), intent(out):: this
        integer, dimension(:,:,:), intent(in):: miller_indices              !! Miller indices of all the slip systems.
        class(HardeningState), allocatable, intent(inout):: initial_state   !! Initial hardening state for all grains using this constitutive model.

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
            schmid_matrix = normalized(:,2) .outer. normalized(:,1)
            this%taylor_coeffs(:,i) = tensor_to_deviatoric(schmid_matrix)
            this%spin_coeffs(:,i) = tensor_to_spin(schmid_matrix)
        end do
        this%basis = basis_indices(this%taylor_coeffs)
    end subroutine
end module
