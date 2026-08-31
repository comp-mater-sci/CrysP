module constitutive_model
    use base_defs
    use math_utils
    use conversions
    use crysp_serialization
    use crysp_model
    use crysp_grain

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
    type, extends(State):: HardeningState
        real(DP), dimension(:,:), allocatable:: crss
    contains
        procedure:: init => hs_init
        procedure:: size => hs_size
        procedure:: serialize => hs_serialize
        procedure:: deserialize => hs_deserialize
    end type

    !> Base constitutive model
    !>
    !> Each concrete constitutive model must extend this model and implement its deferred procedures.
    type, extends(Model), abstract:: ConstitutiveModel
        real(DP), dimension(:,:), allocatable:: taylor_coeffs  !! Taylor coefficients of the slip systems. I.e. the 5D vector representation of the symmetric component of the Schmidt matrix.
        real(DP), dimension(:,:), allocatable:: spin_coeffs    !! Spin coeffiecients of the slip systems. I.e. the 3D vector representation of the antisymmetric part of the Schmidt matrix.
        integer, dimension(5):: basis                          !! Indices of a set of independent columns of the Taylor coefficient matrix that form a basis in stress-strain space. Useful for many calculations.
    contains
        procedure(cm_deform), deferred::        deform                !! Update the hardening state under a given deformation.
        procedure(cm_make_hardening_state), nopass, deferred:: make_hardening_state
        procedure:: init => cm_init
        procedure:: init_hardening_state => cm_init_hardening_state
        procedure:: size => cm_size
        procedure:: serialize => cm_serialize
        procedure:: deserialize => cm_deserialize
    end type

    abstract interface

        pure function cm_make_hardening_state() result(state)
            class(HardeningState), allocatable:: state
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
    subroutine cm_init(this, miller_indices, params)
        class(ConstitutiveModel), intent(out):: this
        integer, dimension(:,:,:), intent(in):: miller_indices              !! Miller indices of all the slip systems.
        type(Parameter), dimension(size(this%get_signature())), intent(in):: params

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

    pure subroutine hs_init(this, n_systems)
        class(HardeningState), intent(out):: this
        integer, intent(in):: n_systems

        allocate(this%crss(2, n_systems))
    end subroutine

    pure subroutine cm_init_hardening_state(this, state)
        class(ConstitutiveModel), intent(in):: this
        class(HardeningState), intent(out):: state

        call state%init(size(this%taylor_coeffs, 2))
    end subroutine

    pure function cm_size(this) result(size)
        class(ConstitutiveModel), intent(in):: this
        integer:: size

        size = 3
    end function

    pure function cm_serialize(this) result(params)
        class(ConstitutiveModel), target, intent(in):: this
        type(Parameter), dimension(this%get_size()):: params

        params(1:3) = [serialize(this%taylor_coeffs), &
                       serialize(this%spin_coeffs), &
                       serialize(this%basis)]
    end function

    pure subroutine cm_deserialize(this, params)
        class(ConstitutiveModel), target, intent(out):: this
        type(Parameter), dimension(this%get_size()), intent(in):: params

        this%taylor_coeffs = params(1)
        this%spin_coeffs = params(2)
        this%basis = params(3)
    end function

    pure subroutine hs_size(this) result(size)
        class(HardeningState), intent(in):: this
        integer:: size

        return 1
    end subroutine

    pure function hs_serialize(this) result(params)
        class(HardeningState), target, intent(in):: this
        type(Parameter), dimension(this%size()):: params

        params(1) = serialize(this%crss)
    end function

    pure subroutine hs_deserialize(this, params)
        class(HardeningState), intent(out):: this
        type(Parameter), dimension(this%size()), intent(in):: params

        this%crss = params(1)
    end subroutine
end module
