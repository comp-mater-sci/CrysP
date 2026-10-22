module crysp_constitutive_model
    use base_defs
    use math_utils
    use conversions
    use crysp_parameters
    use crysp_model
    use crysp_status
    implicit none

    private
    public:: ConstitutiveModel, &
             HardeningState, &
             Phase

    character(*), parameter:: MOD_NAME = 'constitutive_model'

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
        procedure:: serialize => hard_stat_serialize
        procedure:: deserialize => hard_stat_deserialize
    end type

    !> Base constitutive model
    !>
    !> Each concrete constitutive model extends this model and overrides @c deform and @c make_hardening_state where it has
    !> hardening behaviour of its own. The defaults below describe a model without hardening: @c deform leaves the state untouched
    !> and @c make_hardening_state creates a plain [[HardeningState]].
    !> @note
    !> This type is deliberately NOT abstract, because concrete models call their parent's procedures via
    !> `this%ConstitutiveModel%...`, and the Fortran standard only permits such a type-bound procedure reference when the parent
    !> part-name is not of abstract type (F2018 19.4.5).
    !> @endnote
    type, extends(Model):: ConstitutiveModel
        real(DP), dimension(:,:), allocatable:: taylor_coeffs  !! Taylor coefficients of the slip systems. I.e. the 5D vector representation of the symmetric component of the Schmidt matrix.
        real(DP), dimension(:,:), allocatable:: spin_coeffs    !! Spin coeffiecients of the slip systems. I.e. the 3D vector representation of the antisymmetric part of the Schmidt matrix.
        integer, dimension(5):: basis                          !! Indices of a set of independent columns of the Taylor coefficient matrix that form a basis in stress-strain space. Useful for many calculations.
    contains
        procedure:: deform => cm_deform             !! Update the hardening state under a given deformation.
        procedure, nopass:: make_hardening_state => cm_make_hardening_state
        procedure:: init => cm_init
        procedure:: init_hardening_state => cm_init_hardening_state
        procedure:: describe => cm_describe
    end type

    !> Wrapper type for the constitutive model backing a phase.
    !>
    !> Different phases may be backed by different subtypes of [[ConstitutiveModel]], but Fortran requires lists to be of
    !> homogeneous type, so the concrete model is stored as an allocatable polymorphic component inside this thin wrapper.
    !> This lets an array of `Phase` hold heterogeneous concrete models.
    type:: Phase
        class(ConstitutiveModel), allocatable:: model !! The constitutive model backing the phase
    end type

contains

    !> Base of the serialization chain for hardening states. Serialization only covers model-specific state, and the CRSS is
    !> common to all hardening states, so there is nothing to serialize here.
    !>
    !> The result is allocated empty rather than left unallocated so that extensions can append to it unconditionally.
    function hard_stat_serialize(this) result(params)
        class(HardeningState), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        allocate(params(0))
    end function

    !> Counterpart of [[hard_stat_serialize]]: there is nothing to restore here.
    subroutine hard_stat_deserialize(this, params)
        class(HardeningState), target, intent(inout):: this
        type(Parameter), dimension(:), intent(in):: params
    end subroutine

    !> Basic iniitialization common to all constitutive models.
    !>
    !> Initializes taylor and spin coefficients and allocates memory for the CRSS.
    subroutine cm_init(this, miller_indices, params)
        class(ConstitutiveModel), intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices              !! Miller indices of all the slip systems.
        !> The params argument is assumed-shape because gfortran cannot statically resolve a call to the type-bound
        !> procedure get_signature() inside an array-specifier when the call is dynamically dispatched on a polymorphic object.
        type(Parameter), dimension(:), intent(in):: params

        integer:: i, &
                  n_systems
        real(DP):: normalized(3, 2), &
                   schmid_matrix(3, 3)

        n_systems = size(miller_indices, 3)

        !`this` is intent(inout) so that init preserves the ID stamped by the factory; the components it rebuilds must
        !therefore be released explicitly rather than by the intent(out) that used to do it.
        if (allocated(this%taylor_coeffs)) deallocate(this%taylor_coeffs)
        if (allocated(this%spin_coeffs)) deallocate(this%spin_coeffs)
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

    subroutine cm_init_hardening_state(this, state)
        class(ConstitutiveModel), intent(in):: this
        class(HardeningState), intent(out):: state

        allocate(state%crss(2, size(this%taylor_coeffs, 2)))
    end subroutine

    !> Default @c deform: no hardening, so the state is left untouched.
    function cm_deform(this, state, time, slip_rates) result(stat)
        class(ConstitutiveModel), intent(inout):: this
        class(HardeningState), target, intent(inout):: state
        real(DP), intent(in)                :: time
        real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in):: slip_rates
        type(Status):: stat

    end function

    !> Default @c make_hardening_state: a plain [[HardeningState]] holding only the CRSS.
    function cm_make_hardening_state() result(state)
        class(HardeningState), allocatable:: state

        allocate(state)
    end function

    function cm_describe(this) result(desc)
        class(ConstitutiveModel), intent(in):: this
        type(ModelDescriptor):: desc

        desc = this%Model%describe()
    end function
end module
