module constitutive_model
    use base_defs
    use math_utils
    use conversions
    use crysp_parameters
    use crysp_model
    use crysp_status
    implicit none

    private
    public:: ConstitutiveModel, &
             ConstitutiveModel_C, &
             HardeningState, &
             HardeningState_C, &
             Phase, &
             assignment(=)

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

    !> C-compatible counterpart of [[HardeningState]].
    !>
    !> `crss` holds the columns of its Fortran counterpart one after the other. A `HardeningState_C` produced by libCrysP owns
    !> `crss`, malloc'ed so that C can release it with `free()`.
    type, bind(C):: HardeningState_C
        integer(C_INT):: n_systems
        type(C_PTR):: crss     !! n_systems * 2 * double
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
        procedure:: serialize => cm_serialize
        procedure:: deserialize => cm_deserialize
        procedure:: describe => cm_describe
    end type

    !> C-compatible counterpart of [[ConstitutiveModel]].
    !>
    !> `hardening_state` holds the model ID and the full serialized state of the concrete model, see [[crysp_model:ModelData_C]].
    !> The coefficient matrices hold the columns of their Fortran counterparts one after the other. A `ConstitutiveModel_C`
    !> produced by libCrysP owns all of its pointers, malloc'ed so that C can release them with `free()`.
    type, bind(C):: ConstitutiveModel_C
        type(ModelData_C):: hardening_state
        integer(C_INT):: n_systems
        type(C_PTR):: taylor_coeffs !! n_systems * 5 * real(C_DOUBLE)
        type(C_PTR):: spin_coeffs   !! n_systems * 3 * real(C_DOUBLE)
        integer(C_INT), dimension(5):: basis
    end type

    !> Wrapper type for the constitutive model backing a phase.
    !>
    !> Different phases may be backed by different subtypes of [[ConstitutiveModel]], but Fortran requires lists to be of
    !> homogeneous type, so the concrete model is stored as an allocatable polymorphic component inside this thin wrapper.
    !> This lets an array of `Phase` hold heterogeneous concrete models.
    type:: Phase
        class(ConstitutiveModel), allocatable:: model !! The constitutive model backing the phase
    end type

    interface assignment(=)
        module procedure cm_to_c, cm_from_c, hard_stat_to_c, hard_stat_from_c
    end interface

contains

    !> Convert a constitutive model to its C-compatible counterpart. The caller takes ownership of all pointers, see
    !> [[ConstitutiveModel_C]].
    !>
    !> Not `pure`: the coefficients and the serialized state are malloc'ed.
    subroutine cm_to_c(cm_c, cm)
        type(ConstitutiveModel_C), intent(out):: cm_c
        class(ConstitutiveModel), intent(in):: cm

        integer:: n_systems
        real(C_DOUBLE), dimension(:,:), pointer:: ptr

        cm_c%hardening_state = ModelData(cm%id, cm%serialize())
        n_systems = size(cm%taylor_coeffs,2)
        cm_c%n_systems = int(n_systems, C_INT)
        cm_c%taylor_coeffs = malloc(int(5 * n_systems, C_SIZE_T) * c_sizeof(0._C_DOUBLE))
        call c_f_pointer(cm_c%taylor_coeffs, ptr, [5,n_systems])
        ptr = real(cm%taylor_coeffs, C_DOUBLE)
        cm_c%spin_coeffs = malloc(int(3 * n_systems, C_SIZE_T) * c_sizeof(0._C_DOUBLE))
        call c_f_pointer(cm_c%spin_coeffs, ptr, [3,n_systems])
        ptr = real(cm%spin_coeffs, C_DOUBLE)
        cm_c%basis = int(cm%basis,C_INT)
    end subroutine

    !> Restore a constitutive model from its C-compatible counterpart.
    !>
    !> `cm` must already have the dynamic type matching `cm_c%hardening_state%id`, as allocated by [[micro:micro_get_model]]: the
    !> ID is handled at factory level, and this module cannot reach the factory. `cm` is `intent(inout)` rather than
    !> `intent(out)` so that the ID stamped by the factory survives.
    !>
    !> The state is restored from `cm_c%hardening_state`, which already holds the coefficients and basis. The separate coefficient
    !> matrices are therefore only freed. Everything in `cm_c` is freed, so it must have been malloc'ed and is dangling afterwards.
    !>
    !> `cm_c` is assumed to be well-formed: it is not validated here.
    subroutine cm_from_c(cm, cm_c)
        class(ConstitutiveModel), intent(inout):: cm
        type(ConstitutiveModel_C), intent(in):: cm_c

        type(ModelData):: model_data

        model_data = cm_c%hardening_state
        call cm%deserialize(model_data%parameters)
        call free(cm_c%taylor_coeffs)
        call free(cm_c%spin_coeffs)
    end subroutine

    !> Convert a hardening state to its C-compatible counterpart. The caller takes ownership of `hs_c%crss`.
    !>
    !> Only the CRSS is converted: fields added by extensions of [[HardeningState]] are not.
    !>
    !> Not `pure`: the CRSS is malloc'ed.
    subroutine hard_stat_to_c(hs_c, hs)
        type(HardeningState_C), intent(out):: hs_c
        class(HardeningState), intent(in):: hs

        integer:: n_systems
        real(C_DOUBLE), dimension(:,:), pointer:: crss

        n_systems = size(hs%crss, 2)
        hs_c%n_systems = int(n_systems, C_INT)
        hs_c%crss = malloc(int(2 * n_systems, C_SIZE_T) * c_sizeof(0._C_DOUBLE))
        call c_f_pointer(hs_c%crss, crss, [2, n_systems])
        crss = real(hs%crss, C_DOUBLE)
    end subroutine

    !> Restore a hardening state from its C-compatible counterpart.
    !>
    !> Only the CRSS is restored. `hs` is `intent(inout)` so that the dynamic type and any fields added by extensions of
    !> [[HardeningState]] are left as they were. The CRSS is copied and then freed, so `hs_c%crss` must have been malloc'ed and is
    !> dangling afterwards.
    !>
    !> `hs_c` is assumed to be well-formed: it is not validated here.
    subroutine hard_stat_from_c(hs, hs_c)
        class(HardeningState), intent(inout):: hs
        type(HardeningState_C), intent(in):: hs_c

        real(C_DOUBLE), dimension(:,:), pointer:: crss

        call c_f_pointer(hs_c%crss, crss, [2, int(hs_c%n_systems)])
        hs%crss = real(crss, DP)
        call free(hs_c%crss)
    end subroutine

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
