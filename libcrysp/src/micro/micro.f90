!> Top-level module at the micro scale.
!>
!> The micro level is concerned with all phenomena occuring inside a single grain. These phenomena determine how a single grain
!> responds to imposed stress or strain.
!>
!> This module is merely an interface to the microscopic layer and does not contain any implementation. It does however define which
!> deformation mechanisms and hardening models are supported. Together these form constitutive models, which implement the logic
!> needed for the interface defined here. Higher-level modules can query this module for all of the information they need regarding
!> the supported constitutive  models, which parameters they use etc. In this way, no hard-coded information about the available models needs to be kept at all at higher levels.

module micro
    use base_defs
    use parameters
    use logging
    use constitutive_model
    use grain_module

    implicit none

    public

    !> Supported hardening models.
    !>
    !> Refer to the documentation of the implementation each model for details on the model itself as well as its parameters.
    !> @note
    !> Constants set for backwards compatibility with input file format.
    !> @endnote
    enum, bind(C)
        enumerator:: HARDENING_NONE             = 0  !! No hardening.
        enumerator:: HARDENING_VOCE             = 1  !! Isotropic hardening according to Voce law.
        enumerator:: HARDENING_HOCKETT_SHERBY   = 2  !! Isotropic hardening according to Hockett-Sherby law.
        enumerator:: HARDENING_SWIFT            = 3  !! Isotropic hardening according to SWift law.
        enumerator:: HARDENING_DSH_EDGE         = 11 !! Physics-based hardening model based on edge dislocation movement.
        enumerator:: HARDENING_DSH_SCREW        = 12 !! Variant of DSH hardening using screw dislocations.
        enumerator:: HARDENING_DSH_LOOP         = 13 !! Variant of DSH hardening using loop dislocations.
    end enum

    !> Supported deformation mechanisms (i.e. slip system sets).
    enum, bind(C)
        enumerator:: SLIP_SYSTEMS_FCC   !! Face-Centered Cubic.
        enumerator:: SLIP_SYSTEMS_BCC24 !! Body-Centered Cubic excluding the 123-planes.
        enumerator:: SLIP_SYSTEMS_BCC48 !! Body-Centered Cubic including the 123-planes.
    end enum

    !> Wrapper type for constitutive model. Needed because different phases may be backed by different subtypes of ConstitutiveModel
    !and Fortran semantics require lists to be of homogeneous type.
    type:: Phase
        class(ConstitutiveModel), allocatable:: model !! The constitutive model backing the phase
    end type

    !> High-level description of a phase. Used for passing phase information to and from higher-level program units. Necessary
    !> because much of the phase description may vary in size between phases so using regular arrays is inconvenient/inefficient/unsafe.
    type:: PhaseDescriptor
        integer:: model_id                                      !! ID of the hardening model used by this phase. Must exist in the
                                                                !! enum above.
        integer:: deformation_mechanism                         !! Deformation mechanism for all grains of this phase.
        character(C_CHAR), dimension(:), allocatable:: parameters
        real(DP), dimension(:,:), allocatable:: orientations    !! List of Euler angle triplets in Bunge convention in the macroscopic frame representing grain orientations.
    end type

    interface
      !  !> Returns the parameter list for a particular hardening model.
      !  !>
      !  !> The parameters are used to initialize the hardening model.
      !  !> ID must exist in the enum above. If not, the procedure crashes the program.
      !  module function micro_get_parameters(model_id) result(params)
      !      integer, intent(in)::          model_id  !! ID of the hardening model. Must exist in the list above.
      !      type(Parameter), allocatable:: params(:) !! List of parameters for the hardening model corresponding to the provided ID.
      !  end function

      !  !> Check if a list of initialized parameters is valid for a given hardening model.
      !  !>
      !  !> If any of the parameters is invalid, the implementation is expected to crashes the program.
      !  module subroutine micro_validate_parameters(model_id, params)
      !      integer, intent(in):: model_id                             !! ID of the hardening model to be initialized. Must
      !                                                                 !! exist in the enum above.
      !      type(Parameter), dimension(:), target, intent(in):: params !! List of initialized parameters to be validated.
      !  end subroutine

        !> Initialize the micro-level entities of the simulation: The constitutive models and the grains.
        !>
        !> For each provided phase descriptor, a constitutive model is initialized. For each of the orientations in each of
        !> the phase descriptors, a grain object is initialized which points to its constitutive model. All of the grains across all
        !> phases are assimilated as output.
        module subroutine  micro_init(phase_descriptors, phases, grains)
            type(PhaseDescriptor), dimension(:), intent(in):: phase_descriptors    !! Phase descriptors for each phase.
            type(Phase), dimension(:), allocatable, target, intent(out):: phases
            type(Grain), dimension(:), allocatable, intent(out):: grains !! List of initialized grain objects.
        end subroutine
    end interface
end module

!> Implementation of the interface declared in the micro module.
!>
!> Links the different deformation mechanism and hardening model IDs to specific constitutive models and keeps a reference to the
!> models currently in use.
submodule(micro) micro_imp

    implicit none

    character(*), parameter:: MOD_NAME = 'micro'             !! Module name. Simplifies logging.

contains

    !> Brief Retrieve an unitialized instance of a given hardening model.
    !>
    !> Workaround to be able to call type-bound overriden procedures.
    !> If an invalid model ID is provided, this routine crashes the program.
    function get_model_instance(model_id) result(instance)
        use none
        use swift
        use hockett_sherby
        use voce
        use dsh_edge
        use dsh_screw
        use dsh_loop

        integer, intent(in):: model_id                           !! ID of the hardening model. Must be contained in the enum above.
        class(ConstitutiveModel), allocatable, target:: instance !! Uninitialized instance of the requested hardening model.

        select case(model_id)
            case(HARDENING_NONE)
                allocate(ConstitutiveModelNone:: instance)
            case(HARDENING_VOCE)
                allocate(ConstitutiveModelVoce:: instance)
            case(HARDENING_HOCKETT_SHERBY)
                allocate(ConstitutiveModelHockettSherby:: instance)
            case(HARDENING_SWIFT)
                allocate(ConstitutiveModelSwift:: instance)
            case(HARDENING_DSH_EDGE)
                allocate(ConstitutiveModelDSHEdge:: instance)
            case(HARDENING_DSH_SCREW)
                allocate(ConstitutiveModelDSHScrew:: instance)
            case(HARDENING_DSH_LOOP)
                allocate(ConstitutiveModelDSHLoop:: instance)
            case default
                call log_error(MOD_NAME, 'get_model_instance', ERR_VAL, 'Invalid hardening model ID')
        end select
    end function

    !> Get the list of miller indices associated to ta given deformation mechanism.
    !>
    !> If an invalid deformation mechanism ID is provided, this routine crashes the program.
    function  get_miller_indices(deformation_mechanism) result(miller_indices)
        use slip_systems

        integer, intent(in):: deformation_mechanism             !! ID of the deformation mechanism. Must exist in the enum above.
        integer, dimension(:,:,:), allocatable:: miller_indices !! Miller indices for the deformation ordered as
                                                                !! [slip plane normal, slip direction] for each slip system

        select case(deformation_mechanism)
            case (SLIP_SYSTEMS_FCC)
                miller_indices = FCC12
            case (SLIP_SYSTEMS_BCC24)
                miller_indices = BCC24
            case (SLIP_SYSTEMS_BCC48)
                miller_indices = BCC48
            case default
                call log_error(MOD_NAME, 'get_miller_indices', ERR_VAL, 'Invalid slip system set')
        end select
    end function

    !> See interface domentation
    module procedure micro_get_parameters
        class(ConstitutiveModel), allocatable:: dummy_instance

        dummy_instance = get_model_instance(model_id)
        params = dummy_instance%get_parameters()
    end procedure

    !> See interface domentation
    module procedure micro_validate_parameters
        class(ConstitutiveModel), allocatable:: dummy_instance

        dummy_instance = get_model_instance(model_id)
        call dummy_instance%validate_parameters(params)
    end procedure

    !> See interface domentation
    module procedure micro_init
        integer:: i, j, k, &
                  n_grains
        integer, allocatable:: miller_indices(:,:,:)
        real(DP):: orientation(3)
        class(HardeningState), allocatable:: initial_state
        type(Phase), pointer:: phase_ptr

        !First determine the total number of grains so we  can allocate the return array.
        allocate(phases(size(phase_descriptors)))
        n_grains = 0
        do i = 1, size(phases)
            n_grains = n_grains+size(phase_descriptors(i)%orientations, 2)
        end do
        allocate(grains(n_grains))

        j = 1
        do i = 1, size(phases)
            phase_ptr => phases(i)  ! Gfortran crashes when directly assigning into phases array
            phase_ptr%model = get_model_instance(phase_descriptors(i)%model_id)
            miller_indices = get_miller_indices(phase_descriptors(i)%deformation_mechanism)
            initial_state = phases(i)%model%init(miller_indices, phase_descriptors(i)%parameters)

            do k = 1, size(phase_descriptors(i)%orientations, 2)
                !assignment of orientation needed for gfortran
                orientation = phase_descriptors(i)%orientations(:,k)
                call grains(j)%init(orientation, phases(i)%model, initial_state)
                j = j+1
            end do
        end do
    end procedure
end submodule
