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
    use crysp_parameters
    use crysp_input
    use conversions, only: int_to_str
    use crysp_status
    use constitutive_model
    use crysp_grain
    use crysp_model

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

    !> All supported hardening models. Must list every enumerator above.
    integer, dimension(*), parameter:: SUPPORTED_HARDENING_MODELS = [HARDENING_NONE, &
                                                                     HARDENING_VOCE, &
                                                                     HARDENING_HOCKETT_SHERBY, &
                                                                     HARDENING_SWIFT, &
                                                                     HARDENING_DSH_EDGE, &
                                                                     HARDENING_DSH_SCREW, &
                                                                     HARDENING_DSH_LOOP]
    !> Supported deformation mechanisms (i.e. slip system sets).
    enum, bind(C)
        enumerator:: SLIP_SYSTEMS_FCC   !! Face-Centered Cubic.
        enumerator:: SLIP_SYSTEMS_BCC24 !! Body-Centered Cubic excluding the 123-planes.
        enumerator:: SLIP_SYSTEMS_BCC48 !! Body-Centered Cubic including the 123-planes.
    end enum

    !> High-level description of a phase. Used for passing phase information to and from higher-level program units. Necessary
    !> because much of the phase description may vary in size between phases so using regular arrays is inconvenient/inefficient/unsafe.
    type:: PhaseData
        type(ModelData):: model
        integer:: deformation_mechanism                         !! Deformation mechanism for all grains of this phase.
        real(DP), dimension(:,:), allocatable:: orientations    !! List of Euler angle triplets in Bunge convention in the macroscopic frame representing grain orientations.
    end type

    interface
        !> Initialize the micro-level entities of the simulation: The constitutive models and the grains.
        !>
        !> For each provided phase descriptor, a constitutive model is initialized. For each of the orientations in each of
        !> the phase descriptors, a grain object is initialized which points to its constitutive model. All of the grains across all
        !> phases are assimilated as output.
        module function  micro_init(phase_descriptors, phases, grains) result(stat)
            type(PhaseData), dimension(:), intent(in):: phase_data    !! Phase descriptors for each phase
            type(Phase), dimension(:), allocatable, target, intent(out):: phases
            type(Grain), dimension(:), allocatable, intent(out):: grains !! List of initialized grain objects.
            type(Status):: stat
        end subroutine

        module function micro_get_model(id, model) result(stat)
            integer, intent(in):: id
            class(ConstitutiveModel), allocatable, intent(out):: model
            type(Status):: stat
        end function
    end interface
end module

!> Implementation of the interface declared in the micro module.
!>
!> Links the different deformation mechanism and hardening model IDs to specific constitutive models and keeps a reference to the
!> models currently in use.
submodule(micro) micro_imp
    use none
    use swift
    use hockett_sherby
    use voce
    use dsh_edge
    use dsh_screw
    use dsh_loop

    implicit none

    character(*), parameter:: MOD_NAME = 'micro'             !! Module name. Simplifies logging.

contains

    !> Retrieve an unitialized instance of a given hardening model.
    !>
    !> Workaround to be able to call type-bound overriden procedures.
    !> The ID originates from the caller, so an unknown one yields `CRYSP_ERR_TYPE` and leaves `model` unallocated.
    module procedure micro_get_model
        select case(id)
            case (HARDENING_NONE)
                allocate(ConstitutiveModelNone:: model)
            case (HARDENING_VOCE)
                allocate(ConstitutiveModelVoce:: model)
            case (HARDENING_HOCKETT_SHERBY)
                allocate(ConstitutiveModelHockettSherby:: model)
            case (HARDENING_SWIFT)
                allocate(ConstitutiveModelSwift:: model)
            case (HARDENING_DSH_EDGE)
                allocate(ConstitutiveModelDSHEdge:: model)
            case (HARDENING_DSH_SCREW)
                allocate(ConstitutiveModelDSHScrew:: model)
            case (HARDENING_DSH_LOOP)
                allocate(ConstitutiveModelDSHLoop:: model)
            case default
                stat = fail(MOD_NAME, 'micro_get_model', CRYSP_ERR_TYPE, 'Invalid hardening model ID: ' // int_to_str(id))
                return
        end select

        model%id = id
    end procedure

    !> Get the list of miller indices associated to ta given deformation mechanism.
    !>
    !> The mechanism ID originates from the caller, so an unknown one yields `CRYSP_ERR_VAL` and leaves `miller_indices` unallocated.
    function  get_miller_indices(deformation_mechanism, miller_indices) result(stat)
        use slip_systems

        integer, intent(in):: deformation_mechanism             !! ID of the deformation mechanism. Must exist in the enum above.
        integer, dimension(:,:,:), allocatable, intent(out):: miller_indices !! Miller indices for the deformation ordered as
                                                                !! [slip plane normal, slip direction] for each slip system
        type(Status):: stat                                     !! `CRYSP_OK`, or `CRYSP_ERR_VAL` if the deformation mechanism is unknown.

        select case(deformation_mechanism)
            case (SLIP_SYSTEMS_FCC)
                miller_indices = FCC12
            case (SLIP_SYSTEMS_BCC24)
                miller_indices = BCC24
            case (SLIP_SYSTEMS_BCC48)
                miller_indices = BCC48
            case default
                stat = fail(MOD_NAME, 'get_miller_indices', CRYSP_ERR_VAL, 'Invalid slip system set: ' // int_to_str(deformation_mechanism))
        end select
    end function

    !> See interface documentation
    module procedure micro_init
        integer:: i, j, k, &
                  n_grains
        integer, allocatable:: miller_indices(:,:,:)
        real(DP):: orientation(3)
        class(HardeningState), allocatable:: initial_state
        type(Phase), pointer:: phase_ptr

        !First determine the total number of grains so we  can allocate the return array.
        allocate(phases(size(phase_data)))
        n_grains = 0
        do i = 1, size(phases)
            n_grains = n_grains+size(phase_data(i)%orientations, 2)
        end do
        allocate(grains(n_grains))

        j = 1
        do i = 1, size(phases)
            phase_ptr => phases(i)  ! Gfortran crashes when directly assigning into phases array
            stat = micro_get_model(phase_data(i)%model%id, phase_ptr%model)
            if (failed(stat)) return
            stat = get_miller_indices(phase_data(i)%deformation_mechanism, miller_indices)
            if (failed(stat)) return
            call phases(i)%model%init(miller_indices, phase_data(i)%)
            call phases(i)%model%init(miller_indices, phase_data(i)%model%parameters)

            initial_state = phases(i)%model%make_hardening_state()
            call phases(i)%model%init_hardening_state(initial_state)

            do k = 1, size(phase_descriptors(i)%orientations, 2)
                !assignment of orientation needed for gfortran
                orientation = phase_descriptors(i)%orientations(:,k)
                call grains(j)%init(orientation, phases(i)%model, initial_state)
                j = j+1
            end do
        end do
    end procedure
end submodule
