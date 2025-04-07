!> Dispatcher of hardening models
module micro
    use utils
    use parameters
    use logging
    use constitutive_model
    use grain_module

    implicit none
    public

    !> Supported hardening models.
    enum, bind(C)
        !> Constants set for backwards compatibility with input file format.
        enumerator:: HARDENING_NONE       = 0,  &
                     HARDENING_VOCE       = 1,  &
                     HARDENING_SWIFT      = 3,  &
                     HARDENING_DSH_EDGE   = 11, &
                     HARDENING_DSH_SCREW  = 12, &
                     HARDENING_DSH_LOOP   = 13
    end enum

    !> Supported deformation mechanisms
    enum, bind(C)
        enumerator:: SLIP_SYSTEMS_FCC  , &
                     SLIP_SYSTEMS_BCC24, &
                     SLIP_SYSTEMS_BCC48
    end enum

    !> Wrapper type for constitutive model. Needed because different phases may be backed by different subtypes of ConstitutiveModel
    !and Fortran semantics require lists to be of homogeneous type.
    type:: Phase
        class(ConstitutiveModel), allocatable:: model !> The constitutive model backing the phase
    end type

    !> High-level description of a phase. Used for passing phase information to and from higher-level program units. Necessary
    !because much of the phase description may vary in size between phases so using regular arrays is inconvenient/inefficient/unsafe.
    type:: PhaseDescriptor
        integer:: model_id                                      !> ID of the hardening model to be initialized. Must exist in the enum list provided in this module.
        integer:: deformation_mechanism                         !> Deformation mechanism to be employed for all grains.
        type(Parameter), dimension(:), allocatable:: parameters !> Parameters used to initialize the hardening model. Must correspond to the parameter list obtained by calling micro_get_parameters(model_id) and must pass micro_validate_parameters(model_id).
        real(DP), dimension(:,:), allocatable:: orientations    !> List of Euler angle triplets in Bunge convention in the macroscopic frame representing grain orientations.
    end type

    interface
        !>@Brief Returns the parameter list for a particular hardening model.
        !>@Details The parameters are used to initialize the hardening model. If no valid ID is provided, the procedure crashes the program.
        module function micro_get_parameters(model_id) result(params)
            integer, intent(in)::          model_id !> ID of the hardening model. Valid IDs are listed above.
            type(Parameter), allocatable:: params(:) !> List of parameters for the hardening model corresponding to the provided ID.
        end function

        !> @Brief Check if a list of initialized parameters is valid for a given hardening model.
        !> @Details If the parameters do not meet the specified constraints, this routine crashes the program.
        !!          Each hardening model is free do define additional constraints on the parameters. Refer to the documentation of the
        !!          individual hardening models for details.
        module subroutine micro_validate_parameters(model_id, params)
            integer, intent(in):: model_id                             !> ID of the hardening model to be initialized. Must
                                                                       !! exist in the enum list provided in this module.
            type(Parameter), dimension(:), target, intent(in):: params !> List of initialized parameters to be validated. \
                                                                       !! Must correspond to the parameter list
                                                                       !! obtained by calling micro_get_parameters(model_id)
        end subroutine

        !>@Brief Initialize the micro-level entities of the simulation: The hardening model and the grains.
        !>@Details If the parameters do not meet the specified constraints, this procedure crashes the program.
        !!         Must be subroutine to avoid problems with IFX copying too much to the stack.
        module subroutine  micro_init(phases_, grains)
            type(PhaseDescriptor), dimension(:), intent(in):: phases_    !> Phase descriptors for each phase.
            type(Grain), dimension(:), allocatable, intent(out):: grains !> List of initialized grain objects.
        end subroutine

        !>@Brief Update the critical resolved shear stresses (CRSS) of the grain.
        !>@Details Calculates the evolution of the CRSS on each slip system given the slip rate on each slip system and the elapsed
        !time since the last update of the CRSS. The slip rates are assumed constant during the time interval.
        module subroutine micro_deform(grain_, time, slip_rates)
            type(Grain), intent(inout)::  grain_     !> Grain for which to update the CRSS.
            real(DP), intent(in)::     time, &       !> Elapsed time since last update of the CRSS for this grain.
                                       slip_rates(:) !> Slip rate for each slip system of the grain. Size(slip_rates) must equal
                                                     !> the number of slip systems in the grain.
        end subroutine
    end interface
end module

submodule(micro) micro_imp
    implicit none

    character(*), parameter:: MOD_NAME = 'micro'

    class(Phase), dimension(:), allocatable, target:: phases

contains

    !>@Brief Retrieve an unitialized instance of a given hardening model.
    !>@Details Workaround to be able to call type-bound overriden procedures.
    !>@return Uninitialized instance of the requested hardening model.
    function get_model_instance(model_id) result(instance)
        use none
        use swift
        use voce
        use dsh_edge
        use dsh_screw
        use dsh_loop

        integer, intent(in):: model_id                              !> ID of the hardening model. Must be contained in the list provided in this
                                                                    !!  module. If not, this routine crashes the program.
        class(ConstitutiveModel), allocatable, target:: instance               !> Uninitialized instance of the requested hardening model.

        select case(model_id)
            case(HARDENING_NONE)
                allocate(ConstitutiveModelNone:: instance)
            case(HARDENING_VOCE)
                allocate(ConstitutiveModelVoce:: instance)
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

    !> @Brief Get the list of miller indices associated to ta given deformation mechanism.
    !> @Details On invalid input, this routine crashes the program.
    function  get_miller_indices(deformation_mechanism) result(miller_indices)
        use slip_systems

        integer, intent(in):: deformation_mechanism             !> ID of the deformation mechanism. Must exist in the list provided
                                                                !! in this module.
        integer, dimension(:,:,:), allocatable:: miller_indices !> Miller indices for the deformation ordered as  [slip plane
                                                                !! normal, slip direction] for each slip system

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

    module procedure micro_get_parameters
        class(ConstitutiveModel), allocatable:: dummy_instance

        dummy_instance = get_model_instance(model_id)
        params = dummy_instance%get_parameters()
    end procedure

    module procedure micro_validate_parameters
        class(ConstitutiveModel), allocatable:: dummy_instance

        dummy_instance = get_model_instance(model_id)
        call dummy_instance%validate_parameters(params)
    end procedure

    module procedure micro_init
        integer:: i, j, k, &
                  n_phases, &
                  n_grains
        integer, allocatable:: miller_indices(:,:,:)
        class(HardeningState), allocatable:: initial_state
        type(Phase), pointer:: phase_ptr

        n_phases = size(phases_)

        !First determine the total number of grains so we  can allocate the return array.
        n_grains = 0
        do i = 1, n_phases
            n_grains = n_grains+size(phases_(i)%orientations, 2)
        end do


        if (.not. allocated(phases)) &
            allocate(phases(n_phases))
        allocate(grains(n_grains))

        j = 1
        do i = 1, n_phases
            phase_ptr => phases(i)  ! Gfortran crashes when directly assigning into phases array
            phase_ptr%model = get_model_instance(phases_(i)%model_id)
            miller_indices = get_miller_indices(phases_(i)%deformation_mechanism)
            initial_state = phases(i)%model%init(miller_indices, phases_(i)%parameters)

            do k = 1, size(phases_(i)%orientations, 2)
                call grains(j)%init(phases_(i)%orientations(:,k), phases(i)%model, initial_state)
                j = j+1
            end do
        end do
    end procedure

    module procedure micro_deform
        call grain_%model%deform(grain_%state, time, slip_rates)
    end procedure
end submodule
