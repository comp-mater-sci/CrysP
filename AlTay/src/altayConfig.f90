!> Basic configuration of AlTay in a form of formalized data structures.

module altayConfig
    use base_defs
    use math_utils
    use parameters
    use slip_systems

    implicit none

    integer, parameter  :: fname_len = 512 !< Length of filenames

    !> \name Named constants for identifiers of the supported models
    !>@{
    integer, parameter:: modelFCTaylor = 1, modelAlamel = 2
    !>@}

    !> type that subsumes step input and output data
    type:: simulStepData
        real(DP), dimension(3,3):: velocity_gradient !! Velocity gradient applied during the step
        real(DP), dimension(3,3):: stress            !! Homogenized stress tensor over the sample
    end type

    type:: outputConfig
        integer                                   :: nfile = 0 !< (SIMUL)
    end type

   type:: simulData
        !> Model selection. At the same time it controls number of grains in the cluster.
        !> Possible values are:
        !>   - 1-FC Taylor
        !>   - 2-Alamel
        integer                   :: NGR = 2  ! Number of grains in the cluster
        real(DP), dimension(3, 3):: FMicro = UNIT_MATRIX_3X3
    end type

    !> Root-level configuration structure of Altay
    type:: altayConfigData
        integer                                   :: model_id      = modelAlamel
        character(len = fname_len)                  :: output_prefix = 'alamel'
        character(len = fname_len)                  :: jobtitle      = 'alamel'
        character(len = fname_len)                  :: micros_fname  = 'equiaxed.smt'
        character(len = fname_len)                  :: texture_input_fname = ''
        type(outputConfig)                        :: output_config !< output file prefix, incremental output request flag, verbosity level
        type(simulData)                           :: simul_init
        integer:: hardening_model_id
        type(Parameter), allocatable:: hardening_parameters(:)
        integer:: deformation_mechanism
    end type

    type:: altayStateData
        real(DP)                                :: eps = 0.D0
        integer                                         :: nSimulCalls = 0 !< Corresponds to NBLOC config data
        type(simulStepData), dimension(:), allocatable    :: simulCalls
        integer                                         :: this = 0        !< Iterator over simulCalls
    end type

    ! Definition of the singleton objects
    type(altayConfigData):: acnf
    type(altayStateData)  :: astate


contains

    !> Configure the cnf object for using selected model type (FCTaylor, ALAMEL).
    subroutine setModelType(cnf, modelId, info)
        type(altayConfigData), intent(inout)       :: cnf
        integer, intent(in)                        :: modelId
        integer, intent(out)                       :: info

        select case(modelId)
            case(modelFCTaylor)
                cnf%simul_init%ngr = 1
            case(modelAlamel)
                cnf%simul_init%ngr = 2
            case default
                info = -1
                return
        end select
        ! OK, supported model
        cnf%model_id = modelId
        info = 0
    end subroutine

end module
