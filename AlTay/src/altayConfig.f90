!> Basic configuration of AlTay in a form of formalized data structures.

module altayConfig
    use altayTexFormats
    use definitions
    use criMathUtils
    use parameters

    implicit none

    integer,parameter  :: fname_len = 512 !< Length of filenames

    !> \name Named constants for identifiers of the supported models
    !>@{
    integer,parameter :: modelFCTaylor = 1, modelAlamel = 2
    !>@}

    type :: slipSystemData
        !> Name of the file containing definitions of slipsystems
        character(len=fname_len)                  :: input_fname = ''
    end type

    type :: textureData
        !> Type of texture representation
        integer                                   :: input_type = TF_SMT
        character(len=fname_len)                  :: input_fname = ''
        integer                                   :: block_id = 1
    end type

    type :: simulStepInputData ! no hardening data!
        logical                                   :: keep_texture = .true.  !< Flag that decides if this step leads to modification of the texture.
        !> Flag that decides if this step leads to an update of the state components
        !> (other than texture)
        logical                                   :: keep_state = .true.
        !> Flag that decides if the full model is to be employed.
        !>   If set .false.: 1) a simplified formula is used for calculations of the microscopic stress
        !>                   2) texture is NOT updated, so "keep_texture" must be set, too.
        logical                                   :: full_model = .true.
        !> Flag that decides if the initial texture should be written out as a CUR output of the step.
        !> \note The texture is actually written out for initial configuration that is available
        !> at the beginning of the step.
        !> \remark This flag takes effect if outputConfig::nfile is non-zero. \sa outputConfig::nfile
        logical                                   :: do_output_init = .false.
        !> Flag that decides if the final texture (as it is at the end of the call) should be written out
        !> as a a CUR output of the step.
        !> \remark This flag takes effect if outputConfig::nfile is non-zero. \sa outputConfig::nfile
        logical                 :: do_output_final = .false.
        integer                 :: nsteps = 1                    !< Number of steps per call
        real(dp),dimension(3,3) :: dgf  = 0.D0                   !< Deformation gradient tensor to be imposed. (MB: this is rather a velocity gradient.)
    end type

    type :: simulStepOutputData
        !> Macroscopic (homogenized) stress
        real(dp), dimension(3,3)    :: stress_tensor = 0.D0
        real(dp)                    :: taylor_factor = 0.D0             !< Macroscopic (homogenized) Taylor factor
        real(dp)                    :: strain_rate_heterogeneity = 0.D0 !< Strain Rate Heterogeneity in polycrystal.
                                                                        ! \f$ \kappa = (||d-D||) / ||D|| \f$
        real(dp)                    :: equivalent_stress = 0.D0         !< Macroscopic stress, defined as the work conjugate to D_vM:
                                                                        ! \f$ \sigma_{eq} = (\mathbf{S} \cdot \mathbf{D}) / D_{vM} \f$
        real(dp)                    :: effective_stress = 0.D0          !< Macroscopic (homogenized) effective von Mises stress
        real(dp)                    :: homogenised_slip = 0.D0          !< Macroscopic (homogenized) plastic slip
        real(dp)                    :: homogenised_slip_tot = 0.D0      !< Macroscopic (homogenized) plastic slip - total over the calls
        real(dp)                    :: effective_macro_strain = 0.D0    !< Macroscopic (imposed) effective von Mises strain - total over the steps
        real(dp)                    :: effective_macro_strain_tot = 0.D0 !< Macroscopic (imposed) effective von Mises strain - total over the calls
        real(dp)                    :: effective_macro_strain_tot_end = 0.D0 !< Macroscopic (imposed) effective von Mises strain till the end of the current step - total over the calls
    end type

    !> type that subsumes step input and output data
    type :: simulStepData
        type(simulStepInputData)            :: input
        type(simulStepOutputData)           :: output
    end type

    type :: outputConfig
        integer                                   :: nfile = 0 !< (SIMUL) NFILTW (Make output files 0 or 1)
        integer                                   :: nfiltw = 0!< (SIMUL) IPR  0-3 Print switch
    end type

   type :: simulData
        !> Model selection. At the same time it controls number of grains in the cluster.
        !> Possible values are:
        !>   - 1 - FC Taylor
        !>   - 2 - Alamel
        integer                   :: NGR = 2 ! Number of grains in the cluster
        real(dp), dimension(3,3) :: FMicro = unit_sr_Matrix
    end type

    !> Root-level configuration structure of Altay
    type :: altayConfigData
        integer                                   :: model_id      = modelAlamel
        character(len=fname_len)                  :: output_prefix = 'alamel'
        character(len=fname_len)                  :: jobtitle      = 'alamel'
        character(len=fname_len)                  :: micros_fname  = 'micro1.smt'
        type(slipSystemData)                      :: slipsystem    !< slip systems file name
        type(outputConfig)                        :: output_config !< output file prefix, incremental output request flag, verbosity level
        type(textureData)                         :: texture
        type(simulData)                           :: simul_init
        type(Parameter), allocatable :: hardening_parameters(:)
    end type

    type :: altayStateData
        real(dp)                                :: eps = 0.D0
        integer                                         :: nSimulCalls = 0 !< Corresponds to NBLOC config data
        type(simulStepData),dimension(:),allocatable    :: simulCalls
        integer                                         :: this = 0        !< Iterator over simulCalls
    end type

    ! Definition of the singleton objects
    type(altayConfigData) :: acnf
    type(altayStateData)  :: astate


contains

    !> Configure the cnf object for using selected model type (FCTaylor, ALAMEL).
    subroutine setModelType(cnf,modelId,info)
        type(altayConfigData),intent(inout)       :: cnf
        integer,intent(in)                        :: modelId
        integer,intent(out)                       :: info

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
