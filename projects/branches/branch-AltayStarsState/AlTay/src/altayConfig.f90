!
! $Id$
!

!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of the initial release (under name of alamelConfig): 2010-10-17
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!


!> Basic configuration of AlTay in a form of formalized data structures.
!>
!>
!> Named constants defined in this module are prefixed with `CNF_`
!> \remark The data types defined in altayConfig deliberately do not make use
!>         of inheritance. This is set by the constraint on the content
!>         of coarrays (CAF) that are not permitted to contain polymorphic 
!>         objects.
module altayConfig
! Import configuration structures from AlTay modules
use altayHardConstants
use altayCRSSTypes
use altayHardLaw_Simple, only: VoceConfig, SwiftKConfig, SwiftSConfig
#ifdef PEBP_ENABLED
use altayHardLaw_DSH, only: PAR
#endif
use altayHardLaw_KM
use altayTexFormatConstants
use altayDeformationMechanismConstants
use altayStatePersistenceConstants
use criMathUtils
use criPath

implicit none

    integer,parameter :: CNF_ValueNone = 0

    integer,parameter :: CNF_jobtitle_maxlen = 256
    
    integer,parameter :: CNF_stepname_maxlen = 32
    
    integer,parameter :: CNF_phasename_maxlen = 32
    
    !> \name Named constants for identifiers of the supported models
    !>@{ 
    integer,parameter :: CNF_modelNone = 0
    integer,parameter :: CNF_modelFCTaylor = 1    !< Full Constraint Taylor
    integer,parameter :: CNF_modelAlamel = 2      !< ALAMEL (single phase)
    integer,parameter :: CNF_modelAlamelMP = 3    !< ALAMEL (multi-phase)
    !>@}
    
    integer,parameter,private :: nsupported_models = 3
    integer,parameter,dimension(nsupported_models) :: CNF_supported_models = [ &
                CNF_modelFCTaylor, CNF_modelAlamel, CNF_modelAlamelMP ]
    
    !> \name Named constants/identifiers of the supported state persistency schemes
    !>@{ 
    integer,parameter :: CNF_StatePersistencyUnknown = -1
    integer,parameter :: CNF_StatePersistencyNone = 0
    integer,parameter :: CNF_StatePersistencyNative = 1
    integer,parameter :: CNF_StatePersistencyHDF5 = 2
    !>@}

    !> Max. length of state file. This is large enough to permit extended names
    !> such as file_path:object_path, with both file_path and object_path of 
    !> length `max_pathlen`.
    integer,parameter,private :: state_file_max_pathlen = 2 * max_pathlen + 1

    !> Configuration of texture data exchange
    !> \fixme Update the names of the fields - they are not exclusively related to input files.
    type :: TextureConfig
        !> Type of texture representation
        !>
        !> See altayTexFormatConstants for the list of possible values. \sa altayTexFormatConstants
        integer                                   :: format_id = TF_NONE

        character(len=max_pathlen)                :: file_name = ''
        integer                                   :: block_id = 0
    end type

    
    type :: PhasePersistenceConfig
        
        !> Refined description of texture ODF persisten data. One may use this
        !> to override the default settings from state persistence scheme.
        type(TextureConfig)     :: odf
        
        ! Hardening-related parts below
    end type


    !> Configuration of state persistence
    type :: StatePersistenceConfig
        
        integer                                 :: scheme_id = CNF_StatePersistencyNone
        
        integer                                 :: access_mode = StatePersistence_Read
        !> Path to the location of state persistency components.
        !>
        !> All paths to state persistence components are relavive to this path.
        !> If HDF5 persistency scheme is used, the path typically points to the
        !> HDF5 .h5 file.
        character(len=state_file_max_pathlen)   :: path = ''
        
        !> Per-phase configuration of 
        type(PhasePersistenceConfig),dimension(:),allocatable :: phases
        
    end type

    interface StatePersistenceConfig
        module procedure StatePersistenceConfig_init
    end interface


    !> Configuration of deformmation mechanisms
    type :: DeformationMechanismConfig
        !> Identifier for source type of deformation mechanism
        !> See altayDeformationMechanism for the list of possible values. 
        !> \sa altayDeformationMechanism
        integer                                     :: id = DM_none

        !> Name of the file/preconfiguration containing definition of deformation mechanism
        !> Relevant only if id sets a file-based source of deformation mechanism data.
        character(len=max_pathlen)                  :: input_fname = ''
    end type


    !> Configuration of state variables of the hardening model. It is 
    !> relevant only for stateful hardening laws.
    !> \todo To be eliminated in the future.
    type :: HardeningStateConfig
          
        !> Flag that decides if state variables should be read from file.
        logical                       :: read_state = .false.
            
        !> Name of file that contains state variables
        character(len=max_pathlen)      :: input_fname = ''
            
        !> Number of blocks to be skipped while reading the input file
        integer                       :: block_id = 0
          
    end type
    
    !> Parameters of available hardening models.
    type :: HardeningConfig
        !> Selector of the model for hardening of deformation systems. 
        !> 
        !> Acceptable values depend on availability of CRSS (aka TAUC) hardening models 
        !> that are implemented in the code.
        !> See module altayHard for details about available hardening laws.
        !> \sa crss_ratios 
        integer                 :: hardLawID = hard_None

        type(HardeningStateConfig)  :: stateCnf
            
        !> Initial values of CRSS ratios
        type(CRSSData)          :: crss_ratios

        !> Parameters of Voce hardening law.
        type(VoceConfig)        :: VoceCnf
            
        !> Parameters of Swift hardening law ('engineering-type')
        type(SwiftKConfig)      :: SwiftKCnf
            
        !> Parameters of Swift hardening law ('scientific-type')
        type(SwiftSConfig)      :: SwiftSCnf
#ifdef PEBP_ENABLED
        !> Parameters of Dislocation Substructural Hardening models (PEBP variants)
        type(PEBPConfig)        :: PEBPCnf
#endif PEBP_ENABLED
        !> Parameters of Kocks-Mecking law
        type(KMConfig)          :: KMCnf
    end type
    
    type :: MesostructureConfig
        
        double precision, dimension(3,3)    :: FMicro = unit_sr_matrix
        
        character(len=max_pathlen)          :: file_path = ''
        
    end type
    
    type :: PhaseConfig
        
        character(len=CNF_phasename_maxlen) :: name
        
        type(DeformationMechanismConfig)    :: deformation_mechanism

        type(HardeningConfig)               :: hardening

        type(MesostructureConfig)           :: intraphase_interfaces
        
    end type


    
    type :: MaterialConfig
        
        type(PhaseConfig),dimension(:),allocatable          :: phases
        
        type(MesostructureConfig),dimension(:),allocatable  :: interphase_interfaces
        
    end type
         
    interface MaterialConfig
        module procedure MaterialConfig_init_nphases
    end interface
    

    type :: AssemblyStepConfig
        
        
    end type
    
    
    type :: InitializationStepConfig
        
        type(StatePersistenceConfig)        :: input
        
    end type
    
    
    type :: AnalysisStepConfig
        
        integer :: model_type = CNF_modelAlamel
        
        logical :: advance_state = .true.
        
        logical :: full_model = .true.
        
        double precision,dimension(sr_tensor_dim,sr_tensor_dim) :: input = 0.D0
        
        !> Number of increments
        integer                                   :: nincrements = 1
        
    end type
    
    
    type :: OutputStepConfig
        
        logical                     :: store_state = .true.
        
        logical                     :: use_output_filters = .true.
        
        character(len=max_pathlen)  :: file_path = ''
        
    end type
    
    
    !> Aggregate of Altay step types. 
    !>
    !> Only one member can have the allocated status.
    type :: StepConfig
        
        character(len=CNF_stepname_maxlen)          :: name = 'Step'
        
        logical                                     :: incremental_name = .true. 
        
        type(AssemblyStepConfig),allocatable        :: assembly_step

        type(InitializationStepConfig),allocatable  :: initialization_step
        
        type(AnalysisStepConfig),allocatable        :: analysis_step
        
        type(OutputStepConfig),allocatable          :: output_step
        
    end type
    

    type :: SimulationConfig
        
        character(len=CNF_jobtitle_maxlen)              :: jobtitle = 'default'
        
        type(MaterialConfig)                            :: material
        
        !> Description of the steps.
        !>
        !> This part of configuration can be deferred and provided at later stage.
        !> In such case the field must retain unallocated state.
        type(StepConfig),dimension(:),allocatable       :: steps
        
        type(StatePersistenceConfig)                    :: state_output
    end type

    
    !> Generic name of procedures that verify if a component of configuration
    !> appears valid and internally consistent. Return .true. on success and 
    !> .false. otherwise.
    interface check
        module procedure :: StepConfig_check
    end interface

    
    !
    ! OLD STYLE CONFIG. To be phased out.
    !
    
      
      type :: simulStepInputData
            !> Flag that decides if this step leads to modification of the texture.
            logical                                   :: keep_texture = .true.
            
            !> Flag that decides if this step leads to an update of the state components
            !> (other than texture)
            logical                                   :: keep_state = .true.
            
            !> Flag that decides if the full model is to be employed.
            !>   If set .false.: 1) a simplified formula is used for calculations of the microscopic stress
            !>                   2) texture is NOT updated, so "keep_texture" must be set, too.
            logical                                   :: full_model = .true.
            
            !> Flag that decides if the initial texture should be written out as a CUR output of the step.
            !>
            !> \note The texture is actually written out for initial configuration that is available
            !> at the beginning of the step.
            !> \remark This flag takes effect if outputConfig::nfile is non-zero. \sa outputConfig::nfile
            logical                                   :: do_output_init = .false.

            !> Flag that decides if the final texture (as it is at the end of the call) should be written out 
            !> as a a CUR output of the step.
            !>
            !> \remark This flag takes effect if outputConfig::nfile is non-zero. \sa outputConfig::nfile
            logical                                   :: do_output_final = .false.

            
            !> Number of steps per call
            integer                                   :: nsteps = 1
            
            
            !> Deformation gradient tensor to be imposed.
            double precision,dimension(3,3)           :: dgf  = 0.D0 
            
      end type

      
      type :: simulStepOutputData
            !> Macroscopic (homogenized) stress
            double precision,dimension(3,3)     :: stress_tensor = 0.D0
            !> Macroscopic (homogenized) Taylor factor
            double precision                    :: taylor_factor = 0.D0
            !> Strain Rate Heterogeneity in polycrystal. Non-zero only for models that consider clusters of grains:
            !> \f$ \kappa = (||d-D||) / ||D|| \f$
            double precision                    :: strain_rate_heterogeneity = 0.D0
            !> Macroscopic stress, defined as the work conjugate to D_vM: 
            !> \f$ \sigma_{eq} = (\mathbf{S} \cdot \mathbf{D}) / D_{vM} \f$
            double precision                    :: equivalent_stress = 0.D0
            !> Macroscopic (homogenized) effective von Mises stress
            double precision                    :: effective_stress = 0.D0
            !> Macroscopic (homogenized) plastic slip
            double precision                    :: homogenised_slip = 0.D0
            !> Macroscopic (homogenized) plastic slip - total over the calls
            double precision                    :: homogenised_slip_tot = 0.D0
            !> Macroscopic (imposed) effective von Mises strain - total over the steps
            double precision                    :: effective_macro_strain = 0.D0
            !> Macroscopic (imposed) effective von Mises strain - total over the calls
            double precision                    :: effective_macro_strain_tot = 0.D0
       end type

      type :: simulStepData
            type(simulStepInputData)            :: input      
            type(simulStepOutputData)           :: output
      end type
      
      
      type :: outputConfig
            !> (SIMUL) NLIST (Make an output listing 0 or 1)
            integer                                   :: nlist = 0
            !> (SIMUL) NFILE (Make output files 0 or 1) (CUR output)
            integer                                   :: nfile = 0
            !> (SIMUL) NFILTW (Make output files 0 or 1)
            integer                                   :: nfiltw = 0
            !> (SIMUL) IPR  0-3 Print switch
            integer                                   :: ipr = 0
            !> (SIMUL) NRES (Make output for stresses with per-grain resolution)
            integer                                   :: nres = 0
            !> (SIMUL) NPEBP (Make state variable file for DSH model)
            integer                                   :: npebp = 0
            !> (SIMUL) NMSS (output of macroscopic homogenized strain-stress)
            integer                                   :: nmss = 0
            logical                                   :: use_curfile = .false.
            logical                                   :: use_cubfile = .false.
      end type


      
#ifdef PEBP_ENABLED
      type :: PEBPConfig
            type(PAR)                   :: params
      end type
#endif
      

      
      !> 
      type :: simulData
            
            !> Model selection. At the same time it controls number of grains in the cluster.
            !>
            !> Possible values are:
            !>   - 1 - FC Taylor
            !>   - 2 - Alamel
            !>   - 3 - MAS-AL
            integer                                   :: NGR = 2
            
            double precision, dimension(3,3)          :: FMicro = unit_sr_matrix
      end type

      !> Root-level configuration structure.
      type :: altayConfigData
            !>
            integer                                   :: model_id      = CNF_modelAlamel
            character(len=max_pathlen)                  :: output_prefix = 'alamel'
            character(len=max_pathlen)                  :: jobtitle      = 'alamel'
            character(len=max_pathlen)                  :: micros_fname  = 'micro1.smt'
            type(DeformationMechanismConfig)          :: deformationmechanism
            type(outputConfig)                        :: output_config
            type(HardeningConfig)                       :: hardening
            type(TextureConfig)                         :: texture
            type(simulData)                           :: simul_init
            ! 
      end type

      ! Former altayStateData
      type :: altayOutputData
            double precision                          :: eps = 0.D0

            integer                                   :: nSimulCalls = 0           !< Corresponds to NBLOC config data
            
            type(simulStepData),dimension(:),allocatable  :: simulCalls
            !
            ! Iterator over simulCalls
            integer                                   :: this = 0
      end type
      
      
#ifdef ENABLE_EXPERIMENTAL
    !> Basic description of phase
    type :: PhaseBasicConfig
          character(len=CNF_phasename_maxlen)   :: name = ''
          integer                               :: deformation_mechanism_id = DM_none
          character(len=max_pathlen)            :: odf_input_name = ''
          character(len=max_pathlen)            :: odf_output_name = ''
          character(len=max_pathlen)            :: interfaces_input_name = ''
    end type
#endif
      
contains

#ifdef ENABLE_EXPERIMENTAL
    function SimulationConfig_basic(jobtitle, phases, input_scheme, output_scheme, info) result(this)
    implicit none
    type(SimulationConfig)                          :: this
    character(len=*),intent(in)                     :: jobtitle
    type(PhaseBasicConfig),dimension(:),intent(in)  :: phases
    integer,intent(in)                              :: input_scheme
    integer,intent(in)                              :: output_scheme
    integer,intent(out)                             :: info
    !
    integer :: i
    !
        this%jobtitle = jobtitle
        this%material = MaterialConfig(size(phases))
        do i = 1, size(phases)
            associate(p => this%material%phases(i), c => phases(i))
                p%name = c%name
                p%deformation_mechanism%id = c%deformation_mechanism_id
                p%odf_input%file_name = c%odf_input_name
                p%odf_output%file_name = c%odf_output_name
                p%intraphase_interfaces%file_path = c%interfaces_input_name
            end associate
        enddo
        info = criSuccess
    !
    end function
#endif

    pure function StatePersistenceConfig_init(scheme_id, access_mode, path, n_phases) result(this)
    implicit none
    integer,intent(in),optional             :: scheme_id
    integer,intent(in),optional             :: access_mode
    character(len=*),intent(in),optional    :: path
    integer,intent(in)                      :: n_phases !< Number of phases. It must be >= 1
    type(StatePersistenceConfig)            :: this
    !
        if (present(scheme_id)) this%scheme_id = scheme_id
        if (present(access_mode)) this%access_mode = access_mode
        if (present(path)) this%path = path
        if (n_phases >= 1) allocate(this%phases(n_phases))
    !
    end function



    !> Allocate components for n phases. 
    pure function MaterialConfig_init_nphases(n_phases) result(res)
    integer,intent(in)      :: n_phases !< Number of phases. It must be >= 1
    type(MaterialConfig)  :: res
    integer :: ierr
    !
        if (n_phases < 1) return
        allocate(res%phases(n_phases),stat=ierr)
        allocate(res%interphase_interfaces(n_phases-1), stat=ierr)
    !
    end function

    !> Verify if the StepConfig fulfills the pre-requisities.
    logical function StepConfig_check(this) result(is_ok)
    implicit none
    class(StepConfig),intent(in)     :: this
    !
    integer,parameter :: nfields = 4 !< number of fields of interest in the object
    logical,dimension(nfields) :: is_allocated
    !
        is_allocated = [allocated(this%assembly_step), &
                        allocated(this%initialization_step), &
                        allocated(this%analysis_step), &
                        allocated(this%output_step)]
        ! Only one allocatable allowed
        is_ok = sum(merge(1, 0, is_allocated)) == 1
    !
    end function
    
    
    
    
    !
    ! OLD STYLE CONFIG. To be phased out.
    !

      !> Configure the cnf object for using selected model type.
      subroutine setModelType(cnf,modelId,info)
      type(altayConfigData),intent(inout)       :: cnf
      integer,intent(in)                        :: modelId
      integer,intent(out)                       :: info
      !
            select case(modelId)
            case(CNF_modelFCTaylor)
                  cnf%simul_init%ngr = 1
            case(CNF_modelAlamel)
                  cnf%simul_init%ngr = 2
            case default
                  info = -1
                  return
            end select
            ! OK, supported model
            cnf%model_id = modelId
            info = 0
      !
      end subroutine
      
      !> Verify if integer value modelId represents any supported AlTay model.
      pure logical function isValidModelType(modelId)
      implicit none
      integer,intent(in) :: modelId
      !
            isValidModelType = .false.
            select case(modelId)
            case(CNF_modelFCTaylor,CNF_modelAlamel)
                  ! OK, supported model
                  isValidModelType = .true.
            end select
      end function
      
      
#ifdef USE_ALTAYSIMUL
    !> Very imperfect translator of new config type into old config type.
    function translateConfig(config,io_enabled) result(old)
    use criAlgorithm
    implicit none
    type(SimulationConfig),intent(in) :: config
    logical,intent(in),optional        :: io_enabled !<Write out results to external files (default: true)
    type(altayConfigData) :: old
    !
    integer :: info, i
    !
        old%jobtitle = config%jobtitle
        old%hardening = config%material%phases(1)%hardening
        ! Nasty trick: we take the very first analysis step
        ! and take data from there
        do i = 0, ubound(config%steps,dim=1)
            if (allocated(config%steps(i)%analysis_step)) then
                call setModelType(old,config%steps(i)%analysis_step%model_type, info)
                exit
            endif
        enddo
        !
        if (optionalDefault(io_enabled, .true.)) then
            old%output_config%NLIST = 1
            old%output_config%NFILE = 1
            old%output_config%NRES = 1
        endif
    !
    end function
#endif

end module

