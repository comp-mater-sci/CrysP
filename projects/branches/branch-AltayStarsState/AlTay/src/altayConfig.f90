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
!>    \file altayConfig.f90 Provides configuration data for AlTay
!>    
!

!> \remark The module is derived from the module alamelConfig, taken from the alamelSub project.
!> However, the differences in the API are drastic. For this reason, the API is 
!> intentionally made even more incompatibile (e.g. changes in the names of datastructures)
!> to force the users of the alamelSub to make a deliberate, conscious and well-thought decision of 
!> upgrading their code to the altaySub.


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
use criMathUtils
use criPath

implicit none

    integer,parameter :: CNF_ValueNone = 0

    integer,parameter :: CNF_jobtitle_maxlen = 256
    
    !> \name Named constants for identifiers of the supported models
    !>@{ 
    integer,parameter :: CNF_modelNone = 0
    integer,parameter :: CNF_modelFCTaylor = 1    !< Full Constraint Taylor
    integer,parameter :: CNF_modelAlamel = 2      !< ALAMEL (single phase)
    integer,parameter :: CNF_modelAlamelMP = 3    !< ALAMEL (multi-phase)
    !>@}
    
    
    !> \name Named constants for identifiers of the supported models
    !>@{ 
    integer,parameter :: CNF_UnknownDataPersistency = -1
    integer,parameter :: CNF_DataPersistencyNone = 0
    integer,parameter :: CFN_NoDataPersistency = 1
    integer,parameter :: CFN_NativeDataPersistency = 2
    integer,parameter :: CFN_HDF5DataPersistency = 3
    !>@}


    !> Configuration of deformmation mechanisms
    type :: DeformationMechanismConfig
        !> Identifier for source type of deformation mechanism
        !> See altayDeformationMechanism for the list of possible values. \sa altayDeformationMechanism
        integer                                     :: id = DM_none

        !> Name of the file/preconfiguration containing definition of deformation mechanism
        !> Relevant only if id sets a file-based source of deformation mechanism data.
        character(len=max_pathlen)                  :: input_fname = ''
    end type



    !> Configuration of texture data exchange
    !> \todo Update the names of the fields - they are not exclusively related to input files.
    type :: TextureConfig
        !> Type of texture representation
        !>
        !> See altayTexFormatConstants for the list of possible values. \sa altayTexFormatConstants
        integer                                   :: input_type = TF_SMT
        character(len=max_pathlen)                :: input_fname = ''
        integer                                   :: block_id = 1
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
        
        type(DeformationMechanismConfig)    :: deformation_mechanism

        type(HardeningConfig)               :: hardening

        type(MesostructureConfig)           :: intraphase_interfaces
        
    end type


    
    type :: MaterialConfig
        
        type(PhaseConfig),dimension(:),allocatable          :: phases
        
        type(MesostructureConfig),dimension(:),allocatable  :: interphase_interfaces
        
    end type
         

    integer,parameter :: CNF_step_name_maxlen = 32

    type :: AssemblyStepConfig
        
        
    end type
    
    
    type :: InitializationStepConfig
        
        integer :: data_persistency_scheme = CNF_DataPersistencyNone
        
        character(len=max_pathlen)      :: file_path = ''
        
        
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
    
    
    
    type :: StepConfig
        
        character(len=CNF_step_name_maxlen)         :: name = 'Step'
        
        type(AssemblyStepConfig),allocatable        :: assembly_step

        type(InitializationStepConfig),allocatable  :: initialization_step
        
        type(AnalysisStepConfig),allocatable        :: analysis_step
        
        type(OutputStepConfig),allocatable          :: output_step
        
    end type
    

    type :: SimulationConfig
        
        character(len=CNF_jobtitle_maxlen)              :: jobtitle = 'default'
        
        type(MaterialConfig)                            :: materials
        
        type(StepConfig),dimension(:),allocatable       :: steps
        
    end type

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
      


contains


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
      
end module

