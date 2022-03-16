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
module altayConfig
! Import configuration structures from AlTay modules
use altayHardTypes
use altayHardLaw_Simple, only: VoceConfig, SwiftKConfig, SwiftSConfig
use altayHardLaw_DSH, only: PAR
use altayTexFormatConstants

implicit none

      integer,parameter  :: fname_len = 512 !< Length of filenames

      !> \name Named constants for identifiers of the supported models
      !>@{ 
      integer,parameter :: modelFCTaylor = 1, modelAlamel = 2, modelMASAL = 3
      
      !>@}
      
      type :: slipSystemData
            !> Name of the file containing definitions of slipsystems
            character(len=fname_len)                  :: input_fname = ''
      end type

      type :: textureData
            !> Type of texture representation
            !>
            !> See altayTexFormatConstants for the list of possible values. \sa altayTexFormatConstants
            integer                                   :: input_type = TF_SMT     
            character(len=fname_len)                  :: input_fname = ''
            integer                                   :: block_id = 1
      end type

      
      type :: simulStepInputData ! no hardening data!  
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
            
            !> Selection of relaxations
            logical                                   :: rlx1 = .true., rlx2 = .true.
            
            !> Deformation gradient tensor to be imposed. (MB: this is rather a velocity gradient.)
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
            !> Macroscopic (imposed) effective von Mises strain till the end of the current step - total over the calls
            double precision                    :: effective_macro_strain_tot_end = 0.D0
       end type

      type :: simulStepData
            !> type that subsumes step input and output data
            !>
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
      !
      !> PEBP model parameters (no state variables)
      type :: PEBPConfig
            !> contains BP parameters, saturation and lower bounds for dislocation densities (defined in altayHardLawDSH.f90)
            type(PAR)                     :: params 
            
            !> Flag that decides if state variables should be read from file.
            logical                       :: read_state = .false.
            
            !> Name of file that contains state variables
            character(len=fname_len)      :: input_fname = ''
            
            !> Number of blocks to be skipped while reading the input file
            integer                       :: block_id = 0
            
      end type

      !> Parameters of available hardening models.
      type :: hardeningData
            !> Selector of the model for hardening of slipsystems. 
            !> 
            !> Acceptable values depend on availability of CRSS (aka TAUC) hardening models 
            !> that are implemented in the code.
            !> See module altayHard for details about available hardening laws.
            !> \sa crss_ratios 
            integer                 :: HardLawID = hard_None

            !> Initial values of CRSS ratios
            type(CRSS)              :: crss_ratios

            !> Parameters of Voce hardening law.
            type(VoceConfig)        :: VoceCnf
            
            !> Parameters of Swift hardening law ('engineering-type')
            type(SwiftKConfig)      :: SwiftKCnf
            
            !> Parameters of Swift hardening law ('scientific-type')
            type(SwiftSConfig)      :: SwiftSCnf

            !> Parameters of Dislocation Substructural Hardening models (PEBP variants)
            type(PEBPConfig)        :: PEBPCnf
            
      end type
      
      !> 
      type :: simulData
            
            !> Model selection. At the same time it controls number of grains in the cluster.
            !>
            !> Possible values are:
            !>   - 1 - FC Taylor
            !>   - 2 - Alamel
            !>   - 3 - MAS-AL
            integer                                   :: NGR = 2 ! Number of grains in the cluster
            
            !> It is relevant only in MAS-AL
            double precision                          :: ENTA = 1.D0
            
            double precision, dimension(3,3)          :: FMicro = reshape(       & 
                                                            [ 1.D0, 0.D0, 0.D0,  &
                                                              0.D0, 1.D0, 0.D0,  &
                                                              0.D0, 0.D0, 1.D0], &
                                                            [ 3, 3 ])
      end type

      !> Root-level configuration structure of Altay
      type :: altayConfigData
            !>
            integer                                   :: model_id      = modelAlamel
            character(len=fname_len)                  :: output_prefix = 'alamel'
            character(len=fname_len)                  :: jobtitle      = 'alamel'
            character(len=fname_len)                  :: micros_fname  = 'micro1.smt'
            type(slipSystemData)                      :: slipsystem !< slip systems file name
            type(outputConfig)                        :: output_config !< output file prefix, incremental output request flag, verbosity level
            type(hardeningData)                       :: hardening !< hardening model parameters
            type(textureData)                         :: texture
            type(simulData)                           :: simul_init
            ! 
      end type


      type :: altayStateData
            double precision                          :: eps = 0.D0

            integer                                   :: nSimulCalls = 0           !< Corresponds to NBLOC config data
            
            type(simulStepData),dimension(:),allocatable  :: simulCalls
            !
            ! Iterator over simulCalls
            integer                                   :: this = 0
      end type
      

      ! Definition of the singleton objects
       
      type(altayConfigData),save    :: acnf
      
      type(altayStateData),save     :: astate
      

contains

      !> Configure the stp object for using selected model type (FCTaylor, ALAMEL, MASAL).
      subroutine setStepType(stp,modelId,info)
      type(simulStepInputData),intent(inout)    :: stp
      integer,intent(in)                        :: modelId
      integer,intent(out)                       :: info
      !
            info = 0
            select case(modelId)
            case(modelFCTaylor)
                  stp%rlx1 = .false. 
                  stp%rlx2 = .false.
            case(modelAlamel,modelMASAL) 
                  stp%rlx1 = .true. 
                  stp%rlx2 = .true.
            case default
                  info = -1
            end select
      !
      end subroutine

      !> Configure the cnf object for using selected model type (FCTaylor, ALAMEL, MASAL).
      subroutine setModelType(cnf,modelId,info)
      type(altayConfigData),intent(inout)       :: cnf
      integer,intent(in)                        :: modelId
      integer,intent(out)                       :: info
      !
            select case(modelId)
            case(modelFCTaylor)
                  cnf%simul_init%ngr = 1
            case(modelAlamel)
                  cnf%simul_init%ngr = 2
            case(modelMASAL)
                  cnf%simul_init%ngr = 3
            case default
                  info = -1
                  return
            end select
            ! OK, supported model
            cnf%model_id = modelId
            info = 0
      !
      end subroutine
      
      !> Verify if integer value modelId represents any supported AlTay model (FCTaylor, ALAMEL, MASAL).
      pure logical function isValidModelType(modelId)
      implicit none
      integer,intent(in) :: modelId
      !
            isValidModelType = .false.
            select case(modelId)
            case(modelFCTaylor,modelAlamel,modelMASAL)
                  ! OK, supported model
                  isValidModelType = .true.
            end select
      end function
      
end module

