!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of first release (under name of alamelConfig): 2010-10-17
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!>    \file altayConfig.f90 Provides configuration data for AlTay
!>    
!

!> \remark The module is derived from alamelConfig, taken from alamelSub project.
!> However, the differences in the API are drastic. For this reason, the API is 
!> intentionally made even more incompatibile (e.g. changes in the names of datastructures)
!> to force the users of alamelSub to make a deliberate, conscious and well-thought decision of 
!> upgrading their code to the altaySub.


!> Basic configuration of AlTay in a form of formalized data structures.
module altayConfig
! Import configuration structures from AlTay modules
use hardVoce, only: VoceConfig
use KOST1x, only: PAR11

implicit none

      integer,parameter  :: fname_len = 512 !< Length of filenames

      !> \name Named constants for identifiers of the supported models
      !>@{ 
      integer,parameter :: modelFCTaylor = 1, modelAlamel = 2, modelMASAL = 3
      
      !>@}
      
      type :: slipSystemData
            
            !> Name of the file containing definitions of slipsystems
            character(len=fname_len)                  :: input_fname = ''

            !> Selector of the model for hardening of slipsystems. 
            !> 
            !> Acceptable values depend on availability of CRSS (aka TAUC) hardening models 
            !> that are implemented in the code:
            !>   - kost == 0: TAUC are set to 1.0, no hardening of slip systems.
            !>   - kost == 1: values from FK1 used, isotropic Voce equation is used for hardening. 
            !>     \sa crss_ratios 
            integer                                   :: kost =  0

            !> Array of CRSS ratios. 
            !>
            !> The data layout must correspond to FK1.
            double precision,dimension(2,96)          :: crss_ratios = 1.D0
            
      end type

      type :: textureData
            !> Type of texture representaion: 1 - SMT, 2 - CUR, 3 - CUB
            integer                                   :: input_type = 1     
            character(len=fname_len)                  :: input_fname = ''
            integer                                   :: block_id = 1
      end type

      
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
            
            !> Selection of relaxations
            logical                                   :: rlx1 = .true., rlx2 = .true.
            
            !> Deformation gradient tensor to be imposed.
            double precision,dimension(3,3)           :: dgf  = 0.D0 
            
      end type

      
      type :: simulStepOutputData
            !> Macroscopic (homogenized) stress
            double precision,dimension(3,3)     :: stress_tensor = 0.D0
            !> Macroscopic (homogenized) Taylor factor
            double precision                    :: taylor_factor = 0.D0
            !> Macroscopic average stress
            double precision                    :: average_stress = 0.D0
            !> Macroscopic (homogenized) effective von Mises stress
            double precision                    :: effective_stress = 0.D0
            !> Macroscopic (homogenized) effective von Mises strain
            double precision                    :: effective_strain = 0.D0
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
            !> (SIMUL) NPEBP (Make state variable file for BP (KOST1x) model)
            integer                                   :: npebp = 0
            !> (SIMUL) NMSS (output of macroscopic homogenized strain-stress)
            integer                                   :: nmss = 0
            logical                                   :: use_curfile = .false.
            logical                                   :: use_cubfile = .false.
      end type


      !> Parameters of available hardening models.
      type :: hardeningData
            
            !> Parameters of Voce hardening law.
            type(VoceConfig)        :: VoceCnf

            !> Parameters of PEBP models (KOST1x)
            type(PAR11)             :: PEBPCnf
            
      end type
      
      !> 
      type :: simulData
            
            !> Model selection. At the same time it controls number of grains in the cluster.
            !>
            !> Possible values are:
            !>   - 1 - FC Taylor
            !>   - 2 - Alamel
            !>   - 3 - MAS-AL
            integer                                   :: NGR = 2
            
            !> It is relevant only in MAS-AL
            double precision                          :: ENTA = 1.D0
            
            double precision, dimension(3,3)          :: FMicro = reshape(       & 
                                                            [ 1.D0, 0.D0, 0.D0,  &
                                                              0.D0, 1.D0, 0.D0,  &
                                                              0.D0, 0.D0, 1.D0], &
                                                            [ 3, 3 ])
      end type

      !> Root-level configuration structure.
      type :: altayConfigData
            !>
            integer                                   :: model_id      = modelAlamel
            character(len=fname_len)                  :: output_prefix = 'alamel'
            character(len=fname_len)                  :: jobtitle      = 'alamel'
            character(len=fname_len)                  :: micros_fname  = 'micro1.smt'
            type(slipSystemData)                      :: slipsystem
            type(outputConfig)                        :: output_config
            type(hardeningData)                       :: hardening
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
end module

