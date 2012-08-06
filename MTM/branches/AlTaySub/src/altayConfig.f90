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
!> intentionally made even more incompatibile (e.g. changes in name of datastructures)
!> to force the users of alamelSub to make a deliberate, conscious and well-thought decision of 
!> upgrading their code to the altaySub.


!> Basic configuration of AlTay in a form of formalized data structures.
module altayConfig
implicit none

      integer,parameter  :: fname_len = 512 !< Length of filenames

      !>@{ \name Named constants for identifiers of the supported models
      integer,parameter :: modelFCTaylor = 1, modelAlamel = 2, modelMASAL = 3
      
      !>@}
      
      type :: slipSystemData
            character(len=fname_len)                  :: input_fname = '' !< File containing definitions of slipsystems
            double precision,dimension(2,96)          :: taucrit = 1.D0   !< CRSS - the data layout must correspond to FK1
            integer                                   :: kost =  0        !< (SIMUL) KOST If =0: TAUC are set to 1; if=1: values from FK1 used.
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
            !> Flag that decides if the full model is to be employed.
            !>   If set .false.: 1) a simplified formula is used for calculations of the microscopic stress
            !>                   2) texture is NOT updated, so "keep_texture" must be set, too.
            logical                                   :: full_model = .true.
            
            integer                                   :: do_output = 0      !< Request for output
            integer                                   :: nsteps = 1         !< Number of steps per call
            integer                                   :: rlx1 = 1, rlx2 = 1 !< Selection of relaxations
            double precision,dimension(3,3)           :: dgf  = 0.D0        !< Deformation gradient tensor
      end type

      
      type :: simulStepOutputData
            !> Macroscopic (homogenized) stress
            double precision,dimension(3,3)     :: stress_tensor = 0.D0
            !> Macroscopic (homogenized) Taylor factor
            double precision                    :: taylor_factor = 0.D0
            !> Macroscopic average stress
            double precision                    :: average_stress = 0.D0
            double precision                    :: effective_strain = 0.D0
      end type

      type :: simulStepData
            type(simulStepInputData)            :: input      
            type(simulStepOutputData)           :: output
      end type
      
      
      type :: outputConfig
            integer                                   :: nlist = 0    !< (SIMUL) NLIST (Make an output listing 0 or 1)
            integer                                   :: nfile = 0    !< (SIMUL) NFILE (Make output files 0 or 1) (CUR output)
            integer                                   :: nfiltw = 0   !< (SIMUL) NFILTW (Make output files 0 or 1)
            integer                                   :: ipr = 0      !< (SIMUL) IPR  0-3 Print switch. All except Van Houtte must use 0
            integer                                   :: nres = 0     !< (SIMUL) NRES (Make output for 
            logical                                   :: use_curfile = .false.
            logical                                   :: use_cubfile = .false.
      end type
    
      type :: hardeningData
            !> Parameters of Voce hardening law. Some 'reasonable' defaults are used.
            double precision                          :: TIII1  = 1.486     
            double precision                          :: TIIIS  = 2.476     
            double precision                          :: TIVS   = 8.357 
            double precision                          :: THIII1 = 2.75      
            double precision                          :: THT    = 0.55
      end type

      type :: simulData
            
            integer                                   :: NGR = 2
            
            double precision                          :: ENTA = 1.D0
            
            double precision, dimension(3,3)          :: FMicro = reshape(       & 
                                                            [ 1.D0, 0.D0, 0.D0,  &
                                                              0.D0, 1.D0, 0.D0,  &
                                                              0.D0, 0.D0, 1.D0], &
                                                            [ 3, 3 ])
      end type

      type :: altayConfigData
            
            integer                                   :: model_selection = modelAlamel
            
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
      

      ! Definition of singleton objects
       
      type(altayConfigData),save	:: acnf
      
      type(altayStateData),save     :: astate
      

contains

      !> Initialization of alamel configuration.
      !>
      !> This subroutine must be called prior to any modifications in 
      !> alamelConfigData object.
      subroutine initConfig(cnf,info)
      implicit none
      type(altayConfigData),intent(inout)  :: cnf
      !
      integer,intent(out)     :: info
      !
            info = 0
      !  
      end subroutine

      
      
      !> Initialization of datastructures for storing inputs and outputs for a set of calls to the model
      subroutine initCallStructures(ncalls,state,info)
      implicit none
      integer,intent(in)                        :: ncalls
      type(altayStateData),intent(inout)        :: state
      integer,intent(out)                       :: info
      !
            info = 1
      !
      end subroutine


      subroutine setStepType(stp,modelId,info)
      type(simulStepInputData),intent(inout)    :: stp
      integer,intent(in)                        :: modelId
      integer,intent(out)                       :: info
      !
            info = 0
            select case(modelId)
            case(modelFCTaylor)
                  stp%rlx1 = 0 
                  stp%rlx2 = 0
            case(modelAlamel,modelMASAL) 
                  stp%rlx1 = 1 
                  stp%rlx2 = 1
            case default
                  info = -1
            end select
      !
      end subroutine
end module

