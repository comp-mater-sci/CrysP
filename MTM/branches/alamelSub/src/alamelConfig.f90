!
! $Id$
!

module alamelConfig
implicit none

      integer,parameter  :: fname_len = 512 !< Length of filenames

      type slipSystemData
            character(len=fname_len)                  :: input_fname   !< File containing definitions of slipsystems
            integer                                   :: ntau          !< Number of tau-crit values
            double precision,dimension(:),allocatable :: taucrit    
            integer                                   :: nsym =  0     !< (SIMUL) NSYM If =0: TAUC are set to 1; if=1: values from FK1 used.
      end type

      type textureData
            !> Type of texture representaion: 1 - SMT, 2 - CUR, 3 - CUB
            integer                                   :: input_type = 1     
            character(len=fname_len)                  :: input_fname = ''
            integer                                   :: block = 1
      end type

      type simulStepData
            !> Flag that decides if this step leads to modification of the texture.
            logical                                   :: keep_texture = .true. 
            integer                                   :: do_output          !< Request for output
            integer                                   :: nsteps = 1         !< Number of steps per call
            integer                                   :: rlx1 = 1, rlx2 = 1 !< Selection of relaxations
            double precision, dimension(3,3)          :: dgf  = 0.D0        !< Deformation gradient tensor
      end type
    
    
      type outputConfig
            integer                                   :: nlist = 1    !< (SIMUL) NLIST (Make an output listing 0 or 1)
            integer                                   :: nfile = 1    !< (SIMUL) NFILE (Make output files 0 or 1)
            integer                                   :: nfiltw = 0   !< (SIMUL) NFILTW (Make output files 0 or 1)
            integer                                   :: nten =  1    !< (SIMUL) NTEN (Print distortion tensor 0 or 1)
            integer                                   :: iglij = 0    !< (SIMUL) IGLIJ  0 or 1 (a print switch. Only for very short runs!)
            integer                                   :: ipr = 0      !< (SIMUL) IPR  0-3 Print switch. All except Van Houtte must use 0
      end type
    
      type hardeningData
            !> Parameters of Voce hardening law
            double precision                          :: TIII1 = 0.0, TIIIS = 0.0 ,TIVS =0.0, THIII1 = 0.0 , THT = 0.0
      end type

      type simulData
            double precision                          :: ETAFAK = 0.D0
            double precision                          :: ATTENF = 0.D0
            double precision, dimension(3,3)          :: FMicro = reshape(       & 
                                                            [ 1.D0, 0.D0, 0.D0,  &
                                                              0.D0, 1.D0, 0.D0,  &
                                                              0.D0, 0.D0, 1.D0], &
                                                            [ 3, 3 ])
      end type

      type alamelConfigData
            character(len=fname_len)                  :: output_prefix = 'alamel'
            character(len=fname_len)                  :: jobtitle      = 'alamel'
            character(len=fname_len)                  :: micros_fname  = 'micro1.smt'
            type(slipSystemData)                      :: slipsystem
            type(outputConfig)                        :: output_config
            type(hardeningData)                       :: hardening_data
            type(textureData)                         :: texture
            type(simulData)                           :: simul_init
            ! 
            integer                                   :: nSimulCalls = 0           !< Corresponds to NBLOC config data
            type(simulStepData),dimension(:),allocatable  :: simulCalls
            !
            type(simulStepData)                       :: initSimulCall = simulStepData(.false., 0, 1, 0, 0, &
                                                            reshape([ 2.5D-2, 0.D0, 0.D0,  &
                                                                      0.D0, 2.5D-2, 0.D0,  &
                                                                      0.D0, 0.D0, 2.5D-2], &
                                                                    [ 3, 3 ]))
            ! Iterator over stepData
            integer                                   :: current_call = 0                                                        
      end type


contains

      !> Initialization of alamel configuration.
      !>
      !> This subroutine must be called prior to any modifications in 
      !> alamelConfigData object.
      subroutine initConfig(acnf,ntau,ncalls)
      implicit none
      type(alamelConfigData),intent(out)  :: acnf
      !
      integer,intent(in)      :: ntau
      integer,intent(in)      :: ncalls
      !
      acnf%slipsystem%ntau = ntau
      if ((ntau > 0) ) then
            allocate(acnf%slipsystem%taucrit(ntau))
            ! Fill taucrit array with 1.D0
            acnf%slipsystem%taucrit = 1.D0
            acnf%slipsystem%nsym = 1
      else
            acnf%slipsystem%nsym = 0
      endif
      !
      if (ncalls > 0) then
            acnf%nSimulCalls = ncalls      
            allocate(acnf%simulCalls(acnf%nSimulCalls))
      endif      
      !  
      end subroutine

end module

