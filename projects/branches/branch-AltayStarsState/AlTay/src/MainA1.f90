! MODIFICATIONS AUG 2010
! THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
! WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY
!
      PROGRAM MAINA1
      use altayConfig
      use altayState
      use altayStateTypes
      use altayMaterial
      use altayMiscutils
      use altayIOConfig
#ifdef PEBP_ENABLED
      use altayDSHstate
#endif
      use altayMesostructure
      use altaySimul
      use altayCRSSTypes
      use altayHardTypes
      use altayHard
      use altayTexFormats
      use altayMacroKinematic
      use altayStatePersistenceUtils
      implicit none ! double precision (a-h,o-z)
!      Several simulations (usually several-steps each),
!      following each other.
!
!      There is a "last block with Current situation"
!      (without simulation, only for output to the CUR-file).
!
!     LEC= data set with slip systems
!     KLEC= data set with parameters
!     IMP= printer
!     IMP1=output-file with successive "current situations"
!     IMP2=output-file with successive "responses to imposed strain"
!     IDISK1= work file (obsolete, not used)
!     NDAT1= Input-texture file
!
      !
      common /CEIGEN/ JBLOC
      integer :: JBLOC
      double precision, dimension(3,3) :: DG
      character(len=pathlength) :: fnam1,fname_prefix
      integer :: info
      integer :: I,J,K,L
      integer :: NLINES, ISIGN, NBLOC,  NFILE0

#ifdef PEBP_ENABLED
      character(len=pathlength) :: fname_pebp
      logical :: read_state
      integer :: nblock
#endif
#ifdef FINALCUB_ENABLED
      integer :: icubunit
#endif
      type(DeformationRate) :: MacroDefRate
      !
      ! Components of the new-style data management
      type(altayConfigData)             :: config
      type(altayStateData),target       :: state
      type(MaterialData)                :: material
      type(MaterialConfig)              :: material_config !> \todo Resove this temporary fix
      ! Storage for persistent state variables
      class(StatePersistenceScheme), pointer :: input_storage, output_storage
#ifdef TESTING_ENABLED
      class(StatePersistenceScheme), pointer :: native_storage
#endif
      !
      SAVE
      
!     UNIT KLEC = CONTROL FILE
  90  format (a)
#ifndef MAINDIRECT
      open (unit=KLEC,file='MAINA1.CTL',status='old')
      read (KLEC,90) fnam1
      call stripComment(fnam1)
      write (*,93) trim(fnam1)
      close (unit=KLEC)
!     UNIT KLEC = PARAMETER FILE
      open (unit=KLEC,file=fnam1,status='old')
#else
      open (unit=KLEC,file='MAIN.CTL',status='old')
#endif
      read (KLEC,90) config%output_prefix
      call stripComment(config%output_prefix)
      write (*,92) trim(config%output_prefix)
  92  format (' Code for this simulation: ',a)
  93  format(' Input file:',a)
      fname_prefix = config%output_prefix
!     UNIT IMP = PRINTER
      open (unit=IMP,file=trim(fname_prefix)//'.LST',status='replace')
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ NLIST is not assigned a value yet! so supressed it! QGX 28/10/2011
!      write (IMP,92) 
!EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
!     UNIT IMP2 = PRINTER
      open (unit=IMP2,file=trim(fname_prefix)//'.RES',status='replace')
!     UNIT IMP3 = PRINTER
      open (unit=IMP3,file=trim(fname_prefix)//'.TWN',status='replace')
      ! 
!     UNIT IMP5 = homogenized strain-stress
      open (unit=IMP5,file=trim(fname_prefix)//'.MSS',status='replace')
      call writeMSSHeader(IMP5,info)
      !
      open(unit=IMP6,file=trim(fname_prefix)//'.RPT',status='replace',   &
           iostat=info)
      !
#ifdef PEBP_ENABLED
      open(unit=IPEBPSDV,file=trim(fname_prefix)//'.SDV',                &
           status='replace',iostat=info)
#endif
      read (KLEC,90) config%deformationmechanism%input_fname
      call stripComment(config%deformationmechanism%input_fname)
      write (*,93) trim(config%deformationmechanism%input_fname)
      !
      !Set config%deformationmechanism%ID from
      ! interpretation of config%deformationmechanism%input_fname
      I = 0
      I = index(config%deformationmechanism%input_fname,'.',back=.TRUE.)
      if (I /= 0) then
          ! string '.' appears; assume configuration from file
          select case (config%deformationmechanism%input_fname(I+1:I+3))
          case ('pre','PRE')
              config%deformationmechanism%ID = DM_format_pre
          case ('dat','DAT')
              config%deformationmechanism%ID = DM_format_dat
          case default
              config%deformationmechanism%ID = DM_user ! an unsupported value
          end select
      else
          ! string '.' does not appear; assume pre-configuration
          I = len_trim(config%deformationmechanism%input_fname)
          if ( I < 5 ) config%deformationmechanism%ID = DM_user ! an unsupported value       
          select case (config%deformationmechanism%input_fname(I-4:I))
          case ('fcc12', 'FCC12')
              config%deformationmechanism%ID = DM_fcc12
          case ('bcc24', 'BCC24')
              config%deformationmechanism%ID = DM_bcc24
          case ('bcc48', 'BCC48')
              config%deformationmechanism%ID = DM_bcc48
          case default
              config%deformationmechanism%ID = DM_user ! an unsupported value
          end select
      end if
      !
      READ(KLEC,96) NLINES
      write (*,97) NLINES
  97  format (' number of lines with tau-crit values:',i3)
  96  FORMAT (I5) 
      ! Set the config field if needed
      if (NLINES > 0) then
          call CRSSData_init(config%hardening%crss_ratios, 6*NLINES,info)
      endif
      DO 3 ISIGN=1,2                                                       
      DO 1 J=1,NLINES                                                   
      K=1+6*(J-1)                                                       
      L=K+5                                                             
      READ(KLEC,98) (config%hardening%crss_ratios%crss(ISIGN,I),I=K,L)
  98  FORMAT (6F10.0)
! 
   1  CONTINUE
  3   continue
      read (KLEC,99) NBLOC
  99  format (i5)
      write (*,102) NBLOC
      if(NLIST.eq.1) then
      write (IMP,102) NBLOC
      end if
 102  format (' NBLOC=',I5)
      read (KLEC,88) config%micros_fname
  88  format (a)
      call stripComment(config%micros_fname)
      write (*,103) trim(config%micros_fname)
103   format (' InterfaceDataset_readfromSMTfile - Input Texture File:',a)            

!
!     Initialisation of SIMUL
!
#ifdef TESTING_ENABLED
      !!! TESTING -->>
      call altayStateData_printStatus(state)
      !!! <<-- TESTING
#endif
      info = altayStateData_init(state)
#ifdef TESTING_ENABLED
      !!! TESTING -->>
      call altayStateData_printStatus(state)
      !!! <<-- TESTING
#endif

      ! Read components of the config:
      read (KLEC,99) config%simul_init%NGR
      read (KLEC,99) config%output_config%NLIST
      read (KLEC,99) config%output_config%NFILE
      read (KLEC,99) config%output_config%NFILTW
      read (KLEC,99) config%hardening%hardLawID
      read (KLEC,99) config%output_config%IPR
      ! Set derived fields (i.e the ones that don't have a separate switch)
      config%output_config%NRES = config%output_config%NFILE
      config%output_config%NMSS = config%output_config%NLIST
      !
#ifdef PEBP_ENABLED
      select case(config%hardening%hardLawID)
      case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
          config%output_config%NPEBP  = config%output_config%NFILE
      endselect
#endif
      !
      ! Read the F tensor
      do i=1,3
         read (KLEC,94)(config%simul_init%FMicro(i,j),j=1,3)
         if(config%output_config%NLIST == 1) then
         write (IMP,106)(config%simul_init%FMicro(i,j),j=1,3)
         end if
      enddo
 106  format ('F_Microstructure=',3f12.6)
  94  format (3F10.0)
  16  read (KLEC,90) config%jobtitle

      ! Read the config%hardening from KLEC, before initializing SIMUL
      call altayHard_readConfig(KLEC, config%hardening, info)
      if (info /= criSuccess) then
            write(*,fmt=9010) 'hardening section'
            call terminate(stopcode_inputerror)
      endif
      !> \todo Resove temporary fix to material_config
      ! temporary fix -->>
      allocate(material_config%phases(1))
      associate(phase => material_config%phases(1))
          phase%deformation_mechanism = config%deformationmechanism
          phase%hardening = config%hardening
          phase%intraphase_interfaces = MesostructureConfig(config%simul_init%FMicro, config%micros_fname)
      end associate
      call initialize(material, material_config, info)
      if (info /= criSuccess) then
            write(*,*) 'material%init returned error; info=', info
            call terminate(stopcode_inputerror)
      endif
      ! <<--
      !
      CALL SIMUL(config, state, material, 0, 1)

      !
      ! Load state variables
      
      !> \todo there is no need to initilize and finalize FH5 if neither 
      !>       input nor output data persistence scheme uses HDF5.
      info = FH5_initialize()
      
      associate(texcnf => config%texture)
          ! Get the initial texture
          read(KLEC,99) texcnf%input_type
          read(KLEC,'(A)') texcnf%input_fname
          read(KLEC,99) texcnf%block_id
          call stripComment(texcnf%input_fname)
          !
          ! Initialize appropriate backend: texcnf%input_type
          info = criError
          input_storage => statePersistenceFactory(texcnf,as_input=.true.)
          if (associated(input_storage)) then
              call input_storage%loadState(state%old, info)
              deallocate(input_storage)
          endif
          !
          if (info /= 0) then
                write(*,fmt=9980) trim(texcnf%input_fname)
                call exit(stopcode_ioerror)
     9980 format('An error has occurred while processing texture data file:' &
                 ,1X,A)
          endif
      end associate
      
#ifdef PEBP_ENABLED
      ! PEBP model
      NREC = size(DFIL)
      select case(HardLawID)
      case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
            ! UNIT IMP4 = state variables of PEBP KOST11
            info = KS_openStateFile(IMP4,trim(fname_prefix)//'.BPM','w')
            !
            if (KS_initState(NREC) /= 0) then
                  write(IMP,fmt=600) 
                  call exit(stopcode_runtimeerror)
            endif
            read_state = .false.
            nblock = 0
            read(KLEC,66) read_state, nblock, fname_pebp
            if (read_state) then
                  call stripComment(fname_pebp)
                  info = KS_readState(fname_pebp,IPEBPSTAT,nblock)
                  if (info /= 0) then 
                        write(IMP,fmt=601) trim(fname_pebp)
                        write(*,fmt=601) trim(fname_pebp)
                        call exit(stopcode_ioerror)
                  endif
            endif
      endselect
 66   format(L2,I5,A)      
 600  format('Cannot initialize DSH state variables')
 601  format('Cannot read DSH state variables from file: ',A)
#endif
#ifdef TESTING_ENABLED
      !!! TESTING -->>
      call altayStateData_printStatus(state)
      !!! <<-- TESTING
#endif

      if (altayStateData_assemble(state) /= criSuccess) then
          write(*,fmt=9002) 'altayStateData_assemble has failed.'
          call exit(stopcode_ioerror)
      endif
      
#ifdef TESTING_ENABLED
      !!! TESTING -->>
      call altayStateData_printStatus(state)
      !!! <<-- TESTING
#endif
    !
      ! Initialize HDF5 output storage
      !!! FIXME -->>
      block
           type(TextureConfig) :: texcnf
           !
           texcnf%input_type = TF_HDF5
           texcnf%input_fname = trim(config%output_prefix)//'.h5:state' 
           output_storage => statePersistenceFactory(texcnf, as_input=.false.)
           !> \todo: if output of the initial state is requested
           call output_storage%saveState(state%old, info) 
      end block
      !!! <<--
      !
#ifdef TESTING_ENABLED
      ! Only for testing: CUR file
      block
            type(TextureConfig) :: texcnf
            texcnf%input_type = TF_CUR
            texcnf%input_fname = trim(config%output_prefix)//'.CUR'
            native_storage => statePersistenceFactory(texcnf, as_input=.false.)
            call native_storage%saveState(state%old, info)
      end block
#endif
#ifdef TESTING_ENABLED
      ! Only for testing: CUB file
      block
            class(StatePersistenceScheme), pointer :: storage
            type(TextureConfig) :: texcnf
            !
            texcnf%input_type = TF_CUB
            texcnf%input_fname = trim(config%output_prefix)//'_init.CUB'
            storage => statePersistenceFactory(texcnf, as_input=.false.)
            call storage%saveState(state%new, info) 
            deallocate(storage)
      end block
#endif
      
      DO 2 JBLOC=1,NBLOC
!
!     Simulation of a certain number of steps.
!     The "response to the imposed strain" is only printed
!     after the 1st step.
!
!     Reading of NFILE0: if 1, make output record; if 0, no output record
!
      read (KLEC,99) NFILE0
!
!     Reading of displacement gradient
!
!
      if(NLIST.eq.1) then
      write (IMP,101) JBLOC,NFILE0
      end if
      write (*,101) JBLOC,NFILE0
 101  format (' SIMUL CALL NR.',I5,'   Output parameter',I5,/,           &
      ' Displacement gradient:')
      DO 35 I=1,3
      READ (KLEC,*) (DG(I,K),K=1,3)
      if(NLIST.eq.1) then
      WRITE (IMP,109) (DG(I,K),K=1,3)
      end if
      WRITE (*,109) (DG(I,K),K=1,3)
 109  FORMAT (1x,3F10.5)
  35  CONTINUE
      ! Here DG = [L]*dt, where [L] is the velocity gradient 
      ! and the time step dt = 1.0
      call Set_DeformationRate(DG,MacroDefRate)
      !
      CALL SIMUL(config, state, material, 1, NFILE0, MacroDefRate)
      call output_storage%saveState(state%new, info)
#ifdef TESTING_ENABLED
      call native_storage%saveState(state%new, info)
#endif
   2  CONTINUE
!
!     Output of last "current situation"
!

#ifdef FINALCUB_ENABLED
    block
        class(StatePersistenceScheme), pointer :: storage
        type(TextureConfig) :: texcnf
        !
        texcnf%input_type = TF_CUB
        texcnf%input_fname = trim(config%output_prefix)//'.CUB'
        storage => statePersistenceFactory(texcnf, as_input=.false.)
        call storage%saveState(state%new, info) 
        deallocate(storage)
    end block
#endif

#if defined(PEBP_ENABLED) && defined(FINALBPM_ENABLED)
      select case(HardLawID)
      case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
#ifndef INTERMEDIATEBPM_DISABLED
          if(NFILE0 == 0) info = KS_writeState(IMP4)
#else
          info = KS_writeState(IMP4)
#endif
      endselect
#endif
      
      deallocate(output_storage)
#ifdef TESTING_ENABLED
      ! Only for testing: CUR file
      deallocate(native_storage)
#endif
      !> \todo FIXME This should be done in a _correct_ and elegant way
      info = FH5_finalize()
      
      STOP
      
9002 format('Catastrophic error in MAIN:',1X,A)
9010 format('Format error in the configuration file:',1X,A)
END PROGRAM
