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
      use altayAssembly
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
      character(len=pathlength) :: fnam1
      integer :: info
      integer :: I,J,K,L
      integer :: NLINES, ISIGN, NBLOC, NSTP, NFILE0
      integer :: ICRAT1,ICRAT2
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
      type(MaterialData),target         :: material
      type(MaterialConfig)              :: material_config !> \todo Resove this temporary fix
      type(StatePersistenceConfig)      :: input_storage_config !> \todo Resove this temporary fix
      ! Storage for persistent state variables
      class(StatePersistenceScheme), pointer :: input_storage, output_storage
#ifdef FORCE_CUR_OUTPUT_ENABLED
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
      !
      call openStandardOutputFiles(config%output_prefix, info)
      !
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
      !
      associate(texcnf => config%texture)
          ! Get the initial texture
          read(KLEC,99) texcnf%format_id
          read(KLEC,'(A)') texcnf%file_name
          read(KLEC,99) texcnf%block_id
          call stripComment(texcnf%file_name)
      end associate
      !
      ! Set material
      !
      material_config = MaterialConfig(1)
      associate(phase => material_config%phases(1))
          phase%name = config%output_prefix
          phase%deformation_mechanism = config%deformationmechanism
          phase%hardening = config%hardening
          phase%intraphase_interfaces = MesostructureConfig(config%simul_init%FMicro, config%micros_fname)
      end associate
      !
      call initialize(material, material_config, info)
      if (info /= criSuccess) then
            write(*,*) 'material%init returned error; info=', info
            call terminate(stopcode_inputerror)
      endif
      !
      ! Initialize the state
      !
#ifdef TESTING_ENABLED
      call altayStateData_printStatus(state)
#endif
      info = altayStateData_init(state)
      state%old%material => material
      state%new%material => material

#ifdef TESTING_ENABLED
      call altayStateData_printStatus(state)
#endif

      !
      ! Load state variables
      !
      !> \todo there is no need to initilize and finalize FH5 if neither 
      !>       input nor output data persistence scheme uses HDF5.
      info = FH5_initialize()
      associate(texcnf => config%texture)
          !
          ! Initialize appropriate backend: texcnf%input_type
          select case(texcnf%format_id)
          case(TF_SMT,TF_CUR,TF_CUB)
              input_storage_config%scheme_id = CNF_StatePersistencyNative
          case(TF_HDF5)
              input_storage_config%scheme_id = CNF_StatePersistencyHDF5
          end select
          !
          allocate(input_storage_config%phases(1))
          input_storage_config%phases(1)%odf = texcnf
          !
          input_storage => statePersistenceFactory(input_storage_config, info)
          if (associated(input_storage) .and. (info == criSuccess)) then
              call input_storage%loadState(state%old, info)
              deallocate(input_storage)
          else
              call terminate(stopcode_ioerror)
          endif
          !
          if (info /= 0) then
                write(*,fmt=9980) trim(texcnf%file_name)
                call exit(stopcode_ioerror)
     9980 format('An error has occurred while processing texture data file:' &
                 ,1X,A)
          endif
      end associate
#ifdef TESTING_ENABLED
      ! This code is absolutely unnecessary here. It simply tests how
      ! cluster assemblies are built.
      block
        type(AssemblyMultiPhaseDirective) :: directives(1)
        if (config%simul_init%NGR == 1) then
            directives(1)%phase_ids = [1]
        else
            directives(1)%phase_ids = [1, 1]
        endif
        directives(1)%number_instances = size(state%old%grainstates%grains) / config%simul_init%NGR
        call altayAssembly_ClusterAssembly_basic(state%old, directives, info)
      end block
#endif
      !
      !     Initialisation of SIMUL
      !
      CALL SIMUL(config, state, material, 0, 0, 1)

#ifdef PEBP_ENABLED
      ! PEBP model
      NREC = size(DFIL)
      select case(HardLawID)
      case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
            ! UNIT IMP4 = state variables of PEBP KOST11
            info = KS_openStateFile(IMP4,trim(config%output_prefix)//'.BPM','w')
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
           type(StatePersistenceConfig) :: outcnf
           outcnf%scheme_id = CNF_StatePersistencyHDF5
           outcnf%access_mode=StatePersistence_Write
           !
           outcnf%path = trim(config%output_prefix)//'.h5:state' 
           output_storage => statePersistenceFactory(outcnf, info)
           !> \todo: if output of the initial state is requested
           call output_storage%saveState(state%old, info) 
      end block
      !!! <<--
      !
#ifdef TESTING_ENABLED
      !!! TESTING -->>
      call altayStateData_printStatus(state)
      !!! <<-- TESTING
#endif

#ifdef FORCE_CUR_OUTPUT_ENABLED
      ! Only for testing: CUR file
      block
           type(StatePersistenceConfig) :: outcnf
           outcnf = StatePersistenceConfig(CNF_StatePersistencyNative, &
                                           StatePersistence_Write, &
                                           n_phases=1)
           outcnf%phases(1)%odf%format_id = TF_CUR
           outcnf%phases(1)%odf%file_name = trim(config%output_prefix)//'.CUR'
           !
           native_storage => statePersistenceFactory(outcnf, info)
           call native_storage%saveState(state%old, info)
      end block
#endif
#ifdef EXTENDED_TESTING_ENABLED
      ! Only for testing: CUB file
      block
            class(StatePersistenceScheme), pointer :: storage
            !
            type(StatePersistenceConfig) :: outcnf
            outcnf%scheme_id = CNF_StatePersistencyNative
            outcnf%access_mode=StatePersistence_Write
            outcnf%odf_config%format_id = TF_CUB
            outcnf%odf_config%file_name = trim(config%output_prefix)//'_init.CUB'
            !
            storage => statePersistenceFactory(outcnf, material_config, info)
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
      read (KLEC,99) NSTP
      read (KLEC,'(2I5)') ICRAT1,ICRAT2 ! Dummy, never used
      !
      CALL SIMUL(config, state, material, NSTP, 1, NFILE0, MacroDefRate)
      call output_storage%saveState(state%new, info)
#ifdef FORCE_CUR_OUTPUT_ENABLED
      call native_storage%saveState(state%new, info)
#endif
   2  CONTINUE
!
!     Output of last "current situation"
!

#ifdef FINALCUB_ENABLED
    block
        class(StatePersistenceScheme), pointer :: storage
        type(StatePersistenceConfig) :: outcnf
        outcnf = StatePersistenceConfig(CNF_StatePersistencyNative, &
                                        StatePersistence_Write, &
                                        n_phases=1)
        outcnf%phases(1)%odf%format_id = TF_CUB
        outcnf%phases(1)%odf%file_name = trim(config%output_prefix)//'.CUB'
        !
        storage => statePersistenceFactory(outcnf, info)
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
#ifdef FORCE_CUR_OUTPUT_ENABLED
      ! CUR file
      deallocate(native_storage)
#endif
      !> \todo FIXME This should be done in a _correct_ and elegant way
      info = FH5_finalize()
      
      STOP
      
9002 format('Catastrophic error in MAIN:',1X,A)
9010 format('Format error in the configuration file:',1X,A)
END PROGRAM
