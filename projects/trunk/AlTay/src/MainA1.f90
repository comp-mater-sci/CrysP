! MODIFICATIONS AUG 2010
! THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
! WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY
! NGLS is replaced by M11
!
      PROGRAM MAINA1
      use altayMiscutils
      use altayIOConfig
      use altayDSHstate
      use altayDynfil
      use altayMesostructure
      use altaySimul
      use altayHard,only: HardLawID, hard_BP, hard_PEBPscrew,            &
                          hard_PEBPloop
      use altayHardTypes
      use altayTexFormats
      use altayMacroKinematic
      implicit double precision (a-h,o-z)
!      Several simulations (usually several-steps each),
!      following each other.
!
!      There is a "last block with Current situation"
!      (without simulation, only for output to the CUR-file).
!
!     LEC= data set with slip systems (file handle to .pre file, name stored in fnam2 (defined in KLEC))
!     KLEC= data set with parameters (initially file handle to MAINA1.CTL (which again leads to mainA1.i01))
!     IMP= printer
!     IMP1=output-file with successive "current situations"
!     IMP2=output-file with successive "responses to imposed strain"
!     IDISK1= work file (obsolete, not used)
!     NDAT1= Input-texture file
!
      COMMON /IGLIJS/ M11,CC(2,96)
      COMMON /TEXTUR/ DUM1(27),IDUM1,ITW,GEWF
      common /CEIGEN/ IOR,ISTP,JBLOC
      common /PE/ Fmicro !Temporary!!!
      double precision, dimension(3,3) :: Fmicro, DG
      character(len=pathlength) :: fnam1,fnam2,fnam3,fname_prefix
      character(len=pathlength-4) :: codsim
      integer :: info
      integer :: tex_type, tex_nblock
      character(len=pathlength) :: tex_fname
#ifdef PEBP_ENABLED      
      character(len=pathlength) :: fname_pebp ! PEBP state variables file from which initial state gets read
      logical :: read_state
      integer :: nblock
#endif
#ifdef FINALCUB_ENABLED
      integer,parameter :: icubunit = 444
#endif
      type(DeformationRate) :: MacroDefRate
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
! fnam1 points now either to file mainA1.i01 or to MAIN.CTL
#endif

      read (KLEC,90) codsim
      call stripComment(codsim)
      write (*,92) trim(codsim)
  92  format (' Code for this simulation: ',a)
  93  format(' Input file:',a)
      fname_prefix=codsim
!     UNIT IMP = PRINTER
      open (unit=IMP,file=trim(fname_prefix)//'.LST',status='replace')
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ NLIST is not assigned a value yet! so supressed it! QGX 28/10/2011
!      write (IMP,92) codsim
!EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
!     UNIT IMP1 = PRINTER
      open (unit=IMP1,file=trim(fname_prefix)//'.CUR',status='replace')
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
      read (KLEC,90) fnam2
      call stripComment(fnam2)
      write (*,93) trim(fnam2)
!     UNIT LEC = SLIP SYSTEMS
      open (unit=LEC,file=fnam2,status='old')
      READ(KLEC,96) NLINES
      write (*,97) NLINES
  97  format (' number of lines with tau-crit values:',i3)
  96  FORMAT (I5) 
      DO 3 ISIGN=1,2                                                       
        DO 1 J=1,NLINES                                                   
          K=1+6*(J-1)                                                       
          L=K+5                                                             
          READ(KLEC,98) (crss_ratiosIN%crss(ISIGN,I),I=K,L)
  98  FORMAT (6F10.0)
! 
   1    CONTINUE
  3   continue
      read (KLEC,99) NBLOC
  99  format (i5)
      write (*,102) NBLOC
      if(NLIST.eq.1) then
      write (IMP,102) NBLOC
      end if
 102  format (' NBLOC=',I5)
      read (KLEC,88) fnam3
  88  format (a)
      call stripComment(fnam3)
      write (*,103) trim(fnam3)
 103  format (' GRFIL - Input Texture File:',a)            

!
!     Initialisation of SIMUL
!
      CALL SIMUL(0,1) 
      
      ! Initializing microstructure      
      ! NOTE: this is done after initialisation of SIMUL, since SIMUL currently reads a.o. NLIST
      CALL GRFIL(fnam3,Fmicro,info) 
      if (info.ne.0) then
          write(*,215)
          call exit(stopcode_ioerror)
 215      format('Error condition is returned by GRFIL')
      endif      
      
      ! Get the initial texture
      read(KLEC,99) tex_type
      read(KLEC,'(A)') tex_fname
      read(KLEC,99) tex_nblock
      call stripComment(tex_fname)
      ! 
      call loadTexture(tex_type,NDAT1,trim(tex_fname),tex_nblock,info)
      if (info /= 0) then
            write(*,fmt=9980) trim(tex_fname)
            call exit(stopcode_ioerror)
 9980 format('An error has occurred while processing texture data file:' &
             ,1X,A)
      endif
      
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
                  info = KS_readState(fname_pebp,IPEBPSTAT,nblock) ! interface to KS_readState_file; IPEBPSTAT is the IO unit number
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
      ! 
      
      DO 2 JBLOC=1,NBLOC
!
!     Simulation of a certain number of steps.
!     The "current situation" is printed on IMP1 in the beginning.
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
      CALL SIMUL(1,NFILE0,MacroDefRate)
   2  CONTINUE
!
!     Output of last "current situation"
!
#ifdef FINALCUB_ENABLED
      info = openTextureFile(icubunit,trim(fname_prefix)//'.cub',TF_CUB, &
                             'w')
      if (info == 0) then 
            call outputCurrentTexture(icubunit,TF_CUB,.true.,info)
      endif
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
      if(NLIST.eq.1) then
      write (IMP,110)
      end if
      write (*,110)
 110  format (//,' Final call of SIMUL (for output only)')
      CALL SIMUL(2,NFILE0,MacroDefRate)
      STOP
      END
