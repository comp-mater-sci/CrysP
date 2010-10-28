#ifndef ALAMEL_SUBROUTINE
      PROGRAM MAIN1
      implicit double precision (a-h,o-z)
c      Several simulations (usually several-steps each),
C      following each other.
C
C      LEESOR is called once.
C
C      There is a "last block with Current situation"
C      (without simulation, only for output to the CUR-file).
C
C     LEC= data set with slip systems
C     KLEC= data set with parameters
C     IMP= printer
C     IMP1=output-file with successive "current situations"
C     IMP2=output-file with successive "responses to imposed strain"
C     IDISK1= work file
C     NDAT1= Input-texture file     Opened in LEESOR
C     NDAT2=
C
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1,NDAT2
      COMMON /ES1/ IMP3
c <jg>      
      COMMON /OUTMIC/NUMIC,NUCUB
      double precision resid
      integer iiter
c </jg>        
      COMMON /IGLIJS/ FK1(96),NUNGL,NGLS,CC(96)
C      COMMON /RCFILS/ NUNRC(2)
      COMMON /TEXTUR/ DUM1(29),IDUM1,DG(3,3),
     1ITW,IPR,DELTAW,GEWF,NLIST
      common /CEIGEN/ IOR,ISTP,JBLOC,RELEXP
!      character * 12 fnam1,fnam2,cods1
      integer pathlength
      parameter (pathlength=512)
      character (LEN=pathlength) fnam1,fnam2,cods1 ! jg
      character * 8 codsim
      integer iblank
      integer NUMIC,NUCUB
      data NUMIC/122/,NUCUB/123/
      DATA MPOINT /8000/,NUNIT/2/
      SAVE
C     UNIT NUNIT = Temporary file
      open(unit=NUNIT,status='SCRATCH',form='unformatted')
C     UNIT IDISK1 = Temporary file
      open(unit=IDISK1,status='SCRATCH',form='unformatted')
C     UNIT KLEC = CONTROL FILE
      open (unit=KLEC,file='MAIN1.CTL',status='old')
c <jg>
  93  format(' Input file:',a)
#ifndef MAINDIRECT
      read (KLEC,90) fnam1
      write (*,93) fnam1
      close (unit=KLEC)
C     UNIT KLEC = PARAMETER FILE
      open (unit=KLEC,file=fnam1,status='old')
#endif
c </jg>
  90  format (a)
      read (KLEC,90) codsim
      call stripComment(codsim,len(codsim))
      write (*,92) codsim
  92  format (' Code for this simulation: ',a)
      L=LEN_TRIM(codsim)
      cods1=codsim
      cods1(L+1:L+4)='.LST'
C     UNIT IMP = PRINTER
      open (unit=IMP,file=cods1,status='replace')
      write (IMP,92) codsim
c <jg>
#ifndef NOCURFILE
      cods1(L+1:L+4)='.CUR'
C     UNIT IMP1 = PRINTER
      open (unit=IMP1,file=cods1,status='replace')
#endif
#ifndef NORESFILE
      cods1(L+1:L+4)='.RES'
C     UNIT IMP2 = PRINTER
      open (unit=IMP2,file=cods1,status='replace')
#endif
c </jg>
      cods1(L+1:L+4)='.TWN'
C     UNIT IMP3 = PRINTER
      open (unit=IMP3,file=cods1,status='replace')
      read (KLEC,90) fnam2
c <jg>
      call stripComment(fnam2,len(fnam2))
c </jg>
      write (*,93) TRIM(fnam2)
C     UNIT LEC = SLIP SYSTEMS
      open (unit=LEC,file=TRIM(fnam2),status='old')
c <jg>: Open file for output SMT 
#ifdef WITHSMTFILE
      cods1(L+1:L+4)='.smt'
      open(unit=NUMIC,file=cods1,status='replace',action='write')
#endif      
#ifdef WITHCUBFILE
      cods1(L+1:L+4)='.cub'
      open(unit=NUCUB,file=cods1,status='replace',
     &     form='UNFORMATTED',action='write')
#endif
c </jg>
      READ(KLEC,96) NKAART
      write (*,97) NKAART
  97  format (' number of lines with tau-crit values:',i3)
  96  FORMAT (I5)                                                       
      DO 1 J=1,NKAART                                                   
      K=1+6*(J-1)                                                       
      L=K+5                                                             
      READ(KLEC,98) (FK1(I),I=K,L)                                      
  98  FORMAT (6F10.0)                                                   
      WRITE (IMP,100) K, (FK1(I),I=K,L)                                 
C      WRITE (*,100) K, (FK1(I),I=K,L)
 100  FORMAT (1X,I2,2x,6F12.4)
   1  CONTINUE
      read (KLEC,99) NBLOC
      write (*,102) NBLOC
      write (IMP,102) NBLOC
 102  format (' NBLOC=',I5)
  99  format (i5)

      CALL GRFIL  

C
C     Initialisation of SIMUL
C
      CALL SIMUL(0,1,EPS,1,NUNIT,0)
C
C     Initialisation of SG0 (average von Mises stress)
C
      CALL LEESOR(NUNIT,MPOINT)
C     Reading of NFILE0: if 1, make output record; if 0, no output record
C
      read (KLEC,99) NFILE0
      write (IMP,103) NFILE0
      write (*,103) NFILE0
 103  format (' SIMUL CALLED for first estimation of SG0.',
     1'Output parameter:',I5,/,
     2' Displacement gradient:')
      DO I=1,3
         READ (KLEC,95) (DG(I,K),K=1,3)
         WRITE (IMP,109) (DG(I,K),K=1,3)
         WRITE (*,109) (DG(I,K),K=1,3)
      enddo
      CALL SIMUL(1,1,EPS,NFILE0,NUNIT,1)
C
C     Come back to initial texture
C
      CALL LEESOR(NUNIT,MPOINT)
      DO 2 JBLOC=1,NBLOC
      
C
C     Simulation of a certain number of steps.
C     The "current situation" is printed on IMP1 in the beginning.
C     The "response to the imposed strain" is only printed
C     after the 1st step.
C
C     Reading of NFILE0: if 1, make output record; if 0, no output record
C
      read (KLEC,99) NFILE0
C
C     Reading of displacement gradient
C
C
      write (IMP,101) JBLOC,NFILE0
      write (*,101) JBLOC,NFILE0
 101  format (' SIMUL CALL NR.',I5,'   Output parameter',I5,/,
     1' Displacement gradient:')
      DO 35 I=1,3
      READ (KLEC,*) (DG(I,K),K=1,3)
  95  FORMAT (3F10.0)
      WRITE (IMP,109) (DG(I,K),K=1,3)
      WRITE (*,109) (DG(I,K),K=1,3)
 109  FORMAT (1x,3F10.5)
  35  CONTINUE
c >>> jg: preempt round-off errors due to IO format
      resid = DG(1,1)+DG(2,2)+DG(3,3)
      if (dabs(resid) .GT. 1.D-9) then
            resid = resid / 3.D0
            do iiter=1,3
                  DG(iiter,iiter) = DG(iiter,iiter)-resid
            enddo
      endif
c <<< 
      CALL SIMUL(1,1,EPS,NFILE0,NUNIT,0)
   2  CONTINUE
C
C     Output of last "current situation"
C
      write (IMP,110)
      write (*,110)
 110  format (//,' Final call of SIMUL (for output only)')
      CALL SIMUL(2,1,EPS,NFILE0,NUNIT,0)
c <jg>
      close(NUMIC)
#ifdef WITHCUBFILE        
      close(NUCUB)
#endif
c </jg>
      STOP
      END
#endif
! end of: ALAMEL_SUBROUTINE
