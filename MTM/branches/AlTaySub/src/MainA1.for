C MODIFICATIONS AUG 2010
C THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
C WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY
C NGLS is replaced by M11
C
      PROGRAM MAINA1
      use miscutils
      use IOConfig
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
C     IDISK1= work file (obsolete, not used)
C     NDAT1= Input-texture file     Opened in LEESOR
C
      COMMON /IGLIJS/ FK1(2,96),M11,CC(2,96)
      COMMON /TEXTUR/ DUM1(29),IDUM1,DG(3,3),ITW,DELTAW,GEWF
      common /CEIGEN/ IOR,ISTP,JBLOC
      integer,parameter :: pathlength = 512
      character(len=pathlength) :: fnam1,fnam2,cods1
      character(len=pathlength-4) :: codsim
      DATA MPOINT /8000/,NUNIT/2/
      SAVE
C     UNIT KLEC = CONTROL FILE
      open (unit=KLEC,file='MAINA1.CTL',status='old')
  90  format (a)
#ifndef MAINDIRECT
      read (KLEC,90) fnam1
      call stripComment(fnam1)
      write (*,93) trim(fnam1)
      close (unit=KLEC)
C     UNIT KLEC = PARAMETER FILE
      open (unit=KLEC,file=fnam1,status='old')
#endif
      read (KLEC,90) codsim
      call stripComment(codsim)
      write (*,92) trim(codsim)
  92  format (' Code for this simulation: ',a)
  93  format(' Input file:',a)
      L=LEN_TRIM(codsim)
      cods1=codsim
      cods1(L+1:L+4)='.LST'
C     UNIT IMP = PRINTER
      open (unit=IMP,file=cods1,status='replace')
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ NLIST is not assigned a value yet! so supressed it! QGX 28/10/2011
C      write (IMP,92) codsim
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      cods1(L+1:L+4)='.CUR'
C     UNIT IMP1 = PRINTER
      open (unit=IMP1,file=cods1,status='replace')
      cods1(L+1:L+4)='.RES'
C     UNIT IMP2 = PRINTER
      open (unit=IMP2,file=cods1,status='replace')
      cods1(L+1:L+4)='.TWN'
C     UNIT IMP3 = PRINTER
      open (unit=IMP3,file=cods1,status='replace')
      read (KLEC,90) fnam2
      call stripComment(fnam2)
      write (*,93) trim(fnam2)
C     UNIT LEC = SLIP SYSTEMS
      open (unit=LEC,file=fnam2,status='old')
      READ(KLEC,96) NLINES
      write (*,97) NLINES
  97  format (' number of lines with tau-crit values:',i3)
  96  FORMAT (I5) 
      DO 3 ISIGN=1,2                                                       
      DO 1 J=1,NLINES                                                   
      K=1+6*(J-1)                                                       
      L=K+5                                                             
      READ(KLEC,98) (FK1(ISIGN,I),I=K,L)                                      
  98  FORMAT (6F10.0)
c 
   1  CONTINUE
  3   continue
      read (KLEC,99) NBLOC
  99  format (i5)
      write (*,102) NBLOC
      if(NLIST.eq.1) then
      write (IMP,102) NBLOC
      end if
 102  format (' NBLOC=',I5)
      CALL GRFIL(ierr)
      if (ierr.ne.0) then
      write(*,215)
      stop
 215  format('Error condition is returned by GRFIL')
      endif
C
C     Initialisation of SIMUL
C
      CALL SIMUL(0,EPS,1) 
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
      if(NLIST.eq.1) then
      write (IMP,101) JBLOC,NFILE0
      end if
      write (*,101) JBLOC,NFILE0
 101  format (' SIMUL CALL NR.',I5,'   Output parameter',I5,/,
     1' Displacement gradient:')
      DO 35 I=1,3
      READ (KLEC,*) (DG(I,K),K=1,3)
      if(NLIST.eq.1) then
      WRITE (IMP,109) (DG(I,K),K=1,3)
      end if
      WRITE (*,109) (DG(I,K),K=1,3)
 109  FORMAT (1x,3F10.5)
  35  CONTINUE
#ifdef MAINDIRECT
C     Preempt rounding errors
      resid = DG(1,1)+DG(2,2)+DG(3,3)
      if (dabs(resid) .GT. 1.D-9) then
            resid = resid / 3.D0
            do i=1,3
                  DG(i,i) = DG(i,i)-resid
            enddo
      endif
#endif
      CALL SIMUL(1,EPS,NFILE0)
   2  CONTINUE
C
C     Output of last "current situation"
C
      if(NLIST.eq.1) then
      write (IMP,110)
      end if
      write (*,110)
 110  format (//,' Final call of SIMUL (for output only)')
      CALL SIMUL(2,EPS,NFILE0)
      STOP
      END
