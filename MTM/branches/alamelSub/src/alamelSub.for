!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>    \author Paul Van Houtte
!>    Email:  Paul.VanHoutte@mtm.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2010-10-18/2010-10-28
!>    $Revision$                                        
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>    * This module is based on ALAMEL main program code by PVH and co-workers.
!>    * Several modifications have been introduced by JG to make this code more
!>      "procedure-like".                  
!>    * The code inside this file was initially a part of Main1.for      
!
!>    \file alamelSub.for ALAMEL as a subroutine
!>                                  
!                                                
#ifdef ALAMEL_SUBROUTINE
! Simplified version of alamel, suitable for calls as a subroutine
      module AlamelSub


      contains

      !> Subroutine ALAMEL
      !>
      !> \param ireason: 
      !>    * 1 - initialization of data structures and initialization of sg0
      !>    * 2 - initialization of sg0
      !>    * 3 - regular call 
      !>    * 4 - request for output (if compiled with support for output)     
      subroutine ALAMEL(ireason)
      use alamelConfig
      use alamelInterface
      use UDYNFIL
      implicit double precision (a-h,o-z)
C
C     LEC= data set with slip systems
C     KLEC= data set with parameters
C     IMP= printer
C     IMP1=output-file with successive "current situations"
C     IMP2=output-file with successive "responses to imposed strain"
C     IDISK1= work file
C     NDAT1= Input-texture file     Opened in LEESOR
C
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1,NDAT2
      COMMON /ES1/ IMP3
c <jg>      
      integer,intent(in)      ::  ireason
      !
      COMMON /OUTMIC/NUMIC,NUCUB
      double precision resid
      integer iiter
c </jg>        
      COMMON /IGLIJS/ FK1(96),NUNGL,NGLS,CC(96)
      COMMON /TEXTUR/ DUM1(29),IDUM1,DG(3,3),
     1ITW,IPR,DELTAW,GEWF,NLIST
      common /CEIGEN/ IOR,ISTP,JBLOC,RELEXP
      integer pathlength
      parameter (pathlength=512)
      character (LEN=pathlength) fnam1,fnam2,cods1 ! jg
      character * 8 codsim
      integer iblank
      integer NUMIC,NUCUB
      data NUMIC/122/,NUCUB/123/
      DATA MPOINT /8000/,NUNIT/2/
      SAVE

      if (ireason < 2) then      

C     UNIT NUNIT = Temporary file
      open(unit=NUNIT,status='SCRATCH',form='unformatted')
C
  90  format (a)
      codsim = trim(acnf%output_prefix)
  92  format (' Code for this simulation: ',a)
      L=LEN_TRIM(codsim)
      cods1=codsim
#ifndef NOLSTFILE      
      cods1(L+1:L+4)='.LST'
C     UNIT IMP = PRINTER
      open (unit=IMP,file=cods1,status='replace')
      write (IMP,92) codsim
#endif
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
#ifndef NOTWNFILE
      cods1(L+1:L+4)='.TWN'
C     UNIT IMP3 = PRINTER
      open (unit=IMP3,file=cods1,status='replace')
#endif  
      fnam2 = trim(acnf%slipsystem%input_fname)
C     UNIT LEC = SLIP SYSTEMS
      open (unit=LEC,file=TRIM(fnam2),status='old')

      ! nsym decides how  the slip systems are dealt with 
      if (acnf%slipsystem%nsym == 0) then
            FK1 = 1.D0
      else
            FK1(1:acnf%slipsystem%ntau) = acnf%slipsystem%taucrit
      endif
      NBLOC = acnf%nSimulCalls
  99  format (i5)

      CALL GRFIL()  

C
C     Initialisation of SIMUL
C
      CALL SIMUL(0,1,EPS,1,NUNIT,0)
      ! go back to initial texture, initialize UDynfil
      CALL LEESOR(NUNIT,MPOINT)
      
      endif  ! ireason < 2
            
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (ireason < 3) then
            NFILE0 = acnf%simulCalls(0)%do_output
            DG = acnf%simulCalls(0)%dgf

C
C     Initialisation of SG0 (average von Mises stress)
C
            CALL SIMUL(1,1,EPS,NFILE0,NUNIT,1)
            !
            !     Come back to initial texture
            !
            !CALL LEESOR(NUNIT,MPOINT)
            !
            call useUDyn() !! AKA: call fleesor()
      endif ! ireason < 3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (ireason == 3) then
      
      DO 2 JBLOC=1,NBLOC
C
C     Simulation of a certain number of steps.
C     The "current situation" is printed on IMP1 in the beginning.
C     The "response to the imposed strain" is only printed
C     after the 1st step.
C
      acnf%current_call = JBLOC

      NFILE0 = acnf%simulCalls(acnf%current_call)%do_output
      DG = acnf%simulCalls(acnf%current_call)%dgf

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
      
      if (acnf%simulCalls(acnf%current_call)%keep_texture) then
            !      CALL LEESOR(NUNIT,MPOINT)
            call useUDyn()  ! AKA: call fleesor
      else
            call updateUDyn()      
      endif

   2  CONTINUE
   
      endif 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      if (ireason == 4) then
C
C     Output of last "current situation"
C
#ifdef WITHSMTFILE
c <jg>: Open file for output SMT 
            cods1(L+1:L+4)='.smt'
            open(unit=NUMIC,file=cods1,status='replace',action='write')
#endif      
#ifdef WITHCUBFILE
            cods1(L+1:L+4)='.cub'
            open(unit=NUCUB,file=cods1,status='replace',
     &            form='UNFORMATTED',action='write')
#endif
c </jg>
            NFILE0 = 1
            CALL SIMUL(2,1,EPS,NFILE0,NUNIT,0)

#ifdef WITHSMTFILE
            close(NUMIC)
#endif      
#ifdef WITHCUBFILE        
            close(NUCUB)
#endif
      
      endif
      
      end subroutine

      end module

#endif

