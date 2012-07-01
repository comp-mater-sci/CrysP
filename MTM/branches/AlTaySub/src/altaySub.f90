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

module altaySub


contains

      
      subroutine initAltay(cnf,info)
      use altayConfig, only: altayConfigData,fname_len
      use altayInterface
!!! FIXME !!!
!      use UDYNFIL
!!! FIXME !!!
      implicit none
      !
      type(altayConfigData),intent(in)    :: cnf
      integer,intent(out)                 :: info
!
!     LEC= data set with slip systems
!     KLEC= data set with parameters
!     IMP= printer
!     IMP1=output file with successive "current situations"
!     IMP2=output file with successive "responses to imposed strain"
!     IMP3=output file with "twinning" stuff (.TWN)
!     IDISK1= work file
!     NDAT1= Input-texture file     Opened in LEESOR
!
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1
      integer  :: LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1
      COMMON /ES1/ IMP3
      integer  ::  IMP3  
      !
      ! COMMON /OUTMIC/NUMIC,NUCUB
      double precision :: resid
      integer :: iiter
      COMMON /IGLIJS/ FK1(2,96),M11,CC(2,96)
      double precision :: FK1, M11, CC
      
      !
      character(len=fname_len) :: fnam1,fnam2, cods1 
      character(len=8)  :: codsim
      integer :: iblank
      
      
      
      !DATA MPOINT /8000/,NUNIT/2/
      integer,parameter :: MPOINT = 8000, NUNIT = 2
      
      !data NUMIC/122/,NUCUB/123/
      integer,parameter :: NUMIC = 122, NUCUB = 123
      
      integer :: L
      double precision :: EPS
      !
      
            ! UNIT NUNIT = Temporary file
            open(unit=NUNIT,status='SCRATCH',form='unformatted')
!
  
            codsim = trim(cnf%output_prefix)
            L=len_trim(codsim)
            cods1=codsim
#ifndef NOLSTFILE      
            cods1(L+1:L+4)='.LST'
      !     UNIT IMP = PRINTER
            open (unit=IMP,file=cods1,status='replace')
#endif
!
#ifndef NOCURFILE
            if (cnf%output_config%use_curfile) then
                  cods1(L+1:L+4)='.CUR'
                  ! IMP1=output file with successive "current situations"
                  open (unit=IMP1,file=cods1,status='replace')
            endif
#endif
!
#ifndef NORESFILE
            cods1(L+1:L+4)='.RES'
            open (unit=IMP2,file=cods1,status='replace')
#endif
!
#ifndef NOTWNFILE
            cods1(L+1:L+4)='.TWN'
            open (unit=IMP3,file=cods1,status='replace')
#endif  
      fnam2 = trim(cnf%slipsystem%input_fname)
!     UNIT LEC = SLIP SYSTEMS
      open (unit=LEC,file=TRIM(fnam2),status='old')

      !!! FIXME !!!
/*      
      ! nsym decides how  the slip systems are dealt with 
      if (cnf%slipsystem%nsym == 0) then
            FK1 = 1.D0
      else
            FK1(1:cnf%slipsystem%ntau) = cnf%slipsystem%taucrit
      endif
*/
      !!! FIXME !!!
      
      !!!! FIXME ????
      ! NBLOC = cnf%nSimulCalls
      !!!! FIXME ????

      CALL GRFIL()  

      !
      ! Initialisation of SIMUL
      !
      EPS = 0.D0
      CALL SIMUL(0,EPS,1,NUNIT)
      ! Go back to initial texture, initialize UDynfil
      CALL LEESOR(NUNIT,MPOINT)
      !!! FIXME !!! ???? - do I need this anymore?
      ! call useUDyn() !! AKA: call fleesor()
      !!! FIXME !!!  
      info = 0            
      
      end subroutine
      
      
      !> Initialization of input and output data for the steps.
      !>
      !> 
      subroutine initStepData(nsteps,steps,info)
      use altayConfig, only: altayStateData
      implicit none
      integer,intent(in)                  :: nsteps
      type(altayStateData),intent(out)    :: steps
      integer,intent(out)                 :: info !< Exit code: 0 on success
      !
      integer :: ierr
            info = 1
            if (nsteps <= 0) return
            ! Allocate the structures
            allocate(steps%simulCalls(nsteps),stat=ierr)
            steps%nSimulCalls = nsteps
            ! No need to specifically initialize other components,
            ! since there are initializers provided in the datatype.
            info = ierr
      !
      end subroutine
      
      
      subroutine runSteps(steps,info)
      use altayConfig, only: altayStateData
      use altayInterface
      !!! FIXME !!!  
      ! use UDYNFIL
      !!! FIXME !!!  

      implicit none
      type(altayStateData),intent(inout)        :: steps
      integer,intent(out)                       :: info
      
      COMMON /TEXTUR/ DUM1(29),IDUM1,DG(3,3), ITW,IPR,DELTAW,GEWF,NLIST
      double precision :: DUM1,DG, DELTAW,GEWF
      integer :: IDUM1,ITW,IPR,NLIST
      integer :: NFILE0
      !
      integer :: i,j
      double precision :: resid
      
      !!! FIXME !!!
      integer,parameter :: MPOINT = 8000, NUNIT = 2
      !!! FIXME !!!
      !
      ! TODO: check validity of the inputs (priority: size of the array!!)
     
      info = 1
      
      do i = 1, steps%nSimulCalls
            steps%this = i
            !
            NFILE0 = steps%simulCalls(i)%input%do_output
            DG = steps%simulCalls(i)%input%dgf

            ! preempt round-off errors due to IO format
            resid = DG(1,1)+DG(2,2)+DG(3,3)
            if (dabs(resid) .GT. 1.D-9) then
                  resid = resid / 3.D0
                  do j=1,3
                        DG(j,j) = DG(j,j) - resid
                  enddo
            endif
            
            ! Run simul.
            call SIMUL(1,steps%eps,NFILE0,NUNIT)

      !!! FIXME !!!  
/*
            if (cnf%simulCalls(i)%keep_texture) then
                  !      CALL LEESOR(NUNIT,MPOINT)
                  call useUDyn()  ! AKA: call fleesor
            else
                  call updateUDyn()      
            endif
*/
      !!! FIXME !!!  

      enddo
      

      
      end subroutine

      
      subroutine outputCurrentTexture(info)
      implicit none
      integer,intent(out)           :: info

/*      
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      if (ireason == 4) then
!
!     Output of last "current situation"
!
#ifdef WITHSMTFILE
! <jg>: Open file for output SMT 
            cods1(L+1:L+4)='.smt'
            open(unit=NUMIC,file=cods1,status='replace',action='write')
#endif      
#ifdef WITHCUBFILE
            cods1(L+1:L+4)='.cub'
            open(unit=NUCUB,file=cods1,status='replace',                 &
                  form='UNFORMATTED',action='write')
#endif
! </jg>
            NFILE0 = 1
            CALL SIMUL(2,1,EPS,NFILE0,NUNIT,0)

#ifdef WITHSMTFILE
            close(NUMIC)
#endif      
#ifdef WITHCUBFILE        
            close(NUCUB)
#endif
      
      endif
*/      
      
      
            info = 1
      
      
      end subroutine
      
      
      
      subroutine outputCurrentState(info)
      implicit none
      integer,intent(out)           :: info
      
            info = 1
            
      end subroutine
      
      
      
      
end module


