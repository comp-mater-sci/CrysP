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

#include "altayRCM.fpp"

module altaySub

      !>@{ \name Error codes in altaySub
      integer,parameter :: altaySub_OK = 0
      integer,parameter :: altaySub_Err = -1
      integer,parameter :: altaySub_Exception = -2
      integer,parameter :: altaySub_IOErr = -3
      !>@}
      
contains

      !> Initialize 
      subroutine initAltay(cnf,info)
      use altayConfig, only: altayConfigData,fname_len
      use altayInterface
      use altayRCM
      use IOConfig
      use TexFormats
      implicit none
      !
      type(altayConfigData),intent(in)    :: cnf
      integer,intent(out)                 :: info
      !
      character(len=fname_len) :: fnam2, cods1 
      character(len=8)  :: codsim
      integer :: ierr
      integer,parameter :: extlen = 4
      
     integer,parameter :: MPOINT = 8000, NUNIT = 2
      
      !data NUMIC/122/,NUCUB/123/
      integer,parameter :: NUMIC = 122, NUCUB = 123
      
      integer :: L
      double precision :: EPS
      !
            info = altaySub_IOErr
!
            codsim = trim(cnf%output_prefix)
            L=len_trim(codsim)
            ! Precaution against buffer overflow:
            if (L+extlen > fname_len) L = fname_len - extlen
            cods1=codsim(1:L)
#ifndef NOLSTFILE
            if (cnf%output_config%nlist /= 0) then
                  cods1(L+1:L+4)='.LST'
                  ! UNIT IMP = PRINTER
                  open (unit=IMP,file=cods1,status='replace')
            endif
#endif
!
#ifndef NORESFILE
            if (cnf%output_config%nres /= 0) then
                  cods1(L+1:L+4)='.RES'
                  open (unit=IMP2,file=cods1,status='replace')
            endif
#endif
!
#ifndef NOTWNFILE
            if (cnf%output_config%nfiltw /= 0) then
                  cods1(L+1:L+4)='.TWN'
                  open (unit=IMP3,file=cods1,status='replace')
            endif
#endif  
            fnam2 = trim(cnf%slipsystem%input_fname)
            ! UNIT LEC = SLIP SYSTEMS
            open (unit=LEC,file=TRIM(fnam2),status='old',iostat=ierr)
            
            if (ierr /= 0) return
!
#ifndef NOCURFILE
            if (cnf%output_config%nfile) then
            ! if (cnf%output_config%use_curfile) then
                  cods1(L+1:L+4)='.CUR'
                  ! IMP1=output file with successive "current situations"
                  open (unit=IMP1,file=cods1,status='replace')
            endif
#endif

            info = altaySub_Exception
      
            CALL GRFIL(info)
            if (info /= 0) return
            RCM_HANDLE(info)
      
            !
            ! Initialisation of SIMUL
            !
            EPS = 0.D0
            CALL SIMUL(0,EPS,1)
            RCM_HANDLE(info)

            !
            ! No need for the slip system definition anymore.
            close(LEC)
#ifdef USE_LEESOR
            ! Get the initial texture
            CALL LEESOR(NUNIT,MPOINT)
            RCM_HANDLE(info)
#else
            call loadTexture(cnf%texture%input_type,NDAT1,trim(cnf%texture%input_fname),cnf%texture%block_id,info)
            call xleesor()
#endif
      !
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
            steps%this = 0
            ! No need to specifically initialize other components,
            ! since there are initializers provided in the datatype.
            info = ierr
      !
      end subroutine
      
      
      subroutine runSteps(steps,info)
      use altayConfig, only: altayStateData
      use altayInterface
      use altayRCM
      use IOConfig
      implicit none
      type(altayStateData),intent(inout)        :: steps
      integer,intent(out)                       :: info
      ! We need this common block just for the DG tensor.
      COMMON /TEXTUR/ DUM1(29),IDUM1,DG(3,3),ITW,DELTAW,GEWF
      double precision :: DUM1,DG, DELTAW,GEWF
      integer :: IDUM1,ITW
      integer :: NFILE0
      !
      integer :: i,j
      double precision :: resid
      
      !
      ! TODO: check validity of the inputs (priority: size of the array!!)
     
            info = altaySub_Exception
      
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
                  call SIMUL(1,steps%eps,NFILE0)
                  if (RCM_signal()) then
                        RCM_RAISE(info,'runSteps','SIMUL has thrown exception',RCM_RTN) 
                  endif

            enddo
      
            info = altaySub_OK
      !
      end subroutine

      

      
      
      
end module


