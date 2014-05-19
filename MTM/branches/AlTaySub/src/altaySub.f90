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
!>    \file altaySub.f90 ALAMEL as a subroutine

#include "altayRCM.fpp"

!> API for "AlTay as a subroutine"
module altaySub

      !> \name Named constants for error codes in altaySub
      !>@{ 
      integer,parameter :: altaySub_OK = 0
      integer,parameter :: altaySub_Err = -1
      integer,parameter :: altaySub_Exception = -2
      integer,parameter :: altaySub_IOErr = -3
      integer,parameter :: altaySub_BadVal = -10
      integer,parameter :: altaySub_BadDim = -11
      !>@}
      
contains

      !> Initialize the module.
      !>
      !> This subroutine must be called prior to any call to other
      !> module subroutines.
      subroutine initAltay(cnf,info,errmsg)
      use altayConfig, only: altayConfigData,fname_len,acnf
      use altaySimul
      use altayRCM
      use altayIOConfig
      use altayTexFormats
      use altayHard,only: hard_none,hard_voce,hard_BP,hard_PEBPscrew,hard_PEBPloop
      use altayMiscutils
      use altayMesostructure
#ifdef PEBP_ENABLED
      use AltayDSHstate
#endif
      implicit none
      !
      type(altayConfigData),intent(in)    :: cnf      !< configuration data 
      integer,intent(out)                 :: info     !< exit code (0 on success)
      character(len=*),intent(out),optional :: errmsg !< Error message (set if info /= 0)
      !
      character(len=fname_len) :: fnam2, cods1 
      character(len=fname_len) :: codsim
      integer :: ierr
      integer,parameter :: extlen = 4
      
      COMMON /IGLIJS/ FK1(2,96),M11,CC(2,96) ! Needed for FK1
      double precision :: FK1,CC
      integer :: M11
      
      integer :: L
      double precision :: EPS
      !
            info = altaySub_IOErr
            if (present(errmsg)) errmsg = ''
            ierr = 0
            ! Set the singleton object to the cnf
            acnf = cnf
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
            if (ierr /= 0) then
                  if (present(errmsg)) errmsg = 'Cannot open slip system definition file: ' // trim(fnam2)
                  return
            endif
!
#ifndef NOCURFILE
            if (cnf%output_config%nfile /= 0) then
            ! if (cnf%output_config%use_curfile) then
                  cods1(L+1:L+4)='.CUR'
                  ! IMP1=output file with successive "current situations"
                  open (unit=IMP1,file=cods1,status='replace')
            endif
#endif
!
#if defined(PEBP_ENABLED) .and. .not. defined(NOBEPFILE)
            if (cnf%output_config%npebp /= 0) then 
                  ! PEBP model
                  cods1(L+1:L+4)='.BPM'
                  ! UNIT IMP4 = state variables of PEBP KOST11
                  info = KS_openStateFile(IMP4,fname=cods1,mode='w')
                  if (info /= 0) then
                        if (present(errmsg)) errmsg = 'Cannot create PEBP state file: ' // trim(cods1)
                        return
                  endif
            endif
#endif
            if (acnf%output_config%NMSS /= 0) then
                  cods1(L+1:L+4)='.MSS'
                  ! UNIT IMP5 = homogenized strain-stress
                  open (unit=IMP5,file=cods1,status='replace')
                  call writeMSSHeader(IMP5,info)
            endif
            !
            info = altaySub_Exception
            !
            ! Set the data for CRSS calculations
            if (cnf%slipsystem%kost == hard_voce) then
                  FK1 = cnf%slipsystem%crss_ratios
            endif
            
            CALL GRFIL(acnf%micros_fname,acnf%simul_init%FMicro,info)
            if (info /= 0) then
                  if (present(errmsg)) errmsg = 'Cannot process the microstructure file: ' // acnf%micros_fname
                  return
            endif
            !
            ! Initialisation of SIMUL
            !
            if (present(errmsg)) errmsg = 'Initialization call to the micromechanical model failed.'
            EPS = 0.D0
            CALL SIMUL(0,EPS,1)
            RCM_HANDLE(info)
            if (present(errmsg)) errmsg = ''
            !
            ! Get the initial texture
            call loadTexture(cnf%texture%input_type,NDAT1,trim(cnf%texture%input_fname),cnf%texture%block_id,info)
            if (info /= 0) then
                  if (present(errmsg)) errmsg = 'Cannot process the texture data file: ' // trim(cnf%texture%input_fname)
                  return
            endif
            !
#ifdef PEBP_ENABLED
            ! PEBP model
            select case(cnf%slipsystem%kost)
            case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
                  info = KS_initState(size(DFIL))
                  if (info /= 0) return
                  if (acnf%hardening%PEBPCnf%read_state) then 
                        ! Load state variables
                        info = KS_openStateFile(IPEBPSTAT,acnf%hardening%PEBPCnf%input_fname, mode='r')
                        if (info /= 0) then
                              if (present(errmsg)) errmsg = 'Cannot open PEBP state file: '// trim(acnf%hardening%PEBPCnf%input_fname) 
                              return
                        endif
                        info = KS_readState(IPEBPSTAT,acnf%hardening%PEBPCnf%block_id)
                        if ((info /= 0) .and. present(errmsg)) then  
                              errmsg = 'Cannot read from PEBP state file: '// trim(acnf%hardening%PEBPCnf%input_fname)
                              return
                        endif
                  endif
            endselect      
#endif
            !
            ! No need for the slip system definition anymore.
            close(LEC)
      !
      end subroutine
      
      !> Finalizes the module and releases the resources.
      subroutine finalizeAltay(info)
      use altayConfig, only: altayConfigData,fname_len,acnf, astate
      use altayIOConfig
      use altayMesostructure, only: MICROSTR_finalize
      use altayDynfil
#ifdef PEBP_ENABLED
      use AltayDSHstate
#endif
      implicit none
      integer,intent(out)                 :: info     !< exit code (0 on success)
      !
            ! Close all units.
            close(LEC)
            close(KLEC)
            close(IMP)
            close(IMP1)
            close(IMP2)
            close(IMP3)
#ifdef PEBP_ENABLED
            close(IMP4) 
#endif
            close(IMP5)
            call MICROSTR_finalize(info)
            if (info /= 0) return
            call DYNFIL_finalize(info)
            if (info /= 0) return
#ifdef PEBP_ENABLED
            info = KS_finalize()
#endif
            ! Finalize altayConfig
            if (allocated(astate%simulCalls)) then
                  deallocate(astate%simulCalls)
                  astate%nSimulCalls = 0
            endif
      !
      end subroutine
      
      
      
      !> Initialization of input and output data for the steps.
      !>
      !> 
      subroutine initStepData(nsteps,steps,info)
      use altayConfig, only: altayStateData
      implicit none
      integer,intent(in)                  :: nsteps   !< Number of steps to be created
      type(altayStateData),intent(out)    :: steps    !< Definiton of the steps.
      integer,intent(out)                 :: info     !< Exit code: 0 on success
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
      
      !> Run the AlTay for the set of steps
      subroutine runSteps(steps,info)
      use altayConfig, only: altayStateData,astate
      use altaySimul
      use altayRCM
      use altayIOConfig
      implicit none
      type(altayStateData),intent(inout)        :: steps !< Definiton of the steps.
      integer,intent(out)                       :: info  !< Exit code: 0 on success.
      ! We need this common block just for the DG tensor.
      COMMON /TEXTUR/ DUM1(27),IDUM1,DG(3,3),ITW,GEWF
      double precision :: DUM1,DG,GEWF
      integer :: IDUM1,ITW
      integer :: NFILE0
      !
      integer :: i,j
      double precision :: resid
      !
      logical :: input_ok
      !
            ! Validate input
            info = altaySub_BadVal
            input_ok = .false.
            if (allocated(steps%simulCalls)) then
                  input_ok = (size(steps%simulCalls) == steps%nSimulCalls)
            endif
            if (.not. input_ok) return
            !
            ! Assign steps with astate
            astate = steps
            !            
            info = altaySub_Exception
            !
            do i = 1, steps%nSimulCalls
                  steps%this = i
                  !
                  if (steps%simulCalls(i)%input%do_output_init) then
                        NFILE0 = 1
                  else
                        NFILE0 = 0
                  endif
                  
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

                  if (steps%simulCalls(i)%input%do_output_final) call outputCurrentState(info)
            enddo
      
            info = altaySub_OK
      !
      end subroutine

      !> Write out the current state variables.
      !>
      !> The call may involve IO units: IMP1 (CUR file), IMP4 (PEBP state file) and IMP5 (MSS file).
      !> Appropriate control fields in acnf%output_config are checked to decide if the data have to
      !> be actually written to corresponding IO units.  
      subroutine outputCurrentState(info)
      use altayIOConfig
      use altayCurAccess
      use altayConfig, only: acnf,astate
      use altayHard, only: hard_BP,hard_PEBPscrew,hard_PEBPloop
      use AltayDSHstate
      use altayMiscutils
      implicit none
      integer,intent(out)           :: info
      !
            info = altaySub_OK
            if (acnf%output_config%nfile == 1) then
                  call CURwriteBlock(IMP1,info)
            endif
            if (info /= 0) return
            !
            select case(acnf%slipsystem%kost)
            case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
                if (acnf%output_config%npebp == 1) then
                      info = KS_writeState(IMP4)
                endif
            endselect
            if (info /= 0) return
            !
            if ((acnf%output_config%nmss == 1) .and. allocated(astate%simulCalls)) then
                  !
                  associate (callout => astate%simulCalls(astate%this)%output)
                        call writeMSSRecord(IMP5, &
                                            callout%effective_macro_strain, callout%effective_macro_strain_tot, &
                                            callout%homogenised_slip,callout%homogenised_slip_tot, &
                                            callout%stress_tensor, &
                                            callout%taylor_factor, callout%strain_rate_heterogeneity, &
                                            info)
                  end associate
                  !
            endif
      !
      end subroutine

      
      
      
end module


