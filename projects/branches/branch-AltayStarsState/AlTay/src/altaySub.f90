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
!>    * The code inside this file was initially based on Main1.for.
!>      Now it is only loosely related to its predecessor.
!
!>    \file altaySub.f90 ALAMEL as a subroutine

#include "altayRCM.fpp"

!> API for "AlTay as a subroutine"
module altaySub
use criErrcodes
implicit none

      !> \name Named constants for error codes in altaySub (OBSOLETE)
      !>@{ 
      ! TODO: replace altaySub_Exception with a meaningful alternative
      integer,parameter :: altaySub_Exception = criError
      !>@}
      
contains

    !> Initialize the module.
    !>
    !> This subroutine must be called prior to any call to other
    !> module subroutines.
    subroutine initAltay(cnf, state, info, errmsg)
    use altayConfig, only: altayConfigData,fname_len,acnf
    use altayState
    use altaySimul
    use altayRCM
    use altayIOConfig
    use altayTexFormats
    use altayHard,only: hard_BP,hard_PEBPscrew,hard_PEBPloop,InitModuleAltayHard
    use altayMesostructure
#ifdef PEBP_ENABLED
    use AltayDSHstate
#endif
    implicit none
    !
    type(altayConfigData),intent(in)    :: cnf      !< configuration data
    type(altayStateData),intent(out)    :: state    !< state variables
    integer,intent(out)                 :: info     !< exit code (criSuccess on success)
    character(len=*),intent(out),optional :: errmsg !< Error message (set if info /= criSuccess)
    !
    integer :: ierr
    type(InterfaceDataset) :: interface_dataset
    !
        if (present(errmsg)) errmsg = ''
        ierr = 0
        ! Set the singleton object to the cnf
        acnf = cnf
        !
        info = criErr_BadArgs
        if (altayStateData_init(state) /= criSuccess) return
        !
        ! Open input files
        !
        ! UNIT LEC = SLIP SYSTEMS
        open (unit=LEC,file=trim(cnf%slipsystem%input_fname),status='old',iostat=ierr)
        if (ierr /= 0) then
            if (present(errmsg)) errmsg = 'Cannot open slip system definition file: ' // trim(cnf%slipsystem%input_fname)
            info = criErr_IO
            return
        endif
        !
        if (info /= 0) then
                if (present(errmsg)) errmsg = 'Cannot process the microstructure file: ' // trim(acnf%micros_fname)
                info = criErr_IO
                return
        endif
        !
        ! Get the initial texture
        call loadTexture(cnf%texture%input_type, trim(cnf%texture%input_fname), &
                            cnf%texture%block_id, state%old%mesostructure%deformationgradient, state%old%texture, info)
        if (info /= 0) then
                if (present(errmsg)) errmsg = 'Cannot process the texture data file: ' // trim(cnf%texture%input_fname)
                info = criErr_IO
                return
        endif
        !
        ! Update meso deformation gradient with input from config
        call state%old%mesostructure%deformationgradient%update(config%simul_init%FMicro, info)
        !
        ! Get the interface data
        call InterfaceDataset_readfromSMTfile( interface_dataset, acnf%micros_fname, info)
        ! interface_dataset: currently not yet exploited, leaving altaySub broken.
        !
        ! Open output files
        !
        call openOutputFiles(cnf, info, errmsg)
        if (info /= criSuccess) return
        !
        ! Initialize altay modules
        !
        ! Set the data for CRSS calculations
        call InitModuleAltayHard(cnf%hardening, info) 
        if (info /= 0) then
                if (present(errmsg)) errmsg = 'Cannot initialize hardening law'
                info = criError
                return
        endif
        !
        ! Initialisation of SIMUL
        if (present(errmsg)) errmsg = 'Initialization call to the micromechanical model failed.'
        info = altaySub_Exception
        CALL SIMUL(state,0,1)
        RCM_HANDLE(info)
        if (present(errmsg)) errmsg = ''
        !
#ifdef PEBP_ENABLED
        ! PEBP model
        select case(cnf%hardening%hardLawID)
        case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
                info = KS_initState(size(DFIL))
                if (info /= 0) return
                if (acnf%hardening%PEBPCnf%read_state) then 
                    ! Load state variables
                    info = KS_openStateFile(IPEBPSTAT,acnf%hardening%PEBPCnf%input_fname, mode='r')
                    if (info /= 0) then
                            if (present(errmsg)) errmsg = 'Cannot open PEBP state file: ' & 
                                                        // trim(acnf%hardening%PEBPCnf%input_fname) 
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
        info = altayStateData_assemble(state)
    !
    end subroutine
      
      !> Finalizes the module and releases the resources.
      subroutine finalizeAltay(info)
      use altayConfig, only: altayConfigData,fname_len,acnf, astate
      use altayIOConfig
      use altayMesostructure, only: MICROSTR_finalize
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
            close(IMP6)
            call MICROSTR_finalize(info)
            if (info /= 0) return
#ifdef PEBP_ENABLED
            info = KS_finalize()
            close(IPEBPSTAT)
            close(IPEBPSDV)
#endif
            ! Finalize altayConfig
            if (allocated(astate%simulCalls)) then
                  deallocate(astate%simulCalls)
                  astate%nSimulCalls = 0
            endif
      !
      end subroutine
      
      subroutine openOutputFiles(cnf, info, errmsg)
      use altayConfig, only: altayConfigData,fname_len
      use altayIOConfig
      use altayMiscutils
#ifdef PEBP_ENABLED
      use AltayDSHstate
#endif
      implicit none
      type(altayConfigData),intent(in)    :: cnf      !< configuration data 
      integer,intent(out)                 :: info     !< exit code (criSuccess on success)
      character(len=*),intent(out),optional :: errmsg !< Error message (set if info /= criSuccess)
      !
      character(len=fname_len) :: fname_prefix, fname
      !
            fname_prefix = cnf%output_prefix
            info = criErr_IO
#ifndef NOLSTFILE
            ! UNIT IMP = PRINTER
            if (cnf%output_config%nlist /= 0) then
                  fname = trim(fname_prefix)//'.LST'
                  open(unit=IMP,file=fname,status='replace',err=9999)
            endif
#endif
!
#ifndef NORESFILE
            if (cnf%output_config%nres /= 0) then
                  fname = trim(fname_prefix)//'.RES'
                  open(unit=IMP2,file=fname,status='replace',err=9999)
            endif
#endif
!
#ifndef NOTWNFILE
            if (cnf%output_config%nfiltw /= 0) then
                  fname = trim(fname_prefix)//'.TWN'
                  open (unit=IMP3,file=fname,status='replace',err=9999)
            endif
#endif
!
#ifndef NOCURFILE
            if (cnf%output_config%nfile /= 0) then
                  fname = trim(fname_prefix)//'.CUR'
                  ! IMP1=output file with successive "current situations"
                  open (unit=IMP1,file=fname,status='replace',err=9999)
            endif
#endif
            if (cnf%output_config%NMSS /= 0) then
                  fname = trim(fname_prefix)//'.MSS'
                  ! UNIT IMP5 = homogenized strain-stress
                  open (unit=IMP5,file=fname,status='replace',err=9999)
                  call writeMSSHeader(IMP5,info)
            endif
!
#if defined(PEBP_ENABLED) .and. .not. defined(NOBEPFILE)
            if (cnf%output_config%npebp /= 0) then 
                  ! PEBP model
                  ! UNIT IMP4 = state variables of PEBP
                  fname = trim(fname_prefix)//'.BPM'
                  info = KS_openStateFile(IMP4,fname=fname,mode='w')
                  if (info /= 0) then
                        if (present(errmsg)) errmsg = 'Cannot create PEBP state file: ' // fname
                        info = criErr_IO
                        return
                  endif
            endif
#endif
            !
            ! Successful end of processing
            info = criSuccess
            return
            !
            ! Error handler
            9999 continue
            info = criErr_IO
            if (present(errmsg)) errmsg = 'Cannot open file '//trim(fname)
      !
      end subroutine
      
      !> Initialization of input and output data for the steps.
      !>
      !> 
      subroutine initStepData(nsteps,steps,info)
      use altayConfig, only: altayOutputData
      implicit none
      integer,intent(in)                  :: nsteps   !< Number of steps to be created
      type(altayOutputData),intent(out)    :: steps    !< Definiton of the steps.
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
      subroutine runSteps(state, steps,info)
      use altayConfig, only: altayOutputData,astate
      use altaySimul
      use altayRCM
      use altayIOConfig
      use altayMacroKinematic
      implicit none
      type(altayStateData),intent(inout)        :: state    !< state variables
      type(altayOutputData),intent(inout)       :: steps !< Definiton of the steps.
      integer,intent(out)                       :: info  !< Exit code: 0 on success.
      integer :: NFILE0
      !
      integer :: i
      logical :: input_ok
      type(DeformationRate) :: MacroDefRate
      !
            ! Validate input
            info = criErr_BadArgs
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
                  
                  !Set the macro velocity gradient in module MacroKinematic
                  call Set_DeformationRate(steps%simulCalls(i)%input%dgf,MacroDefRate)

                  ! Run simul.
                  call SIMUL(state, 1, NFILE0, MacroDefRate)
                  if (RCM_signal()) then
                        RCM_RAISE(info,'runSteps','SIMUL has thrown exception',RCM_RTN) 
                  endif

                  if (steps%simulCalls(i)%input%do_output_final) call outputCurrentState(state%new, info)
            enddo
      
            info = criSuccess
      !
      end subroutine

      !> Write out the current state variables.
      !>
      !> The call may involve IO units: IMP1 (CUR file), IMP4 (PEBP state file) and IMP5 (MSS file).
      !> Appropriate control fields in acnf%output_config are checked to decide if the data have to
      !> be actually written to corresponding IO units.  
      subroutine outputCurrentState(statevars, info)
      use altayIOConfig
      use altayCurAccess
      use altayState
      use altayConfig, only: acnf,astate
      use altayHard, only: hard_BP,hard_PEBPscrew,hard_PEBPloop
      use AltayDSHstate
      use altayMiscutils
      implicit none
      type(altayStateVariables),target,intent(in) :: statevars
      integer,intent(out)           :: info
      !
      type(TextureAssembly) :: assembly
            info = criSuccess
            if (acnf%output_config%nfile == 1) then
                  assembly = TextureAssembly(statevars%texture, statevars%mesostructure%deformationgradient)
                  call CURwriteBlock(assembly,IMP1,info)
            endif
            if (info /= 0) return
            !
#ifdef PEBP_ENABLED
            select case(acnf%hardening%hardLawID)
            case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
                if (acnf%output_config%npebp == 1) then
                      info = KS_writeState(IMP4)
                endif
            endselect
            if (info /= 0) return
#endif
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


