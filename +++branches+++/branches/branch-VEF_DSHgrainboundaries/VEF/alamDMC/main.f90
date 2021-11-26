!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2011-09-19
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!>    \file main.f90 in AlamDMC provides entry point for other modules.
!>
!
program alamDMC
!
use criRuntime
!
#if defined(DMC_USE_SLIS) || defined(DMC_USE_TOKENS)
use,intrinsic :: iso_c_binding, only: C_NULL_CHAR
use fslis
#define ALAMDMC_FEATURE_UUID 'ff921f1e-fa42-11e5-97dc-ecf4bb152acb'//C_NULL_CHAR
#endif
!
use dmcUtils, only: display_unit
use dmcBasicModule
use dmcASR
use dmcQRS
use dmcUDSA
use dmcYld
use dmcEWC
use dmcADP
!
implicit none
      !
      integer,parameter       :: ncommands = 6  !< total number of alamDMC modules (currently 6: QRS, UDSA, ASR, YLD, EWC, ADP)
      integer,parameter       :: Q_id = 1, UDSA_id = 2, ASR_id = 3, YLD_id = 4, EWC_id = 5, ADP_id = 6  !< numerical identifiers of alamDMC modules
      type(MapItem),dimension(ncommands)  :: command_map =  [ MapItem('QRS',Q_id), MapItem('UDSA',UDSA_id), &
                                                              MapItem('ASR',ASR_id), MapItem('YLD',YLD_id), &
                                                              MapItem('EWC',EWC_id), MapItem('ADP',ADP_id) ]
#ifndef DMC_USE_TOKENS
      ! 2 command line parameters when DMC_USE_TOKENS not defined
      integer,parameter             :: argc_min = 2, argc_max=2
      character(len=*),parameter    :: prog_desc = 'parameters: command_name configuration_file'
#else
      ! Additional parameter: token file
      integer,parameter       :: argc_min = 3, argc_max=3
      integer,parameter       :: tokenfile_argpos = 3
      character(len=*),parameter    :: prog_desc = 'parameters: command_name configuration_file token_file'
#endif
      integer,parameter       :: command_argpos = 1, & !< module identifier is 1st argument
                                 configfile_argpos = 2 !< configuration file is 2nd argument
      type(commandLine)       :: cmdline               !< type commandLine defined in criRuntime.f90
      
      logical                 :: moduleFound = .false.
      character(len=32)       :: moduleName = ''
      !
      class(BasicModule),pointer     :: the_module => null()
      !
      integer                 :: info, ioerr, cnfunit
      !
      character(len=128)  :: progname
      !
      info = criError !< see criErrcodes.f90
      ioerr = 0
      !
      write(progname,fmt=300)
      !
      cmdline = commandLine(progname,description=prog_desc) ! create commandLine type object with progname and description defined and assign to cmdline
      call processCommandLine(cmdline,argc_min,argc_max,command_map,command_argpos,info,terminate=.true.) ! call processCommandLine with 7 arguments, last one optional
      moduleFound = .false.
      if (info == criSuccess) moduleFound = resolveId(command_map, cmdline%command_id,moduleName) ! logical function defined in criLinearMap.f90: resolveId(themap,id,name[,index])
      if ((info /= criSuccess) .or. (.not. moduleFound)) then
            errmsg = 'Error in processing the command line'
            call finalize(stopcode_inputerror)
      endif
      ! Print the banner
      write(display_unit,fmt=300) 
#ifdef DMC_EXPERIMENTAL
      300 format('AlamDMC $Rev$',1X,'EXPERIMENTAL')
#else
      300 format('AlamDMC $Rev$')
#endif
!
#ifdef DMC_USE_SLIS
      if (initSlis('VEF_ROOT'//C_NULL_CHAR) /= 0) then
            write(display_unit,fmt=900) 'Cannot access the license. Check if "license.slis" file is in your VEF_ROOT'
            call finalize(stopcode_runtimeerror)
      endif
      if (.not. isLicenseValid(ALAMDMC_FEATURE_UUID, logical(.true.,kind=c_bool))) then
            write(display_unit,fmt=900) 'There is no valid license for AlamDMC'
            call finalize(stopcode_runtimeerror)
      endif
#endif
      !
      ! Configure the module
      !
      write(display_unit,'(/,A,1X,A,/)') 'Processing config file', trim(cmdline%argv(configfile_argpos)) ! display_unit = output_unit defined in dmcutils.f90 
      cnfunit = openOrDie(fpath=trim(cmdline%argv(configfile_argpos)),status='old') ! create new unit (handle) for config file; openOrDie(fpath,status[,unit]) defined in criRuntime.f90
      !
      info = -1
      ! Create a module of appropriate type:
      select case(cmdline%command_id)
      case(Q_id) ! dmcQRS
            allocate(QRSModule :: the_module) ! create object the_module of type QRSModule
      case(UDSA_id) ! dmcUDSA
            allocate(UDSAModule :: the_module)
      case(ASR_id) ! dmcASR
            allocate(ASRModule :: the_module)
      case(YLD_id) ! dmcYld
            allocate(YldModule :: the_module)
      case(EWC_id) ! dmcEWC
            allocate(EWCModule :: the_module)
      case(ADP_id) ! dmcSD
            allocate(ADPModule :: the_module)
      end select
      
      if (.not. associated(the_module)) then
            write(errmsg,'(A)')  'Internal error: cannot instantiate the requested module.'
            call finalize(stopcode_runtimeerror)
      endif
      !
      ! Read the configuration file:
      info = the_module%ReadConfig(cnfunit)
      close(cnfunit)
      if (info /= 0) then
            write(errmsg,'(A)') 'Configuration file contains errors.'
            call finalize(stopcode_runtimeerror)
      endif
#ifdef DMC_USE_TOKENS
      !
      ! Initialize token
      !
      if (the_module%token%setTokenPath(cmdline%argv(tokenfile_argpos)) /= criSuccess) then
          write(errmsg,'(A)') 'Incorrect path to the token file'
          call finalize(stopcode_runtimeerror)
      endif
#endif
      !
      ! OK, the configuration stage has been finished. 
      ! Initialize the module
      !
      if (the_module%initialize() /= criSuccess) then
            if (len(errmsg) == 0) errmsg = 'Cannot initialize the module.'
            call finalize(stopcode_runtimeerror)
      endif
       
      ! Show general configuration of the multilevel model
      info = the_module%printConfig(display_unit)
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Run the module
      call the_module%run(info)
      !
      write(display_unit,'(A,1X,A,1X,A)',advance='no') 'Execution of module', trim(moduleName), 'finished'
      if (info == 0) then
            write(display_unit,'(1X,A)') 'successfully.'
      else
            write(display_unit,'(1X,A)') 'with errors.'
      endif
      !
      ! Finalize the module
      info = the_module%finalize()
      !
      call finalize(info)

#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      
end program
