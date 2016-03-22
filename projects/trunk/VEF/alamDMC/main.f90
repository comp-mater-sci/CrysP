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
use alamYLP
use alamEval, only: alamEval_objFx_call_count
use dmcUtils
use altaySub
use altayConfig, only: altayConfigData
use commonConfig
use commonUtils
!
use dmcASR
use dmcQRS
use dmcUDSA
use dmcYld
use dmcEWC
!
use criRuntime
!
implicit none
      
      
      integer,parameter       :: cnfunit_default = 90, ofunit = 91
      !
      !
      integer,parameter       :: ncommands = 5
      integer,parameter       :: Q_id = 1, UDSA_id = 2, ASR_id = 3, YLD_id = 4, EWC_id = 5
      type(MapItem),dimension(ncommands)  :: command_map =  [ MapItem('QRS',Q_id), MapItem('UDSA',UDSA_id), &
                                                              MapItem('ASR',ASR_id), MapItem('YLD',YLD_id), &
                                                              MapItem('EWC',EWC_id) ]
      integer,parameter       :: argc_min = 2, argc_max=2, command_argpos = 1
      type(commandLine)       :: cmdline
      
      logical                 :: moduleFound = .false.
      character(len=32)       :: moduleName = ''
      !
      class(BasicModule),pointer     :: the_module => null()      
      !
      integer                 :: info, ioerr, cnfunit
      !
      character(len=errmsg_len) :: error_message
      character(len=128)  :: progname
      !
      info = 1
      ioerr = 0
      !
      write(progname,fmt=300)
      !
      cmdline = commandLine(progname,description='parameters: command_name configuration_file')
      call processCommandLine(cmdline,argc_min,argc_max,command_map,command_argpos,info,terminate=.true.)
      moduleFound = .false.
      if (info == criSuccess) moduleFound = resolveId(command_map, cmdline%command_id,moduleName)
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
      ! open and read the config file      
      write(display_unit,'(/,A,1X,A,/)') 'Processing config file', trim(cmdline%argv(2))
      cnfunit = openOrDie(fpath=trim(cmdline%argv(2)),status='old',unit=cnfunit_default)
      !
      info = -1
      ! Create a module of appropriate type and read its configuration:
      select case(cmdline%command_id)
      case(Q_id) ! dmcQRS
            allocate(QRSModule :: the_module)
      case(UDSA_id) ! dmcUDSA
            allocate(UDSAModule :: the_module)
      case(ASR_id) ! dmcASR
            allocate(ASRModule :: the_module)
      case(YLD_id) ! dmcYld
            allocate(YldModule :: the_module)
      case(EWC_id) ! dmcEWC
            allocate(EWCModule :: the_module)
      end select
      
      if (.not. associated(the_module)) then
            write(errmsg,'(A)')  'Internal error: cannot instantiate the requested module.'
            call finalize(stopcode_runtimeerror)
      endif
      !
      info = the_module%ReadConfig(cnfunit)
      close(cnfunit)
      if (info /= 0) then
            write(errmsg,'(A)') 'Configuration file contains errors.'
            call finalize(stopcode_runtimeerror)
      endif
      !
      ! OK, the configuration stage has been finished. 
      ! Initialize the micro-scale model
      !
      
      if (the_module%initialize() /= 0) then
            if (len(errmsg) == 0) then
                  errmsg = 'Fatal error: cannot initialize the multilevel model.'
            else
                  error_message = errmsg
                  errmsg = 'Fatal error during initialization of the multilevel model: ' // trim(error_message)
            endif
            call finalize(stopcode_runtimeerror)
      endif
       
      ! Show general configuration of the multilevel model
      info = the_module%printConfig(display_unit)
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Run the module
      call the_module%run(info)
      !
      write(display_unit,'(A,1X,I8,1X,A)') 'Objective function was called', alamEval_objFx_call_count, 'times'
      write(display_unit,'(A,1X,A,1X,A,\)') 'Execution of module', trim(moduleName), 'finished'
      if (info == 0) then
            write(display_unit,'(1X,A)') 'succesfully.'
      else
            write(display_unit,'(1X,A)') 'with errors.'
      endif

      call finalizeAltay(info)
      if (info /= 0) then
            write(display_unit,'(A)') 'Problems have been encountered while finalizing libaltay'
      endif
      


#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      
end program
