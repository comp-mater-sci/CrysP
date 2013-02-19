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
use nllsTR
use Kutils
use alamYLP
use alamEval, only: alamEval_objFx_call_count
use alamUtils
use altaySub
use altayConfig, only: altayConfigData
use commonConfig
use commonUtils
!
use alamASR
use alamQ
use alamTSA
use alamYld
!
use fngRuntime
!
implicit none
      
      
      integer,parameter       :: cnfunit = 90, ofunit = 91
      !
      !
      integer,parameter       :: ncommands = 4
      integer,parameter       :: Q_id = 1, TSA_id = 2, ASR_id = 3, Yld_id = 4
      type(MapItem),dimension(ncommands)  :: command_map =  [ MapItem('Q',Q_id), MapItem('TSA',TSA_id), &
                                                              MapItem('ASR',ASR_id), MapItem('Yld',Yld_id) ]
      integer,parameter       :: argc_min = 2, argc_max=2, command_argpos = 1
      type(commandLine)       :: cmdline
      
      logical                 :: moduleFound = .false.
      character(len=32)       :: moduleName = ''
      !
      class(BasicModule),pointer     :: the_module => null()      
      !
      integer                 :: info, ioerr
      !
      info = 1
      ioerr = 0
      !
      cmdline = commandLine('AlamDMC ' //'$Rev$',description='Parameters: command configuration_file')
      call processCommandLine(cmdline,argc_min,argc_max,command_map,command_argpos,info,terminate=.true.)
      moduleFound = .false.
      if (info == fngSuccess) moduleFound = resolveId(command_map, cmdline%command_id,moduleName)
      if ((info /= fngSuccess) .or. (.not. moduleFound)) then
            errmsg = 'Error in processing the command line'
            call finalize(stopcode_inputerror)
      endif
      ! Print banner
      write(display_unit,'(A)') 'AlamDMC: $Rev$'
      !
      ! open and read the config file      
      write(display_unit,'(/,A,1X,A,/)') 'Processing config file', trim(cmdline%argv(2))
      open(cnfunit,file=trim(cmdline%argv(2)),status='old',iostat=ioerr)
      if (ioerr /= 0) then
            write(display_unit,*) 'Cannot open config file: ', trim(cmdline%argv(2))
            call finalize(stopcode_inputerror)
      endif
      !
      !
      info = -1
      ! Create a module of appropriate type and read its configuration:
      select case(cmdline%command_id)
      case(Q_id) ! Alamq
            allocate(QModule :: the_module)
      case(TSA_id) ! AlamTSA
            allocate(TSAModule :: the_module)
      case(ASR_id) ! AlamASR
            allocate(ASRModule :: the_module)
      case(Yld_id) ! AlamYld
            allocate(YldModule :: the_module)
      end select
      
      if (.not. associated(the_module)) then
            write(errmsg,'(A)')  'Internal error: cannot instantiate requested module.'
            call finalize(stopcode_runtimeerror)
      endif
      !
      info = the_module%ReadConfig(cnfunit)
      close(cnfunit)
      if (info /= 0) then
            call finalize(stopcode_runtimeerror)
      endif
      !
      ! OK, the configuration stage has been finished. 
      ! Initialize the micro-scale model
      !
      
      if (the_module%initialize() /= 0) then
            write(errmsg,'(A)')  'Fatal error: cannot initialize the multilevel model.'
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
