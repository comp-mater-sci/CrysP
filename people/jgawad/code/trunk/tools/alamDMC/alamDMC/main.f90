!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2011-09-19
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
      type(MapItem),dimension(ncommands)  :: command_map =  [ MapItem('alamQ',Q_id), MapItem('alamTSA',TSA_id), &
                                                              MapItem('alamASR',ASR_id), MapItem('alamYld',Yld_id) ]
      integer,parameter       :: argc_min = 2, argc_max=2, command_argpos = 1
      type(commandLine)       :: cmdline
      
      logical                 :: moduleFound = .false.
      character(len=32)       :: moduleName = ''
      !
      type(altayConfigData)   :: cnf
      type(TSAConfig)         :: TSAcnf
      type(YldConfig)         :: Yldcnf
      type(QConfig)           :: Qcnf
      type(ASRConfig)         :: ASRcnf
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
      write(*,'(A)') 'AlamDMC: $Rev$'
      !
      ! open and read the config file      
      write(*,'(/,A,1X,A,/)') 'Processing config file', trim(cmdline%argv(2))
      open(cnfunit,file=trim(cmdline%argv(2)),status='old',iostat=ioerr)
      if (ioerr /= 0) then
            write(*,*) 'Cannot open config file: ', trim(cmdline%argv(2))
            call finalize(stopcode_inputerror)
      endif
      !
      call readAlamelConfigSection(cnfunit,cnf,info)
      if (info /= 0) then
            write(errmsg,fmt=901) 'check ALAMEL config section'
            call finalize(stopcode_runtimeerror) 
      endif
      !
      ! Read multilevelYLP configuration
      call readYLPConfigSection(cnfunit,info)
      if (info /= 0) then
            write(errmsg,fmt=901) 'check YLP config section' 
            call finalize(stopcode_runtimeerror) 
      endif
      !
      info = -1            
      select case(cmdline%command_id)
      case(Q_id) ! Alamq
            call Alamq_ReadConfig(Qcnf,cnfunit,info)
            ! Override the requests for outputs: 
            cnf%output_config%nfile = 0   ! texture
            cnf%output_config%npebp = 0   ! KOST1x state
            outputRequest = .false.       ! idem.
      case(TSA_id) ! AlamTSA    
            call AlamTSA_ReadConfig(TSAcnf,cnfunit,info)
      case(ASR_id) ! AlamASR 
            call AlamASR_ReadConfig(ASRcnf,cnfunit,info)
      case(Yld_id) ! AlamYld
            call AlamYld_ReadConfig(Yldcnf,cnfunit,info)
      end select
      close(cnfunit)
      !
      if (info /= 0) then
            write(errmsg,901) 'Module configuration section'
            call finalize(stopcode_runtimeerror)
      endif
      !
      ! OK, configuration has been finished. 
      ! Initialize ALAMEL
      !
      
      cnf%jobtitle = trim(cnf%output_prefix)//' '//trim(moduleName)
      write(*,fmt=30) 'Initializing the multilevel model...'
      call initAltay(cnf,info)
      if (info == 0) then
            write(*,fmt=31) 'Done.'
      else
            write(*,fmt=31) 'Failed.'
            write(errmsg,'(A)')  'Fatal error: cannot initialize the multilevel model.'
            call finalize(stopcode_runtimeerror)
      endif
       
      ! Show general configuration of the multilevel model
      call displayConfig(display_unit,info)
      !
      ! Output the initial state variables (texture etc) if requested.
      if (outputRequest) then
            call outputTexture(info)
            if (info /= 0) then
                  write(errmsg,'(A)') 'Error: cannot write initial state'
                  call finalize(stopcode_runtimeerror)
            endif
      endif
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Run the module
      select case(cmdline%command_id)
      case(Q_id) ! Alamq
            call Alamq_Run(Qcnf,info)
      case(TSA_id) ! AlamTSA    
            call AlamTSA_Run(TSAcnf,info)
      case(ASR_id) ! AlamASR 
            call AlamASR_Run(ASRcnf,info)
      case(Yld_id) ! AlamYld
            call AlamYld_Run(Yldcnf,info)
      end select
      !
      write(*,'(A,1X,I8,1X,A)') 'Objective function was called', alamEval_objFx_call_count, 'times'
      write(*,'(A,1X,A,1X,A,\)') 'Execution of module', trim(moduleName), 'finished'
      if (info == 0) then
            write(*,'(1X,A)') 'succesfully.'
      else
            write(*,'(1X,A)') 'with errors.'
      endif

      call finalizeAltay(info)
      if (info /= 0) then
            write(*,'(A)') 'Problems have been encountered while finalizing libaltay'
      endif
      
      30 format(A,\)
      31 format(1X,A)


#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      
end program
