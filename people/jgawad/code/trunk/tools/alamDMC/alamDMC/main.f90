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
implicit none
      integer                 :: info
      integer                 :: i
      integer                 :: argc
      integer,parameter       :: argc_min = 2, argc_max=2
      character(len=128)      :: argv(0:argc_max)
      integer                 :: ioerr
      integer,parameter       :: cnfunit = 90, ofunit = 91
      !
      !
      integer,parameter       :: nmodules = 4
      character(len=20),dimension(nmodules) :: moduleNames = [character(len=20) :: 'alamQ','alamTSA','alamASR','alamYld']
      logical                 :: moduleFound = .false.
      integer                 :: moduleId = 0
      !
      type(altayConfigData)   :: cnf
      type(TSAConfig)         :: TSAcnf
      type(YldConfig)         :: Yldcnf
      type(QConfig)           :: Qcnf
      !
      info = 1
      ioerr = 0
      !
      ! Print banner
      write(*,'(A)') 'AlamDMC: $Rev$ $Date$ '
      !
      argc = command_argument_count()
      if (argc < argc_min) then
            write(*,'(/,A)') 'Two parameters are required:  module_name configuration_file'
            call listModules()
            call finalize(1)
      endif
      do i=1,argc_max
            call get_command_argument(i,argv(i))
      enddo
      ! Check module name
      moduleFound = .false.
      do moduleId = 1,nmodules
            if (trim(argv(1)) == trim(moduleNames(moduleId))) then
                  moduleFound = .true.
                  write(*,'(A,1X,A)') 'Selected module:',trim(moduleNames(moduleId))
                  exit
            endif
      enddo

      if (.not. moduleFound) then
            write(*,'(A,1X,A)') 'Unknown name of module:',trim(argv(1))
            call listModules()
            call finalize(1)
      endif
      !
      ! open and read the config file      
      write(*,'(/,A,1X,A,/)') 'Processing config file', trim(argv(2))
      open(cnfunit,file=trim(argv(2)),status='old',iostat=ioerr)
      if (ioerr /= 0) then
            write(*,*) 'Cannot open config file: ', trim(argv(2))
            call finalize(1)
      endif
      !
      call readAlamelConfigSection(cnfunit,cnf,info)
      if (info /= 0) then
            write(*,fmt=901) 'check ALAMEL config section'
            call finalize(1) 
      endif
      !
      ! Read multilevelYLP configuration
      call readYLPConfigSection(cnfunit,info)
      if (info /= 0) then
            write(*,fmt=901) 'check YLP config section' 
            call finalize(1) 
      endif
      !
      info = -1            
      select case(moduleId)
      case(1) ! Alamq
            call Alamq_ReadConfig(Qcnf,cnfunit,info)
            ! Override the requests for outputs: 
            cnf%output_config%nfile = 0   ! texture
            cnf%output_config%npebp = 0   ! KOST1x state
            outputRequest = .false.       ! idem.
      case(2) ! AlamTSA    
            call AlamTSA_ReadConfig(TSAcnf,cnfunit,info)
      case(3) ! AlamASR 
            call AlamASR_ReadConfig(cnfunit,info)
      case(4) ! AlamYld
            call AlamYld_ReadConfig(Yldcnf,cnfunit,info)
      end select
      close(cnfunit)
      !
      if (info /= 0) then
            write(*,901) 'Module configuration section'
            call finalize(1)
      endif
      !
      ! OK, configuration has been finished. 
      ! Initialize ALAMEL
      !
      cnf%jobtitle = trim(cnf%output_prefix)//' '//trim(moduleNames(moduleId))
      write(*,fmt=30) 'Initializing the multilevel model...'
      call initAltay(cnf,info)
      if (info == 0) then
            write(*,fmt=31) 'Done.'
      else
            write(*,fmt=31) 'Failed.'
            write(*,'(A)')  'Fatal error: cannot initialize the multilevel model.'
            call finalize(1)
      endif
       
      ! Show general configuration of the multilevel model
      call displayConfig(display_unit,info)
      !
      ! Output the initial state variables (texture etc) if requested.
      if (outputRequest) then
            call outputTexture(info)
            if (info /= 0) then
                  write(*,*) 'Error: cannot write initial state'
                  call finalize(1)
            endif
      endif
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Run the module
      select case(moduleId)
      case(1) ! Alamq
            call Alamq_Run(Qcnf,info)
      case(2) ! AlamTSA    
            call AlamTSA_Run(TSAcnf,info)
      case(3) ! AlamASR 
            call AlamASR_Run(info)
      case(4) ! AlamYld
            call AlamYld_Run(Yldcnf,info)
      end select
      !
      write(*,'(A,1X,I8,1X,A)') 'Objective function was called', alamEval_objFx_call_count, 'times'
      write(*,'(A,1X,A,1X,A,\)') 'Execution of module', trim(moduleNames(moduleId)), 'finished'
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


      contains 
      
            subroutine listModules()
            implicit none
            integer :: i
                  write(*,'(A,1X)') 'Available modules:'
                  write(*,'(A,1X)') (trim(moduleNames(i)), i =1,nmodules)           
            end subroutine

end program
