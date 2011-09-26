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
use AlamelSub
use alamelConfig
use nllsTR
use Kutils
use alamYLP
use alamEval, only: alamEval_objFx_call_count
use alamUtils
use commonConfig
!
use alamASR
use alamQ
use alamTSA
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
      integer,parameter       :: nmodules = 3
      character(len=20),dimension(nmodules) :: moduleNames = ['alamQ','alamTSA','alamASR']
      logical                 :: moduleFound = .false.
      integer                 :: moduleId = 0
      !
      info = 1
      !
      ! Print banner
      write(*,'(A)') 'AlamDMC: $Id$'
      !
      argc = command_argument_count()
      if (argc < argc_min) then
            write(*,'(/,A)') 'Two parameters are required:  module_name configuration_file'
            write(*,'(A,1X)') 'Available modules:'
            write(*,'(A,1X)') (trim(moduleNames(i)), i =1,nmodules)           
            stop
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
            write(*,*) 'Module name cannot be identified.'
      endif
      !
      call initAlamelStructures(info)
      if (info /= 0) then
            write(*,*) 'Error: Cannot initialize libAlamel'
            stop
      endif
      !
      ! open and read config file      
      write(*,'(/,A,1X,A,/)') 'Processing config file', trim(argv(2))
      open(cnfunit,file=trim(argv(2)),status='old',iostat=ioerr)
      if (ioerr /= 0) then
            write(*,*) 'Cannot open config file: ', trim(argv(2))
            stop
      endif
      !
      call readAlamelConfigSection(cnfunit,info)
      if (info /= 0) then
            write(*,fmt=901) 'check ALAMEL config section'
            stop 
      endif
      !
      ! Read multilevelYLP configuration
      call readYLPConfigSection(cnfunit,info)
      if (info /= 0) then
            write(*,fmt=901) 'check YLP config section' 
            stop 
      endif
      !
      info = -1            
      select case(moduleId)
      case(1) ! Alamq
            call Alamq_ReadConfig(cnfunit,info)
      case(2) ! AlamTSA    
            call AlamTSA_ReadConfig(cnfunit,info)
      case(3) ! AlamASR 
            call AlamASR_ReadConfig(cnfunit,info)      
      end select
      close(cnfunit)
      !
      if (info /= 0) then
            write(*,901) 'Module configuation section'
            stop
      endif
      !
      ! OK, configuration has been finished. 
      ! Initialize ALAMEL
      !
      ! Apply modifications to acnf:
      acnf%jobtitle = trim(acnf%output_prefix)//' '//trim(moduleNames(moduleId))
      call initAlamel()
      ! Show general configuration of the multilevel model
      call displayConfig(display_unit,info)
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Run the module
      select case(moduleId)
      case(1) ! Alamq
            call Alamq_Run(info)
      case(2) ! AlamTSA    
            call AlamTSA_Run(info)
      case(3) ! AlamASR 
            call AlamASR_Run(info)      
      end select
      !
      write(*,'(A,1X,I8,1X,A)') 'Objective function was called', alamEval_objFx_call_count, 'times'
      write(*,'(A,1X,A,1X,A,\)') 'Execution of module', trim(moduleNames(moduleId)), 'finished'
      if (info == 0) then
            write(*,'(1X,A)') 'succesfully.'
      else
            write(*,'(1X,A)') 'with errors.'
      endif
            

#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS


end program
