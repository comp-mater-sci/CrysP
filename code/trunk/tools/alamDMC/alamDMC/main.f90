!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2010-11-03
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
implicit none
      ! Strain rate and stress tensors in Material coordinate system and "Tensile sample"
      ! coordinate system
      integer                 :: info, iw
      integer                 :: i,j
      integer                 :: argc
      integer,parameter       :: argc_min = 2, argc_max=2
      character(len=128)      :: argv(0:argc_max)
      integer                 :: ioerr
      integer,parameter       :: cnfunit = 90, ofunit = 91
      !
      !
      integer,parameter       :: nmodules = 3
      character(len=20),dimension(nmodules) :: moduleNames = ['alamq','alamsra','alamasr']
      logical                 :: moduleFound = .false.
      integer                 :: moduleId = 0
      !
      info = 1
      iw = 3
      !
      ! Print banner
      write(*,'(A)') 'AlamDMC: $Id$'
      !
      argc = command_argument_count()
      if (argc < argc_min) then
            write(*,*) 'two parameters are required:  module_name configuration_file'
            write(*,*) 'Available modules:'
            write(*,*) (trim(moduleNames(i)), i =1,nmodules)           
            stop
      endif
      call get_command_argument(1,argv(1))
      ! Check module name
      moduleFound = .false.
      do moduleId = 1,nmodules
            if (trim(argv(1)) == moduleNames(moduleId)) then
                  moduleFound = .true.
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
      write(*,'(/,A,1X,A,/)') 'Processing config file', trim(argv(1))
      open(cnfunit,file=trim(argv(1)),status='old',iostat=ioerr)
      if (ioerr /= 0) then
            write(*,*) 'Cannot open config file: ', trim(argv(1))
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
            
      select case(moduleId)
      
      case(1) ! Alamq
      !      call Alamq_ReadConfig(cnfunit,info)
      case(2) ! AlamSRA    
      !      call AlamSRA_ReadConfig(cnfunit,info)
      case(3) ! AlamASR 
      !      call AlamASR_ReadConfig(cnfunit,info)      
      end select

      close(cnfunit)
      901 format('Incorrect format of configuration file:',1X,A)

end program
