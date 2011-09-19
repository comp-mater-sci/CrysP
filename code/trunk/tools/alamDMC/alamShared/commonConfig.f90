! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2011-09-17
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!> This module contains subroutines, data structures and common variables 
!> for shared configuration features of all alamDMC programs
module commonConfig
use alamelConfig
use alamYLP
use alamUtils

      integer                       :: nslips = 16*6
      integer                       :: nsteps = 1 

      integer                       :: simtype = 0    !< Selection of multilevel model: 0 - Alamel, 1 - FC-Taylor

      character(len=512)            :: outputPrefix

      logical           :: outputRequest = .false.

      type(multilevelYLPConfig)     :: ylpCnf

      character(len=20),parameter   :: fmtMsg2Msg   = '(A,T35,A)'
      character(len=20),parameter   :: fmtMsg2Int   = '(A,T35,I4)'
      character(len=20),parameter   :: fmtMsg2Float = '(A,T35,F12.8)'

      character(len=20),parameter   :: fmtMsg2Any   = '(A,T35)'
      character(len=20),parameter   :: fmtMsg2Other = '(A,T35,'  ! Note: user is responsible for finishing the format string      
      

contains
      
      subroutine initAlamelStructures(info)
      implicit none
      integer,intent(out)           :: info
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! INITIALIZATION OF ALAMEL: it should be done in different way !!!!
            info = -1
            call initConfig(acnf,nslips,nsteps,info)
            if (info /= 0) then
                  write(*,*) 'Cannot initialize alamel config'
                  return
            endif
            call initResults(ares,nsteps,info)
            if (info /= 0) then
                  write(*,*) 'Cannot initialize data structure for alamel results'
                  return
            endif
            info = 0
      end subroutine

      subroutine initAlamel()
      implicit none
            write(*,'(A,\)') 'Initializing the multilevel model...'
            call ALAMEL(1)
            write(*,'(1X,A)') 'Done.'
      end subroutine


      subroutine readAlamelConfigSection(cnfunit,info)
      implicit none
      integer,intent(in)            :: cnfunit
      integer,intent(out)           :: info
      !
      integer                       :: ioerr
      !
            info = -1
            !
            read(cnfunit,'(I2,1X,A)',iostat=ioerr) acnf%texture%input_type
            select case(acnf%texture%input_type)
                  case(1,3)     ! SMT or CUB
                        read(cnfunit,'(A)',iostat=ioerr) acnf%texture%input_fname
                  case(2)       ! CUR file    
                        read(cnfunit,'(I2,1X,A)',iostat=ioerr) acnf%texture%block, acnf%texture%input_fname
                  case default
                        write(*,*) 'Incorrect texture type: ', acnf%texture%input_type    
            end select
            if (.not. ioStatusOK(ioerr)) return
            call stripComment(acnf%texture%input_fname)
            !
            read(cnfunit,fmt=*,iostat=ioerr)  simtype
            read(cnfunit,'(A)' ,iostat=ioerr) outputPrefix
            if (.not. ioStatusOK(ioerr)) return
            call stripComment(outputPrefix)
            read(cnfunit,'(A)' ,iostat=ioerr) acnf%slipsystem%input_fname 
            call stripComment(acnf%slipsystem%input_fname)
            read(cnfunit,'(A)' ,iostat=ioerr) acnf%micros_fname
            call stripComment(acnf%micros_fname)
            read(cnfunit,'(L)' ,iostat=ioerr) outputRequest
            !
            if (.not. ioStatusOK(ioerr)) return
            !
            acnf%output_prefix = trim(outputPrefix)
            !
            if (simtype == 0) then
                  ! rlx1 and rlx2 are by default set to 1, but nonetheless...
                  acnf%simulCalls(1)%rlx1 = 1
                  acnf%simulCalls(1)%rlx2 = 1
            else
                  write(*,'(A)', iostat=ioerr) 'FC Taylor'
                  acnf%simulCalls(1)%rlx1 = 0
                  acnf%simulCalls(1)%rlx2 = 0
            endif

            info = 0
      
      end subroutine


      subroutine readYLPConfigSection(cnfunit,info)
      implicit none
      integer,intent(in)            :: cnfunit
      integer,intent(out)           :: info
      !
      integer                       :: ioerr
      !
            info = -1
            read(cnfunit,fmt=*,iostat=ioerr) ylpCnf%jacobi_eps, ylpCnf%linearize
            read(cnfunit,fmt=*,iostat=ioerr) ylpCnf%default_eps, ylpCnf%obj_func_eps
            if (ioStatusOK(ioerr)) info = 0
      end subroutine



      logical function ioStatusOK(ioerr)
      implicit none
      integer,intent(in) :: ioerr
            ! Status 
            if (ioerr /= 0) then
                  write(*,*) 'An error has occured while reading config file'
                  ioStatusOK = .false.  
            endif
            ioStatusOK = .true.
      end function


      subroutine displayConfig(outunit,info)
      implicit none
      integer,intent(in)            :: outunit
      integer,intent(out)           :: info
      !
            info = -1
            
            if (simtype == 0) then
                  write(*,fmt=202) 'ALAMEL'
            else
                  write(*,fmt=202) 'FC Taylor'
            endif
            !
      
            ! Print configuration     
            select case(acnf%texture%input_type)
                  case(1)     ! SMT or CUB
                        write(outunit,fmt=200) 'SMT'
                  case(2)       ! CUR file    
                        write(outunit,fmt=200) 'CUR'
                  case(3)
                        write(outunit,fmt=200) 'CUB'                       
            end select
            write(outunit,fmt=201) trim(acnf%texture%input_fname)
            
            write(outunit,fmt=101) 'Prefix for output files:', trim(acnf%output_prefix) 
            write(outunit,fmt=101) 'Slip systems definition:', trim(acnf%slipsystem%input_fname)
            !
            if (ylpCnf%linearize) then
                  write(*,100) 'Info: the program will first attempt to linearize the identification problems.'
            else
                  write(*,100) 'Info: The program will attempt to solve the nonlinear problems.'
            endif
            !
            info = 0
            !
            !!!!     
            100 format(/,A,/)
            101 format(A,T35,A)
            !
            200 format('Input texture format:', T35,A)
            201 format('Input texture file:', T35,A)
            202 format('Multilevel model:', T35,A)
      
      end subroutine


end module