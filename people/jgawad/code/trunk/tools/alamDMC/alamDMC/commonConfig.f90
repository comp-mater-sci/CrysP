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
use alamYLP
use alamUtils

      character(len=512),save       :: outputPrefix = ''

      logical,save                  :: outputRequest = .false.

      type(multilevelYLPConfig),save:: ylpCnf

      character(len=20),parameter   :: fmtMsg2Msg   = '(A,T35,A)'
      character(len=20),parameter   :: fmtMsg2Int   = '(A,T35,I4)'
      character(len=20),parameter   :: fmtMsg2Float = '(A,T35,F12.8)'

      character(len=20),parameter   :: fmtMsg2Any   = '(A,T35)'
      character(len=20),parameter   :: fmtMsg2Other = '(A,T35,'  ! Note: user is responsible for finishing the format string      
      

contains
      
 
      subroutine readAlamelConfigSection(cnfunit,cnf,info)
      use altayConfig
      implicit none
      integer,intent(in)                  :: cnfunit
      type(altayConfigData),intent(inout) :: cnf
      integer,intent(out)                 :: info
      !
      integer                       :: ioerr, simtype
      !
            info = -1
            simtype = -1; ioerr = -1;
            !
            read(cnfunit,'(I2,1X,A)',iostat=ioerr) cnf%texture%input_type
            if (.not. ioStatusOK(ioerr)) return
            select case(cnf%texture%input_type)
                  case(1,3)     ! SMT or CUB
                        read(cnfunit,'(A)',iostat=ioerr) cnf%texture%input_fname
                  case(2)       ! CUR file    
                        read(cnfunit,'(I2,1X,A)',iostat=ioerr) cnf%texture%block_id, cnf%texture%input_fname
                  case default
                        write(*,*) 'Incorrect texture type: ', cnf%texture%input_type    
            end select
            if (.not. ioStatusOK(ioerr)) return
            call stripComment(cnf%texture%input_fname)
            !
            read(cnfunit,fmt=*,iostat=ioerr)  simtype
            read(cnfunit,'(A)' ,iostat=ioerr) outputPrefix
            if (.not. ioStatusOK(ioerr)) return
            call stripComment(outputPrefix)
            read(cnfunit,'(A)' ,iostat=ioerr) cnf%slipsystem%input_fname 
            call stripComment(cnf%slipsystem%input_fname)
            read(cnfunit,'(A)' ,iostat=ioerr) cnf%micros_fname
            call stripComment(cnf%micros_fname)
            read(cnfunit,'(L)' ,iostat=ioerr) outputRequest
            if (.not. ioStatusOK(ioerr)) return
            if (outputRequest)   cnf%output_config%nfile = 1
            !
            cnf%output_prefix = trim(outputPrefix)
            !
            select case(simtype)
            case(0)     ! 0 - alamel
                  cnf%model_id = modelAlamel
                  cnf%simul_init%ngr = 2
            case(1)     ! 1 - FC Taylor
                  cnf%model_id = modelFCTaylor
                  cnf%simul_init%ngr = 1
            case(2)     ! 2 - MAS-Al
                  cnf%model_id = modelMASAL
                  cnf%simul_init%ngr = 3
            end select
            !
            info = 0
      !
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
      use altayConfig
      implicit none
      integer,intent(in)            :: outunit
      integer,intent(out)           :: info
      !
            info = -1
            
            select case (acnf%model_id)
            case(modelAlamel)
                  write(*,fmt=202) 'ALAMEL'
            case(modelFCTaylor)
                  write(*,fmt=202) 'FC Taylor'
            end select
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
            
            write(outunit,fmt=101) 'Prefix for output files:', trim(outputPrefix) 
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
