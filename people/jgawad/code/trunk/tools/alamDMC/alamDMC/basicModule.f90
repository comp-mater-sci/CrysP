! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2013-02-16, partly based on contents of 'commonConfig.f90'
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

!> Implementation of a basic DMC computiational module.
module dmcBasicModule
use dmcAbstractModule
use alamYLP, only: multilevelYLPConfig
use altayConfig, only: fname_len, altayConfigData
use commonConfig
use alamUtils

      type :: outputConfig

            character(len=fname_len)      :: outputPrefix = '' !< Prefix for the output files. 

            logical                       :: outputRequest = .false.
            
            integer                       :: verbosity = 0  !< Level of verbosity sent to the stdout and to the log file (if any)
            
            integer                       :: log_unit = 6
            
      end type
      
      !> Abstract class implementing basic subset of operations that are shared by all 
      !> computational modules
      type,abstract,extends(abstractModule) :: BasicModule
      
            type(outputConfig)            :: output

            type(multilevelYLPConfig)     :: ylp

            type(altayConfigData)         :: altay

      contains
      
            procedure,pass(this)     :: initialize =>  BasicModule_initialize
      
            procedure,pass(this)     :: readConfig => BasicModule_readConfig

            procedure,pass(this)     :: printConfig => BasicModule_printConfig

      end type



contains

      integer function BasicModule_Initialize(this) result(info)
      use altaySub
      use altayHard, only: hard_none, hard_voce, hard_pebp
      use commonUtils
      use fngRuntime
      implicit none
      class(BasicModule),intent(inout)          :: this
      !
            info = -1
            ! Finish the configuration:
            this%altay%output_config%nfile = merge(1,0,this%output%outputRequest)
            this%altay%output_prefix = trim(this%output%outputPrefix)
            this%altay%jobtitle = trim(this%output%outputPrefix)
            ! 
            if ((this%altay%slipsystem%kost == hard_pebp) .and. (this%output%outputRequest)) then
                  this%altay%output_config%npebp = 1
                  !cnf%output_config%nmss = 1 
            endif
            !
            write(*,fmt=30) 'Initializing the multilevel model...'
            call initAltay(this%altay,info)
            if (info == 0) then
                  write(*,fmt=31) 'Done.'
            else
                  write(*,fmt=31) 'Failed.'
            endif
            30 format(A,\)
            31 format(1X,A)
      
            ! Output the initial state variables (texture etc) if requested.
            if (this%output%outputRequest) then
                  call outputTexture(info)
                  if (info /= 0) then
                        write(errmsg,'(A)') 'Error: cannot write initial state'
                        !call finalize(stopcode_runtimeerror)
                  endif
            endif
            !
      end function
      
      
      integer function BasicModule_ReadConfig(this,cnfunit) result(info)
      use fngRuntime
      implicit none
      class(BasicModule),intent(inout)          :: this
      integer,intent(in)                        :: cnfunit
      !
            info = -1
            !
            call readOutputConfigSection(cnfunit,this%output,info)
            if (info /= 0) then 
                  write(errmsg,fmt=901) 'check output config section'
                  return
            endif
      
            call readAlamelConfigSection(cnfunit,this%altay,info)
            if (info /= 0) then
                  write(errmsg,fmt=901) 'check libaltay config section'
                  return
            endif
            !
            ! Read multilevelYLP configuration
            call readYLPConfigSection(cnfunit,this%ylp,info)
            if (info /= 0) then
                  write(errmsg,fmt=901) 'check YLP config section' 
                  return
            endif
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      !            
      end function
      
      
      
      subroutine BasicModule_printConfig(this,outunit,info)
      use altayConfig
      implicit none
      class(BasicModule),intent(in)       :: this
      integer,intent(in)                  :: outunit
      integer,intent(out)                 :: info
      !
            info = -1
            
            select case (this%altay%model_id)
            case(modelAlamel)
                  write(*,fmt=202) 'ALAMEL'
            case(modelFCTaylor)
                  write(*,fmt=202) 'FC Taylor'
            end select
            !
      
            ! Print configuration     
            select case(this%altay%texture%input_type)
                  case(1)     ! SMT or CUB
                        write(outunit,fmt=200) 'SMT'
                  case(2)       ! CUR file    
                        write(outunit,fmt=200) 'CUR'
                  case(3)
                        write(outunit,fmt=200) 'CUB'                       
            end select
            write(outunit,fmt=201) trim(this%altay%texture%input_fname)
            
            write(outunit,fmt=101) 'Slip systems definition:', trim(this%altay%slipsystem%input_fname)
            !
            if (this%ylp%linearize) then
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
      !
      end subroutine
      

      !
      ! Procedures for processing sections of the configuration file
      !
      
      subroutine readOutputConfigSection(cnfunit,cnf,info)
      implicit none
      integer,intent(in)                  :: cnfunit
      type(outputConfig),intent(inout) :: cnf
      integer,intent(out)                 :: info
      !
      integer                       :: ioerr
      !     
            info = -1
            read(cnfunit,'(A)' ,iostat=ioerr) cnf%outputPrefix
            if (.not. ioStatusOK(ioerr)) return
            call stripComment(cnf%outputPrefix)
            !
            read(cnfunit,*,iostat=ioerr) cnf%outputRequest
            if (.not. ioStatusOK(ioerr)) return
            
            read(cnfunit,*,iostat=ioerr) cnf%verbosity
            if (ioStatusOK(ioerr)) info = 0
      
      end subroutine
      
      subroutine readAlamelConfigSection(cnfunit,cnf,info)
      use altayConfig
      use altayHard, only: hard_none, hard_voce, hard_pebp
      implicit none
      integer,intent(in)                  :: cnfunit
      type(altayConfigData),intent(inout) :: cnf
      integer,intent(out)                 :: info
      !
      integer                       :: ioerr, simtype, model_id
      !
            info = -1
            simtype = -1; ioerr = -1; model_id = -1
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
            if (.not. ioStatusOK(ioerr)) return
            read(cnfunit,'(A)' ,iostat=ioerr) cnf%slipsystem%input_fname 
            call stripComment(cnf%slipsystem%input_fname)
            read(cnfunit,'(A)' ,iostat=ioerr) cnf%micros_fname
            call stripComment(cnf%micros_fname)
            if (.not. ioStatusOK(ioerr)) return
            !
            read(cnfunit,fmt=*,iostat=ioerr) cnf%slipsystem%kost
            if (.not. ioStatusOK(ioerr)) return
            select case(cnf%slipsystem%kost)
            case(hard_none)
                  ! no action needed
                  continue
            case(hard_Voce)
                  ! Read one line
                  read(cnfunit,fmt=*,iostat=ioerr) cnf%hardening%VoceCnf
            case(hard_pebp)
                  call readPEPBhardening(cnfunit,cnf%hardening%PEBPCnf,info)
                  if (info /= 0) return
            case default
                  info = -1
                  return
            end select
            if (.not. ioStatusOK(ioerr)) return
            !
            info = 0
            select case(simtype)
            case(0)     ! 0 - alamel
                  model_id = modelAlamel
            case(1)     ! 1 - FC Taylor
                  model_id = modelFCTaylor
            case(2)     ! 2 - MAS-Al
                  model_id = modelMASAL
            case default
                  info = -1
            end select
            if (info == 0) call setModelType(cnf,model_id,info)
      !
      end subroutine

      
      subroutine readPEPBhardening(cnfunit,hc,info)
      use altayConfig
      use KOST1x, only: ReadPar11
      implicit none
      integer,intent(in)                  :: cnfunit
      type(PEBPConfig),intent(out)        :: hc
      integer,intent(out)                 :: info
      !
      integer                       :: ioerr
      character(len=fname_len)      :: tmp_fname
      integer                       :: tmp,nparunit
      !
            info = -1
            read(cnfunit,fmt='(A)',iostat=ioerr) tmp_fname
            call stripComment(tmp_fname)
            ! Interpret the fname
            open(newunit=nparunit,file=tmp_fname,iostat=ioerr)
            if (ioerr /= 0) return
            info = ReadPar11(nparunit,hc%params)
            close(nparunit)
            if (info /= 0) return
            read(cnfunit,fmt='(I5,A)',iostat=ioerr) tmp, tmp_fname
            if (ioerr /= 0) return
            if (tmp >= 0) then
                  hc%read_state = .true.
                  call stripComment(tmp_fname)
                  hc%input_fname = tmp_fname
                  hc%block_id = tmp
            endif
            info = 0
      !
      end subroutine
      

      subroutine readYLPConfigSection(cnfunit,cnf,info)
      implicit none
      integer,intent(in)                        :: cnfunit
      type(multilevelYLPConfig),intent(out)     :: cnf
      integer,intent(out)                       :: info
      !
      integer                       :: ioerr
      !
            info = -1
            read(cnfunit,fmt=*,iostat=ioerr) cnf%jacobi_eps, cnf%linearize
            if (.not. ioStatusOK(ioerr)) return
            read(cnfunit,fmt=*,iostat=ioerr) cnf%default_eps, cnf%obj_func_eps
            if (ioStatusOK(ioerr)) info = 0
      end subroutine

      

      
      
      
end module