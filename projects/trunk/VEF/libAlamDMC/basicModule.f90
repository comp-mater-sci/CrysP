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
use alamYLP
use alamYLPConstants
use altayConfig, only: fname_len, altayConfigData
use commonConfig
use dmcUtils
use criUncomment
use criMathUtils
use criAlgorithm, only: optionalDefault
use fngVec5D

      !> Size of time increment
      !>
      !> Note: this should be taken either from altayConfigData (if it was provided there)
      !> or from some time incrementation procedure. Presently we always assume delta_t = 1
      double precision,parameter          :: delta_t = 1.D0


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

            procedure,pass(this)     :: findSolution => BasicModule_findSolution
            
      end type

      type :: YLPResult
            double precision,dimension(alamEval_vSD_dim) :: vA = 0.D0 
            double precision,dimension(alamEval_vSD_dim) :: vS = 0.D0 
            double precision,dimension(alamEval_vSD_dim) :: vSonA = 0.D0 
            double precision,dimension(alamEval_vSD_dim) :: vSonAn = 0.D0 
            double precision :: R = 0.D0
            double precision :: plast_pot = 0.D0
            double precision :: scal_s = 0.D0
      end type


contains

      integer function BasicModule_Initialize(this) result(info)
      use altaySub
      use altayHardTypes, only: hard_none, hard_voce, hard_BP, hard_PEBPscrew, hard_PEBPloop
      use commonUtils
      use criRuntime
      implicit none
      class(BasicModule),intent(inout)          :: this
      !
            info = -1
            ! Finish the configuration:
            this%altay%output_config%nfile = merge(1,0,this%output%outputRequest)
            this%altay%output_prefix = trim(this%output%outputPrefix)
            this%altay%jobtitle = trim(this%output%outputPrefix)
            ! 
            if (this%output%outputRequest) then
                  select case(this%altay%hardening%HardLawID)
                  case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
                        this%altay%output_config%npebp = 1
                  case default
                        this%altay%output_config%npebp = 0
                  end select
                  this%altay%output_config%nmss = 1
            endif
            !
            write(display_unit,fmt=30) 'Initializing the multilevel model...'
            call initAltay(this%altay,info,errmsg)
            if (info == 0) then
                  write(display_unit,fmt=31) 'Done.'
            else
                  write(display_unit,fmt=31) 'Failed.'
                  return
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
      use criRuntime
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
      
      
      
      integer function BasicModule_printConfig(this,outunit) result(info)
      use altayConfig
      implicit none
      class(BasicModule),intent(in)       :: this
      integer,intent(in)                  :: outunit
      !
            info = -1
            
            select case (this%altay%model_id)
            case(modelAlamel)
                  write(display_unit,fmt=202) 'ALAMEL'
            case(modelFCTaylor)
                  write(display_unit,fmt=202) 'FC Taylor'
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
                  write(display_unit,100) 'Info: the program will first attempt to linearize the identification problems.'
            else
                  write(display_unit,100) 'Info: The program will attempt to solve the nonlinear problems.'
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
      end function
      

    !> Calculate plastic strain rate D that corresponds to the superimposed input stress `sigma`
    !> by performing an iterative search.
    !>
    !> The results of the iterative search are placed in ylp_results.
    integer function BasicModule_findSolution(this,sigma, D, ylp_result, vM_guess) result(info)
    implicit none
    class(BasicModule),intent(in)   :: this
    type(SRTensor),intent(in)       :: sigma !< Input stress
    type(SRTEnsor),intent(inout)    :: D     !< Plastic strain rate
    type(YLPResult),intent(out)     :: ylp_result !< Results of the interative search
    !> Flag: use von Mises inital guess (default: .true.). If false, D will be used as the
    !> starting point for the iterative search.
    logical,optional                :: vM_guess 
    !
    double precision :: vS_norm, vA_norm, SonA_norm, pressure
    logical :: use_vM_guess
    !
        info = criErr_BadArgs

        ! Convert input to the 5D space and make the unit vector(s).
        ! This also makes sure it is deviatoric.
        ylp_result%vS = tens2vec5D(sigma%t)
        vS_norm = norm2(ylp_result%vS)
        if (vS_norm < epsilon(0.D0)) return
        ylp_result%vS = ylp_result%vS / vS_norm
        !
        use_vM_guess = optionalDefault(vM_guess, .true.)
        if (.not. use_vM_guess) then
            ylp_result%vA = tens2vec5D(D%t)
            vA_norm = norm2(ylp_result%vA)
            if (vA_norm < epsilon(0.D0)) return
        endif
        
        ! Calculate the corresponding strain rate vA
        call multilevelYLP( ylp_result%vS,    &
                            ylp_result%vA,    &
                            ylp_result%vSonA, &
                            ylp_result%R,     &
                            info,             &
                            useVMGuess=use_vM_guess, &
                            YLPconfig=this%ylp, &
                            verbose=this%output%verbosity)
        SonA_norm = norm2(ylp_result%vSonA)
        if ((info /= 0) .or. (SonA_norm < epsilon(0.D0))) then
            info = criError
            return
        endif
        !
        ylp_result%plast_pot = dot_product(ylp_result%vA, ylp_result%vSonA)
        ylp_result%scal_s = SonA_norm / vS_norm
        ! Calculate normalized stess
        ylp_result%vSonAn = ylp_result%vSonA / SonA_norm
        D%t = vec5D2tens(ylp_result%vA)
        info = criSuccess
    !
    end function
      
      
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
      use altayHard, only: hard_none, hard_voce, hard_BP, hard_PEBPscrew, hard_PEBPloop
      implicit none
      integer,intent(in)                  :: cnfunit
      type(altayConfigData),intent(inout) :: cnf
      integer,intent(out)                 :: info
      !
      integer                       :: ioerr, simtype, model_id
      logical                       :: flag
      integer                       :: i
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
                        write(display_unit,*) 'Incorrect texture type: ', cnf%texture%input_type    
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
            ! Process advanced microstructure characterization
            flag = .false.
            read(cnfunit,fmt=*,iostat=ioerr) flag
            if (.not. ioStatusOK(ioerr)) return
            if (flag) then 
                  do i=1,3
                        read(cnfunit,fmt=*,iostat=ioerr) cnf%simul_init%Fmicro(:,i)
                  enddo
            endif
            if (.not. ioStatusOK(ioerr)) return
            !
            read(cnfunit,fmt=*,iostat=ioerr) cnf%hardening%HardLawID
            if (.not. ioStatusOK(ioerr)) return
            select case(cnf%hardening%HardLawID)
            case(hard_none)
                  ! no action needed
                  continue
            case(hard_Voce)
                  ! Read one line
                  read(cnfunit,fmt=*,iostat=ioerr) cnf%hardening%VoceCnf
            case(hard_SwiftK)
                  read(cnfunit,fmt=*,iostat=ioerr) cnf%hardening%SwiftKCnf
            case(hard_SwiftS)
                  read(cnfunit,fmt=*,iostat=ioerr) cnf%hardening%SwiftSCnf
            case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
                  call readPEPBhardening(cnfunit,cnf%hardening%HardLawID,cnf%hardening%PEBPCnf,info)
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

      
      subroutine readPEPBhardening(cnfunit,kost,hc,info)
      use altayConfig
      use altayHardLaw_DSH, only: ReadPar
      implicit none
      integer,intent(in)                  :: cnfunit
      integer,intent(in)                  :: kost
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
            open(newunit=nparunit,file=tmp_fname,status='old',iostat=ioerr)
            if (ioerr /= 0) return
            info = ReadPar(nparunit,kost,hc%params)
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
