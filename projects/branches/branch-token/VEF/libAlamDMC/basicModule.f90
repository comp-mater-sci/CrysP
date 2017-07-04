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
use criRuntime
use criUncomment
use criConfigReader
use criMathUtils
use criAlgorithm, only: optionalDefault
use criPath, only: max_pathlen, splitExt
use criLinearMap
use criLog
use fngVec5D
use dmcAbstractModule
use altayConfig, only: altayConfigData
use commonConfig
use dmcUtils
use dmcFuture
implicit none


    public :: outputConfig, BasicModule
    private

      type :: outputConfig

            character(len=max_pathlen)      :: outputPrefix = '' !< Prefix for the output files. 

            logical                       :: outputRequest = .false.
            
            integer                       :: verbosity = 0  !< Level of verbosity sent to the stdout and to the log file (if any)
            
            integer                       :: log_unit = 6
            
      end type
      
      !> Class implementing basic subset of operations that are shared by all 
      !> computational modules.
      !>
      !> \note This class is essentially an abstract class, but declaring it
      !>       that way prevents the subclasses from calling _ANY_ superclass 
      !>       method (including the ones that have an actual implementation
      !>       in BasicModule) in the OO-acceptable style:
      !>       `this%ParentClassName%method()`
      type,extends(abstractModule) :: BasicModule
      
            type(outputConfig)            :: output

            type(altayConfigData)         :: altay
            
      contains
      
            procedure,pass(this)     :: initialize =>  BasicModule_initialize
      
            procedure,pass(this)     :: readConfig => BasicModule_readConfig

            procedure,pass(this)     :: printConfig => BasicModule_printConfig

            procedure,pass(this)     :: run => BasicModule_run

            procedure,pass(this)     :: finalize => BasicModule_finalize

            !>@{ \name Helper procedures
            procedure,pass(this)      :: openOutputFile => BasicModule_openOutputFile
            !>@}
      end type




contains

      integer function BasicModule_initialize(this) result(info)
      use altaySub
      use altayHardTypes, only: hard_none, hard_voce, hard_BP, hard_PEBPscrew, hard_PEBPloop
      use commonUtils
      implicit none
      class(BasicModule),intent(inout)          :: this
      !
      integer :: ierr
      !
            info = criError
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
            endif
            !
            if (doLogging(criLoginfo,this%output%verbosity)) write(display_unit,fmt=30, advance='no')
            call initAltay(this%altay,ierr,errmsg)
            if (doLogging(criLoginfo,this%output%verbosity)) then
                if (ierr == altaySub_OK) then
                      write(display_unit,fmt=31) 'Done.'
                else
                      write(display_unit,fmt=31) 'Failed.'
                endif
            endif
            if (ierr /= altaySub_OK) return
            
            30 format('Initializing the multilevel model...')
            31 format(1X,A)
      
            ! Output the initial state variables (texture etc) if requested.
            if (this%output%outputRequest) then
                  call outputTexture(ierr)
                  if (ierr /= altaySub_OK) then
                        write(errmsg,'(A)') 'Error: cannot write initial state'
                        return
                  endif
            endif
            info = criSuccess
            !
      end function
      
      
      integer function BasicModule_readConfig(this,cnfunit) result(info)
      implicit none
      class(BasicModule),intent(inout)          :: this
      integer,intent(in)                        :: cnfunit
      !
            info = criErr_IORead
            !
            call readOutputConfigSection(cnfunit,this%output,info)
            if (info /= criSuccess) then 
                  write(errmsg,fmt=901) 'check output config section'
                  return
            endif
      
            call readAlTayConfigSection(cnfunit,this%altay,info)
            if (info /= criSuccess) then
                  write(errmsg,fmt=901) 'check libaltay config section'
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
            info = criErr_BadArgs
            if (doLogging(criLogInfo,this%output%verbosity)) then
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
                write(outunit,fmt=101) 'Microstructure definition:', trim(this%altay%micros_fname)
                !
            endif
            !
            info = criSuccess
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
      


    subroutine BasicModule_run(this,info)
    class(BasicModule),intent(inout) :: this
    integer,intent(out)                 :: info
    !
        info = criSuccess
    !
    end subroutine


    !> Finalization of the module
    integer function BasicModule_finalize(this) result(info)
    use altaySub
    implicit none
    class(BasicModule),intent(inout) :: this
    !
    integer :: ierr
    !
        call finalizeAltay(ierr)
        if (ierr /= altaySub_OK) then
            errmsg = 'Problems have been encountered while finalizing libaltay'
            info = criError
        else
            info = criSuccess
        endif
    !
    end function
    

    !> Open output file
    integer function BasicModule_openOutputFile(this, ext, ofunit, suffix) result(info)
    implicit none
    class(BasicModule),intent(in)           :: this
    character(len=*),intent(in)             :: ext !< File extension (with leading dot)
    integer,intent(out)                     :: ofunit !< IO unit of the output
    character(len=*),intent(in),optional    :: suffix !< Suffix to the file
    !
    character(len=max_pathlen) :: output_path
    integer :: ierr
    !
        if (present(suffix)) then
            output_path = trim(this%output%outputPrefix)// trim(suffix) //trim(ext)
        else
            output_path = trim(this%output%outputPrefix)// trim(ext)
            
        endif
        open(newunit=ofunit, file=output_path, status='replace', iostat=ierr)
        if (ierr /= 0) then
            write(display_unit, fmt=952) output_path
            info = criErr_IOWrite
            return
        endif
        info = criSuccess
    !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
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
            info = criErr_IORead
            if (.not. readValue(cnfunit, cnf%outputPrefix)) return
            if (.not. readValue(cnfunit, cnf%outputRequest)) return
            if (.not. readValue(cnfunit, cnf%verbosity)) return
            info = criSuccess
      !
      end subroutine


      !> Read configuration of libaltay
      subroutine readAlTayConfigSection(cnfunit,cnf,info)
      use altayConfig
      implicit none
      integer,intent(in)                  :: cnfunit
      type(altayConfigData),intent(inout) :: cnf
      integer,intent(out)                 :: info
      !
      integer                       :: model_id, dm_id
      logical                       :: use_default_microstructure
      integer                       :: i
      character(len=max_pathlen) :: root, ext
      type(MapItem),dimension(3) :: extensions = [MapItem('.smt',1), &
                                                  MapItem('.cur',2), &
                                                  MapItem('.cub',3)]
      type(MapItem),dimension(2) :: model_types = [MapItem('ALAMEL', modelAlamel), &
                                                   MapItem('FCTaylor', modelFCTaylor)]
      type(MapItem),dimension(4) :: slipsystem_types = [MapItem('fcc12', DM_fcc12), &
                                                        MapItem('bcc24', DM_bcc24), &
                                                        MapItem('bcc48', DM_bcc48), &
                                                        MapItem('pre', DM_format_pre)]
      !
           info = criErr_IORead
           model_id = -1
           dm_id = -1
            !
            if (.not. readValue(cnfunit, cnf%texture%input_fname)) return
            !
            ! Deduce the input type from the extension
            call splitExt(cnf%texture%input_fname, root, ext)
            cnf%texture%input_type = findName(extensions, ext)
            if (ext == '' .or. cnf%texture%input_type == 0) then
                write(display_unit,*) 'Cannot determine texture input type from the extension'
                info = criErr_BadArgs
                return
            endif
            select case(cnf%texture%input_type)
                  case(1,3)     ! SMT or CUB
                        continue
                  case(2)       ! CUR file, the only multi-block file now.
                       if (.not. readValue(cnfunit, cnf%texture%block_id)) return 
                  case default
                        write(display_unit, fmt=900) 'Incorrect texture type.'
                        return
                  end select
            !
            if (.not. readKeyword(cnfunit, model_types, model_id)) return
            if (.not. readKeyword(cnfunit, slipsystem_types, dm_id)) return
            !
            ! Process advanced microstructure characterization
            use_default_microstructure = .true.
            if (.not. readValue(cnfunit, use_default_microstructure)) return
            if (.not. use_default_microstructure) then 
                  if (.not. readValue(cnfunit, cnf%micros_fname)) return
                  do i=1,3
                        if (.not. readValue(cnfunit, cnf%simul_init%Fmicro(:,i))) return
                  enddo
            else
                  call incurMicrostructureFile(cnf%micros_fname, info)
                  if (info /= criSuccess) then
                        write(display_unit, fmt=930) 'Cannot locate default microstructure file.'
                        return
                  endif
            endif
            !
            call readHardeningSection(cnfunit,  cnf%hardening, info)
            if (info /= criSuccess) return
            ! the keyword is mapped to a proper model_id, we can instantly
            ! set it.
            call setModelType(cnf,model_id,info)
            if (info /= criSuccess) return
            ! Let's map DM_id to a file
            call incurSlipsystemFile(dm_id, cnf%slipsystem%input_fname, info)
            if (info /= criSuccess) then
                  write(display_unit, fmt=930) 'Cannot locate slipsystem file.'
                  return
            endif
            !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      !
      end subroutine


      !> Read configuration of hardening models
      subroutine readHardeningSection(cnfunit, hardening, info)
      use altayHard, only: hard_none, hard_voce, hard_BP, hard_PEBPscrew, hard_PEBPloop, hard_SwiftK, hard_SwiftS
      use altayConfig, only: hardeningData, VoceConfig, SwiftKConfig, SwiftSConfig
      implicit none
      integer,intent(in)                  :: cnfunit
      type(hardeningData),intent(out)     :: hardening
      integer,intent(out)                 :: info
      !
      double precision,dimension(5) :: tmp ! Temporary for hardening parameters.
      logical :: use_default_hardening
      !
            use_default_hardening = .true.
            if (.not. readValue(cnfunit, use_default_hardening)) return
            if (.not. use_default_hardening) then
                  if (.not. readValue(cnfunit, hardening%HardLawID)) return
                  select case(hardening%HardLawID)
                  case(hard_none)
                        ! no action needed
                        info = criSuccess
                  case(hard_Voce)
                        ! Read one line
                        if (readValue(cnfunit, tmp(1:5))) then
                              hardening%VoceCnf = VoceConfig(tmp(1), tmp(2), tmp(3), tmp(4), tmp(5))
                              info = criSuccess
                        endif
                  !
                  case(hard_SwiftK)
                        ! Read one line
                        if (readValue(cnfunit, tmp(1:3))) then
                              hardening%SwiftKCnf = SwiftKConfig(tmp(1),tmp(2), tmp(3))
                              info = criSuccess
                        endif
                  !
                  case(hard_SwiftS)
                        ! Read one line
                        if (readValue(cnfunit, tmp(1:3))) then
                              hardening%SwiftSCnf = SwiftSConfig(tmp(1),tmp(2), tmp(3))
                              info = criSuccess
                        endif
                  !

                  case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
#ifdef PEBP_ENABLED                        
                        call readPEPBhardening(cnfunit,hardening%HardLawID,hardening%PEBPCnf,info)
#else
                        write(display_unit,fmt=900) 'The selected hardening model is not available in your version'
                        info = criError
#endif
                  case default
                        info = criError
                        return
                  end select
            else
                  info = criSuccess
            endif
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      !
      end subroutine


      !> Read configuration of PEBP hardening module.
      !>
      !> The current implementation reads the coefficients of the hardening law
      !> from an external file that is specified in the configuration file.
      subroutine readPEPBhardening(cnfunit,kost,hc,info)
      use altayConfig
      use altayHardLaw_DSH, only: ReadPar
      implicit none
      integer,intent(in)                  :: cnfunit
      integer,intent(in)                  :: kost
      type(PEBPConfig),intent(out)        :: hc
      integer,intent(out)                 :: info
      !
      integer                             :: ioerr
      character(len=max_pathlen)          :: tmp_fname
      integer                             :: nparunit
      !
            info = criErr_IORead
            if (.not. readValue(cnfunit, tmp_fname)) return
            ! Interpret the fname
            open(newunit=nparunit,file=tmp_fname,status='old',iostat=ioerr)
            if (ioerr /= 0) return
            info = ReadPar(nparunit,kost,hc%params)
            close(nparunit)
            if (info /= 0) return
            ! Read state file path and block id.
            if (.not. readValue(cnfunit, hc%read_state)) return
            if (hc%read_state) then
                  if (.not. readValue(cnfunit,hc%input_fname)) return
                  if (.not. readValue(cnfunit,hc%block_id)) return
            endif
            !
            info = criSuccess
      !
      end subroutine


      !> Deduce the path to VEF common data files.
      function getVEFDataDir()
      implicit none
      character(len=max_pathlen) :: getVEFDataDir
      !
      character(len=max_pathlen) :: vef_root_path
      integer :: ierr
      !
            getVEFDataDir = ''
            call get_environment_variable('VEF_ROOT', vef_root_path, status=ierr)
            if (ierr == 0 .and. len_trim(vef_root_path) > 0) then
                  getVEFDataDir = pathjoin(vef_root_path,'data')
            endif
      !
      end function
      
      
      !> Determines location of data file and checks its existence.
      !>
      !> The places where the procedure looks for the files are:
      !> 1. $VEF_ROOT/data
      !> 2. current directory
      subroutine getDataPath(fname, path, info)
      character(len=*),intent(in)   :: fname
      character(len=*),intent(out)  :: path
      integer,intent(out)           :: info
      !
      character(len=max_pathlen) :: prefix
      integer :: ierr
      logical :: file_exists
      !
            info = criErr_BadArgs
            prefix = getVEFDataDir()
            path = pathjoin(prefix, fname)
            inquire(file=path, exist=file_exists, iostat=ierr)
            if (ierr == 0 .and. file_exists) info = criSuccess
      !
      end subroutine


      !> Incur the location of slip system file
      subroutine incurSlipsystemFile(dm_id, slipsystem_path, info)
      implicit none
      integer,intent(in)              :: dm_id !< Deformation mechanism ID
      character(len=*),intent(out)    :: slipsystem_path
      integer,intent(out)             :: info
      !
      character(len=max_pathlen) :: fname
      !
            info = criErr_BadArgs
            select case(dm_id)
            case(DM_fcc12)
                  fname = 'fcc.pre'
            case(DM_bcc24)
                  fname = 'bcc.pre'
            case(DM_bcc48)
                  fname = 'bcc2.pre'
            case default
                  return
            end select
            call getDataPath(fname, slipsystem_path, info)
      !
      end subroutine


      !> Incur the location of the microstructure file
      subroutine incurMicrostructureFile(micros_fname, info)
      character(len=*),intent(out)    :: micros_fname
      integer,intent(out)             :: info
      !
            call getDataPath('equiaxed.smt', micros_fname, info)
      !
      end subroutine
      
end module
