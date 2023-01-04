#include "criMacros.fpp"

!> Implementation of a basic DMC computational module.
module dmcBasicModule
use criRuntime
use criUncomment
use criConfigReader
use criMathUtils
use criAlgorithm, only: optionalDefault
use criPath, only: max_pathlen, splitExt
use criLinearMap
use criLog
use dmcAbstractModule
use altayConfig, only: altayConfigData
use commonConfig
use dmcUtils
implicit none
    !> FCC (111)<110>, through altayDeformationMechanismData_preconfigured
    !> objects
    integer,parameter :: DM_fcc12 = 1

    !> BCC (110)<111> + (112)<111>, through altayDeformationMechanismData_preconfigured
    !> objects
    integer,parameter :: DM_bcc24 =   2

    !> BCC (110)<111> + (112)<111> + (123)<111>, through
    !> altayDeformationMechanismData_preconfigured objects
    integer,parameter :: DM_bcc48 = 3

    integer,parameter :: DM_user =99 !< DM_user (currently not exploited).

    !> Initialization from file of PRE file format
    integer,parameter :: DM_format_pre = 101



    public :: outputConfig, BasicModule, readAlTayConfigSection
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

            type(altayConfigData)         :: altay !< Root-level configuration structure of texture and hardening


      contains ! type-bound procedures

            procedure,pass(this)     :: initialize =>  BasicModule_initialize

            procedure,pass(this)     :: readConfig => BasicModule_readConfig

            procedure,pass(this)     :: printConfig => BasicModule_printConfig

            procedure,pass(this)     :: run => BasicModule_run

            procedure,pass(this)     :: finalize => BasicModule_finalize

            !>@{ \name Helper procedures
            procedure,pass(this)      :: openOutputFile => BasicModule_openOutputFile

            procedure,pass(this)      :: reinitializeLibAltay => BasicModule_reinitializeLibAltay

            procedure,pass(this)      :: finalizeLibAltay => BasicModule_finalizeLibAltay
            !>@}

      end type




contains

      integer function BasicModule_initialize(this) result(info)
      use altaySub
      use altayHardTypes, only: hard_none, hard_voce, hard_BP, hard_PEBPscrew, hard_PEBPloop
      use commonUtils
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

            ! Output the initial state variables (non only texture but also BPM, MSS) if requested.
            if (this%output%outputRequest) then
                  call outputTexture(ierr) !< \todo Rename with more general name
                  if (ierr /= altaySub_OK) then
                        write(errmsg,'(A)') 'Error: cannot write initial state'
                        return
                  endif
            endif
            info = criSuccess
            !
      end function

      !> read output and AlTay configuration sections
      integer function BasicModule_readConfig(this,cnfunit) result(info)
      class(BasicModule),intent(inout)          :: this
      integer,intent(in)                        :: cnfunit !< IO input unit
      !
            info = criErr_IORead
            !
            ! Read output configuration lines
            call readOutputConfigSection(cnfunit,this%output,info) ! top 3 lines after comment header of config file
            if (info /= criSuccess) then
                  write(error_unit,fmt=901) 'Check output configuration section.'
                  return
            endif
            !
            ! Read AlTay configuration lines
            call readAlTayConfigSection(cnfunit,this%altay,info) ! read configuration of texture, slip systems, microstructure and hardening
            if (info /= criSuccess) then
                  write(error_unit,fmt=901) 'Check libaltay configuration section.'
                  return
            endif

#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      !
      end function



      integer function BasicModule_printConfig(this,outunit) result(info)
      use altayConfig
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
                select case(this%altay%hardening%HardLawID)
                      case(0)     ! hard_none
                            write(outunit,fmt=203) 'non-hardening'
                      case(1)     !
                            write(outunit,fmt=203) 'Voce'
                      case(2)       !
                            write(outunit,fmt=203) 'Swift K'
                      case(3)
                            write(outunit,fmt=203) 'Swift S'
                      case(11)
                            write(outunit,fmt=203) 'Peeters'
                      case(12)
                            write(outunit,fmt=203) 'PEBP screw'
                      case(13)
                            write(outunit,fmt=203) 'PEBP loop'
                end select
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
            203 format('Hardening model:', T35,A)
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
    class(BasicModule),intent(inout) :: this
    !
        info = this%finalizeLibAltay()
        if (info /= criSuccess) then
            errmsg = 'Problems have been encountered while finalizing libaltay'
            info = criError
        endif
    !
    end function


    !> Open output file
    integer function BasicModule_openOutputFile(this, ext, ofunit, suffix) result(info)
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



    integer function BasicModule_reinitializeLibAltay(this, output_prefix) result(info)
    use altaySub
    class(BasicModule),intent(inout)        :: this
    character(len=*),intent(in),optional    :: output_prefix !< File prefix
    !
    integer :: ierr
        ! Re-initialize AlTay
        RETURN_IF(info /= criSuccess, info = this%finalizeLibAltay())
        !
        ! Reconfigure:
        !  - Set new prefix
        if (present(output_prefix)) this%altay%output_prefix = output_prefix
        !
        call initAltay(this%altay,ierr)
        CHOOSE(info, ierr == altaySub_OK, criSuccess, criError)
    !
    end function

    !> Finalize libAltay and perform additional actions on finalization.
    integer function BasicModule_finalizeLibAltay(this) result(info)
    use altaySub
    class(BasicModule),intent(inout)        :: this
    !
    integer :: ierr
    !
        info = criError
        RETURN_IF(ierr /= altaySub_OK, call finalizeAltay(ierr))
        !
        ! Action on finalize:
        info = criSuccess
    !
    end function

      !
      ! Procedures for processing sections of the configuration file
      !

      !> Read output configuration from top 3 lines after comment header in configuration file:
      !> prefix for output files, incremental output request flag, verbosity level
      subroutine readOutputConfigSection(cnfunit,cnf,info)
      integer,intent(in)                  :: cnfunit !< configuration file
      type(outputConfig),intent(inout)    :: cnf
      integer,intent(out)                 :: info
      !
            info = criErr_IORead
            if (.not. readValue(cnfunit, cnf%outputPrefix)) then
                write(error_unit,fmt=900) 'Check output file prefix.'
                return
            endif
            if (.not. readValue(cnfunit, cnf%outputRequest)) then
                write(error_unit,fmt=900) 'Check output request flag.'
                return
            endif
            if (.not. readValue(cnfunit, cnf%verbosity)) then
                write(error_unit,fmt=900) 'Check verbosity level.'
                return
            endif
            info = criSuccess
      !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      !
      end subroutine


      !> Read configuration of libaltay
      subroutine readAlTayConfigSection(cnfunit,cnf,info)
      use altayConfig
      integer,intent(in)                  :: cnfunit
      type(altayConfigData),intent(inout) :: cnf !< Root-level configuration structure of texture, microstructure and hardening
      integer,intent(out)                 :: info
      !
      integer                       :: model_id, dm_id
      logical                       :: use_default_microstructure
      logical                       :: use_default_slipsystems
      integer                       :: i
      character(len=max_pathlen) :: root, ext

      type(MapItem),dimension(2*2) :: extensions = [MapItem('.smt',TF_SMT), MapItem('.SMT',TF_SMT), &
                                                    MapItem('.cur',TF_CUR), MapItem('.CUR',TF_CUR)]

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
            ! Read input texture file name
            if (.not. readValue(cnfunit, cnf%texture%input_fname)) return
            !
            ! Deduce the input type from the extension
            call splitExt(cnf%texture%input_fname, root, ext)
            if (ext == '' .or. .not. resolveName(extensions, ext, cnf%texture%input_type)) then
                write(error_unit,fmt=900) 'Unsupported texture input file format.'
                info = criErr_BadArgs
                return
            endif
            select case(cnf%texture%input_type)
                  case(TF_SMT)     ! SMT or CUB
                        continue
                  case(TF_CUR)       ! CUR file, the only multi-block file now.
                       if (.not. readValue(cnfunit, cnf%texture%block_id)) return ! read block id for CUR format
                  case default
                        write(display_unit, fmt=900) 'Incorrect texture type.'
                        return
                  end select
            !
            ! Determine crystal plasticity model type
            !if (.not. readKeyword(cnfunit, model_types, model_id)) return
            if (.not. readKeyword(cnfunit, model_types, model_id)) then
                write(error_unit,fmt=900) 'Unsupported crystal plasticity model.'
                info = criErr_BadArgs
                return
            endif
            !
            ! Determine slip system file
            use_default_slipsystems = .true.
            if (.not. readValue(cnfunit, use_default_slipsystems)) return
            if (.not. use_default_slipsystems) then ! user-supplied slip system definition
                  if (.not. readValue(cnfunit, cnf%slipsystem%input_fname)) return ! read slip system filename
            !
            else ! default slip system definition
                  if (.not. readKeyword(cnfunit, slipsystem_types, dm_id)) then
                      write(error_unit,fmt=900) 'Unsupported slip system family.'
                      info = criErr_BadArgs
                      return
                  endif
                  ! Let's map DM_id to a file
                  call incurSlipsystemFile(dm_id, cnf%slipsystem%input_fname, info)
                  if (info /= criSuccess) then
                        write(error_unit, fmt=930) 'Cannot locate slipsystem file.'
                        return
                  endif
            endif
            !
            ! Process advanced microstructure characterization
            use_default_microstructure = .true.
            if (.not. readValue(cnfunit, use_default_microstructure)) return
            if (.not. use_default_microstructure) then
                  if (.not. readValue(cnfunit, cnf%micros_fname)) return ! read <microstructure>.smt filename
                  ! Deduce the input type from the extension
                  call splitExt(cnf%micros_fname, root, ext)
                  if (ext == '' .or. .not. (ext == '.smt' .or. ext == '.SMT')) then
                        write(error_unit,fmt=900) 'Unsupported microstructure input file format.'
                        info = criErr_BadArgs
                        return
                  endif
                  ! Read user-supplied initial deformation gradient
                  do i=1,3
                        if (.not. readValue(cnfunit, cnf%simul_init%Fmicro(:,i))) then
                            write(error_unit,fmt=900) 'Cannot read deformation gradient.'
                            info = criErr_BadArgs
                            return
                        endif
                  enddo
            else
                  call incurMicrostructureFile(cnf%micros_fname, info) ! verify location of default microstructure file
                  if (info /= criSuccess) then
                        write(error_unit, fmt=930) 'Cannot locate default microstructure file.'
                        return
                  endif
            endif
            !
            ! Process hardening model section
            call readHardeningSection(cnfunit, cnf%hardening, info)
            if (info /= criSuccess) then
                write(error_unit,fmt=900) 'Cannot read the hardening law section.'
                return
            endif
            ! the keyword is mapped to a proper model_id, we can instantly set it.
            call setModelType(cnf,model_id,info)
            if (info /= criSuccess) return
            !
            !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      !
      end subroutine


      !> Read configuration of hardening model from configuration file
      subroutine readHardeningSection(cnfunit, hardening, info)
      use altayHard, only: hard_none, hard_voce, hard_BP, hard_PEBPscrew, hard_PEBPloop, hard_SwiftK, hard_SwiftS
      use altayConfig, only: hardeningData, VoceConfig, SwiftKConfig, SwiftSConfig
      integer,intent(in)                  :: cnfunit
      type(hardeningData),intent(out)     :: hardening !< structure containing hardening configuration
      integer,intent(out)                 :: info
      !
      double precision,dimension(5) :: tmp ! Temporary for hardening parameters.
      logical :: use_default_hardening
      !
            use_default_hardening = .true.
            if (.not. readValue(cnfunit, use_default_hardening)) then ! read default hardening flag
                  write(error_unit,fmt=900) 'Reading of the default hardening flag unsuccessful.'
                  return
            endif
            if (.not. use_default_hardening) then
                  ! read hardening law ID
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
                        write(error_unit,fmt=900) 'The selected hardening model is not available in your version'
                        info = criError
#endif
                  case default
                        write(error_unit,fmt=900) 'Unsupported hardening law.'
                        info = criError
                        return
                  end select
            else
                  info = criSuccess
            endif
! message formats
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      !
      end subroutine


      !> Read configuration of PEBP hardening module.
      !>
      !> The current implementation reads the coefficients of the hardening law
      !> from an external file that is specified in the configuration file. If read_state=True
      !> the state variables are read for a defined increment (block_id) from a state file specified.
      subroutine readPEPBhardening(cnfunit,kost,hc,info)
      use altayConfig
      use altayHardLaw_DSH, only: ReadPar
      integer,intent(in)                  :: cnfunit !< configuration file
      integer,intent(in)                  :: kost    !< HardLawID
      type(PEBPConfig),intent(out)        :: hc      !< data type containing the BP model parameters (no state variables); defined in altayConfig.f90
      integer,intent(out)                 :: info
      !
      integer                             :: ioerr
      character(len=max_pathlen)          :: tmp_fname ! BP parameter file name
      integer                             :: nparunit  ! IO unit of BP parameter file
      !
            info = criErr_IORead
            !
            ! Process PEBP parameter file
            if (.not. readValue(cnfunit, tmp_fname)) return ! read BP parameter file name
            ! Open the PEBP parameter file and read its content
            open(newunit=nparunit,file=tmp_fname,status='old',iostat=ioerr)
            if (ioerr /= 0) then
                write(error_unit,fmt=951) trim(tmp_fname)
                return
            endif
            info = ReadPar(nparunit,kost,hc%params) ! read the BP parameter file
            close(nparunit)
            if (info /= 0) then
                write(error_unit,'(A,1X,A,1X,A)') 'Error: Reading of the parameter file',trim(tmp_fname),'failed.'
                return
            endif
            !
            ! Read PEBP state variable file path and block ID. 3 lines in config file
            if (.not. readValue(cnfunit, hc%read_state)) return ! read read_state flag; default read_state=.false.
            if (hc%read_state) then
                  if (.not. readValue(cnfunit,hc%input_fname)) return ! read state variable file name; default input_fname=''
                  if (.not. readValue(cnfunit,hc%block_id)) return ! read no. of state blocks to skip in state file; default block_id=0
            endif
            !
            info = criSuccess

#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      !
      end subroutine


      !> Deduce the path to VEF common data files.
      function getVEFDataDir()
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


      !> Determines location of data file in VEF distribution and checks its existence.
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


      !> Incur the location of default slip system file
      subroutine incurSlipsystemFile(dm_id, slipsystem_path, info)
      integer,intent(in)              :: dm_id !< Deformation mechanism ID
      character(len=*),intent(out)    :: slipsystem_path
      integer,intent(out)             :: info
      !
      character(len=max_pathlen) :: fname
      !
            info = criErr_BadArgs
            select case(dm_id)
            case(DM_fcc12)
                  fname = 'fcc12.pre'
            case(DM_bcc24)
                  fname = 'bcc24.pre'
            case(DM_bcc48)
                  fname = 'bcc48.pre'
            case default
                  return
            end select
            call getDataPath(fname, slipsystem_path, info)
      !
      end subroutine


      !> Incur the location of the default microstructure file
      subroutine incurMicrostructureFile(micros_fname, info)
      character(len=*),intent(out)    :: micros_fname
      integer,intent(out)             :: info
      !
            call getDataPath('equiaxed.smt', micros_fname, info)
      !
      end subroutine

end module
