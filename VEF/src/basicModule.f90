#include "criMacros.fpp"

!> Implementation of a basic DMC computational module.
module dmcBasicModule
    use,intrinsic :: iso_fortran_env, only: error_unit,output_unit
    use criUncomment
    use criConfigReader
    use criMathUtils
    use dmcAbstractModule
    use altayConfig, only: altayConfigData
    use commonConfig
    use utils
    use hardening
    use parameters
    use logging
    use altaySub, only: initAltay, finalizeAltay
    use commonUtils

    implicit none
    private

    character(*), parameter :: MOD_NAME = 'basicModule'

    public :: outputConfig, &
              BasicModule, &
              readAlTayConfigSection

      type :: outputConfig
            character(max_pathlen)  :: outputPrefix = '' !< Prefix for the output files.
            logical                 :: outputRequest = .false.
            integer                 :: verbosity = 0  !< Level of verbosity sent to the stdout and to the log file (if any)
            integer                 :: log_unit = 6
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
      contains
            procedure :: initialize =>  BasicModule_initialize
            procedure :: readConfig => BasicModule_readConfig
            procedure :: run => BasicModule_run
            procedure :: finalize => BasicModule_finalize
            procedure :: openOutputFile => BasicModule_openOutputFile
            procedure :: reinitializeLibAltay => BasicModule_reinitializeLibAltay
            procedure :: finalizeLibAltay => BasicModule_finalizeLibAltay
      end type

contains

    integer function BasicModule_initialize(this) result(info)
      class(BasicModule),intent(inout)          :: this
      integer :: ierr

            info = VEF_ERROR
            ! Finish the configuration:
            this%altay%output_config%nfile = merge(1,0,this%output%outputRequest)
            this%altay%output_prefix = trim(this%output%outputPrefix)
            this%altay%jobtitle = trim(this%output%outputPrefix)

            call initAltay(this%altay,ierr)

            if (ierr /= VEF_OK) return

            30 format('Initializing the multilevel model...')
            31 format(1X,A)

            ! Output the initial state variables (non only texture but also BPM, MSS) if requested.
            if (this%output%outputRequest) then
                  call outputTexture(ierr) !< \todo Rename with more general name
                  if (ierr /= VEF_OK) &
                        call log_error(MOD_NAME, 'initialize', ERR_IO, 'Cannot write initial state.')
            endif
            info = VEF_OK
      end function

    !> read output and AlTay configuration sections
    integer function BasicModule_readConfig(this,cnfunit) result(info)
        class(BasicModule),intent(inout) :: this
        integer,intent(in)               :: cnfunit !< IO input unit

        character(*), parameter :: PROC_NAME = 'readconfig'

        ! Read output configuration lines
        call readOutputConfigSection(cnfunit,this%output,info) ! top 3 lines after comment header of config file
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Check output configuration section.')

        ! Read AlTay configuration lines
        call readAlTayConfigSection(cnfunit,this%altay,info) !read configuration of texture, slip systems, microstructure and hardening
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Check libAltay configuration section.')
    end function

    subroutine BasicModule_run(this,info)
    class(BasicModule),intent(inout) :: this
    integer,intent(out)                 :: info
    !
        info = VEF_OK
    !
    end subroutine


    !> Finalization of the module
    integer function BasicModule_finalize(this) result(info)
        class(BasicModule),intent(inout) :: this

        info = 0
        if (this%finalizeLibAltay() /= VEF_OK) &
            call log_error(MOD_NAME, 'finalize', ERR)
    end function


    !> Open output file
    integer function BasicModule_openOutputFile(this, ext, ofunit, suffix) result(info)
    class(BasicModule),intent(in)           :: this
    character(len=*),intent(in)             :: ext !< File extension (with leading dot)
    integer,intent(out)                     :: ofunit !< IO unit of the output
    character(len=*),intent(in),optional    :: suffix !< Suffix to the file
    character(len=max_pathlen) :: output_path
    integer :: ierr

        if (present(suffix)) then
            output_path = trim(this%output%outputPrefix)// trim(suffix) //trim(ext)
        else
            output_path = trim(this%output%outputPrefix)// trim(ext)
        endif

        open(newunit=ofunit, file=output_path, status='replace', iostat=ierr)
        if (ierr /= 0) &
            call log_error(MOD_NAME, 'open_output_file', ERR_IO, 'Could not open output file.')

        info = VEF_OK
    end function

    integer function BasicModule_reinitializeLibAltay(this, output_prefix) result(info)
    class(BasicModule),intent(inout)        :: this
    character(len=*),intent(in),optional    :: output_prefix !< File prefix
    !
    integer :: ierr
        ! Re-initialize AlTay
        RETURN_IF(info /= VEF_OK, info = this%finalizeLibAltay())
        !
        ! Reconfigure:
        !  - Set new prefix
        if (present(output_prefix)) this%altay%output_prefix = output_prefix
        !
        call initAltay(this%altay,ierr)
        CHOOSE(info, ierr == VEF_OK, VEF_OK, VEF_ERROR)
    !
    end function

    !> Finalize libAltay and perform additional actions on finalization.
    integer function BasicModule_finalizeLibAltay(this) result(info)
    class(BasicModule),intent(inout)        :: this
    !
    integer :: ierr
    !
        info = VEF_ERROR
        RETURN_IF(ierr /= VEF_OK, call finalizeAltay(ierr))
        !
        ! Action on finalize:
        info = VEF_OK
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

            info = VEF_ERROR
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
            info = VEF_OK
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
      logical                       :: dummy, dummy2
      integer                       :: i
      character(:), allocatable :: slip
      character(5) :: buffer

      type(MapItem),dimension(2) :: model_types = [MapItem('ALAMEL', modelAlamel), &
                                                   MapItem('FCTaylor', modelFCTaylor)]
        character(*), parameter :: PROC_NAME = 'readAltayConfigSection'
            info = ERR_IO
           model_id = -1
           dm_id = -1
            ! Read input texture file name
            if (.not. readValue(cnfunit, cnf%texture_input_fname)) return
            ! Determine crystal plasticity model type
            if (.not. readKeyword(cnfunit, model_types, model_id)) then
                write(error_unit,fmt=900) 'Unsupported crystal plasticity model.'
                info = VEF_ERROR
                return
            endif
            
            dummy2 = readValue(cnfunit, dummy)
            read(cnfunit, '(A)') buffer
            slip = buffer

            select case (slip)
                case ('fcc12')
                    cnf%deformation_mechanism = FCC12
                case ('bcc24')
                    cnf%deformation_mechanism = BCC24
                case ('bcc48')
                    cnf%deformation_mechanism = BCC48
                case default
                    call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Invalid slip system identifier')
            end select

            ! Process advanced microstructure characterization
            use_default_microstructure = .true.
            if (.not. readValue(cnfunit, use_default_microstructure)) return
            if (.not. use_default_microstructure) then
                  if (.not. readValue(cnfunit, cnf%micros_fname)) return ! read <microstructure>.smt filename
                  ! Read user-supplied initial deformation gradient
                  do i=1,3
                        if (.not. readValue(cnfunit, cnf%simul_init%Fmicro(:,i))) then
                            write(error_unit,fmt=900) 'Cannot read deformation gradient.'
                            info = VEF_ERROR
                            return
                        endif
                  enddo
            end if
            ! Process hardening model section
            call readHardeningSection(cnfunit, cnf, info)
            if (info /= VEF_OK) then
                write(error_unit,fmt=900) 'Cannot read the hardening law section.'
                return
            endif

            call parameter_set(cnf%hardening_parameters, 'n_slip_systems', size(cnf%deformation_mechanism,3))
            ! the keyword is mapped to a proper model_id, we can instantly set it.
            call setModelType(cnf,model_id,info)
            if (info /= VEF_OK) return
            !
            !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      !
      end subroutine

      !> Read configuration of hardening model from configuration file
      subroutine readHardeningSection(cnfunit, cnf, info)
      integer,intent(in)                  :: cnfunit
        type(AltayConfigData), intent(inout) :: cnf
      integer,intent(out)                 :: info
        type(Parameter), allocatable :: params(:)
        integer :: hardening_model_id
        real(DP) :: tmp(16)
        character(len=max_pathlen)          :: tmp_fname
        integer                             :: nparunit, ioerr,i
      !
      logical :: use_default_hardening
        logical :: read_state_dummy
      !
        info = VEF_OK
            use_default_hardening = .true.
            if (.not. readValue(cnfunit, use_default_hardening)) then ! read default hardening flag
                  write(error_unit,fmt=900) 'Reading of the default hardening flag unsuccessful.'
                  return
            endif
            if (.not. use_default_hardening) then
                if (.not. readValue(cnfunit, hardening_model_id)) return
                params = hardening_get_parameters(hardening_model_id)

                  select case(hardening_model_id)
                  case(HARDENING_VOCE)
                        if (readValue(cnfunit, tmp(1:5))) then
                            call parameter_set(params, 'TIII1', tmp(1))
                            call parameter_set(params, 'TIIIS', tmp(2))
                            call parameter_set(params, 'TIVS', tmp(3))
                            call parameter_set(params, 'THIII1', tmp(4))
                            call parameter_set(params, 'THT', tmp(5))
                              info = VEF_OK
                        endif
                  case(HARDENING_SWIFT)
                        ! Read one line
                        if (readValue(cnfunit, tmp(1:3))) then
                            call parameter_set(params, 'crss0', tmp(1))
                            call parameter_set(params, 'gamma0', tmp(2))
                            call parameter_set(params, 'n', tmp(3))
                              info = VEF_OK
                        endif
                  !
                  case(HARDENING_BP, HARDENING_PEBP_SCREW, HARDENING_PEBP_LOOP)
                        if (.not. readValue(cnfunit, tmp_fname)) return ! read BP parameter file name
                        open(newunit=nparunit,file=tmp_fname,status='old', iostat=ioerr)

                        if (ioerr /= 0) then
                        return
                        endif

                        do i=1,16
                            read(nparunit,fmt=100,err=666,end=666) tmp(i)
                        end do
100                     format(F12.5)

                            call parameter_set(params, 'b', tmp(1))
                            call parameter_set(params, 'G', tmp(2))
                            call parameter_set(params, 'alfa', tmp(3))
                            call parameter_set(params, 'f', tmp(4))
                            call parameter_set(params, 'tau0', tmp(5))
                            call parameter_set(params, 'I', tmp(6))
                            call parameter_set(params, 'R', tmp(7))
                            call parameter_set(params, 'Iwd', tmp(8))
                            call parameter_set(params, 'Rwd', tmp(9))
                            call parameter_set(params, 'Rncg', tmp(10))
                            call parameter_set(params, 'beta1', tmp(11))
                            call parameter_set(params, 'beta2', tmp(12))
                            call parameter_set(params, 'Iwp', tmp(13))
                            call parameter_set(params, 'Rwp', tmp(14))
                            call parameter_set(params, 'Rrev', tmp(15))
                            call parameter_set(params, 'R2', tmp(16))
                        if (.not. readValue(cnfunit, read_state_dummy)) return
                  end select
                cnf%hardening_parameters = params
            else
                  cnf%hardening_parameters = hardening_get_parameters(HARDENING_NONE)
            endif

666         return
! message formats
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      !
      end subroutine

end module
