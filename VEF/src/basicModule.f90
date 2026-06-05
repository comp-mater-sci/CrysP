!> Implementation of a basic DMC computational module.
module dmcBasicModule
    use, intrinsic:: iso_fortran_env, only: error_unit, output_unit
    use base_defs
    use micro
    use meso
    use parameters
    use logging
    use altay
    use file_io

    implicit none

    private
    public:: outputConfig, &
              BasicModule, &
              readAlTayConfigSection

    character(*), parameter:: MOD_NAME = 'basicModule'


   !> Root-level configuration structure of Altay
   type:: altayConfigData
        integer                                     :: model_id = MESO_MODEL_ALAMEL
        character(len = fname_len)                  :: output_prefix = 'alamel'
        character(len = fname_len)                  :: jobtitle      = 'alamel'
        character(len = fname_len)                  :: micros_fname  = 'equiaxed.smt'
        character(len = fname_len)                  :: texture_input_fname = ''
        integer:: hardening_model_id
        type(Parameter), allocatable:: hardening_parameters(:)
        integer:: deformation_mechanism
        integer                                   :: nfile = 0
   end type

    type:: outputConfig
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
    type:: BasicModule
          type(outputConfig)::    output
          type(altayConfigData):: altay !< Root-level configuration structure of texture and hardening
          type(MaterialState)::   material
    contains
          procedure:: initialize =>  BasicModule_initialize
          procedure:: readConfig => BasicModule_readConfig
          procedure:: run => BasicModule_run
          procedure:: openOutputFile => BasicModule_openOutputFile
    end type

contains

    subroutine init_altay(altay_config, material)
        type(AltayConfigData), intent(in):: altay_config
        type(MaterialState), target, intent(out):: material

        integer:: meso_model_id, &
                  info
        type(Parameter), dimension(:), allocatable, target:: meso_params
        type(Parameter), pointer:: param_ptr
        type(PhaseDescriptor):: phase_

        meso_model_id = altay_config%model_id
        meso_params = meso_get_parameters(meso_model_id)
        if (meso_params .includes. "Boundaries") then
            param_ptr => meso_params .find. "Boundaries"
            param_ptr = read_boundaries(altay_config%micros_fname)
        end if

        call open_output_files(altay_config%output_prefix, altay_config%nfile, info)
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, 'initialize', ERR_IO, 'Cannot open output files.')

        !Even though the back-end logic can handle n phases, the current I/O structure only sopports 1 phase. Therefore, wrap the
        !description of this one phase in a phase descriptor and pass it as a 1-element list to micro_init
        phase_%model_id = altay_config%hardening_model_id
        phase_%deformation_mechanism = altay_config%deformation_mechanism
        phase_%parameters = altay_config%hardening_parameters
        phase_%orientations = read_texture(trim(altay_config%texture_input_fname))

        call altay_new_material(meso_model_id, meso_params, [phase_], material)
    end subroutine


    subroutine BasicModule_initialize(this)
        class(BasicModule), target, intent(inout):: this

        ! Finish the configuration:
        this%altay%nfile = merge(1, 0, this%output%outputRequest)
        this%altay%output_prefix = trim(this%output%outputPrefix)
        this%altay%jobtitle = trim(this%output%outputPrefix)

        call init_altay(this%altay, this%material)
    end subroutine

    !> read output and AlTay configuration sections
    integer function BasicModule_readConfig(this, cnfunit) result(info)
        class(BasicModule), intent(inout):: this
        integer, intent(in)               :: cnfunit !< IO input unit

        character(*), parameter:: PROC_NAME = 'readconfig'

        ! Read output configuration lines
        call readOutputConfigSection(cnfunit, this%output, info)  ! top 3 lines after comment header of config file
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Check output configuration section.')

        ! Read AlTay configuration lines
        call readAlTayConfigSection(cnfunit, this%altay, info)  ! read configuration of texture, slip systems, microstructure and hardening
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Check libAltay configuration section.')
    end function

    subroutine BasicModule_run(this, info)
    class(BasicModule), intent(inout):: this
    integer, intent(out)                 :: info
    !
        info = VEF_OK
    !
    end subroutine

    !> Open output file
    integer function BasicModule_openOutputFile(this, ext, ofunit, suffix) result(info)
    class(BasicModule), intent(in)           :: this
    character(len=*), intent(in)             :: ext !< File extension (with leading dot)
    integer, intent(out)                     :: ofunit !< IO unit of the output
    character(len=*), intent(in), optional    :: suffix !< Suffix to the file
    character(len = max_pathlen):: output_path
    integer:: ierr

        if (present(suffix)) then
            output_path = trim(this%output%outputPrefix)// trim(suffix) //trim(ext)
        else
            output_path = trim(this%output%outputPrefix)// trim(ext)
        endif

        open(newunit = ofunit, file = output_path, status='replace', iostat = ierr)
        if (ierr /= 0) &
            call log_error(MOD_NAME, 'open_output_file', ERR_IO, 'Could not open output file.')

        info = VEF_OK
    end function

    !> Read output configuration from top 3 lines after comment header in configuration file:
    !> prefix for output files, incremental output request flag, verbosity level
    subroutine readOutputConfigSection(cnfunit, cnf, info)
        integer, intent(in)                  :: cnfunit !< configuration file
        type(outputConfig), intent(inout)    :: cnf
        integer, intent(out)                 :: info

        character(*), parameter:: PROC_NAME = 'readoutputconfigsection'

          info = VEF_ERROR
          if (.not. readValue(cnfunit, cnf%outputPrefix)) &
              call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Unable to read output file prefix')
          if (.not. readValue(cnfunit, cnf%outputRequest)) &
              call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Unable to read output request flag')
          if (.not. readValue(cnfunit, cnf%verbosity)) &
              call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Unable to read verbosity level')
          info = VEF_OK
    end subroutine

    !> Read configuration of libaltay
    subroutine readAlTayConfigSection(cnfunit, cnf, info)
        integer, intent(in)                  :: cnfunit
        type(altayConfigData), target, intent(inout):: cnf !< Root-level configuration structure of texture, microstructure and hardening
        integer, intent(out)                 :: info

        character(*), parameter:: PROC_NAME = 'readAltayConfigSection'

        integer                       :: model_id
        logical                       :: use_default_microstructure
        integer                       :: i
        character(20):: buffer
        type(Parameter), pointer:: param_ptr

        info = ERR_IO

        ! Read input texture file name
        if (.not. readValue(cnfunit, cnf%texture_input_fname)) return

        ! Determine crystal plasticity model type
        read(cnfunit, '(A)') buffer
        select case (buffer)
            case ('FCTaylor')
                cnf%model_id = MESO_MODEL_FCTAYLOR
            case ('ALAMEL')
                cnf%model_id = MESO_MODEL_ALAMEL
                if (.not. readValue(cnfunit, cnf%micros_fname)) return  ! read < microstructure>.smt filename
            case default
                call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Invalid mesoscopic model.')
        end select

        read(cnfunit, '(A)') buffer
        select case (buffer)
            case ('fcc12')
                cnf%deformation_mechanism = SLIP_SYSTEMS_FCC
            case ('bcc24')
                cnf%deformation_mechanism = SLIP_SYSTEMS_BCC24
            case ('bcc48')
                cnf%deformation_mechanism = SLIP_SYSTEMS_BCC48
            case default
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Invalid slip system identifier')
        end select

        ! Process hardening model section
        call readHardeningSection(cnfunit, cnf, info)
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Cannot read the hardening law section.')
    end subroutine

    !> Read configuration of hardening model from configuration file
    subroutine readHardeningSection(cnfunit, cnf, info)
        integer, intent(in)                  :: cnfunit
        type(AltayConfigData), intent(inout):: cnf
        integer, intent(out)                 :: info

        character(*), parameter:: PROC_NAME = 'readhardeningsection'

        integer:: hardening_model_id, &
                  nparunit, &
                  ioerr, &
                  i
        type(Parameter), dimension(:), allocatable, target:: params
        real(DP):: tmp(16)
        character(len = max_pathlen)          :: tmp_fname
        type(Parameter), pointer:: param_ptr

        info = VEF_OK

        if (.not. readValue(cnfunit, hardening_model_id)) return
        cnf%hardening_model_id = hardening_model_id
        params = micro_get_parameters(hardening_model_id)

        select case(hardening_model_id)
            case(HARDENING_VOCE)
                if (readValue(cnfunit, tmp(1:5))) then
                    param_ptr => params .find. 'TIII1'
                    param_ptr = tmp(1)
                    param_ptr => params .find. 'TIIIS'
                    param_ptr = tmp(2)
                    param_ptr => params .find. 'TIVS'
                    param_ptr = tmp(3)
                    param_ptr => params .find. 'THIII1'
                    param_ptr = tmp(4)
                    param_ptr => params .find. 'THT'
                    param_ptr = tmp(5)
                    info = VEF_OK
                endif
            case(HARDENING_SWIFT)
                if (readValue(cnfunit, tmp(1:3))) then
                    param_ptr => params .find. 'crss0'
                    param_ptr = tmp(1)
                    param_ptr => params .find. 'gamma0'
                    param_ptr = tmp(2)
                    param_ptr => params .find. 'n'
                    param_ptr = tmp(3)
                    info = VEF_OK
                endif
            case(HARDENING_HOCKETT_SHERBY)
                if (readValue(cnfunit, tmp(1:4))) then
                    param_ptr => params .find. 'tau_0'
                    param_ptr = tmp(1)
                    param_ptr => params .find. 'tau_sat'
                    param_ptr = tmp(2)
                    param_ptr => params .find. 'b'
                    param_ptr = tmp(3)
                    param_ptr => params .find. 'n'
                    param_ptr = tmp(4)
                    info = VEF_OK
                endif
            case(HARDENING_DSH_EDGE, HARDENING_DSH_SCREW, HARDENING_DSH_LOOP)
                if (.not. readValue(cnfunit, tmp_fname)) return  ! read BP parameter file name
                open(newunit = nparunit, file = tmp_fname, status='old', iostat = ioerr)

                if (ioerr /= 0) then
                return
                endif

                do i = 1, 16
                    read(nparunit, fmt = 100, err = 666, end = 666) tmp(i)
                end do
100               format(F12.5)

                param_ptr => params .find. 'b'
                param_ptr = tmp(1)
                param_ptr => params .find. 'G'
                param_ptr = tmp(2)
                param_ptr => params .find. 'alfa'
                param_ptr = tmp(3)
                param_ptr => params .find. 'f'
                param_ptr = tmp(4)
                param_ptr => params .find. 'tau0'
                param_ptr = tmp(5)
                param_ptr => params .find. 'I'
                param_ptr = tmp(6)
                param_ptr => params .find. 'R'
                param_ptr = tmp(7)
                param_ptr => params .find. 'Iwd'
                param_ptr = tmp(8)
                param_ptr => params .find. 'Rwd'
                param_ptr = tmp(9)
                param_ptr => params .find. 'Rncg'
                param_ptr = tmp(10)
                param_ptr => params .find. 'beta1'
                param_ptr = tmp(11)
                param_ptr => params .find. 'beta2'
                param_ptr = tmp(12)
                param_ptr => params .find. 'Iwp'
                param_ptr = tmp(13)
                param_ptr => params .find. 'Rwp'
                param_ptr = tmp(14)
                param_ptr => params .find. 'Rrev'
                param_ptr = tmp(15)
                param_ptr => params .find. 'R2'
                param_ptr = tmp(16)
          end select

          cnf%hardening_parameters = params

666       return
      end subroutine
end module
