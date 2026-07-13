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
    public:: BasicModule

    character(*), parameter:: MOD_NAME = 'basicModule'

   !> Root-level configuration structure of Altay
   type:: altayConfigData
        integer::                      meso_model_id
        character(len = fname_len)::   microstructure_file_name
        character(len = fname_len)::   texture_file_name
        integer::                      hardening_model_id
        type(Parameter), allocatable:: hardening_parameters(:)
        integer::                      deformation_mechanism
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
          character(:), allocatable:: output_prefix
          logical:: print_state
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

        meso_model_id = altay_config%meso_model_id
        meso_params = meso_get_parameters(meso_model_id)
        if (meso_params .includes. "Boundaries") then
            param_ptr => meso_params .find. "Boundaries"
            param_ptr = read_boundaries(altay_config%microstructure_file_name)
        end if

        !Even though the back-end logic can handle n phases, the current I/O structure only sopports 1 phase. Therefore, wrap the
        !description of this one phase in a phase descriptor and pass it as a 1-element list to micro_init
        phase_%model_id = altay_config%hardening_model_id
        phase_%deformation_mechanism = altay_config%deformation_mechanism
        phase_%parameters = altay_config%hardening_parameters
        phase_%orientations = read_texture(trim(altay_config%texture_file_name))

        call altay_new_material(meso_model_id, meso_params, [phase_], material)
    end subroutine


    subroutine BasicModule_initialize(this)
        class(BasicModule), target, intent(inout):: this

        call init_altay(this%altay, this%material)
    end subroutine

    !> read output and AlTay configuration sections
    subroutine BasicModule_readConfig(this, cnfunit)
        class(BasicModule), intent(inout):: this
        integer, intent(in):: cnfunit

        character(*), parameter:: PROC_NAME = 'readconfig'

        logical:: read_success
        character(20):: buffer
        character(FNAME_LEN):: dsh_params_file_name
        integer:: hardening_model_id, &
                  dsh_unit, &
                  ioerr, &
                  i
        real(DP):: tmp(16)
        type(Parameter), dimension(:), allocatable, target:: hardening_params
        type(Parameter), pointer:: param_ptr

        ! Read input texture file name
        if (.not. readValue(cnfunit, this%altay%texture_file_name)) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read texture file name')

        ! Determine crystal plasticity model type
        read(cnfunit, '(A)') buffer
        select case (buffer)
            case ('FCTaylor')
                this%altay%meso_model_id = MESO_MODEL_FCTAYLOR
            case ('ALAMEL')
                this%altay%meso_model_id = MESO_MODEL_ALAMEL
                if (.not. readValue(cnfunit, this%altay%microstructure_file_name)) &
                    call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read microstructure file name')
            case default
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Invalid mesoscopic model.')
        end select

        read(cnfunit, '(A)') buffer
        select case (buffer)
            case ('fcc12')
                this%altay%deformation_mechanism = SLIP_SYSTEMS_FCC
            case ('bcc24')
                this%altay%deformation_mechanism = SLIP_SYSTEMS_BCC24
            case ('bcc48')
                this%altay%deformation_mechanism = SLIP_SYSTEMS_BCC48
            case default
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Invalid slip system identifier')
        end select

        !Read hardening section
        if (.not. readValue(cnfunit, hardening_model_id)) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read hardening model ID')
        this%altay%hardening_model_id = hardening_model_id
        hardening_params = micro_get_parameters(hardening_model_id)

        read_success = .true.
        select case(hardening_model_id)
            case(HARDENING_VOCE)
                if (readValue(cnfunit, tmp(1:5))) then
                    param_ptr => hardening_params .find. 'TIII1'
                    param_ptr = tmp(1)
                    param_ptr => hardening_params .find. 'TIIIS'
                    param_ptr = tmp(2)
                    param_ptr => hardening_params .find. 'TIVS'
                    param_ptr = tmp(3)
                    param_ptr => hardening_params .find. 'THIII1'
                    param_ptr = tmp(4)
                    param_ptr => hardening_params .find. 'THT'
                    param_ptr = tmp(5)
                else
                    read_success = .false.
                endif
            case(HARDENING_SWIFT)
                if (readValue(cnfunit, tmp(1:3))) then
                    param_ptr => hardening_params .find. 'crss0'
                    param_ptr = tmp(1)
                    param_ptr => hardening_params .find. 'gamma0'
                    param_ptr = tmp(2)
                    param_ptr => hardening_params .find. 'n'
                    param_ptr = tmp(3)
                else
                    read_success = .false.
                endif
            case(HARDENING_HOCKETT_SHERBY)
                if (readValue(cnfunit, tmp(1:4))) then
                    param_ptr => hardening_params .find. 'tau_0'
                    param_ptr = tmp(1)
                    param_ptr => hardening_params .find. 'tau_sat'
                    param_ptr = tmp(2)
                    param_ptr => hardening_params .find. 'b'
                    param_ptr = tmp(3)
                    param_ptr => hardening_params .find. 'n'
                    param_ptr = tmp(4)
                else
                    read_success = .false.
                endif
            case(HARDENING_DSH_EDGE, HARDENING_DSH_SCREW, HARDENING_DSH_LOOP)
                if (readValue(cnfunit, dsh_params_file_name)) then
                    open(newunit = dsh_unit, file = dsh_params_file_name, status='old', iostat = ioerr)
                    if (ioerr /= VEF_OK) &
                        call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Unable to open DSH parameter file')
                    do i = 1, 16
                        read(dsh_unit, fmt = '(F12.5)', err = 666, end = 666) tmp(i)
                    end do
                    close(dsh_unit)

                    param_ptr => hardening_params .find. 'b'
                    param_ptr = tmp(1)
                    param_ptr => hardening_params .find. 'G'
                    param_ptr = tmp(2)
                    param_ptr => hardening_params .find. 'alfa'
                    param_ptr = tmp(3)
                    param_ptr => hardening_params .find. 'f'
                    param_ptr = tmp(4)
                    param_ptr => hardening_params .find. 'tau0'
                    param_ptr = tmp(5)
                    param_ptr => hardening_params .find. 'I'
                    param_ptr = tmp(6)
                    param_ptr => hardening_params .find. 'R'
                    param_ptr = tmp(7)
                    param_ptr => hardening_params .find. 'Iwd'
                    param_ptr = tmp(8)
                    param_ptr => hardening_params .find. 'Rwd'
                    param_ptr = tmp(9)
                    param_ptr => hardening_params .find. 'Rncg'
                    param_ptr = tmp(10)
                    param_ptr => hardening_params .find. 'beta1'
                    param_ptr = tmp(11)
                    param_ptr => hardening_params .find. 'beta2'
                    param_ptr = tmp(12)
                    param_ptr => hardening_params .find. 'Iwp'
                    param_ptr = tmp(13)
                    param_ptr => hardening_params .find. 'Rwp'
                    param_ptr = tmp(14)
                    param_ptr => hardening_params .find. 'Rrev'
                    param_ptr = tmp(15)
                    param_ptr => hardening_params .find. 'R2'
                    param_ptr = tmp(16)
                else
                   read_success = .false.
                end if
            end select

          if (read_success) then
              this%altay%hardening_parameters = hardening_params
              return
          end if
          666 call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read hardening parameters')
    end subroutine

    subroutine BasicModule_run(this, info)
    class(BasicModule), intent(inout):: this
    integer, intent(out)                 :: info
    !
        info = VEF_OK
    !
    end subroutine

    !> Open output file
    subroutine BasicModule_openOutputFile(this, ext, ofunit)
        class(BasicModule), intent(in)           :: this
        character(len=*), intent(in)             :: ext !< File extension (with leading dot)
        integer, intent(out)                     :: ofunit !< IO unit of the output
        integer:: ierr

        open(newunit = ofunit, file = this%output_prefix // ext, status='replace', iostat = ierr)
        if (ierr /= 0) &
            call log_error(MOD_NAME, 'open_output_file', ERR_IO, 'Could not open output file.')
    end subroutine
end module
