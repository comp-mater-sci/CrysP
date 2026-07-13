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
          type(MaterialState)::   material
    contains
          procedure:: initialize =>  BasicModule_initialize
          procedure:: run => BasicModule_run
          procedure:: openOutputFile => BasicModule_openOutputFile
    end type

contains

    !> read output and AlTay configuration sections
    subroutine BasicModule_initialize(this, cnfunit)
        class(BasicModule), intent(inout):: this
        integer, intent(in):: cnfunit

        character(*), parameter:: PROC_NAME = 'readconfig'

        logical:: read_success
        character(FNAME_LEN):: texture_file_name, &
                               microstructure_file_name, &
                               dsh_params_file_name
        character(20):: buffer
        integer:: meso_model_id, &
                  dsh_unit, &
                  ioerr, &
                  i
        real(DP):: tmp(16)
        real(DP), dimension(:), allocatable:: boundaries
        type(Parameter), dimension(:), allocatable:: meso_params
        type(PhaseDescriptor), target:: phase_

        ! Read input texture file name
        if (.not. readValue(cnfunit, texture_file_name)) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read texture file name')
        phase_%orientations = read_texture(trim(texture_file_name))

        ! Determine crystal plasticity model type
        read(cnfunit, '(A)') buffer
        select case (buffer)
            case ('FCTaylor')
                meso_model_id = MESO_MODEL_FCTAYLOR
            case ('ALAMEL')
                meso_model_id = MESO_MODEL_ALAMEL
                if (.not. readValue(cnfunit, microstructure_file_name)) &
                    call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read microstructure file name')
            case default
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Invalid mesoscopic model.')
        end select
        meso_params = meso_get_parameters(meso_model_id)
        if (meso_params .includes. "Boundaries") &
            call parameter_set(meso_params, 'Boundaries', read_boundaries(microstructure_file_name))

        read(cnfunit, '(A)') buffer
        select case (buffer)
            case ('fcc12')
                phase_%deformation_mechanism = SLIP_SYSTEMS_FCC
            case ('bcc24')
                phase_%deformation_mechanism = SLIP_SYSTEMS_BCC24
            case ('bcc48')
                phase_%deformation_mechanism = SLIP_SYSTEMS_BCC48
            case default
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Invalid slip system identifier')
        end select

        !Read hardening section
        if (.not. readValue(cnfunit, phase_%model_id)) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read hardening model ID')
        phase_%parameters = micro_get_parameters(phase_%model_id)

        read_success = .true.
        associate(params => phase_%parameters)
            select case(phase_%model_id)
                case(HARDENING_VOCE)
                    if (readValue(cnfunit, tmp(1:5))) then
                        call parameter_set(params, 'TIII1', tmp(1))
                        call parameter_set(params, 'TIIIS', tmp(2))
                        call parameter_set(params, 'TIVS', tmp(3))
                        call parameter_set(params, 'THIII1', tmp(4))
                        call parameter_set(params, 'THT', tmp(5))
                    else
                        read_success = .false.
                    endif
                case(HARDENING_SWIFT)
                    if (readValue(cnfunit, tmp(1:3))) then
                        call parameter_set(params, 'crss0', tmp(1))
                        call parameter_set(params, 'gamma0', tmp(2))
                        call parameter_set(params, 'n', tmp(3))
                    else
                        read_success = .false.
                    endif
                case(HARDENING_HOCKETT_SHERBY)
                    if (readValue(cnfunit, tmp(1:4))) then
                        call parameter_set(params, 'tau_0', tmp(1))
                        call parameter_set(params, 'tau_sat', tmp(2))
                        call parameter_set(params, 'b', tmp(3))
                        call parameter_set(params, 'n', tmp(4))
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
                    else
                       read_success = .false.
                    end if
                end select
        end associate

        if (read_success) then
            call altay_new_material(meso_model_id, meso_params, [phase_], this%material)
            return
        end if
        666 call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read hardening parameters')
    end subroutine

    subroutine BasicModule_run(this, info)
        class(BasicModule), intent(inout):: this
        integer, intent(out)                 :: info

        info = VEF_OK
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
