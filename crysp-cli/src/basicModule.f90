!> Implementation of a basic DMC computational module.
module dmcBasicModule
    use, intrinsic:: iso_fortran_env, only: error_unit, output_unit
    use base_defs
    use micro
    use meso
    use parameters
    use logging
    use libcrysp
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
    end type

contains

    !> read output and AlTay configuration sections
    subroutine BasicModule_initialize(this, cnfunit)
        class(BasicModule), intent(inout):: this
        integer, intent(in):: cnfunit

        character(*), parameter:: PROC_NAME = 'readconfig'

        character(FNAME_LEN):: texture_file_name, &
                               microstructure_file_name
        character(20):: buffer
        integer:: meso_model_id, &
                  i, &
                  n_params
        real(DP), allocatable:: tmp(:)
        character(:), allocatable:: meso_params
        type(PhaseDescriptor):: phase_

        ! Read input texture file name
        call read_value(cnfunit, texture_file_name)
        phase_%orientations = read_orientations(trim(texture_file_name))

        ! Determine crystal plasticity model type
        read(cnfunit, '(A)') buffer
        select case (buffer)
            case ('FCTaylor')
                meso_model_id = MESO_MODEL_FCTAYLOR
            case ('ALAMEL')
                meso_model_id = MESO_MODEL_ALAMEL
                call read_value(cnfunit, microstructure_file_name)
                call json_list_add(meso_params, to_json(read_microstructure(microstructure_file_name))
            case default
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Invalid mesoscopic model.')
        end select

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
        call read_value(cnfunit, phase_%model_id)
        phase_%parameters = micro_get_parameters(phase_%model_id)
        n_params = size(phase_%parameters)

        !Hardening parameters must be provided in the order in which they are defined in the hardening models.
        if (n_params > 0) then
            allocate(tmp(n_params))
            call read_value(cnfunit,tmp)
            do i=1,n_params
                json_list_add(phase_%parameters, to_json(tmp(i)))
            end do
        end if

        call crysp_new_material(meso_model_id, meso_params, [phase_], this%material)
    end subroutine

    subroutine BasicModule_run(this)
        class(BasicModule), intent(inout):: this
    end subroutine
end module
