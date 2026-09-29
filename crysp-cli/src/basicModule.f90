!> Implementation of a basic DMC computational module.
module dmcBasicModule
    use, intrinsic:: iso_fortran_env, only: error_unit, output_unit
    use base_defs
    use crysp_micro
    use meso
    use crysp_model, only: ModelData, ModelDescriptor
    use crysp_material, only: Material
    use crysp_parameters
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
          type(Material):: material
    contains
          procedure:: initialize =>  BasicModule_initialize
          procedure:: run => BasicModule_run
    end type

contains

    !> General intialization shared between all modules.
    !>
    !> `this` is `target` because the grains of the material point to its phases: those pointers must stay valid after return.
    subroutine BasicModule_initialize(this, cnfunit)
        class(BasicModule), target, intent(inout):: this
        integer, intent(in):: cnfunit

        character(*), parameter:: PROC_NAME = 'readconfig'

        character(FNAME_LEN):: texture_file_name, &
                               microstructure_file_name
        character(20):: buffer
        integer:: i, &
                  n_params
        class(ConstitutiveModel), allocatable:: hardening_model
        type(ModelDescriptor):: desc
        real(DP), allocatable:: tmp(:)
        type(ModelData):: meso_data
        type(PhaseData):: phase_data(1)

        ! Read input texture file name
        call read_value(cnfunit, texture_file_name)
        phase_data(1)%orientations = read_orientations(trim(texture_file_name))

        ! Determine crystal plasticity model type
        read(cnfunit, '(A)') buffer
        select case (buffer)
            case ('FCTaylor')
                meso_data%id = MESO_MODEL_FCTAYLOR
                allocate(meso_data%parameters(0))
            case ('ALAMEL')
                meso_data%id = MESO_MODEL_ALAMEL
                call read_value(cnfunit, microstructure_file_name)
                allocate(meso_data%parameters(1))
                meso_data%parameters(1) = read_microstructure(microstructure_file_name)
            case default
                call log_error(MOD_NAME, PROC_NAME, CRYSP_ERR_VAL, 'Invalid mesoscopic model.')
        end select

        read(cnfunit, '(A)') buffer
        select case (buffer)
            case ('fcc12')
                phase_data(1)%deformation_mechanism = SLIP_SYSTEMS_FCC
            case ('bcc24')
                phase_data(1)%deformation_mechanism = SLIP_SYSTEMS_BCC24
            case ('bcc48')
                phase_data(1)%deformation_mechanism = SLIP_SYSTEMS_BCC48
            case default
                call log_error(MOD_NAME, PROC_NAME, CRYSP_ERR_VAL, 'Invalid slip system identifier')
        end select

        !Read hardening section
        call read_value(cnfunit, phase_data(1)%model%id)
        call handle_crysp_status(micro_get_model(phase_data(1)%model%id, hardening_model))
        desc = hardening_model%describe()
        n_params = size(desc%inputs)

        !Hardening parameters must be provided in the order in which they are defined in the hardening models.
        allocate(phase_data(1)%model%parameters(n_params))
        if (n_params > 0) then
            allocate(tmp(n_params))
            call read_value(cnfunit,tmp)
            do i=1,n_params
                phase_data(1)%model%parameters(i) = tmp(i)
            end do
        end if

        call handle_crysp_status(crysp_new_material(meso_data, phase_data, this%material))
    end subroutine

    subroutine BasicModule_run(this)
        class(BasicModule), target, intent(inout):: this
    end subroutine
end module
