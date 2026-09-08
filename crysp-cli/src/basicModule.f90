!> Implementation of a basic DMC computational module.
module dmcBasicModule
    use, intrinsic:: iso_fortran_env, only: error_unit, output_unit
    use iso_c_binding, only: C_INT
    use base_defs
    use micro
    use meso
    use crysp_serialization
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
          type(Parameter), dimension(:), allocatable:: material
    contains
          procedure:: initialize =>  BasicModule_initialize
          procedure:: run => BasicModule_run
    end type

contains

    !> General intialization shared between all modules.
    subroutine BasicModule_initialize(this, cnfunit)
        class(BasicModule), intent(inout):: this
        integer, intent(in):: cnfunit

        character(*), parameter:: PROC_NAME = 'readconfig'

        character(FNAME_LEN):: texture_file_name, &
                               microstructure_file_name
        character(20):: buffer
        integer:: meso_model_id, &
                  hardening_model_id, &
                  n_grains, &
                  i, &
                  n_params
        integer(C_INT):: phase_sizes(1)
        integer(C_INT), allocatable:: deformation_mechanisms(:), &
                                       hardening_model_ids(:)
        real(DP), allocatable:: tmp(:), &
                                orientations(:,:)
        type(Parameter), allocatable:: meso_params(:), &
                                       hardening_params(:)

        ! Read input texture file name
        call read_value(cnfunit, texture_file_name)
        orientations = read_orientations(trim(texture_file_name))
        n_grains = size(orientations, 2)

        ! Determine crystal plasticity model type
        read(cnfunit, '(A)') buffer
        select case (buffer)
            case ('FCTaylor')
                meso_model_id = MESO_MODEL_FCTAYLOR
                allocate(meso_params(0))
            case ('ALAMEL')
                meso_model_id = MESO_MODEL_ALAMEL
                call read_value(cnfunit, microstructure_file_name)
                allocate(meso_params(1))
                meso_params(1) = read_microstructure(microstructure_file_name)
            case default
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Invalid mesoscopic model.')
        end select

        allocate(deformation_mechanisms(1))
        read(cnfunit, '(A)') buffer
        select case (buffer)
            case ('fcc12')
                deformation_mechanisms(1) = SLIP_SYSTEMS_FCC
            case ('bcc24')
                deformation_mechanisms(1) = SLIP_SYSTEMS_BCC24
            case ('bcc48')
                deformation_mechanisms(1) = SLIP_SYSTEMS_BCC48
            case default
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Invalid slip system identifier')
        end select

        !Read hardening section
        call read_value(cnfunit, hardening_model_id)
        n_params = size(micro_get_signature(hardening_model_id))

        !Hardening parameters must be provided in the order in which they are defined in the hardening models.
        if (n_params > 0) then
            allocate(tmp(n_params))
            allocate(hardening_params(n_params))
            call read_value(cnfunit,tmp)
            do i=1,n_params
                hardening_params(i) = tmp(i)
            end do
        else
            allocate(hardening_params(0))
        end if

        !Assemble the model inputs for libcrysp
        allocate(hardening_model_ids(1))
        hardening_model_ids(1) = hardening_model_id
        phase_sizes(1) = n_grains

        call crysp_new_material(phase_sizes, orientations, deformation_mechanisms, &
                                hardening_model_ids, hardening_params, &
                                meso_model_id, meso_params, this%material)
    end subroutine

    subroutine BasicModule_run(this)
        class(BasicModule), intent(inout):: this
    end subroutine
end module
