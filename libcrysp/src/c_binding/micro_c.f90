module crysp_micro_c
    use iso_c_binding
    use crysp_utils_c
    use crysp_micro
    use crysp_constitutive_model
    use crysp_grain

    implicit none

    !> C-compatible counterpart of [[HardeningState]].
    !>
    !> `crss` holds the columns of its Fortran counterpart one after the other. `params` holds the state added by the concrete
    !> hardening state, as produced by its `serialize`. A `HardeningState_C` produced by libCrysP owns `crss`, `params` and the
    !> data of each parameter, malloc'ed so that C can release them with `free()`.
    type, bind(C):: HardeningState_C
        integer(C_INT):: n_systems
        type(C_PTR):: crss     !! n_systems * 2 * double
        integer(C_INT):: n_params
        type(C_PTR):: params !! n_params * type(Parameter_C)
    end type

    !> C-compatible counterpart of [[Grain]].
    !>
    !> `phase` refers to the element of the material's C-side phase list that backs this grain, as [[Grain]] points to its
    !> [[Phase]]. It does not own that element.
    type, bind(C):: Grain_C
        type(C_PTR):: phase !! type(ConstitutiveModel_C).
        real(C_DOUBLE), dimension(3,3):: orientation
        real(C_DOUBLE):: stress_increment
        type(HardeningState_C):: hardening_state
    end type

    !> C-compatible counterpart of [[PhaseData]]. `orientations` holds the Euler angle triplets one after the other.
    type, bind(C):: PhaseData_C
        type(ModelData_C):: model
        integer(C_INT):: deformation_mechanism
        integer(C_INT):: n_orientations
        type(C_PTR):: orientations  !! n_orientations * real(C_DOUBLE) * 3
    end type


    !> C-compatible counterpart of [[ConstitutiveModel]].
    !>
    !> `hardening_model_state` holds the model ID and the state specific to the concrete model, see [[crysp_model:ModelData_C]].
    !> The coefficient matrices hold the columns of their Fortran counterparts one after the other. A `ConstitutiveModel_C`
    !> produced by libCrysP owns all of its pointers, malloc'ed so that C can release them with `free()`.
    type, bind(C):: ConstitutiveModel_C
        type(ModelData_C):: hardening_model_state
        integer(C_INT):: n_systems
        type(C_PTR):: taylor_coeffs !! n_systems * 5 * real(C_DOUBLE)
        type(C_PTR):: spin_coeffs   !! n_systems * 3 * real(C_DOUBLE)
        integer(C_INT), dimension(5):: basis
    end type


    interface assignment(=)
        module procedure phase_data_from_c, &
                         cm_to_c, cm_from_c, &
                         hard_stat_to_c, hard_stat_from_c, &
                         grain_to_c, grain_from_c
    end interface
contains

    !> Convert a grain to its C-compatible counterpart.

    !> The caller takes ownership of the hardening state, see [[constitutive_model:HardeningState_C]].
    !> The phase pointer must be resolved at material level
    subroutine grain_to_c(gr_c, gr)
        type(Grain_C), intent(out):: gr_c
        type(Grain), intent(in):: gr

        gr_c%phase = C_NULL_PTR !Resolved at material level.
        gr_c%orientation = real(gr%orientation,C_DOUBLE)
        gr_c%stress_increment = real(gr%stress_increment, C_DOUBLE)
        gr_c%hardening_state = gr%hardening_state
    end subroutine

    !> Restore a grain from its C-compatible counterpart.
    !>
    !> The phase is resolved at material level.
    !> state in `gr_c` is freed, see [[constitutive_model:hard_stat_from_c]].
    !> `gr_c` is assumed to be well-formed: it is not validated here.
    subroutine grain_from_c(gr, gr_c)
        type(Grain), intent(inout):: gr
        type(Grain_C), intent(in):: gr_c

        gr%orientation = real(gr_c%orientation, DP)
        gr%stress_increment = real(gr_c%stress_increment, DP)
        gr%hardening_state = gr%phase%model%make_hardening_state()
        gr%hardening_state = gr_c%hardening_state
    end subroutine

    !> Convert C-compatible phase data to phase data.
    !>
    !> All allocated buffers are freed.
    !> `phase_data_c` is assumed to be well-formed: it is not validated here.
    subroutine phase_data_from_c(phase_data, phase_data_c)
        type(PhaseData), intent(out):: phase_data
        type(PhaseData_C), intent(in):: phase_data_c

        real(C_DOUBLE), dimension(:,:), pointer:: orientations

        phase_data%model = phase_data_c%model
        phase_data%deformation_mechanism = int(phase_data_c%deformation_mechanism)

        call c_f_pointer(phase_data_c%orientations, orientations, [3, int(phase_data_c%n_orientations)])
        phase_data%orientations = real(orientations, DP)
        call free(phase_data_c%orientations)
    end subroutine

    !> Convert a constitutive model to its C-compatible counterpart. The caller takes ownership of all pointers, see
    !> [[ConstitutiveModel_C]].
    !>
    !> Not `pure`: the coefficients and the serialized state are malloc'ed.
    subroutine cm_to_c(cm_c, cm)
        type(ConstitutiveModel_C), intent(out):: cm_c
        class(ConstitutiveModel), intent(in):: cm

        integer:: n_systems
        real(C_DOUBLE), dimension(:,:), pointer:: ptr

        cm_c%hardening_model_state = ModelData(cm%id, cm%serialize())
        n_systems = size(cm%taylor_coeffs,2)
        cm_c%n_systems = int(n_systems, C_INT)
        cm_c%taylor_coeffs = malloc(int(5 * n_systems, C_SIZE_T) * c_sizeof(0._C_DOUBLE))
        call c_f_pointer(cm_c%taylor_coeffs, ptr, [5,n_systems])
        ptr = real(cm%taylor_coeffs, C_DOUBLE)
        cm_c%spin_coeffs = malloc(int(3 * n_systems, C_SIZE_T) * c_sizeof(0._C_DOUBLE))
        call c_f_pointer(cm_c%spin_coeffs, ptr, [3,n_systems])
        ptr = real(cm%spin_coeffs, C_DOUBLE)
        cm_c%basis = int(cm%basis,C_INT)
    end subroutine

    !> Restore a constitutive model from its C-compatible counterpart.
    !>
    !> All allocated buffers on the C side are freed and the pointers are left dangling.
    !> `cm_c` is assumed to be well-formed: it is not validated here.
    subroutine cm_from_c(cm, cm_c)
        class(ConstitutiveModel), intent(inout):: cm
        type(ConstitutiveModel_C), intent(in):: cm_c

        real(C_DOUBLE), dimension(:,:), pointer:: ptr
        type(ModelData):: model_data

        call c_f_pointer(cm_c%taylor_coeffs, ptr, [5, int(cm_c%n_systems)])
        cm%taylor_coeffs = real(ptr, DP)
        call free(cm_c%taylor_coeffs)
        call c_f_pointer(cm_c%spin_coeffs, ptr, [3, int(cm_c%n_systems)])
        cm%spin_coeffs = real(ptr, DP)
        call free(cm_c%spin_coeffs)
        cm%basis = int(cm_c%basis)

        model_data = cm_c%hardening_model_state
        call cm%deserialize(model_data%parameters)
    end subroutine

    !> Convert a hardening state to its C-compatible counterpart.
    !>
    !> The caller takes ownership of all pointers.
    subroutine hard_stat_to_c(hard_stat_c, hard_stat)
        type(HardeningState_C), intent(out):: hard_stat_c
        class(HardeningState), intent(in):: hard_stat

        integer:: i, n_systems
        real(C_DOUBLE), dimension(:,:), pointer:: crss
        type(Parameter), dimension(:), allocatable:: params
        type(Parameter_C):: mold
        type(Parameter_C), dimension(:), pointer:: params_c

        n_systems = size(hard_stat%crss, 2)
        hard_stat_c%n_systems = int(n_systems, C_INT)
        hard_stat_c%crss = malloc(int(2 * n_systems, C_SIZE_T) * c_sizeof(0._C_DOUBLE))
        call c_f_pointer(hard_stat_c%crss, crss, [2, n_systems])
        crss = real(hard_stat%crss, C_DOUBLE)

        params = hard_stat%serialize()
        hard_stat_c%n_params = int(size(params), C_INT)
        hard_stat_c%params = malloc(int(size(params), C_SIZE_T) * c_sizeof(mold))
        call c_f_pointer(hard_stat_c%params, params_c, [size(params)])
        do i = 1, size(params)
            params_c(i) = params(i)
        end do
    end subroutine

    !> Restore a hardening state from its C-compatible counterpart.
    !>
    !> All allocated buffers on the C side are freed and the pointers are left dangling.
    !> `hard_stat_c` is assumed to be well-formed: it is not validated here.
    subroutine hard_stat_from_c(hard_stat, hard_stat_c)
        class(HardeningState), intent(inout):: hard_stat
        type(HardeningState_C), intent(in):: hard_stat_c

        integer:: i
        real(C_DOUBLE), dimension(:,:), pointer:: crss
        type(Parameter), dimension(:), allocatable:: params
        type(Parameter_C), dimension(:), pointer:: params_c

        call c_f_pointer(hard_stat_c%crss, crss, [2, int(hard_stat_c%n_systems)])
        hard_stat%crss = real(crss, DP)
        call free(hard_stat_c%crss)

        allocate(params(hard_stat_c%n_params))
        call c_f_pointer(hard_stat_c%params, params_c, [int(hard_stat_c%n_params)])
        do i = 1, size(params)
            params(i) = params_c(i)
        end do
        call free(hard_stat_c%params)
        call hard_stat%deserialize(params)
    end subroutine
end module
