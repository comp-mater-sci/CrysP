module crysp_meso_c
    use iso_c_binding
    use crysp_cluster
    use crysp_micro_c
    use crysp_utils_c

    implicit none

    public

    type, bind(C):: Cluster_C
        integer(C_INT):: n_grains
        type(C_PTR):: grains      !! n_grains * type(Grain_C)
        real(C_DOUBLE):: weight
        integer(C_INT):: n_state
        type(C_PTR):: state       !! n_state * type(Parameter_C)
    end type

    interface assignment(=)
        module procedure cluster_to_c, cluster_from_c
    end interface

contains

    !> Convert a cluster to its C-compatible counterpart.
    !>
    !> The caller takes ownership of all pointers. The phase pointer of each grain is left null and must be resolved at material
    !> level, see [[grain_to_c]].
    subroutine cluster_to_c(clstr_c, clstr)
        type(Cluster_C), intent(out):: clstr_c
        class(Cluster), intent(in):: clstr

        integer:: i, &
                  n_grains, &
                  n_state
        type(Grain_C):: mold_grain
        type(Parameter_C):: mold_param
        type(Grain_C), dimension(:), pointer:: grains_ptr
        type(Parameter), dimension(:), allocatable:: state
        type(Parameter_C), dimension(:), pointer:: state_ptr

        n_grains = size(clstr%grains)
        clstr_c%n_grains = int(n_grains, C_INT)
        clstr_c%weight = real(clstr%weight, C_DOUBLE)

        clstr_c%grains = malloc(int(n_grains, C_SIZE_T) * c_sizeof(mold_grain))
        call c_f_pointer(clstr_c%grains, grains_ptr, [n_grains])
        do i=1, n_grains
            grains_ptr(i) = clstr%grains(i)
        end do

        state = clstr%serialize()
        n_state = size(state)
        clstr_c%n_state = int(n_state, C_INT)
        clstr_c%state = malloc(int(n_state, C_SIZE_T) * c_sizeof(mold_param))
        call c_f_pointer(clstr_c%state, state_ptr, [n_state])
        do i=1,n_state
            state_ptr(i) = state(i)
        end do
    end subroutine

    !> Restore a cluster from its C-compatible counterpart.
    !>
    !> phases are resolved at material level, see [[grain_from_c]].
    !> All allocated buffers on the C side are freed and the pointers are left dangling.
    !> `clstr_c` is assumed to be well-formed: it is not validated here.
    subroutine cluster_from_c(clstr, clstr_c)
        class(Cluster), intent(inout):: clstr
        type(Cluster_C), intent(in):: clstr_c

        integer:: i
        type(Grain_C), dimension(:), pointer:: grains_ptr
        type(Parameter), dimension(:), allocatable:: state
        type(Parameter_C), dimension(:), pointer:: state_ptr

        clstr%weight = real(clstr_c%weight, DP)

        call c_f_pointer(clstr_c%grains, grains_ptr, [int(clstr_c%n_grains)])
        do i = 1, size(grains_ptr)
            clstr%grains(i) = grains_ptr(i)
        end do
        call free(clstr_c%grains)

        allocate(state(clstr_c%n_state))
        call c_f_pointer(clstr_c%state, state_ptr, [int(clstr_c%n_state)])
        do i = 1, size(state)
            state(i) = state_ptr(i)
        end do
        call free(clstr_c%state)
        call clstr%deserialize(state)
    end subroutine
end module
