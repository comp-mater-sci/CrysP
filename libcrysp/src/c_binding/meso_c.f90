module crysp_meso_c
    use iso_c_binding
    use crysp_cluster
    use crysp_micro_c
    use crysp_utils_c

    implicit none

    public

    !> C-compatible counterpart of [[Cluster]]. `state` holds the state added by the concrete cluster, as produced by its
    !> `serialize`.
    !>
    !> A `Cluster_C` produced by libCrysP owns `grains`, `state` and everything they own, malloc'ed so that they are released along
    !> with the owning [[crysp_macro_c:Material_C]]. It does not own the phases its grains point to.
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

    !> See [[crysp_utils_c:destroy]].
    interface destroy
        module procedure cluster_c_destroy
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
    !> `clstr_c` is not modified: its buffers remain owned by the caller.
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

        allocate(state(clstr_c%n_state))
        call c_f_pointer(clstr_c%state, state_ptr, [int(clstr_c%n_state)])
        do i = 1, size(state)
            state(i) = state_ptr(i)
        end do
        call clstr%deserialize(state)
    end subroutine

    !> Release the grains and state of a C-compatible cluster, including everything each grain owns.
    subroutine cluster_c_destroy(clstr_c)
        type(Cluster_C), intent(in):: clstr_c

        integer:: i
        type(Grain_C), dimension(:), pointer:: grains_ptr
        type(Parameter_C), dimension(:), pointer:: state_ptr

        call c_f_pointer(clstr_c%grains, grains_ptr, [int(clstr_c%n_grains)])
        do i = 1, size(grains_ptr)
            call grain_c_destroy(grains_ptr(i))
        end do
        call free(clstr_c%grains)

        call c_f_pointer(clstr_c%state, state_ptr, [int(clstr_c%n_state)])
        do i = 1, size(state_ptr)
            call parameter_c_destroy(state_ptr(i))
        end do
        call free(clstr_c%state)
    end subroutine
end module
