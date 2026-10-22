module crysp_macro_c
    use iso_c_binding
    use base_defs
    use crysp_status
    use crysp_constitutive_model
    use crysp_micro
    use meso
    use crysp_material
    use incrementation
    use crysp_utils_c
    use crysp_micro_c
    use crysp_meso_c

    implicit none

    public


    !> C-compatible counterpart of [[Material]].
    !>
    !> The `phase` pointer of every grain in `clusters` points to an element of `phases`. A `Material_C` produced by libCrysP owns
    !> all of its pointers, malloc'ed so that C can release them with `free()`.
    type, bind(C):: Material_C
        type(ModelData_C):: meso_model
        integer(C_INT):: n_phases
        type(C_PTR):: phases  !! n_phases * type(ConstitutiveModel_C)
        integer(C_INT):: n_clusters
        type(C_PTR):: clusters  !! n_clusters * type(Cluster_C)
    end type

    !> C-compatible counterpart of [[StrainIncrement]].
    type, bind(C):: StrainIncrement_C
        real(C_DOUBLE):: duration
        real(C_DOUBLE), dimension(3,3):: deformation_gradient
        real(C_DOUBLE):: vm_strain
        real(C_DOUBLE), dimension(3,3):: stress
        real(C_DOUBLE):: taylor_factor
        integer(C_INT):: n_grains
        type(C_PTR):: texture                   !! `3*n_grains` ints: the Euler angles of each grain in degrees.
    end type

    !> C-compatible counterpart of [[StressIncrement]].
    type, bind(C):: StressIncrement_C
        real(C_DOUBLE), dimension(5):: strain_rate
        real(C_DOUBLE), dimension(5):: residual
        integer(C_INT):: n_strain_increments
        type(C_PTR):: strain_increments !! n_strain_increments * type(StrainIncrement_C)
    end type

    interface assignment(=)
        module procedure material_to_c, material_from_c, &
                         strain_increment_to_c, &
                         stress_increment_to_c
    end interface

contains

    !> Convert a material to its C-compatible counterpart.
    !>
    !> The caller takes ownership of all pointers. The phase pointer of each grain is resolved to the element of `mat_c%phases`
    !> that backs it.
    subroutine material_to_c(mat_c, mat)
        type(Material_C), intent(out):: mat_c
        type(Material), target, intent(in):: mat

        integer:: i, j, k, &
                  n_phases, &
                  n_clusters
        type(ConstitutiveModel_C):: mold_phase
        type(Cluster_C):: mold_cluster
        type(ConstitutiveModel_C), dimension(:), pointer:: phases_ptr
        type(Cluster_C), dimension(:), pointer:: clusters_ptr
        type(Grain_C), dimension(:), pointer:: grains_ptr

        mat_c%meso_model = ModelData(mat%meso_model%id, mat%meso_model%serialize())

        n_phases = size(mat%phases)
        mat_c%n_phases = int(n_phases, C_INT)
        mat_c%phases = malloc(int(n_phases, C_SIZE_T) * c_sizeof(mold_phase))
        call c_f_pointer(mat_c%phases, phases_ptr, [n_phases])
        do i = 1, n_phases
            phases_ptr(i) = mat%phases(i)%model
        end do

        n_clusters = size(mat%clusters)
        mat_c%n_clusters = int(n_clusters, C_INT)
        mat_c%clusters = malloc(int(n_clusters, C_SIZE_T) * c_sizeof(mold_cluster))
        call c_f_pointer(mat_c%clusters, clusters_ptr, [n_clusters])
        do i = 1, n_clusters
            clusters_ptr(i) = mat%clusters(i)

            call c_f_pointer(clusters_ptr(i)%grains, grains_ptr, [int(clusters_ptr(i)%n_grains)])
            do j = 1, size(grains_ptr)
                do k = 1, n_phases
                    if (associated(mat%clusters(i)%grains(j)%phase, mat%phases(k))) then
                        grains_ptr(j)%phase = c_loc(phases_ptr(k))
                        exit
                    end if
                end do
            end do
        end do
    end subroutine

    !> Restore a material from its C-compatible counterpart.
    !>
    !> All allocated buffers on the C side are freed and the pointers are left dangling.
    !> `mat_c` is assumed to be well-formed: it is not validated here.
    subroutine material_from_c(mat, mat_c)
        type(Material), target, intent(out):: mat
        type(Material_C), intent(in):: mat_c

        integer:: i, j, k
        type(Status):: stat
        type(ModelData):: meso_model
        type(ConstitutiveModel_C), dimension(:), pointer:: phases_ptr
        type(Cluster_C), dimension(:), pointer:: clusters_ptr
        type(Grain_C), dimension(:), pointer:: grains_ptr

        call c_f_pointer(mat_c%phases, phases_ptr, [int(mat_c%n_phases)])
        call c_f_pointer(mat_c%clusters, clusters_ptr, [int(mat_c%n_clusters)])

        !`mat_c` is assumed to be valid, so the model lookups cannot fail and their status is ignored.
        stat = meso_get_model(int(mat_c%meso_model%id), mat%meso_model)
        meso_model = mat_c%meso_model
        call mat%meso_model%deserialize(meso_model%parameters)

        allocate(mat%phases(size(phases_ptr)))
        do i = 1, size(phases_ptr)
            stat = micro_get_model(int(phases_ptr(i)%hardening_model_state%id), mat%phases(i)%model)
            mat%phases(i)%model = phases_ptr(i)
        end do

        !The grains' phases point into `phases_ptr`, so the clusters are converted before it is freed.
        allocate(mat%clusters(size(clusters_ptr)), mold=mat%meso_model%make_cluster())
        do i = 1, size(clusters_ptr)
            call c_f_pointer(clusters_ptr(i)%grains, grains_ptr, [int(clusters_ptr(i)%n_grains)])
            allocate(mat%clusters(i)%grains(size(grains_ptr)))
            do j = 1, size(grains_ptr)
                do k = 1, size(phases_ptr)
                    if (c_associated(grains_ptr(j)%phase, c_loc(phases_ptr(k)))) then
                        mat%clusters(i)%grains(j)%phase => mat%phases(k)
                        exit
                    end if
                end do
            end do

            mat%clusters(i) = clusters_ptr(i)
        end do

        call free(mat_c%clusters)
        call free(mat_c%phases)
    end subroutine

    !> Convert a strain increment to its C-compatible counterpart. The caller takes ownership of `inc_c%texture`.
    !>
    !> The caller takes ownership of `inc_c%strain_increments` of allocated buffers.
    subroutine strain_increment_to_c(inc_c, inc)
        type(StrainIncrement_C), intent(out):: inc_c
        type(StrainIncrement), intent(in):: inc

        integer:: n_grains
        integer(C_INT), dimension(:,:), pointer:: texture

        inc_c%duration = real(inc%duration, C_DOUBLE)
        inc_c%deformation_gradient = real(inc%deformation_gradient, C_DOUBLE)
        inc_c%vm_strain = real(inc%vm_strain, C_DOUBLE)
        inc_c%stress = real(inc%stress, C_DOUBLE)
        inc_c%taylor_factor = real(inc%taylor_factor, C_DOUBLE)

        n_grains = size(inc%texture, 2)
        inc_c%n_grains = int(n_grains, C_INT)

        inc_c%texture = malloc(int(3*n_grains, C_SIZE_T) * c_sizeof(1_C_INT))
        call c_f_pointer(inc_c%texture, texture, [3, n_grains])
        texture = int(inc%texture, C_INT)
    end subroutine

    !> Convert a stress increment to its C-compatible counterpart.
    !>
    !> The caller takes ownership of `inc_c%strain_increments` of allocated buffers.
    subroutine stress_increment_to_c(inc_c, inc)
        type(StressIncrement_C), intent(out):: inc_c
        type(StressIncrement), intent(in):: inc

        integer:: i, n
        type(StrainIncrement_C):: mold
        type(StrainIncrement_C), dimension(:), pointer:: strain_increments

        inc_c%strain_rate = real(inc%strain_rate, C_DOUBLE)
        inc_c%residual = real(inc%residual, C_DOUBLE)

        n = size(inc%strain_increments)
        inc_c%n_strain_increments = int(n, C_INT)

        inc_c%strain_increments = malloc(int(n, C_SIZE_T) * c_sizeof(mold))
        call c_f_pointer(inc_c%strain_increments, strain_increments, [n])
        do i = 1, n
            strain_increments(i) = inc%strain_increments(i)
        end do
    end subroutine
end module
