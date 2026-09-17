module crysp_material
    use crysp_serialization
    use constitutive_model
    use crysp_meso_model
    use crysp_cluster
    use meso
    use micro
    use crysp_status
    use iso_c_binding

    implicit none

    private
    public:: Material, &
             TexturePoint

    type, extends(State):: Material
          class(MesoModel), allocatable:: meso_model   !! Global state of the meso model
          type(Phase), dimension(:), allocatable:: phases           !! State associated to all grains of a particular phase
          class(Cluster), dimension(:), allocatable:: clusters      !! State associated to individual clusters. Contains state of each grain.
    contains
        procedure:: size => material_size
        procedure:: serialize => material_serialize
        procedure:: deserialize => material_deserialize
        procedure:: get_texture => material_get_texture
    end type

    !> Orientation of a single grain.
    type, bind(C):: TexturePoint
        real(C_FLOAT), dimension(3):: euler_angles !! Euler angle triplet in Bunge convention, in radians.
        integer(C_INT):: phase_id                  !! Index of the grain's phase in the phase list of the material.
    end type

contains

    !> Get the current texture of a material.
    !>
    !> One texture point per grain, in the order in which the grains were supplied when the material was created, i.e. grouped by
    !> phase.
    function material_get_texture(this) result(texture)
        class(Material), target, intent(in):: this
        type(TexturePoint), dimension(:), allocatable:: texture

        integer:: i, j, k, n

        n = 0
        do i = 1, size(this%clusters)
            n = n + size(this%clusters(i)%grains)
        end do
        allocate(texture(n))

        !Clusters hold the grains in the order in which they were supplied.
        n = 0
        do i = 1, size(this%clusters)
            associate (grains => this%clusters(i)%grains)
                do j = 1, size(grains)
                    !deformation_gradient_to_euler inverts the transpose of euler_to_rotation_matrix, which built the orientation.
                    texture(n+j)%euler_angles = real(deformation_gradient_to_euler(transpose(grains(j)%orientation)), C_FLOAT)
                    do k=1,size(this%phases)
                        if (c_associated(c_loc(this%phases(k)),c_loc(grains(j)%phase))) then
                           texture(n+j)%phase_id = k
                           exit
                        end if
                    end do
                end do
                n = n + size(grains)
            end associate
        end do
    end function

    pure function material_size(this) result(s)
        class(Material), intent(in):: this
        integer:: s

        integer:: i, &
                  n_phases, &
                  n_clusters

        n_phases = size(this%phases)
        n_clusters = size(this%clusters)

        s = 3 + this%meso_model%size()

        do i=1,n_phases
            s = s + 1 + this%phases(i)%model%size()
        end do

        do i=1,n_clusters
            s = s + this%clusters(i)%size()
        end do
    end function

    function material_serialize(this) result(params)
        class(Material), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        integer:: i, &
                  offset, &
                  n_phases, &
                  n_clusters, &
                  s
        type(Parameter), dimension(:), allocatable:: sub

        allocate(params(this%size()))

        !CP model
        params(1) = meso_get_model_id(this%meso_model)
        offset = 1
        sub = this%meso_model%serialize()
        s = size(sub)
        params(offset+1:offset+s) = sub
        offset=offset+s

        !Phases
        n_phases = size(this%phases)
        params(offset+1) = n_phases
        offset = offset+1
        do i=1,n_phases
            associate (m => this%phases(i)%model)
                params(offset+1) = micro_get_model_id(m)
                offset = offset+1
                sub = m%serialize()
                s = size(sub)
                params(offset+1:offset+s) = sub
                offset = offset + s
            end associate
        end do

        !Clusters
        n_clusters = size(this%clusters)
        params(offset+1) = n_clusters
        offset = offset+1
        do i=1,n_clusters
            sub = this%clusters(i)%serialize(this%phases)
            s = size(sub)
            params(offset+1:offset+s) = sub
            offset = offset+s
        end do
    end function

    !> Rebuild a material from a serialized parameter list.
    !>
    !> The model IDs embedded in the list originate from whatever the caller handed back, so an unknown one is reported rather than
    !> fatal.
    subroutine material_deserialize(this, params)
        class(Material), target, intent(out):: this
        type(Parameter), dimension(:), intent(in):: params

        integer:: i, &
                  offset, &
                  n_phases, &
                  n_clusters, &
                  model_id
        type(Status):: stat

        !An unknown model ID is not reported yet, so the status of the model lookups below is ignored.

        !CP model
        model_id = params(1)
        stat = meso_get_model(model_id, this%meso_model)

        offset = 1
        call this%meso_model%deserialize(params(offset+1:))

        offset = offset + this%meso_model%size()

        !Phases
        n_phases = params(offset+1)
        offset = offset + 1
        allocate(this%phases(n_phases))
        do i=1,n_phases
            model_id = params(offset+1)
            offset = offset + 1
            stat = micro_get_model(model_id, this%phases(i)%model)

            call this%phases(i)%model%deserialize(params(offset+1:))

            offset = offset + this%phases(i)%model%size()
        end do

        !Clusters
        n_clusters = params(offset+1)
        offset = offset + 1
        allocate(this%clusters(n_clusters), mold=this%meso_model%make_cluster())
        do i=1,n_clusters
            call this%clusters(i)%deserialize(params(offset+1:), this%phases)

            offset = offset + this%clusters(i)%size()
        end do
    end subroutine
end module
