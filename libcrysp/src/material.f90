module crysp_material
    use crysp_serialization
    use constitutive_model
    use crystal_plasticity_model
    use crysp_cluster
    use meso
    use micro
    use iso_c_binding

    implicit none

    private
    public:: Material

    type, extends(State):: Material
          class(CrystalPlasticityModel), allocatable:: cp_model   !! Global state of the meso model
          type(Phase), dimension(:), allocatable:: phases           !! State associated to all grains of a particular phase
          class(Cluster), dimension(:), allocatable:: clusters      !! State associated to individual clusters. Contains state of each grain.
    contains
        procedure:: size => material_size
        procedure:: serialize => material_serialize
        procedure:: deserialize => material_deserialize
    end type

contains

    pure function material_size(this) result(s)
        class(Material), intent(in):: this
        integer:: s

        integer:: i, &
                  n_phases, &
                  n_clusters

        n_phases = size(this%phases)
        n_clusters = size(this%clusters)

        s = 3 + this%cp_model%size()

        do i=1,n_phases
            s = s + 1 + this%phases(i)%model%size()
        end do

        do i=1,n_clusters
            s = s + this%clusters(i)%size()
        end do
    end function

    pure function material_serialize(this) result(params)
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
        params(1) = meso_get_model_id(this%cp_model)
        offset = 1
        sub = this%cp_model%serialize()
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

    subroutine material_deserialize(this, params)
        class(Material), target, intent(out):: this
        type(Parameter), dimension(:), intent(in):: params

        integer:: i, &
                  offset, &
                  n_phases, &
                  n_clusters, &
                  model_id

        !CP model
        model_id = params(1)
        this%cp_model = meso_get_model(model_id)
        offset = 1
        call this%cp_model%deserialize(params(offset+1:))
        offset = offset + this%cp_model%size()

        !Phases
        n_phases = params(offset+1)
        offset = offset + 1
        allocate(this%phases(n_phases))
        do i=1,n_phases
            model_id = params(offset+1)
            offset = offset + 1
            this%phases(i)%model = micro_get_model(model_id)
            call this%phases(i)%model%deserialize(params(offset+1:))
            offset = offset + this%phases(i)%model%size()
        end do

        !Clusters
        n_clusters = params(offset+1)
        offset = offset + 1
        allocate(this%clusters(n_clusters), mold=this%cp_model%make_cluster())
        do i=1,n_clusters
            call this%clusters(i)%deserialize(params(offset+1:), this%phases)
            offset = offset + this%clusters(i)%size()
        end do
    end subroutine
end module
