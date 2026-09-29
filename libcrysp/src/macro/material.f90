module crysp_material
    use base_defs
    use conversions, only: rad_to_deg, rotation_matrix_to_euler_angles
    use crysp_constitutive_model
    use meso

    implicit none

    private
    public:: Material

    type:: Material
        class(MesoModel), allocatable:: meso_model   !! Global state of the meso model
        type(Phase), dimension(:), allocatable:: phases           !! State associated to all grains of a particular phase
        class(Cluster), dimension(:), allocatable:: clusters      !! State associated to individual clusters. Contains state of each grain.
    contains
        procedure:: get_texture => material_get_texture
    end type

contains

    !> Get the current texture of a material.
    !>
    !> One column per grain, holding its Euler angles in Bunge convention, rounded to whole degrees. The grains are in the order in
    !> which the clusters hold them, which is the order in which they were supplied when the material was created, i.e. grouped by
    !> phase.
    function material_get_texture(this) result(texture)
        class(Material), intent(in):: this
        integer, dimension(:,:), allocatable:: texture

        integer:: i, j, n

        n = 0
        do i = 1, size(this%clusters)
            n = n + size(this%clusters(i)%grains)
        end do
        allocate(texture(3,n))

        n = 0
        do i = 1, size(this%clusters)
            associate (grains => this%clusters(i)%grains)
                do j = 1, size(grains)
                    !A grain orientation is a proper rotation by construction, so the cheap conversion applies: this is called
                    !once per grain per increment, where a polar decomposition would dominate the cost.
                    texture(:,n+j) = nint(rad_to_deg(rotation_matrix_to_euler_angles(grains(j)%orientation)))
                end do
                n = n + size(grains)
            end associate
        end do
    end function
end module
