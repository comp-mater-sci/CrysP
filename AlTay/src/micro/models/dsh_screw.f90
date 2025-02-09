module dsh_screw
    use utils
    use parameters
    use dsh
    use slip_systems
    use grain_module

    implicit none

    private
    public:: HardeningModelDSHScrew

    !Dislocation substructural hardening (DSH) model assuming all slip is carried by screw dislocations.
    type, extends(HardeningModelDSH):: HardeningModelDSHScrew
    contains
        procedure:: init => dsh_screw_init
    end type

contains

    subroutine dsh_screw_init(this, grains, params)
        class(HardeningModelDSHScrew), intent(inout):: this
        type(Grain), dimension(:), intent(inout):: grains
        type(Parameter), dimension(:), target, intent(in):: params

        integer:: i
        real(DP):: screwdir(24, 3)

        !The screw direction is the cross product of the edge direction and the slip plane normal.
        do i = 1, 24
            screwdir(i, :) = normalize(BCC24(:,2, i) .cross. BCC24(:,1, i))
        end do

        call this%init_common(grains, params, matmul(screwdir, transpose(CBBNORMAL)))  ! 'Wall-effectivity' matrix == cosines of the angle between
                                                              !the dislocation movement vectors and the cell block boundary normals.
    end subroutine
end module
