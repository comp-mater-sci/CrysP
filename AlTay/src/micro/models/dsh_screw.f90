module dsh_screw
    use utils
    use parameters
    use dsh
    use slip_systems

    implicit none

    private
    public:: HardeningModelDSHScrew

    !Dislocation substructural hardening (DSH) model assuming all slip is carried by screw dislocations.
    type, extends(HardeningModelDSH):: HardeningModelDSHScrew
    contains
        procedure:: init => dsh_screw_init
    end type

contains

    subroutine dsh_screw_init(this, params)
        class(HardeningModelDSHScrew), intent(inout):: this
        type(Parameter), allocatable, intent(in):: params(:)
        real(DP):: screwdir(24, 3), &
                   prod(3)
        integer:: i

        !The screw direction is the cross product of the edge direction and the slip plane normal.
        do i = 1, 24
            screwdir(i, :) = normalize(BCC24(:,2, i) .cross. BCC24(:,1, i))
        end do

        call dsh_init(this, params)
        this%eff = matmul(screwdir, transpose(cbbnormal))
        call this%initstate()
    end subroutine
end module
