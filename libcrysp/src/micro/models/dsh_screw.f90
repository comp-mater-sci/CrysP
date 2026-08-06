module dsh_screw
    use base_defs
    use math_utils
    use parameters
    use dsh
    use slip_systems
    use constitutive_model

    implicit none

    private
    public:: ConstitutiveModelDSHScrew

    !Dislocation substructural hardening (DSH) model assuming all slip is carried by screw dislocations.
    type, extends(ConstitutiveModelDSH):: ConstitutiveModelDSHScrew
    contains
        procedure:: init => dsh_screw_init
    end type

contains

    function dsh_screw_init(this, miller_indices, params) result(initial_state)
        class(ConstitutiveModelDSHScrew), intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        character(*), target, intent(in):: params
        class(HardeningState), allocatable:: initial_state

        integer:: i
        real(DP):: screwdir(24, 3)

        !The screw direction is the cross product of the edge direction and the slip plane normal.
        do i = 1, 24
            screwdir(i, :) = normalize(BCC24(:,2, i) .cross. BCC24(:,1, i))
        end do

        initial_state = this%init_common(miller_indices, params, matmul(screwdir, transpose(CBBNORMAL)))  ! 'Wall-effectivity' matrix == cosines of the angle between
                                                              !the dislocation movement vectors and the cell block boundary normals.
    end function
end module
