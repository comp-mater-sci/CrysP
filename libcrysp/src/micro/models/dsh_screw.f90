module dsh_screw
    use base_defs
    use math_utils
    use dsh
    use slip_systems
    use constitutive_model
    use crysp_serialization

    implicit none

    private
    public:: ConstitutiveModelDSHScrew

    !Dislocation substructural hardening (DSH) model assuming all slip is carried by screw dislocations.
    type, extends(ConstitutiveModelDSH):: ConstitutiveModelDSHScrew
    contains
        procedure, nopass:: get_name => dsh_screw_get_name
        procedure, nopass:: get_description => dsh_screw_get_description
        procedure:: init => dsh_screw_init
    end type

contains
    pure function dsh_screw_get_name() result(name)
        character(:), allocatable:: name

        name = "Dislocation Substructural Hardening (screw variant)"
    end function

    pure function dsh_screw_get_description() result(description)
        character(:), allocatable:: description

        description = "Physics-based hardening model mapping the movement of (clusters of) dislocations. " // &

                       "First formulated and documented in Bart Peeters's PhD thesis: " // &
                       "'Multiscale modelling of the induced plastic anisotropy in IF steel during sheet forming'. " // &
                       "This variant of the model considers only screw dislocations."
    end function

    function dsh_screw_init(this, miller_indices, params) result(initial_state)
        class(ConstitutiveModelDSHScrew), intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        type(Parameter), dimension(:), intent(in):: params
        class(GrainState), allocatable:: initial_state

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
