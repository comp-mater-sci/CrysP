module dsh_screw
    use base_defs
    use math_utils
    use dsh
    use crysp_model
    use slip_systems
    use crysp_constitutive_model
    use crysp_parameters

    implicit none

    private
    public:: ConstitutiveModelDSHScrew

    !Dislocation substructural hardening (DSH) model assuming all slip is carried by screw dislocations.
    type, extends(ConstitutiveModelDSH):: ConstitutiveModelDSHScrew
    contains
        procedure:: describe => dsh_screw_describe
        procedure:: init => dsh_screw_init
    end type

contains
    function dsh_screw_describe(this) result(desc)
        class(ConstitutiveModelDSHScrew), intent(in):: this
        type(ModelDescriptor):: desc

        desc = this%ConstitutiveModelDSH%describe()
        desc%name = "Dislocation Substructural Hardening (screw variant)"
        desc%description = "Physics-based hardening model mapping the movement of (clusters of) dislocations. " // &
                           "First formulated and documented in Bart Peeters's PhD thesis: " // &
                           "'Multiscale modelling of the induced plastic anisotropy in IF steel during sheet forming'. " // &
                           "This variant of the model considers only screw dislocations."
    end function

    subroutine dsh_screw_init(this, miller_indices, params)
        class(ConstitutiveModelDSHScrew), intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        type(Parameter), dimension(:), intent(in):: params

        integer:: i
        real(DP):: screwdir(24, 3)

        !The screw direction is the cross product of the edge direction and the slip plane normal.
        do i = 1, 24
            screwdir(i, :) = normalize(BCC24(:,2, i) .cross. BCC24(:,1, i))
        end do

        call this%init_common(miller_indices, params, matmul(screwdir, transpose(CBBNORMAL)))  ! 'Wall-effectivity' matrix == cosines of the angle between
                                                              !the dislocation movement vectors and the cell block boundary normals.
    end subroutine
end module
