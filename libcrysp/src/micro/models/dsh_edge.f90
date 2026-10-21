module dsh_edge
    use base_defs
    use math_utils
    use dsh
    use crysp_model
    use slip_systems
    use crysp_grain
    use constitutive_model
    use crysp_serialization

    implicit none
    private

    !Implementation of the 'Dislocation Substructural Hardening (DSH)' model considering only edge dislocations.
    !This is the original implementation of the DSH hardening model family as formulated by Bart Peeters in his PhD thesis.
    type, extends(ConstitutiveModelDSH):: ConstitutiveModelDSHEdge
    contains
        procedure:: describe => dsh_edge_describe
        procedure:: init => dsh_edge_init
    end type

    public:: ConstitutiveModelDSHEdge

contains

    function dsh_edge_describe(this) result(desc)
        class(ConstitutiveModelDSHEdge), intent(in):: this
        type(ModelDescriptor):: desc

        desc = this%ConstitutiveModelDSH%describe()
        desc%name = "Dislocation Substructural Hardening (edge variant)"
        desc%description = "Physics-based hardening model mapping the movement of (clusters of) dislocations. " // &
                           "First formulated and documented in Bart Peeters's PhD thesis: " // &
                           "'Multiscale modelling of the induced plastic anisotropy in IF steel during sheet forming'. " // &
                           "This variant of the model considers only edge dislocations."
    end function

    subroutine dsh_edge_init(this, miller_indices, params)
        class(ConstitutiveModelDSHEdge), intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        type(Parameter), dimension(:), intent(in):: params

        call this%init_common(miller_indices, params, transpose(matmul(CBBNORMAL, normalize(BCC24(:,2, :)))))
    end subroutine
end module
