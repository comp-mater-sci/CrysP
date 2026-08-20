module dsh_edge
    use base_defs
    use math_utils
    use dsh
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
        procedure, nopass:: get_name => dsh_edge_get_name
        procedure, nopass:: get_description => dsh_edge_get_description
        procedure:: init => dsh_edge_init
    end type

    public:: ConstitutiveModelDSHEdge

contains

    pure function dsh_edge_get_name() result(name)
        character(:), allocatable:: name

        name = "Dislocation Substructural Hardening (edge variant)"
    end function

    pure function dsh_edge_get_description() result(description)
        character(:), allocatable:: description

        description = "Physics-based hardening model mapping the movement of (clusters of) dislocations. " // &
                       "First formulated and documented in Bart Peeters's PhD thesis: " // &
                       "'Multiscale modelling of the induced plastic anisotropy in IF steel during sheet forming'. " // &
                       "This variant of the model considers only edge dislocations."
    end function

    function dsh_edge_init(this, miller_indices, params) result(initial_state)
        class(ConstitutiveModelDSHEdge), intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        type(Parameter), dimension(:), intent(in):: params
        class(GrainState), allocatable:: initial_state

        initial_state = this%init_common(miller_indices, params, transpose(matmul(CBBNORMAL, normalize(BCC24(:,2, :)))))
    end function
end module
