module dsh_edge
    use utils
    use parameters
    use dsh
    use slip_systems
    use grain_module

    implicit none
    private

    !Implementation of the 'Dislocation Substructural Hardening (DSH)' model considering only edge dislocations.
    !This is the original implementation of the DSH hardening model family as formulated by Bart Peeters in his PhD thesis.
    type, extends(HardeningModelDSH):: HardeningModelDSHEdge
    contains
        procedure:: init => dsh_edge_init
    end type

    public:: HardeningModelDSHEdge

contains

    function dsh_edge_init(this, miller_indices, params) result(initial_state)
        class(HardeningModelDSHEdge), intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        type(Parameter), dimension(:), target, intent(in):: params
        class(HardeningState), allocatable:: initial_state

        initial_state = this%init_common(miller_indices, params, transpose(matmul(CBBNORMAL, normalize(BCC24(:,2, :)))))
    end function
end module
