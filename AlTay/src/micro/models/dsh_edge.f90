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

    subroutine dsh_edge_init(this, grains, params)
        class(HardeningModelDSHEdge), intent(inout):: this
        type(Grain), dimension(:), intent(inout):: grains
        type(Parameter), dimension(:), target, intent(in):: params

        call this%init_common(grains, params, transpose(matmul(CBBNORMAL, normalize(BCC24(:,2, :)))))
    end subroutine
end module
