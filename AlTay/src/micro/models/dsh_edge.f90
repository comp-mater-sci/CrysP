module dsh_edge
    use utils
    use parameters
    use dsh
    use slip_systems

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

    subroutine dsh_edge_init(this, params)
        class(HardeningModelDSHEdge), intent(inout):: this
        type(Parameter), allocatable, intent(in):: params(:)

        call dsh_init(this, &
                      params, &
                      transpose(matmul(CBBNORMAL, normalize(BCC24(:,2, :)))))  ! 'Wall-effectivity' matrix == cosines of the angle between
                                                                              !dislocation movement vectors and the cell block boundary normals.

    end subroutine
end module
