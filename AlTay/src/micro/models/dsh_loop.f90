module dsh_loop
    use utils
    use parameters
    use dsh
    use slip_systems

    implicit none

    private
    public:: HardeningModelDSHLoop

    !Dislocation substructural hardening (DSH) model assuming slip is carried by dislocation loops with equal slip realized by edge and screw segments.
    type, extends(HardeningModelDSH):: HardeningModelDSHLoop
    contains
        procedure:: init => dsh_loop_init
    end type

contains

    subroutine dsh_loop_init(this, params)
        class(HardeningModelDSHLoop), intent(inout):: this
        type(Parameter), allocatable, intent(in):: params(:)
        integer:: s, i
        real(DP):: normdir(24, 3), &
                   eff(24, 6), &
                   coeff

        normdir = transpose(normalize(BCC24(:,1, :)))

        !Calculate "Wall-effectivity"-matrices
        do s = 1, 24
            do i = 1, 6
                coeff = NormDir(s, :) .dot. CBBnormal(i, :)

                !If coeff is almost +/-1, treat it as 1.
                eff(s, i) = merge(sqrt(1._DP-coeff**2), &
                                  0._DP, &
                                  abs(coeff) < 1-TOLERANCE)
            end do
        end do

        call dsh_init(this, params, eff)
    end subroutine
end module
