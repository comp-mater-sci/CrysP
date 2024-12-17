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
                   eff(24, 6)

        normdir = transpose(normalize(BCC24(:,1, :)))


        !Calculate "Wall-effectivity"-matrices
        do s = 1, 24
            do i = 1, 6
                eff(s, i) = NormDir(s, :) .dot. CBBnormal(i, :)
                !treat as "1" or "-1"
                if (abs(eff(s, i)) >= 0.99999_DP) then
                    eff(s, i)=0._DP
                else
                    eff(s, i)=sqrt(1._DP-(eff(s, i))**2)
                endif
            end do
        end do

        call dsh_init(this, params, eff)
    end subroutine
end module
