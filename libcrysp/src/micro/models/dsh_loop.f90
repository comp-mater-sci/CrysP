module dsh_loop
    use base_defs
    use math_utils
    use parameters
    use dsh
    use slip_systems
    use constitutive_model

    implicit none

    private
    public:: ConstitutiveModelDSHLoop

    !Dislocation substructural hardening (DSH) model assuming slip is carried by dislocation loops with equal slip realized by edge and screw segments.
    type, extends(ConstitutiveModelDSH):: ConstitutiveModelDSHLoop
    contains
        procedure:: init => dsh_loop_init
    end type

contains

    function dsh_loop_init(this, miller_indices, params) result(initial_state)
        class(ConstitutiveModelDSHLoop), intent(inout):: this
        integer, dimension(:,:,:), intent(in):: miller_indices
        character(*), target, intent(in):: params
        class(HardeningState), allocatable:: initial_state

        integer:: s, i
        real(DP):: normdir(24, 3), &
                   eff(24, 6), &
                   coeff

        normdir = transpose(normalize(BCC24(:,1, :)))

        !Calculate "Wall-effectivity"-matrix
        do s = 1, 24
            do i = 1, 6
                coeff = NormDir(s, :) .dot. CBBnormal(i, :)

                !If coeff is almost +/-1, treat it as 1.
                eff(s, i) = merge(sqrt(1._DP-coeff**2), &
                                  0._DP, &
                                  abs(coeff) < 1-TOLERANCE)
            end do
        end do

        initial_state = this%init_common(miller_indices, params, eff)
    end function
end module
