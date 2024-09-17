module slip_systems
    use utils
    use logging

    implicit none
    public

    !Definition of slip system families in terms of miller indices
    !Gfortran does not accept the clean array syntax
    integer, parameter:: SLIP_SYSTEMS_FCC_111(3, 2, 12) = reshape([1, 1, 1,    1, -1, 0,     &
                                                              1, 1, 1,    0, 1, -1,     &
                                                              1, 1, 1,    1, 0, -1,     &
                                                              -1, 1, 1,   1, 1, 0,     &
                                                              -1, 1, 1,   0, 1, -1,    &
                                                              -1, 1, 1,   1, 0, 1,     &
                                                              1, 1, -1,   1, -1, 0,    &
                                                              1, 1, -1,   0, 1, 1,     &
                                                              1, 1, -1,   1, 0, 1,     &
                                                              1, -1, 1,   1, 1, 0,     &
                                                              1, -1, 1,   0, 1, 1,     &
                                                              1, -1, 1,   1, 0, -1], shape(SLIP_SYSTEMS_FCC_111)),   &
                          SLIP_SYSTEMS_BCC_110(3, 2, 12) = reshape([0, 1, -1,   1, 1, 1,   &
                                                              0, 1, -1,   -1, 1, 1, &
                                                              -1, 0, 1,   1, 1, 1,    &
                                                              -1, 0, 1,   1, -1, 1,    &
                                                              1, -1, 0,   1, 1, 1,     &
                                                              -1, 1, 0,   -1, -1, 1,    &
                                                              0, -1, -1,  -1, -1, 1,     &
                                                              0, -1, -1,  1, -1, 1,     &
                                                              1, 0, 1,    -1, -1, 1,     &
                                                              1, 0, 1,    -1, 1, 1,    &
                                                              -1, -1, 0,  -1, 1, 1,    &
                                                              1, 1, 0,    1, -1, 1], shape(SLIP_SYSTEMS_BCC_110)),    &
                          SLIP_SYSTEMS_BCC_112(3, 2, 12) = reshape([2, -1, -1,  1, 1, 1,     &
                                                              -1, 2, -1,  1, 1, 1,     &
                                                              -1, -1, 2,  1, 1, 1,    &
                                                              -2, 1, -1,  -1, -1, 1,    &
                                                              1, -2, -1,  -1, -1, 1,    &
                                                              1, 1, 2,    -1, -1, 1,     &
                                                              -2, -1, -1, -1, 1, 1,    &
                                                              1, 2, -1,   -1, 1, 1,   &
                                                              1, -1, 2,   -1, 1, 1,   &
                                                              2, 1, -1,   1, -1, 1,    &
                                                              -1, -2, -1, 1, -1, 1,     &
                                                              -1, 1, 2,   1, -1, 1], shape(SLIP_SYSTEMS_BCC_112)),    &
                          SLIP_SYSTEMS_BCC_123(3, 2, 24) = reshape([-3, 1, 2,   1, 1, 1,     &
                                                              2, -3, 1,   1, 1, 1,     &
                                                              1, 2, -3,   1, 1, 1,     &
                                                              -3, 2, 1,   1, 1, 1,     &
                                                              1, -3, 2,   1, 1, 1,     &
                                                              2, 1, -3,   1, 1, 1,     &
                                                              3, -1, 2,   -1, -1, 1,   &
                                                              -2, 3, 1,   -1, -1, 1,   &
                                                              -1, -2, -3, -1, -1, 1, &
                                                              3, -2, 1,   -1, -1, 1,   &
                                                              -1, 3, 2,   -1, -1, 1,   &
                                                              -2, -1, -3, -1, -1, 1, &
                                                              3, 1, 2,    -1, 1, 1,     &
                                                              -2, -3, 1,  -1, 1, 1,   &
                                                              -1, 2, -3,  -1, 1, 1,   &
                                                              3, 2, 1,    -1, 1, 1,     &
                                                              -1, -3, 2,  -1, 1, 1,   &
                                                              -2, 1, -3,  -1, 1, 1,   &
                                                              -3, -1, 2,  1, -1, 1,   &
                                                              2, 3, 1,    1, -1, 1,     &
                                                              1, -2, -3,  1, -1, 1,   &
                                                              -3, -2, 1,  1, -1, 1,   &
                                                              1, 3, 2,    1, -1, 1,     &
                                                              2, -1, -3,  1, -1, 1], shape(SLIP_SYSTEMS_BCC_123)), &
                          FCC12(3, 2, 12) = SLIP_SYSTEMS_FCC_111, &
                          BCC24(3, 2, 24) = reshape([SLIP_SYSTEMS_BCC_110, SLIP_SYSTEMS_BCC_112], shape(BCC24)), &
                          BCC48(3, 2, 48) = reshape([BCC24, SLIP_SYSTEMS_BCC_123], shape(BCC48))


    !Type wrapping all state associated to an individual slip system. Even though it contains only pointers to variables allocated
    !elsewhere it is useful because it makes many high-level expressions more clear and concise and reduces chances for mistakes
    !when multiple variables associated to a single slip system must be updated.
    type SlipSystem
        real(DP), dimension(:), pointer, contiguous:: taylor_coeffs, &
                                                      spin_coeffs, &
                                                      crss
        real(DP), pointer:: overstress, &
                            slip_rate, &
                            resolved_shear_stress, &
                            rss
    contains
        procedure:: init => slip_system_init
        procedure:: get_crss => slip_system_get_crss
        procedure:: get_work_rate => slip_system_get_work_rate
    end type

contains

    !Initialize a slip system
    subroutine slip_system_init(this, miller_indices, taylor_coeffs, spin_coeffs, overstress, slip_rate, rss, crss)
        class(SlipSystem), intent(inout):: this
        integer, dimension(3, 2), intent(in):: miller_indices
        real(DP), dimension(5), target, intent(inout):: taylor_coeffs
        real(DP), dimension(3), target, intent(inout):: spin_coeffs
        real(DP), target, intent(in):: overstress
        real(DP), target, intent(in):: slip_rate
        real(DP), target, intent(in):: rss
        real(DP), dimension(2), target, intent(in):: crss

        real(DP):: normalized(3, 2), &
                   tensor(3, 3)

        !Assign pointers
        this%taylor_coeffs => taylor_coeffs
        this%overstress => overstress
        this%slip_rate => slip_rate
        this%rss => rss
        this%crss =>crss

        !Initialize taylor and spin coefficients of the slip system
        normalized = normalize(miller_indices)
        tensor = normalized(:,1) .tensor. normalized(:,2)
        taylor_coeffs = convert_stress_strain_space(tensor)
        spin_coeffs = convert_spin(tensor)
    end subroutine

    pure real(DP) function slip_system_get_crss(this) result(crss)
        class(SlipSystem), intent(in):: this

        crss = merge(this%crss(1), -this%crss(2), this%slip_rate > 0._DP)
    end function

    pure real(DP) function slip_system_get_work_rate(this) result(work_rate)
        class(SlipSystem), intent(in):: this

        work_rate = this%get_crss() * this%slip_rate
    end function
end module
