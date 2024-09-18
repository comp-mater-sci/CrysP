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
        real(DP):: slip_rate, &
                   overstress, &
                   rss
        real(DP), dimension(5):: taylor_coeffs
        real(DP), dimension(3):: spin_coeffs
        real(DP), dimension(2):: crss
    contains
        procedure:: init => slip_system_init
        procedure:: get_work_rate => slip_system_get_work_rate
    end type

contains

    !Initialize a slip system
    subroutine slip_system_init(this, miller_indices)
        class(SlipSystem), intent(inout):: this
        integer, dimension(3, 2), intent(in):: miller_indices

        real(DP):: normalized(3, 2), &
                   tensor(3, 3)

        !Initialize taylor and spin coefficients of the slip system
        normalized = normalize(miller_indices)
        tensor = normalized(:,1) .tensor. normalized(:,2)
        this%taylor_coeffs = convert_stress_strain_space(tensor)
        this%spin_coeffs = convert_spin(tensor)
    end subroutine

    pure real(DP) function slip_system_get_work_rate(this) result(work_rate)
        class(SlipSystem), intent(in):: this

        work_rate = abs(merge(this%crss(1), this%crss(2), this%slip_rate > 0._DP) * this%slip_rate)
    end function
end module
