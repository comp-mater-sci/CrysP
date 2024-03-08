module taylor_ambiguity
    use utils
    use logging

    implicit none
    private

    character(len=*), parameter:: MOD_NAME = "taylor_ambiguity"

    public:: resolve_taylor_ambiguity

contains

    !> @brief Resolves the Taylor ambiguity
    !!
    !! @details This subroutine resolves the Taylor ambiguity for the slip rates.
    !!          After solving the equations using the simplex method, we know
    !!          the stress, strain rate and the active slip systems.
    !!          This still leaves us with several options for the slip rates.
    !!          This subroutine finds the slip rates assuming that the sum
    !!          of the squares of the slip rates must be minimal.
    !!
    !!          The minimum norm solution may not satisfy the yield conditions.
    !!          (i.e. the slip rates may be negative). In that case, we try
    !!          all possible combinations of the slip systems and minimize again.
    !!
    !! @param[out]    slip_rates              real vector (n_slip_systems)
    !!                                        Slip rates for all the slip systems
    !!                                        Note: this variable is marked as [in, out] in the code, 
    !!                                        but it seems to be overwritten immediately.
    !!
    !! @param[in]     n_active_slip_systems   integer
    !!                                        Number of active slip systems
    !!                                        n_active_slip_systems <= 8
    !!
    !! @param[in]     SLIPLP                  real vector (8)
    !!                                        Slip rates of the active slip systems
    !!
    !! @param[in]     TAURLP                  real vector (8)
    !!                                        RSS of the active slip systems
    !!
    !! @param[in]     ind_active_slip_systems integer vector (8)
    !!                                        Indices of the active slip systems
    !!                                        Note: this variable is marked as [in, out] in the code, 
    !!                                        but it is never actually modified.
    !!
    !! @param[in]     BB8                     real vector (5)
    !!                                        The total strain rate (imposed+relaxation)
    !!
    !! @param[in]     A1                      real matrix (5, n_slip_systems)
    !!                                        The symmetric (non-rotational) part of taylor equations matrix for a single grain.
    !!                                        The matrix is represented in crystal frame.
    !!                                        This matrix does not contain the relaxation terms.
    subroutine resolve_taylor_ambiguity(slip_rates, n_active_slip_systems, SLIPLP, TAURLP, ind_active_slip_systems, BB8, A1)
        integer, intent(in):: n_active_slip_systems
        real(DP), intent(in):: A1(:,:),                 &
                               TAURLP(8),               &
                               BB8(5),                  &
                               SLIPLP(8)
        integer, intent(inout):: ind_active_slip_systems(8)
        real(DP), intent(inout):: slip_rates(:)
        integer:: IND(8), ISTOR(0:8, 48), ind_active_slip_systems_original(8)
        real(DP):: SLPR(8), SLSTOR(0:8, 48), sumsq
        integer, parameter:: NSTOR = 48
        integer:: j, i1, i2, i3, N0, N1, N2, N3, NN, NOPL, INEG, IOPL
        real(DP):: sgnn(size(slip_rates))

        ind_active_slip_systems_original = ind_active_slip_systems
        slip_rates = 0.0_DP
        NN = n_active_slip_systems
        NOPL = 0
        sgnn = 0._DP
        sgnn(ind_active_slip_systems(1:n_active_slip_systems))=sign(1._DP, TAURLP(1:n_active_slip_systems))

        ! If solution is totally zero, just return
        if( sum(abs(SLIPLP(1:n_active_slip_systems))) <= TOLERANCE ) then
            NN = n_active_slip_systems
            IND(1:NN)=ind_active_slip_systems_original(1:NN)
            slip_rates(IND(1:NN))=SLIPLP(1:NN)
            return
        end if

        ! First, try the standard minimum norm solution with all active slip systems
        IND(1:NN)=ind_active_slip_systems(1:NN)
        call calc_slip(NN, IND, SLPR, ineg, sumsq, sgnn, BB8, A1)
        if (ineg == 0) then
            ! The solution is valid, and thus also mathematically guaranteed
            ! to be the minimum norm solution. We are done.
            NN = n_active_slip_systems
            slip_rates(IND(1:NN))=SLPR(1:NN)*sgnn(IND(1:NN))
            return
        endif

        ! The solution is not valid, so we need to try all possible combinations
        ! of the active slip systems and take the one with the smallest sum of squares.
        if (NN > 5) then
            !     Let us take all combinations of NN out of n_active_slip_systems
            !
            !     "Levels" in the combination search:
            !     (first level:  if n_active_slip_systems = 8, find all combinations of 7 slip systems
            !      second level: find all combinations of 6-etc.)
            N0 = n_active_slip_systems
            ! Level 1
            N1 = N0-1
            if (N1 >= 5) then
                NN = N1
                do I1 = 1, N0
                    call calc_slip(N1, IND, SLPR, ineg, sumsq, sgnn, BB8, A1)
                    if (ineg == 0) call STORE(NSTOR, NOPL, NN, SLPR, IND, ISTOR, SLSTOR, SUMSQ)
                    J = N0-I1
                    if (J > 0) IND(J)=ind_active_slip_systems(J+1)
                enddo
                ! Level 2
                N2 = N1-1
                if (N2 >= 5) then
                    NN = N2
                    do I1 = 2, N0
                        do I2 = 1, I1-1
                            call fill(IND, ind_active_slip_systems, [I1, I2],N0)
                            call calc_slip(N2, IND, SLPR, ineg, sumsq, sgnn, BB8, A1)
                            if (ineg == 0) call STORE(NSTOR, NOPL, NN, SLPR, IND, ISTOR, SLSTOR, SUMSQ)
                        enddo
                    enddo
                    ! Level 3
                    N3 = N2-1
                    if (N3 >= 5) then
                        NN = N3
                        do I1 = 3, N0
                            do I2 = 2, I1-1
                                do I3 = 1, I2-1
                                    call fill(IND, ind_active_slip_systems, [I1, I2, I3],N0)
                                    call calc_slip(N3, IND, SLPR, ineg, sumsq, sgnn, BB8, A1)
                                    if (ineg == 0) call STORE(NSTOR, NOPL, NN, SLPR, IND, ISTOR, SLSTOR, SUMSQ)
                                enddo
                            enddo
                        enddo
                    endif
                endif
                if (NOPL /= 0) then
                    IOPL = minloc(SLSTOR(0, 1:NOPL), 1)
                    NN = ISTOR(0, IOPL)
                    sumsq = SLSTOR(0, IOPL)
                    IND(1:NN)=ISTOR(1:NN, IOPL)
                    SLPR(1:NN)=SLSTOR(1:NN, IOPL)
                    slip_rates(IND(1:NN))=SLPR(1:NN)*sgnn(IND(1:NN))
                    return
                endif
            endif
        endif
        NN = n_active_slip_systems
        IND(1:NN)=ind_active_slip_systems_original(1:NN)
        slip_rates(IND(1:NN))=SLIPLP(1:NN)
    end subroutine

    !> @brief Fills the IND_ array with the active slip systems
    !!
    !! @details This subroutine fills the IND_ array with the active slip systems
    !!          skipping the slip systems with the indices in the skip array.
    !!
    !! @param[in, out] IND_                    integer vector (8)
    !!                                        Indices of the active slip systems
    !!
    !! @param[in]     ind_active_slip_systems integer vector (8)
    !!                                        Indices of the active slip systems
    !!
    !! @param[in]     skip                    integer vector
    !!                                        Indices of the slip systems to skip
    !!
    !! @param[in]     N_max                   integer
    !!                                        Maximum number of slip systems
    subroutine fill(IND_, ind_active_slip_systems, skip, N_max)
        integer, intent(inout):: IND_(8)
        integer, intent(in):: ind_active_slip_systems(8), N_max
        integer, dimension(:), intent(in):: skip
        integer:: i, j

        j = 1
        do i = 1, N_max
            if (any(i == skip)) cycle
            IND_(j)=ind_active_slip_systems(i)
            j = j+1
        enddo

    end subroutine

    !> @brief Determine the slip rates given the stress and the active slip systems.
    !!
    !! @details This subroutine determines the slip rates given the stress and the active slip systems.
    !!          If there are more than 5 active slip systems, then there are multiple possible solutions.
    !!          This subroutine finds the solution with the smallest sum of squares of the slip rates.
    !!          If there are exactly 5 active slip systems, then there is only one solution.
    !!          If there are less than 5 active slip systems, then there may not be a solution.
    !!          In that case, we solve the system of equations in the least square sense.
    !!
    !! @param[in]     n_slip                  integer
    !!                                        Number of slip systems
    !!
    !! @param[in]     ind                     integer vector (8)
    !!                                        Indices of the active slip systems
    !!
    !! @param[out]    slip_rate               real vector (8)
    !!                                        Slip rates for the active slip systems
    !!
    !! @param[out]    ineg                    integer
    !!                                        Index of the slip system with the most negative slip rate
    !!                                        If ineg == 0, then all slip rates are positive.
    !!                                        Note that ineg > 0, means that one of the slip rates is negative, 
    !!                                        so the solution is not valid.
    !!
    !! @param[out]    sumsq                   real
    !!                                        Sum of squares of the slip rates
    !!
    !! @param[in]     sgnn                    real vector (n_slip_systems)
    !!                                        Signs of the slip rates
    !!
    !! @param[in]     strain                  real vector (5)
    !!                                        The total strain rate (imposed+relaxation)
    !!
    !! @param[in]     A8                      real matrix (5, n_slip_systems)
    !!                                        The symmetric (non-rotational) part of taylor equations matrix for a single grain.
    !!                                        The matrix is represented in crystal frame.
    !!                                        This matrix does not contain the relaxation terms.
    subroutine calc_slip(n_slip, ind, slip_rate, ineg, sumsq, sgnn, strain, A8)

        integer, intent(in):: ind(8), n_slip
        real(DP), intent(in):: A8(:,:), strain(5), sgnn(:)
        integer, intent(out):: ineg
        real(DP), intent(out):: slip_rate(8), sumsq

        real(DP):: A(5, 8), B(8), RES, x, BA(8)
        real(DP), parameter:: TOL = 1.0e-6_dp
        integer:: i, N1, N2

        N1 = 5
        N2 = n_slip
        do i = 1, N2
            A(1:5, i)=sgnn(ind(i))*A8(1:5, ind(i))
        enddo
        B(1:5)=strain

        ! Solve system of equations
        call Kleinkwa(N1, N2, 5, 8, A, B, BA, RES)

        slip_rate(1:n_slip)=BA(1:n_slip)
        sumsq = sum(slip_rate(1:n_slip)**2)
        if (RES > TOL) then
            ineg = -1
            call log_trace(MOD_NAME, 'MINSQU', 'RES too large')
        else
            x = 0.0_dp
            ineg = 0
            do i = 1, n_slip
                if (x > slip_rate(i)) then
                    x = slip_rate(i)
                    ineg = i
                endif
            enddo
        end if
    end subroutine

    !> @brief Stores a valid solution for the slip rates
    subroutine STORE(NSTOR, NOPL, NN, SLPR, IND, ISTOR, SLSTOR, SUMSQ)

        integer, intent(in):: NN, IND(8), NSTOR
        integer, intent(inout):: NOPL, ISTOR(0:8, 48)
        real(DP), intent(inout):: SLSTOR(0:8, 48)
        real(DP), intent(in):: SLPR(8), sumsq

        NOPL = NOPL+1
        if (NOPL > NSTOR) call log_error(MOD_NAME, 'store', ERR_DIMS, 'Too small dimension NSTOR in SLIPRAT')
        ISTOR(0, NOPL)=NN
        ISTOR(1:NN, NOPL)=IND(1:NN)
        SLSTOR(0, NOPL)=SUMSQ
        SLSTOR(1:NN, NOPL)=SLPR(1:NN)

    end subroutine

end module
