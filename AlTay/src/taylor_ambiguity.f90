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
    !!
    !! @param[in]     TAURLP                  real vector (8)
    !!                                        RSS of the active slip systems
    !!
    !! @param[in]     ind_active_slip_systems integer vector (8)
    !!                                        Indices of the active slip systems
    !!                                        Note: this variable is marked as [in, out] in the code, 
    !!                                        but it is never actually modified.
    !!
    !! @param[in]     strain                     real vector (5)
    !!                                        Strain imposed on slip systems of grain (without relaxations)
    !!
    !! @param[in]     taylor_coeffs                      real matrix (5, n_slip_systems)
    !!                                        The symmetric (non-rotational) part of taylor equations matrix for a single grain.
    !!                                        The matrix is represented in crystal frame.
    !!                                        This matrix does not contain the relaxation terms.
    subroutine resolve_taylor_ambiguity(slip_rates, n_active_slip_systems, TAURLP, ind_active_slip_systems, strain, taylor_coeffs)
        integer, intent(in):: n_active_slip_systems
        real(DP), intent(in):: taylor_coeffs(:,:),                 &
                               TAURLP(n_active_slip_systems),               &
                               strain(5)
        integer, intent(inout):: ind_active_slip_systems(n_active_slip_systems)
        real(DP), intent(inout):: slip_rates(:)
        integer:: IND(n_active_slip_systems), ISTOR(0:8, 48)
        real(DP):: slip_rates_active(n_active_slip_systems), SLSTOR(0:8, 48), sum_squares
        integer, parameter:: NSTOR = 48
        integer:: j, i1, i2, i3, n_considered_slip_systems, & !Number of slip systems being considered, if not all active slip systems
                  NOPL, IOPL
        real(DP):: sgnn(size(slip_rates))
        real(DP):: sum_squares_optimal, slip_rates_optimal(n_active_slip_systems)
        integer:: ind_optimal(n_active_slip_systems), n_optimal
        integer:: combination(n_active_slip_systems)
        logical:: negative_slip

        NOPL = 0
        sgnn = 0._DP
        sgnn(ind_active_slip_systems(1:n_active_slip_systems))=sign(1._DP, TAURLP(1:n_active_slip_systems))

        ! If solution is totally zero, just return
        if(sum(abs(slip_rates)) <= TOLERANCE) return

        ! First, try the standard minimum norm solution with all active slip systems
        call calc_slip(ind_active_slip_systems, slip_rates_active, negative_slip, sum_squares, sgnn, strain, taylor_coeffs)
        if (.not. negative_slip) then
            ! The solution is valid, and thus also mathematically guaranteed
            ! to be the minimum norm solution. We are done.
            slip_rates(ind_active_slip_systems)=slip_rates_active*sgnn(ind_active_slip_systems)
            return
        endif

        IND = ind_active_slip_systems
        ! The solution is not valid, so we need to try all possible combinations
        ! of the active slip systems and take the one with the smallest sum of squares.
        if (n_active_slip_systems > 5) then
            n_considered_slip_systems = n_active_slip_systems-1
                sum_squares_optimal = REAL_DP_MAX_VAL
                do while (n_considered_slip_systems >= 5)
                    call iterate_combinations(1, 1)
                    n_considered_slip_systems = n_considered_slip_systems-1
                end do

                if (sum_squares_optimal < REAL_DP_MAX_VAL) then 
                    slip_rates = 0._DP
                    slip_rates(ind_optimal(1:n_optimal))=slip_rates_optimal(1:n_optimal)*sgnn(ind_optimal(1:n_optimal))
                end if
            !endif
        endif

    contains

        recursive subroutine iterate_combinations(index_result, start_index_loop)
            integer, intent(in):: index_result, &
                                  start_index_loop
            integer::             i

            do i = start_index_loop, n_active_slip_systems
                combination(index_result) = ind_active_slip_systems(i)
                if (index_result == n_considered_slip_systems) then
                    call calc_slip(combination(1:n_considered_slip_systems), slip_rates_active, negative_slip, sum_squares, sgnn, strain, taylor_coeffs)
                    if (.not. negative_slip) then 
                        if (sum_squares < sum_squares_optimal) then
                            n_optimal = n_considered_slip_systems
                            ind_optimal = combination
                            slip_rates_optimal = slip_rates_active
                            sum_squares_optimal = sum_squares
                        end if
                    end if 
                else
                    call iterate_combinations(index_result+1, i+1)
                end if
            end do
        end subroutine
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
    !! @param[in]     ind                     integer vector (8)
    !!                                        Indices of the active slip systems
    !!
    !! @param[out]    slip_rate               real vector (8)
    !!                                        Slip rates for the active slip systems
    !!
    !! @param[out]    negative_slip           logical
    !!                                        True if one of the determined slips is negative.
    !!                                        If so, the solution is invalid    
    !!
    !! @param[out]    sum_squares             real
    !!                                        Sum of squares of the slip rates
    !!
    !! @param[in]     sgnn                    real vector (n_slip_systems)
    !!                                        Signs of the slip rates
    !!
    !! @param[in]     strain                  real vector (5)
    !!                                        Strain imposed on the grain 
    !!
    !! @param[in]     A8                      real matrix (5, n_slip_systems)
    !!                                        The symmetric (non-rotational) part of taylor equations matrix for a single grain.
    !!                                        The matrix is represented in crystal frame.
    !!                                        This matrix does not contain the relaxation terms.
    subroutine calc_slip(ind, slip_rate, negative_slip, sum_squares, sgnn, strain, A8)
        integer, intent(in):: ind(:)
        real(DP), intent(in):: A8(:,:), strain(5), sgnn(:)
        logical, intent(out):: negative_slip
        real(DP), intent(out):: slip_rate(size(ind)), sum_squares

        real(DP):: A(5, size(ind)), B(merge(size(ind), 5, size(ind)>5)), RES, slip_rate_buffer(size(B))        
        integer:: i
        real(DP), parameter:: TOL = 1.0e-6_dp

        do i = 1, size(ind)
            A(1:5, i)=sgnn(ind(i))*A8(1:5, ind(i))
        enddo
        B(1:5)=strain

        ! Solve system of equations
        call Kleinkwa(A, B, slip_rate_buffer, RES)

        if (size(ind) < 5) then 
            sum_squares = sum(slip_rate_buffer(size(ind)+1:size(B))**2)
        else 
            sum_squares = 0._DP
        end if
        slip_rate = slip_rate_buffer(1:size(ind))
        negative_slip = .false.
        if (RES > TOL) then
            print *, 'RES too large', RES, sum_squares, size(ind)
            negative_slip = .true.
        else
            do i = 1, size(ind)
                negative_slip = (slip_rate(i) < 0._DP)
                if (negative_slip) return
            end do
        end if
    end subroutine
end module
