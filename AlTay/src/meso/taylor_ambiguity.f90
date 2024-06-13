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
    !! @return    slip_rates                  real vector (n_slip_systems)
    !!                                        Slip rates for all the slip systems
    function resolve_taylor_ambiguity(ind_active_slip_systems, rss, strain, taylor_coeffs, n_active_simplex) result(slip_rates)
        integer, intent(in)::                                               ind_active_slip_systems(:)  !< Indices of the active slip systems.
        real(DP), dimension(:,:), intent(in)::                              taylor_coeffs               !< Slip systems in stress-strain space.
        real(DP), dimension(:), intent(in)::    rss                         !< Resolved shear stress on the active slip systems.
        real(DP), dimension(5), intent(in)::                                strain(5)                   !< Imposed strain on the current grain.
        integer, intent(in)::                                               n_active_simplex            !< Number of active slip systems according to the simplex solution.


        real(DP)::              slip_rates(size(taylor_coeffs, 2)), &
                                coeffs(5, size(taylor_coeffs, 2)), &
                                sum_squares_optimal
        integer::               sign_slip(size(taylor_coeffs, 2)), &
                                i, nlp

        sign_slip = 0._DP
        sign_slip(ind_active_slip_systems)=int(sign(1._DP, rss))

        do i = 1, size(ind_active_slip_systems)
            coeffs(:, ind_active_slip_systems(i)) = taylor_coeffs(:, ind_active_slip_systems(i)) * sign_slip(ind_active_slip_systems(i))
        end do

        sum_squares_optimal = REAL_DP_MAX_VAL
        slip_rates = 0._DP
        call iterate_combinations(coeffs, strain, ind_active_slip_systems, 1, sign_slip, slip_rates, sum_squares_optimal, n_active_simplex)

        if (sum_squares_optimal == REAL_DP_MAX_VAL) &
            call log_error(MOD_NAME, 'resolve_taylor_ambiguity', ERR, 'Could not find optimal solution.')

    end function


    !> @brief iterate over all combinations of active slip systems to find the minimum norm solution
    !!
    !! @details This routine finds the combination of slip rates among the active slip systems which yields the smalles sum of
    !!          squared slips. It starts with evaluating the input combination. If this does not yield a valid solution (all slip rates
    !!          positive), it tries all subsets of the input set with at least 5 slip systems.
    recursive subroutine iterate_combinations(coeffs, strain, ind, start_index, sign_slip, slip_rates, sum_squares_optimal, n_active_simplex)
        real(DP), intent(in)::      coeffs(:,:), &                  !< Taylor coefficients of the slip systems with their sign adjusted based on the rss found
                                                                    !  in simplex so that all slip rates determined by the minimum norm solution should be possitive.
                                    strain(5)                       !< Imposed strain on the grain.
        integer, intent(in)::       ind(:), &                       !< Indices of the currently considered slip systems.
                                    start_index, &                  !< Index from which to start looping over possible subsets. Needed to avoid duplicting
                                                                    !  combinations (e.g. [1, 2] and [2, 1]).
                                    n_active_simplex, &             !< Number of active slip systems according to the simplex solution.
                                    sign_slip(:)                    !< Sign of the slip rates on each of the candidate slip systems.
        real(DP), intent(inout)::   slip_rates(size(sign_slip)), &  !< Current optimal solution for the slip rates.
                                    sum_squares_optimal             !< Optimal sum of squared slip rates found so far

        integer, parameter::        SIZE_WORKSPACE = 10  ! Optimal, refer to LAPACK documentation.
        integer::                   i, &
                                    info, &
                                    n_systems, &
                                    ind_new(size(ind)-1)
        real(DP)::                  A(5, size(ind)), &
                                    B(max(5, size(ind))), &
                                    workspace(SIZE_WORKSPACE), &
                                    sum_squares, &
                                    residual

        n_systems = size(ind)

        !Must copy to local vars because dgels overwrites these internally
        A = coeffs(:, ind)
        B(1:5)=strain

        !Lapack routine to find minimum norm solution
        call dgels('N',5, n_systems, 1, A, 5, B, size(B), workspace, SIZE_WORKSPACE, info)

        ! Dgels expects the input system to be of full rank. If this is not the case, info > 0. We can then safely ignore the
        ! solution because we know at least 1 solution of full rank exists, which was the result from simplex. Thus, we just try different
        ! combinations until we find it.
        if (info == 0) then
            ! If the system is overdetermined, dgels computes the least squares solution instead of the minimum norm solution. Any
            ! valid solutions computed in this way should however be exact, since we can only end up with fewer than 5 slip systems
            ! here if simplex found a degenerate solution (at least one slip rate 0). This means that a valid least squares solution
            ! must have a negligible residual.
            if (n_systems < 5) then
                residual = sum(B(n_systems+1:5)**2)  ! See LAPACK documentation
                if (residual > TOLERANCE) return  ! Residual can only increase by taking a subset of the current systems.
            end if

            sum_squares = sum(B(1:n_systems)**2)

            if (sum_squares < sum_squares_optimal) then
                                !We need to add a tolerance on B because the input system may be ill-conditioned
                if (all(B(1:n_systems) >= -TOLERANCE)) then
                    sum_squares_optimal = sum_squares
                    slip_rates = 0._DP
                    slip_rates(ind) = B(1:n_systems)*sign_slip(ind)
                    return
                end if
            else
                ! Taking subsets will only yield larger residuals.
                return
            end if
        end if

        ! Only when the maximum number of slip systems in a combination is equal to the number of active slip systems simplex found,
        ! we are guaranteed by simplex that the system is nonsingular
        if (size(ind) > n_active_simplex) then
            do i = start_index, n_systems
                ind_new = pack(ind, ind /= ind(i))
                call iterate_combinations(coeffs, strain, ind_new, i, sign_slip, slip_rates, sum_squares_optimal, n_active_simplex)
            end do
        end if
    end subroutine
end module
