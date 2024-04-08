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
    function resolve_taylor_ambiguity(ind_active_slip_systems, rss, strain, taylor_coeffs) result(slip_rates)
        integer, intent(in)::   ind_active_slip_systems(:)              !< Indices of the active slip systems.
        real(DP), intent(in)::  taylor_coeffs(:,:),                 &   !< Slip systems in stress-strainn space.
                                rss(size(ind_active_slip_systems)), &   !< Resolved shear stress on the active slip systems.
                                strain(5)                               !< Imposed strain on the current grain.
                            
        real(DP)::              slip_rates(size(taylor_coeffs, 2)), &
                                coeffs(5, size(taylor_coeffs, 2)), &
                                sum_squares_optimal 
        integer::               sign_slip(size(taylor_coeffs, 2)), &
                                i 

        sign_slip = 0._DP
        sign_slip(ind_active_slip_systems)=int(sign(1._DP, rss))

        do i = 1, size(ind_active_slip_systems)
            coeffs(:, ind_active_slip_systems(i)) = taylor_coeffs(:, ind_active_slip_systems(i)) * sign_slip(ind_active_slip_systems(i))
        end do

        sum_squares_optimal = REAL_DP_MAX_VAL
        slip_rates = 0._DP
        call iterate_combinations(coeffs, strain, ind_active_slip_systems, 1, sign_slip, slip_rates, sum_squares_optimal)
    end function


    !> @brief iterate over all combinations of active slip systems to find the minimum norm solution
    !!
    !! @details This routine finds the combination of slip rates among the active slip systems which yields the smalles sum of
    !!          squared slips. It starts with evaluating the input combination. If this does not yield a valid solution (all slip rates
    !!          positive), it tries all subsets of the input set with at least 5 slip systems. 
    recursive subroutine iterate_combinations(coeffs, strain, ind, start_index, sign_slip, slip_rates, sum_squares_optimal)
        real(DP), intent(in)::      coeffs(:,:), &                  !< Taylor coefficients of the slip systems with their sign adjusted based on the rss found
                                                                    !  in simplex so that all slip rates determined by the minimum norm solution should be possitive.
                                    strain(5)                       !< Imposed strain on the grain.
        integer, intent(in)::       ind(:), &                       !< Indices of the currently considered slip systems.
                                    start_index, &                  !< Index from which to start looping over possible subsets. Needed to avoid duplicting
                                                                    !  combinations (e.g. [1, 2] and [2, 1]).
                                    sign_slip(:)                    !< Sign of the slip rates on each of the candidate slip systems.
        real(DP), intent(inout)::   slip_rates(size(sign_slip)), &  !< Current optimal solution for the slip rates.
                                    sum_squares_optimal             !< Optimal sum of squared slip rates found so far

        integer, parameter::        SIZE_WORKSPACE = 10  ! Optimal, refer to LAPACK documentation.
        integer::                   i, &
                                    info, &
                                    n_systems
        real(DP)::                  A(5, size(ind)), &
                                    B(size(ind)), &
                                    workspace(SIZE_WORKSPACE), &
                                    sum_squares
        
        n_systems = size(ind)

        !Must copy to local vars because dgels overwrites these internally
        A = coeffs(:, ind)
        B(1:5)=strain

        !Lapack routine to find minimum norm solution
        call dgels('N',5, n_systems, 1, A, 5, B, n_systems, workspace, SIZE_WORKSPACE, info)

        !Dgels expects the input system to be of full rank. If this is not the case, info > 0. We can then safely ignore the
        !solution because we know at least 1 solution of full rank exists, which was the result from simplex. Thus, we just try different
        !combinations until we find it.
        if (info == 0) then
            sum_squares = sum(B**2)
            if (sum_squares < sum_squares_optimal) then
                !We need to add a tolerance on B because the input system may be ill-conditioned
                if (all(B >= -TOLERANCE)) then
                    sum_squares_optimal = sum_squares
                    slip_rates = 0._DP
                    slip_rates(ind) = B*sign_slip(ind)
                    return
                end if    
            else 
                !If the problem is of full rank but the sum of squared slip rates is larger than the currently found optimal, we
                !give up because taking subsets of this set of systems will only yield larger residuals.
                return     
            end if
        end if

        if (size(ind) > 5) then 
            do i = start_index, n_systems
               call iterate_combinations(coeffs, strain, pack(ind, ind /= ind(i)), i, sign_slip, slip_rates, sum_squares_optimal) 
            end do
        end if
    end subroutine
end module
