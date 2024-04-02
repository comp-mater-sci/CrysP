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
    !! @param[in]     rss                  real vector (8)
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
    function resolve_taylor_ambiguity(ind_active_slip_systems, rss, strain, taylor_coeffs) result(slip_rates)
        integer, intent(in):: ind_active_slip_systems(:)
        real(DP), intent(in):: taylor_coeffs(:,:),                 &
                               rss(size(ind_active_slip_systems)),               &
                               strain(5)
        real(DP):: slip_rates(size(taylor_coeffs, 2))
        real(DP):: sign_slip(size(taylor_coeffs, 2))
        real(DP):: sum_squares_optimal 
        real(DP):: coeffs(5, size(taylor_coeffs, 2))
        integer:: i

        sign_slip = 0._DP
        sign_slip(ind_active_slip_systems)=sign(1._DP, rss)

        do i = 1, size(ind_active_slip_systems)
            coeffs(:, ind_active_slip_systems(i)) = taylor_coeffs(:, ind_active_slip_systems(i)) * sign_slip(ind_active_slip_systems(i))
        end do

        sum_squares_optimal = REAL_DP_MAX_VAL
        
        slip_rates = 0._DP
        call iterate_combinations(ind_active_slip_systems, 1)

    contains

        !Get a list of all the combinations of size r of the elements of a given list
        recursive subroutine iterate_combinations(ind, start_index)
            integer, intent(in):: ind(:), &
                                  start_index
            integer:: i, &
                      info, &
                      n_systems
            integer, parameter:: SIZE_WORKSPACE = 10  ! Optimal, refer to LAPACK documentation.
            real(DP):: A(5, size(ind)), &
                       B(size(ind)), &
                       workspace(SIZE_WORKSPACE), &
                       sum_squares
            
            n_systems = size(ind)

            A = coeffs(:, ind)
            B(1:5)=strain

            call dgels('N',5, n_systems, 1, A, 5, B, n_systems, workspace, SIZE_WORKSPACE, info)

            if (info == 0 .and. all(B >= -TOLERANCE)) then
                sum_squares = sum(B**2)
                if (sum_squares < sum_squares_optimal) then
                    sum_squares_optimal = sum_squares
                    slip_rates = 0._DP
                    slip_rates(ind) = B*sign_slip(ind)
                end if
            else if (size(ind) > 5) then 
                do i = start_index, n_systems
                   call iterate_combinations(pack(ind, ind /= ind(i)), i) 
                end do
            end if
        end subroutine
    end function
end module
