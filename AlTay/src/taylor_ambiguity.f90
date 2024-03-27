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
        real(DP):: sgnn(size(slip_rates))
        real(DP):: sum_squares_optimal, slip_rates_optimal(n_active_slip_systems)
        integer:: ind_optimal(n_active_slip_systems), n_optimal
        integer:: size_workspace, info
        real(DP):: work(1), coeffs(5, size(taylor_coeffs, 2))
        integer:: i

        

        sgnn = 0._DP
        sgnn(ind_active_slip_systems(1:n_active_slip_systems))=sign(1._DP, TAURLP(1:n_active_slip_systems))

        do i = 1, n_active_slip_systems
            coeffs(:, ind_active_slip_systems(i)) = taylor_coeffs(:, ind_active_slip_systems(i)) * sgnn(ind_active_slip_systems(i))
        end do

        sum_squares_optimal = REAL_DP_MAX_VAL

        
        !Workspace query. Work(1) contains optimal size for workspace in dgels call in iterate_combinations.
        !Taylor_coeffs passed in as dummy argument and is ignored.
        call dgels('N',5, n_active_slip_systems, 1, taylor_coeffs, 5, taylor_coeffs, n_active_slip_systems, work, -1, info)
        call iterate_combinations(ind_active_slip_systems, 1, int(work(1)))

        if (sum_squares_optimal < REAL_DP_MAX_VAL) then 
            slip_rates = 0._DP
            slip_rates(ind_optimal(1:n_optimal))=slip_rates_optimal(1:n_optimal)*sgnn(ind_optimal(1:n_optimal))
        end if

    contains

        !Get a list of all the combinations of size r of the elements of a given list
        recursive subroutine iterate_combinations(ind, start_index, size_workspace)
            integer, intent(in):: ind(:), &
                                  start_index, &
                                  size_workspace
            integer:: i, &
                      info, &
                      n_systems
            real(DP):: A(5, size(ind)), &
                       B(size(ind)), &
                       workspace(size_workspace), &
                       sum_squares
            
            n_systems = size(ind)

            A = coeffs(:, ind)
            B(1:5)=strain

            call dgels('N',5, n_systems, 1, A, 5, B, n_systems, workspace, size_workspace, info)

            if (info == 0 .and. all(B >= 0)) then
                sum_squares = sum(B**2)
                if (sum_squares < sum_squares_optimal) then
                    n_optimal = n_systems
                    ind_optimal(1:n_systems) = ind
                    slip_rates_optimal(1:n_systems) = B
                    sum_squares_optimal = sum_squares
                end if
            else if (size(ind) > 5) then 
                !Workspace query. Work(1) contains optimal size for workspace in dgels call in next iteration.
                !A and B are dummy arguments and are ignored.
                call dgels('N',5, n_systems-1, 1, A, 5, B, n_systems-1, workspace, -1, info)
                do i = start_index, n_systems
                   call iterate_combinations(pack(ind, ind /= ind(i)), i, int(workspace(1))) 
                end do
            end if
        end subroutine
    end subroutine
end module
