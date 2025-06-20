!> This module resolves the Taylor ambiguity.
!>
!> Taylor ambiguity is a well-known problem for Taylor-like crystal plasticity models.
!> Namely, 5 independent slip systems suffice to achieve any imposed strain.
!> However, it is possible that the stress state this causes suffices to activate several other systems as well.
!> This happens especially when isotropic hardening laws are used.
!> Up to 8 slip systems may be active in total due to this issue.
!> Therefore, the actual slip rates in the crystal are 'ambiguous' since any combination
!> of these systems that achieves the imposed strain also minimizes the amount of work.
!>
!> Several methods can be used to decide on which systems to pick.
!> The approach chosen here is to look for the minimum norm solution,
!> i.e. the combination of systems where norm the total slip is minimal.

module taylor_ambiguity
    use base_defs
    use math_utils
    use logging
    use grain_module

    implicit none

    private
    public:: assess_slip_system_activity, &
             resolve_taylor_ambiguity

    character(len=*), parameter:: MOD_NAME = "taylor_ambiguity"


contains

    !> Find the number of active and indices of overstressed slip systems.
    !>
    !> Active slip systems have a nonzero slip rate, while overstressed slip systems are systems where the resolved shear
    !> stress exceeds the critical resolved shear stress. From this we can determine if Taylor ambiguity is occuring.
    subroutine assess_slip_system_activity(grain_, rss, slip_rates, n_active, ind_overstressed)
        class(Grain), intent(in):: grain_                                                 !! Grain suffering from Taylor ambiguity.
        real(DP), dimension(size(grain_%model%taylor_coeffs, 2)), intent(in):: rss        !! The resolved shear stress on each of the slip systems of the grain (as calculated by simplex)
        real(DP), dimension(size(grain_%model%taylor_coeffs, 2)), intent(in):: slip_rates !! Slip rates for each slip system (as calculated by simplex).
        integer, intent(out):: n_active                                                   !! The number of active systems (with nonzero slip rate)
        integer, dimension(:), allocatable, intent(out):: ind_overstressed                !! If taylor ambiguity is occurring, contains the
                                                                                          !! indices of the overstressed slip systems (rss >= crss). Otherwise it is returned unallocated.

        integer:: i, &
                  n_overstressed, &             !Number of active slip systems in the grain
                  ind_overstressed_buffer(8)    !Buffer for the indices of the overstressed slip systems.
                                                !Note that 8 is the theoretical maximum of active slip systems
        real(DP):: overstress                   !Extent to which a particular slip system is overstressed and thus active

        character(*), parameter:: PROC_NAME = 'detect_taylor_ambiguity'

        !Determine if taylor ambiguity may be occuring. While we are iterating over the slip systems, might as well prepare for
        !resolving it
        n_overstressed = 0
        n_active = 0
        do i = 1, size(grain_%model%taylor_coeffs, 2)
            !Overstress is the difference between the resolved shear stress and critical resolved shear stress on a system.
            overstress = merge(rss(i)-grain_%state%crss(1, i), -rss(i)-grain_%state%crss(2, i), rss(i)>0._DP)
            !Overstress should never exceed 0 because then the solution found by simplex is not optimal.
            if (abs(overstress) < TOLERANCE) then
                n_overstressed = n_overstressed+1
                ind_overstressed_buffer(n_overstressed) =i
                if (abs(slip_rates(i)) > TOLERANCE) &
                    n_active = n_active+1
            end if
        end do

        !Check if the number of overstressed slip systems is within theoretical bounds.
        if (n_overstressed > 8) then
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Too many active slip systems.')
        elseif (n_overstressed == 0) then
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'No active slip systems found.')
        endif

        if (n_overstressed > n_active) &
            ind_overstressed = ind_overstressed_buffer(:n_overstressed)
    end subroutine

    !> Resolves the Taylor ambiguity
    !>
    !> This subroutine finds the solution for the slip rates that minimizes the sum their squares.
    function resolve_taylor_ambiguity(ind_active_slip_systems, rss, strain, taylor_coeffs, n_active_simplex) result(slip_rates)
        integer, intent(in)::                        ind_active_slip_systems(:)  !! Indices of the active slip systems.
        real(DP), dimension(:,:), intent(in)::       taylor_coeffs               !! Slip systems in stress-strain space.
        real(DP), dimension(:), intent(in)::         rss                         !! Resolved shear stress on the active slip systems.
        real(DP), dimension(5), intent(in)::         strain(5)                   !! Imposed strain on the current grain.
        integer, intent(in)::                        n_active_simplex            !! Number of active slip systems according to the simplex solution.
        real(DP), dimension(size(taylor_coeffs,2)):: slip_rates                  !! Slip rates for all the slip systems

        real(DP)::              coeffs(5, size(ind_active_slip_systems)), &
                                sum_squares_optimal
        integer::               sign_slip(size(ind_active_slip_systems)), &
                                i

        sign_slip = int(sign(1._DP, rss))

        do i = 1, size(ind_active_slip_systems)
            coeffs(:, i) = taylor_coeffs(:, ind_active_slip_systems(i)) * sign_slip(i)
        end do

        sum_squares_optimal = REAL_DP_MAX_VAL
        call iterate_combinations(coeffs, strain, ind_active_slip_systems, 1, sign_slip, slip_rates, sum_squares_optimal, n_active_simplex)

        if (sum_squares_optimal == REAL_DP_MAX_VAL) &
            call log_error(MOD_NAME, 'resolve_taylor_ambiguity', ERR, 'Could not find optimal solution.')
    end function

    !> Iterate over all combinations of active slip systems to find the minimum norm solution
    !>
    !> This routine finds the combination of slip rates among the active slip systems which yields the smalles sum of
    !> squared slips. It starts with evaluating the input combination. If this does not yield a valid solution (all slip rates
    !> positive), it tries all subsets of the input set with at least as many slip systems as simplex found.
    recursive subroutine iterate_combinations(coeffs, strain, ind, start_index, sign_slip, slip_rates, sum_squares_optimal, n_active_simplex)
        real(DP), intent(in)::      coeffs(:,:), &                  !! Taylor coefficients of the slip systems with their sign adjusted based on the rss found
                                                                    !! in simplex so that all slip rates determined by the minimum norm solution should be possitive.
                                    strain(5)                       !! Imposed strain on the grain.
        integer, intent(in)::       ind(:), &                       !! Indices of the currently considered slip systems.
                                    start_index, &                  !! Index from which to start looping over possible subsets. Needed to avoid duplicting
                                                                    !! combinations (e.g. [1, 2] and [2, 1]).
                                    n_active_simplex, &             !! Number of active slip systems according to the simplex solution.
                                    sign_slip(size(ind))            !! Sign of the slip rates on each of the candidate slip systems.
        real(DP), intent(inout)::   slip_rates(:), &                !! Current optimal solution for the slip rates.
                                    sum_squares_optimal             !! Optimal sum of squared slip rates found so far

        integer, parameter::        SIZE_WORKSPACE = 10  ! Optimal, refer to LAPACK documentation.
        integer::                   i, j, &
                                    info, &
                                    n_systems, &
                                    subset(size(ind)-1)
        real(DP)::                  A(5, size(ind)), &
                                    B(max(5, size(ind))), &
                                    workspace(SIZE_WORKSPACE), &
                                    sum_squares, &
                                    residual

        n_systems = size(ind)

        !Must copy to local vars because dgels overwrites these internally
        A = coeffs
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

            sum_squares = sum(B(:n_systems)**2)

            if (sum_squares < sum_squares_optimal) then
                !We need to add a tolerance on B because the input system may be ill-conditioned
                if (all(B(:n_systems) >= -TOLERANCE)) then
                    sum_squares_optimal = sum_squares
                    slip_rates = 0._DP
                    slip_rates(ind) = B(:n_systems)*sign_slip
                    return
                end if
            else
                ! Taking subsets will only yield larger residuals.
                return
            end if
        end if

        ! Only when the number of slip systems in a combination is larger than the number of active slip systems simplex found,
        ! we are guaranteed by simplex that a subset of this combination exists that is nonsinngular.
        if (size(ind) > n_active_simplex) then
            do i = start_index, n_systems
                do j = 1, i-1
                    subset(j) = j
                end do
                do j = i+1, n_systems
                    subset(j-1) = j
                end do
                call iterate_combinations(coeffs(:,subset), strain, ind(subset), i, sign_slip(subset), slip_rates, sum_squares_optimal, n_active_simplex)
            end do
        end if
    end subroutine
end module
