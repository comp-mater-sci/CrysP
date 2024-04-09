module simplex
    use utils
    use logging

    implicit none
    private

    character(*), parameter, private:: MOD_NAME = 'simplex'

    public:: simplex_solve

contains

    !>@brief Solve a linear program whose solution gives the strain in FC taylor or Alamel
    !!
    !! This routine solves a nonlinear program of the following form:
    !!      min_g tau^T * |g|
    !!        st  A g = A_0
    !!
    !! To avoid the nonlinearity of the absolute value and to accomodate different
    !! values for the critical resolved shear stress in positive and negative directions,
    !! each g_k is replace by two values g_1k and g_2k, so that
    !!     if g_k >= 0 then g_1k = g_k  and g_2k = 0
    !!     if g_k <  0 then g_1k = g_k  and g_2k = 0
    !!
    !! The resulting LP looks like
    !!      min_{g_1, g_2}  tau^T(g_1+g_2)
    !!                  st  A g_1-A g_2 = A_0
    !!                      g_1, g_2 >= 0
    !! Implicitly, this also means that g_1k = 0 or g_2k = 0
    !!
    !! The LP is solved using the revised simplex method, a variant of simplex which avoids
    !! forming the full tableau.
    !!
    !! @param[in] taylor_coeffs      real matrix (m x n)
    !!                               Taylor coefficients, this is the matrix A.
    !!
    !! @param[in] strain             real vector (m)
    !!                               The imposed strain, this is the vector A_0.
    !!
    !! @param[in] crss               real matrix (2 x n)
    !!                               The critical resolved shear stresses in positive and negative directions.
    !!                               The first row contains the CRSS in positive direction.
    !!                               The second row contains the CRSS in negative direction.
    !!                               This is the vector tau.
    !!
    !! @param[in, out] inverse_basis  real matrix (m x m)
    !!                               The inverse of the basis matrix (the matrix of the basis vectors).
    !!
    !! @param[in] basis_systems      integer vector (m)
    !!                               The indices of the active slip systems.
    !!
    !! @param[out] slip              real vector (n)
    !!                               The calculated slip rates. This is the solution to the LP and
    !!                               therefore the most important output  ! This is the vector g.
    !!
    !! @param[out] stress            real vector (m)
    !!                               The stress in the crystal. This is not directly represented in the above formulation.
    !!                               In "simplex language" the stress is the vector of simplex multipliers
    !!
    !! @param[out] rss               real vector (n)
    !!                               The resolved shear stresses
    !!
    !! @param[out] overstress        real vector (n)
    !!                               The overstress of each slip system, i.e. the difference between the resolved shear stress
    !!                               and the critical resolved shear stress.
    !!
    subroutine simplex_solve(taylor_coeffs, strain, crss, inverse_basis, basis_systems, slip, stress, rss, overstress)
        real(DP), intent(in)    ::  taylor_coeffs(:,:),                                 &
            strain(size(taylor_coeffs, 1)),                      &
            crss(2, size(taylor_coeffs, 2))
        real(DP), intent(out)   ::  slip(size(taylor_coeffs, 2)),                        &
            stress(size(taylor_coeffs, 1)),                      &
            rss(size(taylor_coeffs, 2)),                         &
            overstress(size(taylor_coeffs, 2))
        integer, intent(inout)  ::  basis_systems(size(taylor_coeffs, 1))
        real(DP), intent(inout) ::  inverse_basis(size(taylor_coeffs, 1), size(taylor_coeffs, 1))

        real(DP)                ::  new_basis_vector(size(taylor_coeffs, 1)),            &
            rss_basis(size(taylor_coeffs, 1)),                   &
            new_inverse_basis_vector(size(taylor_coeffs, 1)),    &
            slip_basis(size(taylor_coeffs, 1))
        logical                 ::  bas(size(taylor_coeffs, 2))
        integer                 ::  i, iter, max_iters, most_overstressed_system, system_to_remove
        real(DP)                ::  tmp, ratio, min_ratio

        character(*), parameter:: PROC_NAME = 'simplex_solve'

        bas = .false.
        rss = 0._DP
        min_ratio = 0._DP
        bas(basis_systems) = .true.

        ! Calculation of slip rates in basis
        slip_basis = matmul(inverse_basis, strain)

        !Initialization of rss for basis systems
        do i = 1, size(taylor_coeffs, 1)
            !If slip in basis or dot product between glide direction and imposed strain is positive, use positive CRSS
            tmp = merge(slip_basis(i), dot_product(taylor_coeffs(:,basis_systems(i)), strain), abs(slip_basis(i)) >= TOLERANCE)
            rss_basis(i) = merge(crss(1, basis_systems(i)), -crss(2, basis_systems(i)), tmp >= 0._dp)
        end do
        call find_most_overstressed_system(taylor_coeffs, rss_basis, inverse_basis, crss, bas, stress, rss, most_overstressed_system, overstress)

        iter = 0
        max_iters = size(taylor_coeffs, 2)**2
        do while (most_overstressed_system /= 0)
            if (iter > max_iters) call log_error(MOD_NAME, PROC_NAME, ERR, 'Too many iterations')
            iter = iter+1
            ! Search which active slip system must be deactivated (removed from basis)
            new_basis_vector = matmul(inverse_basis, taylor_coeffs(:,most_overstressed_system))
            system_to_remove = 0

            do i = 1, size(taylor_coeffs, 1)
                if (abs(new_basis_vector(i)) > TOLERANCE) then
                    ratio = slip_basis(i) / new_basis_vector(i)
                    !Tmp is mostly a dummy value used for its sign and to see if the inputs are not too close to 0.
                    tmp = merge(rss(basis_systems(i)), slip_basis(i), abs(rss(basis_systems(i))) >= TOLERANCE) * new_basis_vector(i)
                    if (abs(tmp) > TOLERANCE &
                        .and. sign(tmp, rss(most_overstressed_system)) == tmp &
                        .and. (system_to_remove == 0 .or. sign(tmp, ratio-min_ratio) == -tmp)) &
                        then
                        system_to_remove = i
                        min_ratio = ratio
                    end if
                end if
            end do
            if (system_to_remove == 0) &
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'The solution is unbounded.')

            call update_inverse_basis(inverse_basis, new_basis_vector, system_to_remove, new_inverse_basis_vector)
            call update_vector_in_basis(slip_basis, system_to_remove, new_inverse_basis_vector)

            ! Updating of basis: bas and basis_systems
            bas(basis_systems(system_to_remove)) = .false.
            bas(most_overstressed_system) = .true.
            basis_systems(system_to_remove) = most_overstressed_system
            rss_basis(system_to_remove) = merge(crss(1, most_overstressed_system), -crss(2, most_overstressed_system), (slip_basis(system_to_remove) >= 0._DP) .and. (rss(most_overstressed_system) > 0._DP))
            call find_most_overstressed_system(taylor_coeffs, rss_basis, inverse_basis, crss, bas, stress, rss, most_overstressed_system, overstress)
        end do

        call refine_solution(taylor_coeffs, strain, crss, basis_systems, slip_basis)

        slip = 0._DP
        slip(basis_systems) = slip_basis
    end subroutine simplex_solve

    !>@brief Because simplex uses rank-1 updates of the inverse basis matrix, there may be some build-up of numerical error.
    ! Therefore, we recalculate the slip rates using the final basis matrix with dgels (uses QR factorization).
    ! We also check whether the solution is still valid, if not, we do some more iterations, but then always using dgels.
    subroutine refine_solution( taylor_coeffs, strain, crss, basis_systems, slip_basis )
        real(DP), intent(in) :: taylor_coeffs(:,:) !< real matrix (m x n), containing the Taylor coefficients
        real(DP), intent(in) :: strain(size(taylor_coeffs, 1)) !< real vector (m), containing the imposed strain
        real(DP), intent(in) :: crss(2, size(taylor_coeffs, 2)) !< real matrix (2 x n), containing the critical resolved shear stresses
        integer, intent(in) :: basis_systems(size(taylor_coeffs, 1)) !< integer vector (m), containing the indices of the active slip systems
        real(DP), intent(inout) :: slip_basis(size(taylor_coeffs, 1)) !< real vector (m), containing the slip rates in the basis

        real(DP) :: basis(size(taylor_coeffs, 1), size(taylor_coeffs, 1)) !< real matrix (m x m), containing the basis vectors
        real(DP) :: temp_slip(size(taylor_coeffs, 1)) !< real vector (m), containing the slip rates in the basis
        real(DP) :: work(2*size(taylor_coeffs, 1)*size(taylor_coeffs, 1))

        integer :: i, m, lwork, info

        m = size(taylor_coeffs, 1)
        lwork = 2*m*m !< Length of the work array (note: this is the minimum workspace, not optimal, but ok for small matrices)

        ! Initialize the basis matrix
        basis = 0._DP
        do i = 1, m
            basis(:,i) = taylor_coeffs(:,basis_systems(i))
        end do
        ! Initialize rhs
        temp_slip = strain
        ! Use dgels to recalculate the slip rates
        call dgels('N', m, m, 1, basis, m, temp_slip, m, work, lwork, info)

        ! Check info flag
        if (info /= 0) then
            ! Basis matrix is singular, really shouldn't happen.
            call log_error(MOD_NAME, 'refine_solution', ERR_VAL, 'Basis matrix is singular.')
            stop
        end if

        ! Check difference between the recalculated slip rates and the original slip rates
        if (maxval(abs(temp_slip - slip_basis)) > TOLERANCE) then
            ! The recalculated slip rates are different from the original slip rates
            ! This can happen due to numerical errors in the rank-1 updates of the inverse basis matrix
            ! We will do some more iterations, but always using dgels

            ! Note: in the test suite, this code is never reached, so i did not bother implementing it.
            ! If this error is ever present in the logs, then it should be quick to implement.
            call log_error(MOD_NAME, 'refine_solution', ERR_VAL, 'Refinement of the solution is not yet implemented.')
            stop
        end if

        slip_basis = temp_slip

    end subroutine refine_solution

    !>@brief Find the most overstressed system
    !!
    !! @param[in] taylor_coeffs              real matrix (m x n)
    !!                                       Taylor coefficients
    !!
    !! @param[in] rss_basis                  real vector (m)
    !!                                       The resolved shear stresses of the systems that are in the basis.
    !!
    !! @param[in] inverse_basis              real matrix (m x m)
    !!                                       The inverse of the basis matrix (the matrix of the basis vectors).
    !!
    !! @param[in] crss                       real matrix (2 x n)
    !!                                       The critical resolved shear stresses in positive and negative directions.
    !!
    !! @param[in] bas                        logical vector (n)
    !!                                       Logical vector indicating which systems are in the basis.
    !!                                       This contains essentially the same information as basis_systems, but in a different format.
    !!
    !! @param[out] stress                    real vector (m)
    !!                                       The stress in the crystal.
    !!                                       This is calculated as a byproduct during the calculation of the overstress.
    !!
    !! @param[out] rss                       real vector (n)
    !!                                       The resolved shear stresses.
    !!                                       This is calculated as a byproduct during the calculation of the overstress.
    !!
    !! @param[out] most_overstressed_system  integer
    !!                                       The index of the most overstressed system.
    !!                                       If this is 0, then all systems are below their critical resolved shear stress and the solution is found.
    !!
    !! @param[out] overstress                real vector (n)
    !!                                       The overstress of each slip system
    subroutine find_most_overstressed_system(taylor_coeffs, rss_basis, inverse_basis, crss, bas, stress, rss, most_overstressed_system, overstress)
        real(DP), intent(in):: taylor_coeffs(:,:), &
            rss_basis(size(taylor_coeffs, 1)), &
            inverse_basis(size(taylor_coeffs, 1), size(taylor_coeffs, 1)), &
            crss(2, size(taylor_coeffs, 2))
        logical, dimension(size(taylor_coeffs, 2)), intent(in):: bas
        real(DP), intent(out):: stress(size(taylor_coeffs, 1)), &
            rss(size(taylor_coeffs, 2)), &
            overstress(size(taylor_coeffs, 2))
        integer, intent(out):: most_overstressed_system
        real(DP):: tmp
        integer:: i

        stress = matmul(rss_basis, inverse_basis)
        rss = matmul(stress, taylor_coeffs)
        tmp = TOLERANCE
        most_overstressed_system = 0
        do i = 1, size(overstress)
            overstress(i) = merge(rss(i) - crss(1, i), -rss(i) - crss(2, i), rss(i) >= 0._DP)
            if ((overstress(i) > tmp) .and. (.not. bas(i))) then
                tmp = overstress(i) + TOLERANCE
                most_overstressed_system = i
            end if
        enddo
    end subroutine find_most_overstressed_system

    !>@brief Update the inverse of the basis matrix
    !!
    !! If the original basis matrix is A = [a_1, a_2, ..., a_m]
    !! and the new basis vector is a_new, then the new basis matrix is
    !! A_new = [a_1, a_2, ..., a_{system_to_remove-1}, a_new, a_{system_to_remove+1}, ..., a_m]
    !! Instead of calculating the inverse of A_new from scratch, this routine calculates the inverse of A_new
    !! from the inverse of A.
    !!
    !! @param[in, out] inverse_basis          real matrix (m x m)
    !!                                       The inverse of the basis matrix (the matrix of the basis vectors).
    !!
    !! @param[in] new_basis_vector           real vector (m)
    !!                                       The new basis vector.
    !!
    !! @param[in] system_to_remove           integer
    !!                                       The index of the system to remove from the basis.
    !!
    !! @param[out] new_inverse_basis_vector  real vector (m)
    !!
    subroutine update_inverse_basis(inverse_basis, new_basis_vector, system_to_remove, new_inverse_basis_vector)
        real(DP), dimension(:,:), intent(inout):: inverse_basis
        real(DP), intent(in):: new_basis_vector(size(inverse_basis, 1))
        integer, intent(in):: system_to_remove
        real(DP), intent(out):: new_inverse_basis_vector(size(inverse_basis, 1))
        integer:: i

        new_inverse_basis_vector = -new_basis_vector/new_basis_vector(system_to_remove)
        new_inverse_basis_vector(system_to_remove) = 1._DP/new_basis_vector(system_to_remove)

        do i = 1, size(new_basis_vector)
            call update_vector_in_basis(inverse_basis(:,i), system_to_remove, new_inverse_basis_vector)
        end do
    end subroutine update_inverse_basis

    subroutine update_vector_in_basis(inverse_basis_vector, system_to_remove, new_inverse_basis_vector)
        real(DP), intent(inout) ::  inverse_basis_vector(:)
        integer, intent(in):: system_to_remove
        real(DP), intent(in):: new_inverse_basis_vector(size(inverse_basis_vector))
        integer                 ::  i
        real(DP)                ::  prod, &
            vec_at_index

        vec_at_index = inverse_basis_vector(system_to_remove)

        do i = 1, size(inverse_basis_vector)
            prod = vec_at_index*new_inverse_basis_vector(i)
            inverse_basis_vector(i) = merge(prod, inverse_basis_vector(i)+prod, i == system_to_remove)
        end do
    end subroutine update_vector_in_basis
end module simplex
