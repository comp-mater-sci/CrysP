module simplex
    use definitions
    use logging

    implicit none
    private

    character(*), parameter, private :: MOD_NAME = 'simplex'

    public :: simplex_solve

    contains

    subroutine simplex_solve(taylor_coeffs, strain, crss, inverse_basis, basis_systems, slip, stress, rss, overstress)
        real(DP), intent(in)    ::  taylor_coeffs(:,:),                                 &
                                    strain(size(taylor_coeffs,1)),                      &    
                                    crss(2,size(taylor_coeffs,2))
        real(DP), intent(out)   ::  slip(size(taylor_coeffs,2)),                        &
                                    stress(size(taylor_coeffs,1)),                      &
                                    rss(size(taylor_coeffs,2)),                         &
                                    overstress(size(taylor_coeffs,2))
        integer, intent(inout)  ::  basis_systems(size(taylor_coeffs,1)) 
        real(DP), intent(inout) ::  inverse_basis(size(taylor_coeffs,1),size(taylor_coeffs,1))

        real(DP)                ::  new_basis_vector(size(taylor_coeffs,1)),            & 
                                    rss_basis(size(taylor_coeffs,1)),                   & 
                                    new_inverse_basis_vector(size(taylor_coeffs,1)),    &
                                    slip_basis(size(taylor_coeffs,1))        
        logical                 ::  bas(size(taylor_coeffs,2))
        integer                 ::  i,iter,most_overstressed_system, system_to_remove
        real(DP)                ::  x,dt,zr,gmin, tmp

        character(*), parameter :: PROC_NAME = 'simplex'

        bas = .false.
        bas(basis_systems) = .true.

        slip_basis = matmul(inverse_basis, strain)

        ! Calculation of stress, using generalised Schmid law
        do i=1,size(taylor_coeffs,1)
           X=merge(dot_product(taylor_coeffs(:,basis_systems(i)),strain),slip_basis(i), abs(slip_basis(i)) < TOLERANCE)
           rss_basis(i) = merge(crss(1,basis_systems(i)),-crss(2,basis_systems(i)),X>=0.0_dp)
        enddo

        gmin = 0.0_DP
        iter = 0
        do
            iter=iter+1
            if (iter > 50) &
                call log_error(MOD_NAME, PROC_NAME, ERR, 'Too many iterations.')

            call find_most_overstressed_system(taylor_coeffs, rss_basis, inverse_basis, crss, bas, stress, rss, most_overstressed_system, overstress)

            if (most_overstressed_system == 0) exit ! There is no overstressed slip system
            ! There is an overstressed slip system, which we will activate now
            ! Search which active slip system must be deactivated (removed from basis)
            ! Calculate column Mprime-s*, called new_basis_vector
            new_basis_vector = matmul(inverse_basis, taylor_coeffs(:,most_overstressed_system))
            system_to_remove=0
            do i=1,size(taylor_coeffs,1)
                if (abs(new_basis_vector(i)) < TOLERANCE) cycle
                ZR=rss(basis_systems(i))
                if (abs(ZR) < TOLERANCE) then
                    ZR=slip_basis(i)
                    if (abs(Zr) < TOLERANCE) cycle
                endif
                ZR=ZR*new_basis_vector(i)
                if ((rss(most_overstressed_system) > 0.0d0  .and. ZR >= 0.0_DP .and. (system_to_remove == 0 .or. slip_basis(i)/new_basis_vector(i) < Gmin)) .or. &
                    (rss(most_overstressed_system) <= 0.0d0 .and. ZR <= 0.0_DP .and. (system_to_remove == 0 .or. slip_basis(i)/new_basis_vector(i) > Gmin))) then
                    system_to_remove=i
                    Gmin=slip_basis(i)/new_basis_vector(i)
                endif
            enddo
            if (system_to_remove == 0) &
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'The solution is unbounded.')

            call update_inverse_basis(inverse_basis, new_basis_vector, system_to_remove, new_inverse_basis_vector)
            call update_vector_in_basis(slip_basis, system_to_remove, new_inverse_basis_vector)

            ! Updating of basis: bas and basis_systems
            bas(basis_systems(system_to_remove))=.false.
            bas(most_overstressed_system)=.true.
            basis_systems(system_to_remove)=most_overstressed_system
            rss_basis(system_to_remove) = merge(crss(1,most_overstressed_system),-crss(2,most_overstressed_system),(slip_basis(system_to_remove) >= 0.0d0).and.(rss(most_overstressed_system) > 0.0d0))
            ! Go back to stress calculation
        enddo
        ! Solution was found.
        slip = 0._DP
        slip(basis_systems) = slip_basis
    end subroutine simplex_solve

    subroutine find_most_overstressed_system(taylor_coeffs, rss_basis, inverse_basis, crss, bas, stress, rss, most_overstressed_system, overstress)
        real(DP), intent(in) :: taylor_coeffs(:,:), &
                                rss_basis(size(taylor_coeffs,1)), &
                                inverse_basis(size(taylor_coeffs,1), size(taylor_coeffs,1)), &
                                crss(2,size(taylor_coeffs,2))
        logical, dimension(size(taylor_coeffs, 2)), intent(in) :: bas
        real(DP), intent(out) :: stress(size(taylor_coeffs,1)), &
                                 rss(size(taylor_coeffs,2)), &
                                 overstress(size(taylor_coeffs,2))
        integer, intent(out) :: most_overstressed_system
        real(DP) :: tmp      
        integer :: i

        stress = matmul(rss_basis, inverse_basis)
        rss = matmul(stress, taylor_coeffs)
        tmp = TOLERANCE
        most_overstressed_system = 0
        do i=1,size(overstress)
            overstress(i) = merge(rss(i) - crss(1,i), -rss(i) - crss(2,i), rss(i) >= 0._DP)
            if ((overstress(i) > tmp) .and. (.not. bas(i))) then
                tmp = overstress(i) + TOLERANCE
                most_overstressed_system = i
            end if
        enddo
    end subroutine find_most_overstressed_system
    
    subroutine update_inverse_basis(inverse_basis, new_basis_vector, system_to_remove, new_inverse_basis_vector)
        real(DP), dimension(:,:), intent(inout) :: inverse_basis
        real(DP), intent(in) :: new_basis_vector(size(inverse_basis,1))
        integer, intent(in) :: system_to_remove
        real(DP), intent(out) :: new_inverse_basis_vector(size(inverse_basis,1))
        integer :: i

        new_inverse_basis_vector = -new_basis_vector / new_basis_vector(system_to_remove)
        new_inverse_basis_vector(system_to_remove) = 1._DP / new_basis_vector(system_to_remove)

        do i=1,size(new_basis_vector)
            call update_vector_in_basis(inverse_basis(:,i), system_to_remove, new_inverse_basis_vector)
        end do
    end subroutine update_inverse_basis
    
    subroutine update_vector_in_basis(inverse_basis_vector, system_to_remove, new_inverse_basis_vector)
        real(DP), intent(inout) ::  inverse_basis_vector(:)
        integer, intent(in) :: system_to_remove
        real(DP), intent(in) :: new_inverse_basis_vector(size(inverse_basis_vector))
        integer                 ::  i
        real(DP)                ::  prod, &
                                    vec_at_index

        vec_at_index = inverse_basis_vector(system_to_remove)

        do i=1,size(inverse_basis_vector)
            prod = vec_at_index * new_inverse_basis_vector(i)
            inverse_basis_vector(i) = merge(prod, inverse_basis_vector(i)+prod, i==system_to_remove)
        end do
    end subroutine update_vector_in_basis
end module simplex
