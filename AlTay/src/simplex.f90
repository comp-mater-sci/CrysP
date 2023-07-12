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
        integer                 ::  i,iter,jn,in
        real(DP)                ::  x,dt,zr,gmin

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

            stress = matmul(rss_basis, inverse_basis)
            rss = matmul(stress, taylor_coeffs)

            !Search for most severly overstressed slip system
            DT=0.0d0
            jn=0
            do i=1,size(taylor_coeffs,2)
                overstress(i)=merge(rss(i)-crss(1,i),-rss(i)-crss(2,i),rss(i)>=0.0_dp)
                if (abs(overstress(i)-DT) < TOLERANCE) overstress(i)=DT
                if (bas(i) .or. (abs(rss(i)) < TOLERANCE) .or. (overstress(i) <= DT)) cycle
                DT=overstress(i)
                jn=i
            enddo
            if (jn == 0) exit ! There is no overstressed slip system
            ! There is an overstressed slip system, which we will activate now
            ! Search which active slip system must be deactivated (removed from basis)
            ! Calculate column Mprime-s*, called new_basis_vector
            new_basis_vector = matmul(inverse_basis, taylor_coeffs(:,jn))
            in=0
            do i=1,size(taylor_coeffs,1)
                if (abs(new_basis_vector(i)) < TOLERANCE) cycle
                ZR=rss(basis_systems(i))
                if (abs(ZR) < TOLERANCE) then
                    ZR=slip_basis(i)
                    if (abs(Zr) < TOLERANCE) cycle
                endif
                ZR=ZR*new_basis_vector(i)
                if ((rss(jn) > 0.0d0  .and. ZR >= 0.0_DP .and. (in == 0 .or. slip_basis(i)/new_basis_vector(i) < Gmin)) .or. &
                    (rss(jn) <= 0.0d0 .and. ZR <= 0.0_DP .and. (in == 0 .or. slip_basis(i)/new_basis_vector(i) > Gmin))) then
                    in=i
                    Gmin=slip_basis(i)/new_basis_vector(i)
                endif
            enddo
            if (in == 0) &
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'The solution is unbounded.')
            new_inverse_basis_vector=-new_basis_vector/new_basis_vector(in)
            new_inverse_basis_vector(in)=1.0d0/new_basis_vector(in)

            do i=1,size(taylor_coeffs,1)
                call update_inverse_basis_vector(inverse_basis(:,i), new_inverse_basis_vector, in)
            end do
            call update_inverse_basis_vector(slip_basis, new_inverse_basis_vector, in)

            ! Updating of basis: bas and basis_systems
            bas(basis_systems(in))=.false.
            bas(jn)=.true.
            basis_systems(in)=jn
            rss_basis(in) = merge(crss(1,jn),-crss(2,jn),(slip_basis(in) >= 0.0d0).and.(rss(jn) > 0.0d0))
            ! Go back to stress calculation
        enddo
        ! Solution was found.
        slip = 0._DP
        slip(basis_systems) = slip_basis
    end subroutine simplex_solve

    !>Replace basis vector in single vector of transpose of basis.
    subroutine update_inverse_basis_vector(inv_basis_vec, new_vec, index)
        real(DP), intent(inout) :: inv_basis_vec(:)
        real(DP), intent(in) :: new_vec(size(inv_basis_vec))
        integer, intent(in) :: index

        integer                 :: i
        real(DP)                :: inv_basis_vec_at_index, &
                                   prod

        inv_basis_vec_at_index = inv_basis_vec(index)

        do i=1,size(new_vec)
            prod = inv_basis_vec_at_index * new_vec(i)
            inv_basis_vec(i) = merge(prod,prod+inv_basis_vec(i),i==index)
        end do
    end subroutine update_inverse_basis_vector
end module simplex
