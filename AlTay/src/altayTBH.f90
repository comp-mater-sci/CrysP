module altayTBH
    use definitions
    use logging

    implicit none
    private

    real(DP), parameter ::  TOL = 1.0e-10_DP
    character(*), parameter, private :: MOD_NAME = 'TBH'

    public  ::  tbh

    contains

!   solve Taylor-Bishop-Hill for one crystallite. Stresses and strain rates are to be represented  by vectors
    subroutine TBH(A,D,TauC,U,Irp,GDOT,SIG,TauR,DTAU)
        real(dp), intent(in) :: A(:,:)  !< coefficient matrix of Taylor equations, first dim is #Nslip
        real(dp), intent(in) :: D(size(A,1)), &    !< right-hand side of Taylor equations=imposed strain rate
                                TauC(2,size(A,2))
        logical :: bas(size(A,2)) ! bas (logical TRUE=belongs to basis)
        real(dp), intent(out) :: GDOT(size(A,2)),SIG(size(A,1)),TauR(size(A,2)),DTAU(size(A,2))
        integer, intent(inout) :: Irp(size(A,1)) !< indices of active slip systems
        real(dp), intent(inout) :: U(size(A,1),size(A,1))

        real(dp) :: Aprime(size(A,1)), & !< column of U * A
                    Trp(size(A,1)), &    !< resolved shear stress on basis systems
                    CUst(size(A,1)),&    !< compact storage of U* (only one column)
                    Dacc(size(A,1))         !< final slip rates, for the slip systems indexed in Irp

        integer :: i,iter,jn,in
        real(dp) :: x,dt,zr,gmin

        character(*), parameter :: PROC_NAME = 'TBH'

        bas=.FALSE.
        bas(Irp)=.TRUE.
        ! Calculation of slip rates in basis
        Dacc = matmul(U,D)
        ! Calculation of stress, using generalised Schmid law
        do i=1,size(A,1)
           X=merge(dot_product(A(:,Irp(i)),D),Dacc(i), abs(Dacc(i)) < TOL)
           Trp(i) = merge(Tauc(1,Irp(i)),-Tauc(2,Irp(i)),X>=0.0_dp)
        enddo

        gmin = 0.0_DP
        iter = 0
        do
            iter=iter+1
            if (iter > 50) &
                call log_error(MOD_NAME, PROC_NAME, ERR, 'Too many iterations.')

            SIG = matmul(Trp, U)
            ! Calculation of resolved shear stress
            TauR = matmul(SIG, A)

!           Search for most severly overstressed slip system
            DT=0.0d0
            jn=0
            do i=1,size(A,2)
                DTAU(i)=merge(TauR(i)-Tauc(1,i),-TauR(i)-Tauc(2,i),TauR(i)>=0.0_dp)
                if (abs(DTAU(i)-DT) < TOL) DTAU(i)=DT
                if (bas(i) .or. (abs(TauR(i)) < TOL) .or. (DTAU(i) <= DT)) cycle
                DT=DTAU(i)
                jn=i
            enddo
            if (jn == 0) exit ! There is no overstressed slip system
            ! There is an overstressed slip system, which we will activate now
            ! Search which active slip system must be deactivated (removed from basis)
            ! Calculate column Mprime-s*, called Aprime
            Aprime = matmul(U, A(:,jn))
            in=0
            do i=1,size(A,1)
                if (abs(Aprime(i)) < TOL) cycle
                ZR=TauR(Irp(i))
                if (abs(ZR) < TOL) then
                    ZR=Dacc(i)
                    if (abs(Zr) < TOL) cycle
                endif
                ZR=ZR*Aprime(i)
                if ((TauR(jn) > 0.0d0  .and. ZR >= 0.0_DP .and. (in == 0 .or. Dacc(i)/Aprime(i) < Gmin)) .or. &
                    (TauR(jn) <= 0.0d0 .and. ZR <= 0.0_DP .and. (in == 0 .or. Dacc(i)/Aprime(i) > Gmin))) then
                    in=i
                    Gmin=Dacc(i)/Aprime(i)
                endif
            enddo
            if (in == 0) &
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'The solution is unbounded.')
            CUst=-Aprime/Aprime(in)
            CUst(in)=1.0d0/Aprime(in)

            do i=1,size(A,1)
                call update_inverse_basis_vector(U(:,i), CUst, in)
            end do
            call update_inverse_basis_vector(Dacc, CUst, in)

            ! Updating of basis: bas and Irp
            bas(Irp(in))=.FALSE.
            bas(jn)=.TRUE.
            Irp(in)=jn
            Trp(in) = merge(Tauc(1,jn),-Tauc(2,jn),(Dacc(in) >= 0.0d0).and.(TauR(jn) > 0.0d0))
            ! Go back to stress calculation
        enddo
        ! Solution was found.
        Gdot = 0._DP
        Gdot(Irp) = Dacc
    end subroutine tbh

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
end module
