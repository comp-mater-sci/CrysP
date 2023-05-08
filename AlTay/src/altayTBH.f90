#include "altayRCM.fpp"
module altayTBH
    use definitions
    use altayRCM

    implicit none
    private

    real(DP), parameter ::  TOL = 1.0e-10_DP

    public  ::  tbh

    contains
    
!   solve Taylor-Bishop-Hill for one crystallite. Stresses and strain rates are to be represented  by vectors
    subroutine TBH(A,D,TauC,BINV,U,IACT,Irp,GDOT,SIG,TauR,DTAU)
        real(dp), intent(in) :: A(:,:)  !< coefficient matrix of Taylor equations, first dim is #Nslip
        integer, intent(in) :: IACT(size(A,1)) ! < indices of active slip systems: first guess
        real(dp), intent(in) :: D(size(A,1)), &    !< right-hand side of Taylor equations=imposed strain rate
                                BINV(size(A,1),size(A,1)), &
                                TauC(2,size(A,2))
        logical :: bas(size(A,2)) ! bas (logical TRUE=belongs to basis)
        real(dp), intent(out) :: U(size(A,1),size(A,1)), GDOT(size(A,2)),SIG(size(A,1)),TauR(size(A,2)),DTAU(size(A,2))
        integer, intent(out) :: Irp(size(A,1)) !< indices of active slip systems

        real(dp) :: Aprime(size(A,1)), & !< column of U * A
                    Trp(size(A,1)), &    !< resolved shear stress on basis systems
                    UU(size(A,1),size(A,1)),&    !< copy of inverse of basis
                    CUst(size(A,1)),&    !< compact storage of U* (only one column)
                    DD(size(A,1)), &        !< copy of strain rates in some basis
                    Dacc(size(A,1))         !< final slip rates, for the slip systems indexed in Irp

        integer :: i,k,iter,jn,in
        real(dp) :: x,dt,zr,gmin

        U=BINV
        Irp=IACT
        bas=.FALSE.
        TAuR=0.0
        bas(Irp)=.TRUE.
        ! Calculation of slip rates in basis
        Dacc = matmul(U,D)
        ! Calculation of stress, using generalised Schmid law
        do i=1,size(A,1)
           X=Dacc(i)
           if (abs(X) < TOL) X=dot_product(A(:,Irp(i)),D)
           Trp(i) = merge(Tauc(1,Irp(i)),-Tauc(2,Irp(i)),X>=0.0_dp)
        enddo
        
        do
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
                if (TauR(jn) > 0.0d0) then
                    if (ZR >= 0.0_DP .and. (in == 0 .or. Dacc(i)/Aprime(i) < Gmin)) then
                        in=i
                        Gmin=Dacc(i)/Aprime(i)
                   endif
                else
                    if (ZR <= 0.0_DP .and. (in == 0 .or. Dacc(i)/Aprime(i) > Gmin)) then
                        in=i
                        Gmin=Dacc(i)/Aprime(i)
                    endif
                endif
            enddo
            if (in == 0) then
                RCM_RAISE(1,'TBH','The solution is unbounded',RCM_RTN)
            endif
            CUst=-Aprime/Aprime(in)
            CUst(in)=1.0d0/Aprime(in)

            do i=1,size(A,1)
                call update_inverse_basis_vector(U(:,i))
            end do
            call update_inverse_basis_vector(Dacc)

            ! Updating of basis: bas and Irp
            bas(Irp(in))=.FALSE.
            bas(jn)=.TRUE.
            Irp(in)=jn
            Trp(in) = merge(Tauc(1,jn),-Tauc(2,jn),(Dacc(in) >= 0.0d0).and.(TauR(jn) > 0.0d0))
            ! Go back to stress calculation
        enddo
        ! Solution was found.
        Gdot = 0._DP
        do i=1,size(A,1)
            Gdot(Irp(i)) = Dacc(i)
        enddo
    contains
        !>Replace basis vector in single vector of transpose of basis.
        subroutine update_inverse_basis_vector(inv_basis_vec)
            real(DP), intent(inout) :: inv_basis_vec(:)
    
            integer                 :: i
            real(DP)                :: inv_basis_vec_at_index, &
                                       prod
            
            inv_basis_vec_at_index = inv_basis_vec(in)
    
            do i=1,size(Cust)
                prod = inv_basis_vec_at_index * Cust(i)
                if (i == in) then
                    inv_basis_vec(i) = prod
                else
                    inv_basis_vec(i) = inv_basis_vec(i) + prod
                end if
            end do
        end subroutine update_inverse_basis_vector
    end subroutine tbh
end module
