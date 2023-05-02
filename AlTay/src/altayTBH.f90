#include "altayRCM.fpp"
module altayTBH
    use definitions
    use altayRCM

    implicit none
    private

    real(DP), parameter ::  TOL = 1.0e-10_DP

    integer ::  n_slip_systems, &
                n_taylor_eqs 

    public  ::  tbh_init, &
                tbh

    contains

    subroutine tbh_init(nss, n_eqs)
        integer, intent(in) ::  nss, &
                                n_eqs
        n_slip_systems = nss
        n_taylor_eqs = n_eqs
    end subroutine tbh_init

    !Solve Taylor-Bishop-Hill for one crystallite. Stresses and strain rates are to be represented  by vectors
    subroutine TBH(A,D,TauC,U,Irp,Dacc,GDOT,SIG,FakM,TauR,bas,DTAU)
        real(dp), intent(in)    ::  A(n_taylor_eqs,n_slip_systems),      &   !<coefficient matrix of Taylor equations
                                    D(n_taylor_eqs),        &   !<right-hand side of Taylor equations=imposed strain rate
                                    TauC(2,n_slip_systems)           !<critical resolved shear stresses (all Tauc>0)
        logical                 ::  bas(n_slip_systems)              !<MD: "workspace" from pancake and has the 'save' attribute. Might be the reason for strange behavior
        real(dp), intent(out)   ::  Dacc(n_taylor_eqs),     &   !<Final slip rates, for the slip systems indexed in Irp
                                    GDOT(n_slip_systems),        &   !<Slip rates
                                    SIG(n_taylor_eqs),      &   !<Stress
                                    TauR(n_slip_systems),        &   !<Resolved shear stress
                                    DTAU(n_slip_systems),        &   !<abs(TAUR) - TAUC
                                    FakM                !<plastic work  (stress*imposed strain rate)
        integer, intent(inout)  ::  Irp(n_taylor_eqs)           !<Indices of active slip systems
        real(DP), intent(inout) ::  U(n_taylor_eqs,n_taylor_eqs)           !<Inverse of basis, corresponding with IRP
        real(dp)                ::  Aprime(n_taylor_eqs),   &   !<column of U * A
                                    Trp(n_taylor_eqs),      &   !<resolved shear stress on basis systems
                                    CUst(n_taylor_eqs)          !< compact storage of U* (only one column)
        integer                 ::  i,              &
                                    j,              &
                                    k,              &
                                    iter,           &
                                    jn,             &
                                    in
        real(dp)                ::  x,              &
                                    dt,             &
                                    y,              &
                                    z1,             &
                                    z2,             &
                                    zr,             &
                                    gmin
        bas = .FALSE.
        TAuR = 0._DP
        do i=1,n_taylor_eqs
            bas(Irp(i))=.TRUE.
        enddo
        ! Calculation of slip rates in basis
        Dacc = matmul(U,D)
        ! Calculation of stress, using generalised Schmid law
        do i=1,n_taylor_eqs
           X = Dacc(i)
           if (abs(X) < TOL) &
               X = sum(A(:,Irp(i))*D)
           Trp(i) = merge(Tauc(1,Irp(i)),-Tauc(2,Irp(i)),X>=0.0_dp)
        enddo
        iter=0
        do
            iter=iter+1
            SIG = matmul(Trp, U)
            ! Calculation of Taylor factor
            FakM=sum(SIG * D)
            ! Calculation of resolved shear stress
            TauR = matmul(SIG, A)

!           Search for most severly overstressed slip system
            DT=0.0d0
            jn=0
            do i=1,n_slip_systems
                X=TauR(i)
                Y=merge(X-Tauc(1,i),-X-Tauc(2,i),X>=0.0_dp)
                if (abs(Y) < TOL) then
                    Y=0.0d0
                    X=merge(Tauc(1,i),-Tauc(2,i),X>=0.0_dp)
                    TauR(i)=X
                endif
                if (abs(Y-DT) < TOL) Y=DT
                DTAU(i)=Y
                if (bas(i) .or. (abs(X) < TOL) .or. (Y <= DT)) cycle
                DT=Y
                jn=i
            enddo
            if (jn == 0) exit ! There is no overstressed slip system
            ! There is an overstressed slip system, which we will activate now
            ! Search which active slip system must be desactivated (removed from basis)
            X=TauR(jn)
            ! Calculate column Mprime-s*, called Aprime
            Aprime = matmul(U, A(:,jn))
            in=0
            do i=1,n_taylor_eqs
                Z1=Aprime(i)
                if (abs(Z1) < TOL) cycle
                ZR=TauR(Irp(i))
                if (abs(ZR) < TOL) then
                    ZR=Dacc(i)
                    if (abs(Zr) < TOL) cycle
                endif
                ZR=ZR*Z1
                Z2=Dacc(i)/Z1
                if (X > 0.0d0) then
                    if (ZR < 0.0d0) cycle
                    if (in == 0 .or. Z2 < Gmin) then
                        in=i
                        Gmin=Z2
                   endif
                else
                    if (ZR > 0.0d0) cycle
                    if (in == 0 .or. Z2 > Gmin) then
                        in=i
                        Gmin=Z2
                    endif
                endif
            enddo
            if (in == 0) then
                RCM_RAISE(1,'TBH','The solution is unbounded',RCM_RTN)
            endif
            Z1=Aprime(in)
            CUst = -Aprime
            CUst(in)=1._DP
            CUst = CUst/Z1

            do j=1,size(U,2)
                call update_inverse_basis_vector(U(:,j))
            end do
            call update_inverse_basis_vector(Dacc)

            ! Updating of basis: bas and Irp
            bas(Irp(in))=.FALSE.
            bas(jn)=.TRUE.
            Irp(in)=jn
            Trp(in) = merge(Tauc(1,jn),-Tauc(2,jn),(Dacc(in) >= 0.0d0).and.(X > 0.0d0))
            ! Go back to stress calculation
        enddo
        ! Solution was found.
        Gdot = 0._DP
        do i=1,n_taylor_eqs
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
