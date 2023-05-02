#include "altayRCM.fpp"
module altayTBH
    use definitions
    use altayRCM

    implicit none

    real(DP), parameter     ::  TOL = 1.0e-10_DP

    contains

    !Solve Taylor-Bishop-Hill for one crystallite. Stresses and strain rates are to be represented  by vectors
    subroutine TBH(NDIM,N,M,A,D,TauC,U,Irp,Dacc,GDOT,SIG,FakM,TauR,bas,DTAU)
        integer, intent(in)     ::  NDIM,           &   !<number of rows in arrays, must not < N
                                    M,              &   !<There are M slip systems
                                    N                   !<# of independent Taylor equations,N=5, except for cluster models
        real(dp), intent(in)    ::  A(NDIM,M),      &   !<coefficient matrix of Taylor equations
                                    D(NDIM),        &   !<right-hand side of Taylor equations=imposed strain rate
                                    TauC(2,M)           !<critical resolved shear stresses (all Tauc>0)
        logical                 ::  bas(M)              !<MD: "workspace" from pancake and has the 'save' attribute. Might be the reason for strange behavior
        real(dp), intent(out)   ::  Dacc(NDIM),     &   !<Final slip rates, for the slip systems indexed in Irp
                                    GDOT(M),        &   !<Slip rates
                                    SIG(NDIM),      &   !<Stress
                                    TauR(M),        &   !<Resolved shear stress
                                    DTAU(M),        &   !<abs(TAUR) - TAUC
                                    FakM                !<plastic work  (stress*imposed strain rate)
        integer, intent(inout)  ::  Irp(NDIM)           !<Indices of active slip systems
        real(DP), intent(inout) ::  U(NDIM,N)           !<Inverse of basis, corresponding with IRP
        real(dp)                ::  Aprime(NDIM),   &   !<column of U * A
                                    Trp(NDIM),      &   !<resolved shear stress on basis systems
                                    CUst(NDIM)          !< compact storage of U* (only one column)
        integer                 ::  i,              &
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

        bas(1:M) = .FALSE.
        TAuR(1:M) = 0.D0
        do i=1,N
            bas(Irp(i))=.TRUE.
        enddo
        ! Calculation of slip rates in basis
        Dacc(1:N) = matmul(U(1:N,1:N),D(1:N))
        ! Calculation of stress, using generalised Schmid law
        do i=1,N
           X = Dacc(i)
           if (abs(X) < TOL) &
               X = sum(A(1:N,Irp(i))*D(1:N))
           Trp(i) = merge(Tauc(1,Irp(i)),-Tauc(2,Irp(i)),X>=0.0_dp)
        enddo
        iter=0
        do
            iter=iter+1
            SIG(1:N) = matmul(Trp(1:N), U(1:N,:))
            ! Calculation of Taylor factor
            FakM=sum(SIG(1:N)*D(1:N))
            ! Calculation of resolved shear stress
            TauR = matmul(SIG(1:N), A(1:N,:))

!           Search for most severly overstressed slip system
            DT=0.0d0
            jn=0
            do i=1,M
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
            Aprime(1:N) = matmul(U(1:N,:), A(1:N,jn))
            in=0
            do i=1,N
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
            CUst(1:N)=-Aprime(1:N)
            CUst(in)=1.0d0
            CUst(1:N)=CUst(1:N)/Z1
            call update_inverse_basis(U, CUst, in)
            call update_inverse_basis_vector(Dacc, Cust, in)
            ! Updating of basis: bas and Irp
            bas(Irp(in))=.FALSE.
            bas(jn)=.TRUE.
            Irp(in)=jn
            Trp(in) = merge(Tauc(1,jn),-Tauc(2,jn),(Dacc(in) >= 0.0d0).and.(X > 0.0d0))
            ! Go back to stress calculation
        enddo
        ! Solution was found.
        Gdot(1:M)=0.0
        do i=1,N
            Gdot(Irp(i))=Dacc(i)
        enddo
    end subroutine

    !>Replace basis vector in transpose of basis.
    subroutine update_inverse_basis(inv_basis, new_vec, index)
        real(DP), intent(inout) :: inv_basis(:,:)
        real(DP), intent(in)    :: new_vec(:)
        integer, intent(in)     :: index

        integer :: i

        do i=1,size(inv_basis,2)
            call update_inverse_basis_vector(inv_basis(:,i), new_vec, index)
        end do
    end subroutine update_inverse_basis

    !>Replace basis vector in single vector of transpose of basis.
    subroutine update_inverse_basis_vector(inv_basis_vec, new_vec, index)
        real(DP), intent(inout) :: inv_basis_vec(:)
        real(DP), intent(in)    :: new_vec(:)
        integer, intent(in)     :: index

        integer                 :: i
        real(DP)                :: inv_basis_vec_at_index, &
                                   prod
        
        inv_basis_vec_at_index = inv_basis_vec(index)

        do i=1,size(new_vec)
            prod = inv_basis_vec_at_index * new_vec(i)
            if (i == index) then
                inv_basis_vec(i) = prod
            else
                inv_basis_vec(i) = inv_basis_vec(i) + prod
            end if
        end do
    end subroutine update_inverse_basis_vector
end module
