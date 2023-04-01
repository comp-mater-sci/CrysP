#include "altayRCM.fpp"
module altayTBH
    use altay_definitions
    use altayMiscutils, only: terminate, stopcode_runtimeerror
    use altayRCM

    implicit none
    contains

!   solve Taylor-Bishop-Hill for one crystallite. Stresses and strain rates are to be represented  by vectors
    subroutine TBH(NDIM,N,M,A,D,TauC,BINV,U,IACT,Irp,Dacc,GDOT,SIG,FakM,TauR,bas,DTAU)
!     input          TauC=critical resolved shear stresses (all Tauc>0 )
!                     first row:  for positive slip, second row: for negative slip
!     input          BINV=First guess of inverse of basis, corresp. with IACT
!                    (Basis=set of columns from A corresponding with the active slip sytems)
!     output         U=Inverse of basis, corresponding with IRP
!     output         Dacc=final slip rates, for the slip systems indexed in Irp
!     output         GDOT=slip rates
!     output         SIG=stress
!     output         FakM=plastic work  (stress*imposed strain rate)
!     output         TauR (resolved shear stress)
!     workspace      bas (logical TRUE=belongs to basis)
!     output         DTAU=abs(TAUR)-TAUC

        integer, intent(in) :: NDIM,&  !< number of rows in arrays, must not < N
                               M,&     !< There are M slip systems
                               N,&     !< # of independent Taylor equations,N=5, except for cluster models
                               IACT(NDIM) ! < indices of active slip systems: first guess
        real(dp), intent(in) :: A(NDIM,M), &  !< coefficient matrix of Taylor equations
                                D(NDIM), &    !< right-hand side of Taylor equations=imposed strain rate
                                BINV(NDIM,N), &
                                TauC(2,M)
        logical :: bas(M) !< MD: this is "workspace" from pancake and has the 'save' attribute. Might be the reason for strange behavior
        real(dp), intent(out) :: U(NDIM,N), Dacc(NDIM),GDOT(M),SIG(NDIM),TauR(M),DTAU(M),FakM
        integer, intent(out) :: Irp(NDIM) !< indices of active slip systems

        real(dp) :: Aprime(NDIM), & !< column of U * A
                    Trp(NDIM), &    !< resolved shear stress on basis systems
                    UU(NDIM,N),&    !< copy of inverse of basis
                    CUst(NDIM),&    !< compact storage of U* (only one column)
                    DD(NDIM)        !< copy of strain rates in some basis
        real(dp), parameter :: TOL=1.0e-10_dp

        integer :: i,j,k,iter,jn,in
        real(dp) :: x,dt,y,z1,z2,zr,gmin

        if (N > NDIM) then
            RCM_RAISE(1,'TBH','Bad input: N>NDIM',RCM_RTN)
        endif
        U=BINV
        Irp=IACT
        do j=1,M
            bas(j)=.FALSE.
            TAuR(j)=0.0
        enddo
        do i=1,N
            j=Irp(i)
            bas(j)=.TRUE.
        enddo
        ! Calculation of slip rates in basis
        Dacc(1:N) = matmul(U(1:N,1:N),D(1:N))
        ! Calculation of stress, using generalised Schmid law
        do i=1,N
           j=Irp(i)
           X=Dacc(i)
           if (abs(X) < TOL) then
                X=0.0
                do k=1,N
                    X=X+A(k,j)*D(k)
                enddo
           endif
           Trp(i) = merge(Tauc(1,Irp(i)),-Tauc(2,Irp(i)),X>=0.0_dp)
        enddo
        iter=0
        do
            iter=iter+1
            if (iter > 50) then
                RCM_RAISE(1,'TBH','Too many iterations.',RCM_RTN)
            endif
            SIG(1:N) = matmul(Trp(1:N), U(1:N,:))
            ! Calculation of Taylor factor
            FakM=0.0
            do i=1,N
                FakM=FakM+SIG(i)*D(i)
            enddo
            ! Calculation of resolved shear stress
            TauR(:) = matmul(SIG(1:N), A(1:N,:))

!           Search for most severly overstressed slip system
            DT=0.0d0
            jn=0
            do j=1,M
                X=TauR(j)
                Y=merge(X-Tauc(1,j),-X-Tauc(2,j),X>=0.0_dp)
                if (abs(Y) < TOL) then
                    Y=0.0d0
                    X=merge(Tauc(1,j),-Tauc(2,j),X>=0.0_dp)
                    TauR(j)=X
                endif
                if (abs(Y-DT) < TOL) Y=DT
                DTAU(j)=Y
                if (bas(j) .or. (abs(X) < TOL) .or. (Y <= DT)) cycle
                DT=Y
                jn=j
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
                j=Irp(i)
                ZR=TauR(j)
                if (abs(ZR) < TOL) then
                    ZR=Dacc(i)
                    if (abs(Zr) < TOL) cycle
                endif
                ZR=ZR*Z1
                Z2=Dacc(i)/Z1
                if (X > 0.0d0) then
                    if (ZR < 0.0d0) cycle
                    if (in == 0) then
                        in=i
                        Gmin=Z2
                    else
                        if (Z2 < Gmin) then
                           Gmin=Z2
                           in=i
                        endif
                   endif
                else
                    if (ZR > 0.0d0) cycle
                    if (in == 0) then
                        in=i
                        Gmin=Z2
                    else
                        if (Z2 > Gmin) then
                            Gmin=Z2
                            in=i
                        endif
                    endif
                endif
            enddo
            if (in == 0) then
                RCM_RAISE(1,'TBH','The solution is unbounded',RCM_RTN)
            endif
            Z1=Aprime(in)
            do i=1,N
                CUst(i)=-Aprime(i)
            enddo
            CUst(in)=1.0d0
            do i=1,N
                CUst(i)=CUst(i)/Z1
            enddo
            UU=U
            call Ust(C=U,B=UU,Cust=CUst,in=in,N=N,M3=N,NDIM=NDIM)
            ! Updating of Dacc
            DD=Dacc
            call Ust(C=Dacc,B=DD,Cust=CUst,in=in,N=N,M3=1,NDIM=N)
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
            j=Irp(i)
            Gdot(j)=Dacc(i)
        enddo
    end subroutine

    subroutine Ust(C,B,CUst,in,N,M3,NDIM)
        !  MATRIX C=MATRIX Ustar*MATRIX B
        integer :: in,N,M3,NDIM
        real(dp) :: B(NDIM,M3),CUst(N)
        real(dp) :: C(NDIM,M3)
        integer :: i,j

        do j=1,M3
            do i=1,N
                C(i,j)=B(in,j)*CUst(i)
                if (i /= in) C(i,j)=C(i,j)+B(i,j)
            enddo
        enddo
    end subroutine
end module
