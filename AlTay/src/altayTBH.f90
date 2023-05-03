#include "altayRCM.fpp"
module altayTBH
    use definitions
    use altayRCM

    implicit none
    contains

!   solve Taylor-Bishop-Hill for one crystallite. Stresses and strain rates are to be represented  by vectors
    subroutine TBH(A,D,TauC,BINV,U,IACT,Irp,GDOT,SIG,TauR,DTAU)
!     input          TauC=critical resolved shear stresses (all Tauc>0 )
!                     first row:  for positive slip, second row: for negative slip
!     input          BINV=First guess of inverse of basis, corresp. with IACT
!                    (Basis=set of columns from A corresponding with the active slip sytems)
!     output         U=Inverse of basis, corresponding with IRP
!     output         GDOT=slip rates
!     output         SIG=stress
!     output         TauR (resolved shear stress)
!     output         DTAU=abs(TAUR)-TAUC

        real(dp), intent(in) :: A(:,:)  !< coefficient matrix of Taylor equations, second dim is #Nslip
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
        real(dp), parameter :: TOL=1.0e-10_dp

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
        iter=0
        do
            iter=iter+1
            if (iter > 50) then
                RCM_RAISE(1,'TBH','Too many iterations.',RCM_RTN)
            endif
            SIG = matmul(Trp, U)
            ! Calculation of resolved shear stress
            TauR = matmul(SIG, A)

!           Search for most severly overstressed slip system
            DT=0.0d0
            jn=0
            do i=1,size(A,2)
                DTAU(i)=merge(TauR(i)-Tauc(1,i),-TauR(i)-Tauc(2,i),TauR(i)>=0.0_dp)
                if (abs(DTAU(i)) < TOL) then
                    DTAU(i)=0.0d0
                    TauR(i)=merge(Tauc(1,i),-Tauc(2,i),TauR(i)>=0.0_dp)
                endif
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
            UU=U
            call Ust(C=U,B=UU,Cust=CUst,in=in,N=size(A,1),M3=size(A,1),NDIM=size(A,1))
            ! Updating of Dacc
            DD=Dacc
            call Ust(C=Dacc,B=DD,Cust=CUst,in=in,N=size(A,1),M3=1,NDIM=size(A,1))
            ! Updating of basis: bas and Irp
            bas(Irp(in))=.FALSE.
            bas(jn)=.TRUE.
            Irp(in)=jn
            Trp(in) = merge(Tauc(1,jn),-Tauc(2,jn),(Dacc(in) >= 0.0d0).and.(TauR(jn) > 0.0d0))
            ! Go back to stress calculation
        enddo
        ! Solution was found.
        Gdot=0.0
        Gdot(Irp)=Dacc
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
