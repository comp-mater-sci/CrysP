#include "altayRCM.fpp"
module altayTBH
    use altay_definitions
    use altayMiscutils, only: terminate, stopcode_runtimeerror
    use altayRCM

    implicit none
    contains

!   solve Taylor-Bishop-Hill for one crystallite. Stresses and strain rates are to be represented  by vectors
    subroutine TBH(IPR,NDIM,N,M,A,D,TauC,BINV,U,IACT,Irp,Dacc,GDOT,SIG,FakM,TauR,bas,CUst,DD,DTAU)
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
!     workspace      CUst compact storage of U* (only one column)
!     workspace      DD (copy of strain rates in some basis)
!     output         DTAU=abs(TAUR)-TAUC
        use altayIOConfig,IIPR=>IPR!Rename global IPR switch to avoid conflict

        integer, intent(in) :: IPR,&   !< Print parameter: if <2, no printing of  results)
                               NDIM,&  !< number of rows in arrays, must not < N
                               M,&     !< There are M slip systems
                               N,&     !< # of independent Taylor equations,N=5, except for cluster models
                               IACT(NDIM) ! < indices of active slip systems: first guess
        real(dp), intent(in) :: A(NDIM,M), &  !< coefficient matrix of Taylor equations
                                D(NDIM), &    !< right-hand side of Taylor equations=imposed strain rate
                                BINV(NDIM,N), &
                                TauC(2,M)
        logical :: bas(M)

        real(dp), intent(out) :: U(NDIM,N), Dacc(NDIM),GDOT(M),SIG(NDIM),TauR(M), DTAU(M),FakM
        integer, intent(out) :: Irp(NDIM) !< indices of active slip systems
        real(dp), intent(inout) :: CUst(NDIM),DD(NDIM)

        real(dp) :: Aprime(NDIM), & !< column of U * A
                    Trp(NDIM), &    !< resolved shear stress on basis systems
                    UU(NDIM,N)      !< copy of inverse of basis
        integer, parameter :: JPR=2
        real(dp), parameter :: TOL=1.0e-10_dp

        integer :: i,j,k,iter,jn,in
        real(dp) :: x,dt,y,z1,z2,zr,gmin,xx

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
        call mtprd(Dacc,U,D,N,N,1,NDIM,NDIM)
        ! Calculation of stress, using generalised Schmid law
        do i=1,N
           j=Irp(i)
           XX=Dacc(i)
           X=XX
           if (abs(XX) < TOL) then
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
            if (IPR >= JPR .and. NLIST == 1) write (IMP,251) iter
 251        format (/,'  ITERATION NR. ',I5,/)
            call mtprd(SIG,Trp,U,1,N,N,1,NDIM)
            ! Calculation of Taylor factor
            FakM=0.0
            do i=1,N
                FakM=FakM+SIG(i)*D(i)
            enddo
            ! Calculation of resolved shear stress
            call mtprd(TauR,SIG,A,1,N,M,1,NDIM)
            if (IPR >= JPR .and. NLIST == 1) then
                write (IMP,205)
                do i=1,N
                    write (IMP,204) D(i),SIG(i)
                enddo
                write (IMP,201) FakM
                do j=1,M
                    write (IMP,202) j,TauR(j)
                enddo
            endif
  201       format (' M-Factor:',D20.10,/,' Resolved shear stresses:')
  202       format (I5,30X,D20.10)
  204       format (D20.10,5X,D20.10)
  205       format (//,'***************************************************', /,'   Strain                   Stress')
!           Search for most severly overstressed slip system
    6       DT=0.0d0
            jn=0
            do j=1,M
                X=TauR(j)
                Y=merge(X-Tauc(1,j),-X-Tauc(2,j),X>=0.0_dp)
                if (IPR >= JPR .and. NLIST == 1) write (IMP,919) j,jn,X,Y,Y-DT
  919           format ('j=',I5,'  jn=',I5,'  TauR(=X)',D15.5,' DTAU(=Y)',D20.10,' Y-DT=',D20.10)
                if (abs(Y) < TOL) then
                    Y=0.0d0
                    X=merge(Tauc(1,j),-Tauc(2,j),X>=0.0_dp)
                    TauR(j)=X
                endif
                if (abs(Y-DT) < TOL) Y=DT
                DTAU(j)=Y
                if (bas(j) .or. (abs(X) < TOL) .or. (Y <= DT)) cycle
                if (IPR >= JPR .and. NLIST == 1) write (IMP,920) Y,j
  920           format (' DT(=Y)',D15.5,'  New jn=',I5)
                DT=Y
                jn=j
            enddo
            if (IPR >= JPR.and.jn > 0 .and. NLIST == 1) write (IMP,913) jn,DT,TauR(jn)
  913       format ('Overstressed:jn DT',I5, D15.5,'   TauR(jn)',D20.10)
            if (jn == 0) exit ! There is no overstressed slip system
            ! There is an overstressed slip system, which we will activate now
            ! Search which active slip system must be desactivated (removed from basis)
            X=TauR(jn)
            ! Calculate column Mprime-s*, called Aprime
            call mtprd(Aprime,U,A(1,jn),N,N,1,NDIM,NDIM)
            in=0
            if (IPR >= JPR .and.  NLIST == 1)write (IMP,203)
  203       format (' ACTIVE',9x,'Slip rate',11X,'Critical Resolved shear stress',11X,'Aprime')
            do i=1,N
                if (IPR >= JPR .and. NLIST == 1) write (IMP,200)Irp(i),DACC(i),Trp(i),Aprime(i)
  200           format (I5,5x,D26.16,5x,D20.10,5x,D20.10)
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
  101       format (' Subroutine TBH - solution unbounded')
            if (IPR >= JPR .and. NLIST == 1) write (IMP,912) in,jn,Gmin
  912       format ('in jn Gmin',2I5, D15.5)
            Z1=Aprime(in)
            do i=1,N
                CUst(i)=-Aprime(i)
            enddo
            CUst(in)=1.0d0
            do i=1,N
                CUst(i)=CUst(i)/Z1
            enddo
            UU=U
            call Ust(U,UU,CUst,in,N,N,NDIM)
            ! Updating of Dacc
            DD=Dacc
            call Ust(Dacc,DD,CUst,in,N,1,N)
            if (IPR >= JPR .and. NLIST == 1) write (IMP,929) in,Gmin,Dacc(in)
  929       format ('updated slip rate in',I5,2D15.5)
            ! Updating of basis: bas and Irp
            bas(Irp(in))=.FALSE.
            bas(jn)=.TRUE.
            Irp(in)=jn
            Trp(in) = merge(Tauc(1,jn),-Tauc(2,jn),(Dacc(in) >= 0.0d0).and.(X > 0.0d0))
            ! Go back to stress calculation
        enddo
        ! Solution was found.
        Gdot(1:M)=0.0
        if (IPR >= JPR .and. NLIST == 1) write (IMP,212)
        do i=1,N
            j=Irp(i)
            Gdot(j)=Dacc(i)
            if (IPR >= JPR .and. NLIST == 1) write (IMP,211) j,DACC(i)
        enddo
  211   format (I5,5x,D20.10)
  212   format (/,'   SOLUTION ',/)
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


    subroutine mtprd(C,A,B,N1,N2,N3,ND1,ND2)
        ! MATRIX C=MATRIX A*MATRIX B

        integer :: N1,N2,N3,ND1,ND2
        real(dp) :: A(ND1,N2),B(ND2,N3)
        real(dp) :: C(ND1,N3)
        real(dp) :: X
        integer :: I,J,K

        do I=1,N1
            do J=1,N3
                X=0.
                do K=1,N2
                    X=X+A(I,K)*B(K,J)
                enddo
                C(I,J)=X
            enddo
        enddo
    end subroutine

end module
