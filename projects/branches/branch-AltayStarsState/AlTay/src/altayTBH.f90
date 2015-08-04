#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
      module altayTBH
      use altayMiscutils, only: terminate, stopcode_runtimeerror
      
      contains
      
!     SUBROUTINE LINEAR PROGRAMMING TAYLOR-BISHOP-HILL STYLE
       Subroutine TBH(NDIM,N,M,A,D,                                  &
       TauC,BINV,U,IACT,Irp,Dacc,GDOT,SIG,FakM,                          &
       TauR,bas,Trp,Aprime,CUst,UU,DD,DTAU,VALID)
!    
!     Subroutine which solves Taylor-Bishop-Hill for one crystallite
!                    Stresses and strain rates are to be represented
!                    by vectors 
!     input          NDIM: number of rows in arrays, must not < N
!     input          There are M slip systems
!     input          N is the number of independent Taylor equations
!                    N is normally equal to 5, except for cluster models
!     input          A=coefficient matrix of Taylor equations
!     input          D=right-hand side of Taylor equations=imposed strain rate
!     input          TauC=critical resolved shear stresses (all Tauc>0 )
!                     first row:  for positive slip
!                     second row: for negative slip
!     input          BINV=First guess of inverse of basis, corresp. with IACT
!                    (Basis=set of columns from A corresponding
!                                    with the active slip sytems)
!     output         U=Inverse of basis, corresponding with IRP        
!     input          IACT=indices of active slip systems: first guess 
!     output         Irp=indices of active slip systems
!     output         Dacc=final slip rates, for the slip systems indexed in Irp
!     output         GDOT=slip rates
!     output         SIG=stress
!     output         FakM=plastic work  (stress*imposed strain rate) 
!     output         TauR (resolved shear stress)
!     workspace      bas (logical TRUE=belongs to basis)         
!     workspace      Trp (resolved shear stress on basis systems)
!     workspace      Aprime (column of U * A)         
!     workspace      CUst compact storage of U* (only one column)
!     workspace      UU (copy of inverse of basis)         
!     workspace      DD (copy of strain rates in some basis)
!     output         DTAU=abs(TAUR)-TAUC
!
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif
      use altayIOConfig !note: IPR (Print parameter: if <2, no printing of  results)
      implicit double precision (a-h,o-z) 
      dimension A(NDIM,M),D(NDIM),BINV(NDIM,N),U(NDIM,N),IACT(NDIM),     &
       GDOT(M),SIG(NDIM),TauC(2,M),TauR(M),Dacc(NDIM),Irp(NDIM)
      logical bas(M),valid(M)
      dimension Trp(NDIM),Aprime(NDIM),CUst(NDIM),UU(NDIM,N),DD(NDIM),   &
       DTAU(M)
      data JPR /2/,TOL/1.0d-10/
      if (N.gt.NDIM) then
#ifndef ALTAY_SUBROUTINE
                       write (*,100) N,NDIM
                      call terminate(stopcode_runtimeerror)
#else
      RCM_RAISE(1,'TBH','Bad input: N>NDIM',RCM_RTN)
#endif      
                     endif
 100  format (' Subroutine TBH - N=',I5,'  is > NDIM=',I5,               &
       '   this is an error in the calling program')
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
!     Calculation of slip rates in basis
      call mtprd(Dacc,U,D,N,N,1,NDIM,NDIM)
!     Calculation of stress, using generalised Schmid law
      do i=1,N
         j=Irp(i)
         XX=Dacc(i)
         X=XX
         if (abs(XX).lt.TOL) then
                              X=0.0
                              do k=1,N
                                X=X+A(k,j)*D(k)
                              enddo
         endif
          if (X.ge.0.0d0) then
                                Trp(i)=Tauc(1,j)
                               else 
                                Trp(i)=-Tauc(2,j)
                               endif
      enddo
      iter=0
    4 iter=iter+1
      if (iter.le.50) goto 7
#ifndef ALTAY_SUBROUTINE
      if(NLIST.eq.1) then
      write (IMP,250)  
      end if
      write (*,250) 
 250  format (' TBH is looping')
      call terminate(stopcode_runtimeerror)
#else
      RCM_RAISE(1,'TBH','Too many iterations.',RCM_RTN)
#endif
      
    7 if (IPR.GE.JPR) then
      if (NLIST.eq.1) then
      write (IMP,251) iter
      end if
      end if
 251  format (/,'  ITERATION NR. ',I5,/)
      call mtprd(SIG,Trp,U,1,N,N,1,NDIM) 
      do j=1,M
         valid(j)=.TRUE.
      enddo
!     Calculation of Taylor factor
      FakM=0.0
      do i=1,N
         FakM=FakM+SIG(i)*D(i)
      enddo
!     Calculation of resolved shear stress
      call mtprd(TauR,SIG,A,1,N,M,1,NDIM)
      if (IPR.GE.JPR) then
        if (NLIST.eq.1) then
        write (IMP,205)
        do i=1,N
          write (IMP,204) D(i),SIG(i)
        enddo
        write (IMP,201) FakM
        do j=1,M
          write (IMP,202) j,TauR(j)
        enddo
        end if
      endif
  201 format (' M-Factor:',D20.10,/,' Resolved shear stresses:')
  202 format (I5,30X,D20.10)
  204 format (D20.10,5X,D20.10)
  205 format (//,'***************************************************',  &
       /,'   Strain                   Stress')
!     Search for most severly overstressed slip system
    6 DT=0.0d0
      jn=0
      do 1 j=1,M
        X=TauR(j)
        if (X.ge.0.0d0) then
                         Y=X-Tauc(1,j)
                        else
                         Y=-X-Tauc(2,j)
                        endif
        if (IPR.GE.JPR) then
        if (NLIST.eq.1) then
        write (IMP,919) j,jn,X,Y,Y-DT
        end if
        end if
  919 format ('j=',I5,'  jn=',I5,'  TauR(=X)',D15.5,' DTAU(=Y)',D20.10,  &
       ' Y-DT=',D20.10)
        if (abs(Y).lt.TOL) then
                             Y=0.0d0
                             if (X.ge.0.0d0) then
                                               X=Tauc(1,j)
                                             else
                                               X=-Tauc(2,j)
                             endif
                             TauR(j)=X
                           endif
        if (abs(Y-DT).lt.TOL) Y=DT
        DTAU(j)=Y
        if (bas(j)) goto 1   
        if (abs(X).lt.TOL) goto 1 
        if (Y.le.DT) goto 1
!        if (.NOT.valid(j)) goto 1
!        if (IPR.GE.JPR) write(IMP,917) j,VALID(j)
! 917    format (I5,' valid',L8)
        if (IPR.GE.JPR) then
        if (NLIST.eq.1) then
        write (IMP,920) Y,j
        end if
        end if
  920   format (' DT(=Y)',D15.5,'  New jn=',I5)
        DT=Y
        jn=j
    1 continue
      if (IPR.GE.JPR.and.jn.gt.0) then
      if (NLIST.eq.1) then
      write (IMP,913) jn,DT,TauR(jn)
      end if
      end if
  913 format ('Overstressed:jn DT',I5, D15.5,'   TauR(jn)',D20.10)  
!      if (jn.eq.0.and.DT.gt.0.0D0) then
!                                      write (IMP,910)
!                                      write (*,910)
!                                      stop
!                                    endif 
!  910 format (' TBH - There are invalid slip sytems ',
!     1'which are overstressed')
      if (jn.eq.0) goto 2 ! There is no overstressed slip system  
!     There is an overstressed slip system, which we will activate now
!     Search which active slip system must be desactivated (removed from basis)
      X=TauR(jn)
!     Calculate column Mprime-s*, called Aprime
      call mtprd(Aprime,U,A(1,jn),N,N,1,NDIM,NDIM)
      in=0
      if (IPR.GE.JPR) then
      if (NLIST.eq.1) then
      write (IMP,203)
      end if
      end if
  203 format (' ACTIVE',9x,'Slip rate',11X,                              &
       'Critical Resolved shear stress',11X,'Aprime')
      do 3 i=1,N
        if (IPR.GE.JPR) then
        if (NLIST.eq.1) then
        write (IMP,200)Irp(i),DACC(i),Trp(i),Aprime(i)
       end if
       end if
  200 format (I5,5x,D26.16,5x,D20.10,5x,D20.10)
        Z1=Aprime(i)
        if (abs(Z1).lt.TOL) goto 3  
        j=Irp(i)
        ZR=TauR(j)
        if (abs(ZR).lt.TOL) then
                             ZR=Dacc(i)
                             if (abs(Zr).lt.TOL) goto 3
                            endif
        ZR=ZR*Z1 
        Z2=Dacc(i)/Z1 
        if (X.gt.0.0d0) then
                         if (ZR.lt.0.0d0) goto 3
                         if (in.eq.0) then
                                        in=i
                                        Gmin=Z2
                                       else
                                        if (Z2.lt.Gmin) then
                                                         Gmin=Z2
                                                         in=i
                                                        endif
                                       endif
                        else 
                         if (ZR.gt.0.0d0) goto 3 
                         if (in.eq.0) then
                                        in=i
                                        Gmin=Z2
                                       else
                                        if (Z2.gt.Gmin) then
                                                         Gmin=Z2
                                                         in=i
                                                        endif
                                       endif
                        endif
    3 continue
      if (in.eq.0) then
#ifndef ALTAY_SUBROUTINE
                     write (*,101) 
                     call terminate(stopcode_runtimeerror)
#else
      RCM_RAISE(1,'TBH','The solution is unbounded',RCM_RTN)
#endif      
                   endif
  101 FORMAT (' Subroutine TBH - solution unbounded') 
      if (IPR.GE.JPR) then
      if (NLIST.eq.1) then
      write (IMP,912) in,jn,Gmin
      end if
      end if
  912 format ('in jn Gmin',2I5, D15.5)
      if (abs(Gmin).gt.0.0D0) goto 5
!      valid(jn)=.FALSE.
!      goto 6  
!     Updating of inverse of basis: U 
    5 Z1=Aprime(in)
      do i=1,N
        CUst(i)=-Aprime(i)
      enddo
      CUst(in)=1.0d0
      do i=1,N
        CUst(i)=CUst(i)/Z1
      enddo  
      UU=U
      call Ust(U,UU,CUst,in,N,N,NDIM)
!     Updating of Dacc 
      DD=Dacc
      call Ust(Dacc,DD,CUst,in,N,1,N)
      if (IPR.GE.JPR) then
      if (NLIST.eq.1) then
      write (IMP,929) in,Gmin,Dacc(in)
      end if
      end if
  929 format ('updated slip rate in',I5,2D15.5)
!     Updating of basis: bas and Irp
      bas(Irp(in))=.FALSE.
      bas(jn)=.TRUE.
      Irp(in)=jn 
      if ((Dacc(in).ge.0.0d0).and.(X.gt.0.0d0)) then
                                Trp(in)=Tauc(1,jn)
                             else 
                                Trp(in)=-Tauc(2,jn)
                             endif
!     Go back to stress calculation
      goto 4
!     Solution was found.
    2 do j=1,M
        Gdot(j)=0.0
      enddo
      if (IPR.GE.JPR) then
      if (NLIST.eq.1) then
      write (IMP,212)
      end if
      end if
!
      do i=1,N
        j=Irp(i)
        Gdot(j)=Dacc(i)
         if (IPR.GE.JPR) then
         if (NLIST.eq.1) then
         write (IMP,211) j,DACC(i)
         end if
         end if
      enddo
  211 format (I5,5x,D20.10)
  212 format (/,'   SOLUTION ',/)
      return
      end subroutine
      
      
      
      SUBROUTINE Ust(C,B,CUst,in,N,M3,NDIM)
!     MATRIX C=MATRIX Ustar*MATRIX B                                        
      implicit double precision (a-h,o-z)
      DIMENSION B(NDIM,M3),C(NDIM,M3),CUst(N)
      do j=1,M3
        X=B(in,j)
        do i=1,N
          C(i,j)=X*CUst(i)
          if (i.ne.in) C(i,j)=C(i,j)+B(i,j)
        enddo
      enddo
      return
      end subroutine
      
      
      
      SUBROUTINE mtprd(C,A,B,N1,N2,N3,ND1,ND2)
!     MATRIX C=MATRIX A*MATRIX B                                        
      implicit double precision (a-h,o-z)
      DIMENSION A(ND1,N2),B(ND2,N3),C(ND1,N3)
      DO 1 I=1,N1                                                       
      DO 2 J=1,N3                                                       
      X=0.                                                              
      DO 3 K=1,N2                                                       
      X=X+A(I,K)*B(K,J)                                                 
 3    CONTINUE                                                          
      C(I,J)=X                                                          
 2    CONTINUE                                                          
 1    CONTINUE                                                          
      RETURN                                                            
      END SUBROUTINE

      end module
      

      
     
     
     
