#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
      module altayPancake
      use altayMiscutils, only: terminate, stopcode_runtimeerror
      
      contains
      
C MODIFICATIONS AUG 2010
C THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
C WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY
C
      Subroutine Pancak2(KOST,NGL,B,DI1,S33,RHOS33,RHOA33,
     1 SWRLX,BBVM,XX,IPR,GEWF)
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif      
      use altayMesostructure
      use altayIOConfig,IIPR=>IPR !Rename the global IPR to avoid conflict
      use altayHard
      use altayHardTypes
      use altayTBH
      use altayAlgorithms
      use altayMacroKinematic, only: MaKi_VelGrad,
     &                               MaKi_vMeqStrainRate
#ifdef PEBP_ENABLED
      use AltayDSHstate
#endif
      implicit double precision (a-h,o-z)
      COMMON /LAMEL/ laml,fi10b(2),phi0b(2),fi20b(2),TRFb(3,3,2),
     1 gewfb(2),GMMAb(2),Fb(3,3,2),GAXESb(3,2),GEULRb(3,2),
     2 CIJb(3,3,2),TGb(3,3,2),RHOSSb(3,3,2),
     3 fi1b(2),phib(2),fi2b(2),
     4 NGR,NRL,ENTA,ITFMAS
      COMMON /IGLIJS/ M11,CC(2,96)
      COMMON /DOUBLE/ A8(5,96),BB8(5),RHO(5),B5(5)
      common /extra/ A1(10,194),UU(10,10)
      common /CEIGEN/ IOR,ISTP,NBLOC
      COMMON /ACTIVE/ NACTIV,INDACT(8),NLP,INDLP(8),SLIPLP(8),
     1 TLXX,TAURLP(8)
      double precision,dimension(3,3),intent(out):: S33, RHOS33, RHOA33 
      double precision,dimension(5):: RHOS, RHOA 
        dimension ccc2(2,194)
      dimension buftrf(3,3),C1(3,3),C2(3,3),
     1 TDCb(3,3,2),TRCb(3,3,2),
     2 B(5,5),BBVM2(2),relax(3,3,3),buftg(3,3),DACC(10),
     3 rls(3,3,3,2),rla(3,3),rlm(3,3,3),C3(3,3),TRP(10),APRIME(10),
     4 B3(10,3),PLUMIN(2,3),CUst(10)
C     first index op PLUMIN = nr. of grain
C     second index = nr. of relaxation
      dimension spanv(5),XX(194),STRSS(10),BB(10)
      dimension CCC(2,194),DTAU(194),DTAU1(194),TAUR(194),TAUR1(194)
      type (CRSS) :: CRSSmatrix
      logical SWRLX(3),bas(194),VALID(194)
C     rlm is unit relaxation tensor in macroscopic frame
C     rls and rla in crystal frame (symmetric and anti-sym. part)
      dimension B8(5,2),UBUF(10),UU2(10,10),UU3(10,10),DD(10)
      integer DI1(5),DI(10),DI2(10)
      dimension GAMR(2),Tprinc(3,3),TAURL(2)
      data SQR2/0.7071067811865476D+00/,B3/30*0.0D0/,TOLXX/5.0d-6/
C     Definition of the two relaxations, representing a
C     13-simple shear and a 23-simple shear, respectively:
      data relax /0.0D0, 0.0D0, 0.0D0,
     1            0.0D0, 0.0D0, 0.0D0,
     2            1.0D0, 0.0D0, 0.0D0,
     3            0.0D0, 0.0D0, 0.0D0,
     4            0.0D0, 0.0D0, 0.0D0,
     5            0.0D0, 1.0D0, 0.0D0,
     6            0.0D0, 0.0D0, 0.0D0,
     7            0.0D0, 0.0D0, 0.0D0,
     6            0.0D0, 0.0D0, 0.0D0/
      data PLUMIN/1.0D0,-1.0D0,
     1            1.0D0,-1.0D0,
     2            1.0D0, 1.0D0/
      data NDIM/10/
C     NDIM=dimension A 
C     NRL= number of relaxations    NGR= number of grains
      data TAURL/2*0.0d0/
      data GETAL/1.0D6/,TOL/1.0d-6/
#ifdef PEBP_ENABLED      
      integer :: info
#endif
      SAVE

      if (laml.ne.1.and.laml.ne.2) then
#ifndef ALTAY_SUBROUTINE
      write(*,*) 'laml=', laml
      call terminate(stopcode_runtimeerror)
#else
      RCM_RAISE(1,'Pancak2','Wrong selection of lamels',RCM_RTN)
#endif      
      endif
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if (IOR.eq.1) IGrElm=0 
      TWOSQ3=sqrt(2.D0/3.D0)
C     N is number of rows of A1;   NU number of rows of UU2
      TLXX=TOLXX 
      N=5*NGR
      NU=N
      M2=NGR*M11
      M12=NGR*M11+NRL
      if (laml.eq.2) goto 3
C
C     Updating of microstructure
C
      IGrElm=IGrElm+1
      if (IGrElm.gt.NGrElm) IGrElm=1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  QGX
      
      call cluster1(NGR,IGrElm,GEWF,Tprinc,Cofcos,Cofsin)
      


CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      do 33 i=M2+1,M12
      do 33 jsgn=1,2
  33  CCC(jsgn,i)=0.0
  32  do 31 i=1,NU
      do 31 j=1,NU
      UU(j,i)=0.0
  31  continue
      DO 53 I=1,5
      DI(I)=DI1(I)
      DI(I+5)=DI1(I)+M11
  53  CONTINUE                                                          
      do 1 IL=1,NGR
      L1=5*(IL-1)
C     OMREKENING DISPLACEMENT GRADIENT.
      do 45 i=1,3
      do 45 j=1,3
      buftrf(i,j)=TRFb(j,i,IL)
      buftg(i,j)=Tprinc(j,i)
  45  continue
      CALL MATPROD(C1,MaKi_VelGrad,buftrf,3,3,3)
      CALL MATPROD(C2,TRFb(1,1,IL),C1,3,3,3)
      if (NRL.eq.0) goto 87
      do 82 IRL=1,NRL
C     Transform relaxation from grain reference frame to macroscopic frame
      CALL MATPROD(C1,RELAX(1,1,IRL),Tprinc,3,3,3)
      CALL MATPROD(RLM(1,1,IRL),buftg,C1,3,3,3)
C     ... and now to crystal frame:
      CALL MATPROD(C1,RLM(1,1,IRL),buftrf,3,3,3)
      CALL MATPROD(C3,TRFb(1,1,IL),C1,3,3,3)
      do 83 j=1,3
      do 83 i=1,3
      RLS(i,j,IRL,IL)=(C3(I,J)+C3(J,I))*0.5D0
      RLA(i,j)=(C3(I,J)-C3(J,I))*0.5D0
  83  continue
      B3(L1+1,IRL)=PLUMIN(IL,IRL)*RLA(2,3)/sqr2
      B3(L1+2,IRL)=PLUMIN(IL,IRL)*RLA(3,1)/sqr2
      B3(L1+3,IRL)=PLUMIN(IL,IRL)*RLA(1,2)/sqr2
      B5= Vector5D(RLS(1:3,1:3,IRL,IL)) ! sym.(3,3) -> (5)
C     Insert the relaxations as columns in A1-matrix
      j=M2+IRL
      do 84 i=1,5
      i1=i+L1
      x=B5(i)*PLUMIN(IL,IRL)
      A1(i1,j)=x
  84  continue
  82  continue 
  87  continue
      DO 80 I=1,3
      DO 81 J=1,3                                                       
      TDCb(I,J,IL)=(C2(I,J)+C2(J,I))*0.5D0
  81  TRCb(I,J,IL)=(C2(I,J)-C2(J,I))*0.5D0
  80  CONTINUE                                                          
      B5= Vector5D(TDCb(1:3,1:3,IL)) ! sym.(3,3) -> (5)
      do 30 i=1,5
      j=i+L1
      BB(j)=B5(i)
  30  continue
C
C     Calculation of time increment by dividing von Mises equivalent
C     strain by von Mises equivalent strain rate
C
      do 44 j=1,5
      B5(j)=B5(j)/MaKi_vMeqStrainRate
      B8(j,IL)=B5(j)
  44  continue
      BBVM2(IL)=MaKi_vMeqStrainRate
      K1=M11*(IL-1)
      !
      ! Retrieve the CRSSmatrix
      !    IOR+IL-1  = sequence number of current grain 
      !    GMMAb(IL) = the GAMMA of current grain
      call getCRSS(IOR+IL-1,GMMAb(IL),CRSSmatrix,info) 
      !
      ! Assign CRSSmatrix to proper section of CCC
      CCC(:,1+K1:M11+K1)=CRSSmatrix%crss(:,1:M11)  
      !
      ! Set Tau_crit for antitwinning direction equal to
      ! GETAL times Tau_crit for twinning direction       
      do I=NGL+1,M11 ! this do-loop will only be executed for NTW>0
          CCC(2,I+K1)=CCC(1,I+K1)*GETAL
      end do
      !
C   92 write (IMP,914) i,j,CCC(1,j),CCC(2,j)
 914  format (' i,j',2i5, ' CCC ',2d16.4)
      DO 15 J=1,5
      DO 15 I=1,5
      UU(I+L1,J+L1)=B(I,J)
  15  continue
   1  continue 
      DO 54 I=1,N 
C     Conversion of strain to normalized strain rate
      BB(I)=BB(I)/BBVM2(1)
  54  CONTINUE
      if (NRL.eq.0) goto 88
      do 85 j=M2+1,M12             
C    The coefficient of the relaxations is set to a very large number
C    in order to suppress the relaxations in a first call of the TBH program     
      CCC(1,j)=GETAL  
      CCC(2,j)=GETAL 
  85  continue
C     Full constraints calculation
C
C     UITVOEREN VAN DE SIMPLEX-SUBROUTINE
  88  IF (IPR.EQ.2) then
      if(NLIST.eq.1) then 
      WRITE (IMP,218) ((CCC(J,I),I=1,M12),J=1,2)
      end if
      end if
 218  FORMAT(/' COST FUNCTION',/,(2x,12F10.4))
      IF (IPR.EQ.2) then
      if (NLIST.eq.1) then
      WRITE (IMP,219) (BB(I),I=1,N)
      end if
      end if
 219  format (' right hand side',/,(2x,10F10.4),/)
C     First call of Simplex (full constraints)
      if (IPR.eq.2) then
      if(NLIST.eq.1) then
      write (IMP,400) IOR,ISTP,NBLOC
      end if
      end if
 400  format (' First call of TBH   IOR,ISTP,NBLOC',3I5)
      Call TBH(IPR,NDIM,N,M2,A1,BB,
     1 CCC,UU,UU2,DI,DI2,Dacc,XX,UBUF,FakM,
     2 Taur,bas,Trp,Aprime,CUst,UU3,DD,DTAU,VALID) 
C CCC (input): critical resolved shear stresses (Tauc)
C UU (input): initial inverse of "basis" = columns of A1
C      corresponding to thoses slip systems which are active
C      according to first guess 
C UU2 (output): inverse of final "basis" (active slip systems)
C DI (input): indices of basis corresponding to UU
C DI2 (output): indices of basis corresponding to UU2
C Dacc (output): slip rates in basis DI
C XX (output): slip rates (numbered from 1 to M12)
C UBUF (output): stresses, in crystal frames
C                  (2 sets of stresses, one for each crystal)
C Fakm: rate of plastic work of the 2 crsytals together
C Taur (output) resolved shear stress (can be + or -)        
C DTAU (output)=abs(Taur)-Tauc 
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif
C
  345 if (IPR.lt.4) goto 220
#ifndef ALTAY_SUBROUTINE
      if(NLIST.eq.1) then
      write (IMP,221) IPR,IOR,ISTP,NBLOC
      end if
      write (*,221) IPR,IOR,ISTP,NBLOC
 221  format (' Pancak2 ',
     1 ' IPR IOR, ISTP, NBLOC=',4I5)
      if (IPR.ge.4) call terminate(stopcode_runtimeerror) 
#else
      RCM_RAISE(1,'Pancak2','IPR must be < 4',RCM_RTN)
#endif
  220    DTAU1=DTAU 
         TAUR1=TAUR  

      if (NRL.eq.0) then
                      UU=UU2
                      DI=DI2
                      STRSS=UBUF
                      goto 89
                    endif
C@$@$@$@$@$@$@$@$@$@$@$@$@$@$@$@$@$@$@$@$@$$@$@$@$@$@$@$@$@$@$ QGX 15/11/2012 
      IF(ITFMAS.eq.1) then
        CCC(1,M2+1)=GETAL
        CCC(2,M2+1)=GETAL
        CCC(1,M2+2)=0.0
        CCC(2,M2+2)=0.0
      else  !ALAMEL running
        do 86 IRL=1,NRL
        if (.not.swrlx(IRL)) goto 86
        j=M2+IRL  
        CCC(1,j)=TAURL(IRL)
        CCC(2,j)=TAURL(IRL)
  86    continue 
      endif
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      IF (IPR.EQ.2) then
      if(NLIST.eq.1) then 
      WRITE (IMP,218) ((CCC(J,I),I=1,M12),J=1,2)
      end if
      end if
C     Second call of Simplex (relaxed constraints)
C      if (IOR.eq.1967.and.ISTP.eq.11.and.NBLOC.eq.3) IPR=2
      if (IPR.eq.2) then
      if (NLIST.eq.1) then
      write (IMP,401)
      end if
      end if
 401  format (' Second call of TBH')
      Call TBH(IPR,N,N,M12,A1,BB,
     1 CCC,UU2,UU,DI2,DI,Dacc,XX,STRSS,FakM,
     2 Taur,bas,Trp,Aprime,CUst,UU3,DD,DTAU,VALID)
C CCC (input): critical resolved shear stresses (Tauc)
C UU2 (input): initial inverse of "basis" = columns of A1
C      corresponding to thoses slip systems which are active
C      according to first guess 
C UU (output): inverse of final "basis" (active slip systems)
C DI2 (input): indices of basis corresponding to UU2
C DI (output): indices of basis corresponding to UU
C Dacc (output): slip rates in basis DI
C XX (output): slip rates (numbered from 1 to M12)
C STRSS (output): stresses, in crystal frames
C                  (2 sets of stresses, one for each crystal)
C Fakm: rate of plastic work of the 2 crsytals together
C Taur (output) resolved shear stress (can be + or -)        
C DTAU (output)=abs(Taur)-Tauc 
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif
C      if (IOR.eq.1967.and.ISTP.eq.11.and.NBLOC.eq.3) stop
C


      if (IPR.ge.4) then
         if(NLIST.eq.1) then
         write (IMP,222) IPR,IOR,ISTP,NBLOC
         end if
         write (*,222) IPR,IOR,ISTP,NBLOC
 222     format (' Pancak2 222 - Problem with TBH',/,
     1   ' IPR IOR, ISTP, NBLOC=',4I5)
#ifndef ALTAY_SUBROUTINE
          call terminate(stopcode_runtimeerror)  
#else
          RCM_RAISE(1,'Pancak2','Problem with TBH',RCM_RTN)

#endif         
      endif
C@#@#@#@#@#@#@#@#@@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@@#@#@#@#@#@#@@#@# QGX 15/11/2012
C   loop
c   
      IF(ITFMAS.eq.1) then 
      iter=0
  999 iter=iter+1
      call Fakeccc(ccc,ccc2,Cofcos,Cofsin,BB,STRSS,M11,ca1,ca2)
      ccc=ccc2 ! use the Fake CRSS, they are scaled by SDD model
c
      CCC(1,M2+1)=dabs(ENTA*Cofcos*(ca1+ca2)/2.D0)
      CCC(2,M2+1)=dabs(ENTA*Cofcos*(ca1+ca2)/2.D0)
C  Third call of TBH
      Call TBH(IPR,N,N,M12,A1,BB,
     1 CCC,UU,UU2,DI,DI2,Dacc,XX,STRSS,FakM,
     2 Taur,bas,Trp,Aprime,CUst,UU3,DD,DTAU,VALID)
C output for current iteration must be the input for the next iteration
      UU=UU2
      DI=DI2
c
      if(iter.le.0) then
      goto 999
      else
      endif 
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
      UU=UU2
      DI=DI2
      else
      goto 204
      endif
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
C     GAMR will contain the relaxed shears:
 204  if (NRL.gt.0) then
                       do IRL=1,NRL
                         gamr(IRL)=XX(M2+IRL)
                       enddo
                    endif
C     Check whether 1 grain does not deform at all.
  89  j=0
      do 40 IG=1,NGR
      XXTOT=0.0
      do i=1,M11
       j=j+1  
       XXTOT=XXTOT+ABS(xx(j))
      enddo
      if (XXTOT.lt.TOLXX) goto 213
   40 continue
C     If all grains have a non-zero slip, do the following:
  99  DTAU1=DTAU
      TAUR1=TAUR
      UBUF=STRSS
 213  if(NLIST.eq.1) then 
        write (IMP,780) gamr
      end if
 780  format (' RELAXATIONS:                   ',2d12.4)      
   2  continue
C
C     From here on, output is produced for grain number "laml"
C
   3  BBVM=BBVM2(laml)
      jj=M11*(laml-1)
      do 203 j=1,M11
      do jsgn=1,2
         CC(jsgn,j)=CCC(jsgn,j+jj)
      enddo
 203  continue    
      ii=5*(laml-1)
      do 201 i=1,5
C     If one grain does not deform, note that stress UBUF has come
C      from the fullconstraints solution.
      spanv(i)=UBUF(i+ii)
      B5(i)=B8(i,laml)
C      write (IMP,776) laml,B5(i),spanv(i),i+ii
C 776  format (' B5  ',i5,e15.8,   'spanv  ',d15.8,' i+ii',i5)
      x8=0.0
      y8=0.0
      if (NRL.gt.0) then
                     do IRL=1,NRL
                       x8=x8+A1(i+ii,M2+IRL)*gamr(IRL)
                       y8=y8+B3(i+ii,IRL)*gamr(IRL)
                     enddo
                    endif
      BB8(i)=B8(i,laml)-x8
      RHOS(i)=-x8
      RHOA(i)=-y8
 201  continue
      S33=    SymMatrix(spanv) ! (5) -> sym.(3,3)
      RHOS33= SymMatrix(RHOS)  ! (5) -> sym.(3,3) 
      !Conversion of RHOA to dim(3,3)
      RHOA33=0.0d0
      RHOA33(2,3)= RHOA(1)*sqr2*MaKi_vMeqStrainRate
      RHOA33(3,1)= RHOA(2)*sqr2*MaKi_vMeqStrainRate
      RHOA33(1,2)= RHOA(3)*sqr2*MaKi_vMeqStrainRate
      RHOA33(3,2)= -RHOA33(2,3)
      RHOA33(1,3)= -RHOA33(3,1)
      RHOA33(2,1)= -RHOA33(1,2)
C
      if (IPR.EQ.2 .AND. NLIST.eq.1) then 
        WR=0.0
        do i=1,5
            WR=WR+spanv(i)*BB(i+ii)
        end do             
        write (IMP,777) WR
      end if
  777 format (' spanv . BB          :',d10.4)
C     (Modification June 2001: note that if one of the grains does
C      not deform at all, the stress and the active slip systems
C       of the full constraintssolution are used.)
C
C      do i=1,M11
C        j=i+jj
C        write (IMP,308) i,DTAU1(j),XX(j)
C      enddo
C 308  format ('PANCAK2  i,DTAU1, XX',i5,2d12.4)
      NACTIV=0
      do 305 i=1,M11
      j=i+jj
C     If one grain does not deform, then DTAU1 comes from the full
C     constraints solution.
      if (ABS(DTAU1(j)).gt.TOL) goto 305
      NACTIV=NACTIV+1
C      write (IMP,912) NACTIV,i
C 912  format (' NACTIV, i',2I5)
      if (NACTIV.le.8) THEN
                           INDACT(NACTIV)=i
                        ELSE
#ifndef ALTAY_SUBROUTINE
                           if(NLIST.eq.1) then
                                     write (IMP,306)
                           end if
                           write (*,306)
                           call terminate(stopcode_runtimeerror)
#else
      RCM_RAISE(1,'Pancak2','Too many active slip systems',RCM_RTN)
#endif                        
                        endif
 306  format (' PANCAK2 - 306 - TOO MANY ACTIVE SLIP SYSTEMS')
 305  continue
      if (NACTIV.eq.0) then
#ifndef ALTAY_SUBROUTINE      
                           if(NLIST.eq.1) then
                                     write (IMP,307)
                           end if
                           write (*,307)
                           call terminate(stopcode_runtimeerror)
#else
      RCM_RAISE(1,'Pancak2','No active slip systems found',RCM_RTN)
#endif
                       endif
 307  format (' PANCAK2 - 307 - No active slip systems found')
      do 310 NLP=1,NACTIV
      j=INDACT(NLP)
      i1=j
                      INDLP(NLP)=i1
                      SLIPLP(NLP)=XX(j+jj)
                      TAURLP(NLP)=TAUR1(j+jj) 
 310  continue
      RETURN
      END SUBROUTINE        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      





      subroutine Fakeccc(ccc,ccc2,Cofcos,Cofsin,BB,UBUF,M11,ca1,ca2)     
C   Cofsin, Cofcos         -- input
C   BB(10), UBUF(10)       -- input
C   M11                    -- input
C  BB(10) is direction of the relaxation-1
C  UBUF(10) is the BISHOP-HILL stress from TBH routine, in crystal frame
      implicit double precision (a-h,o-z)
      Real*8, INTENT(IN) :: ccc
      Integer, INTENT(IN) :: M11
      Real*8, INTENT(out) :: ccc2
      Real*8, INTENT(out) :: ca1
      Real*8, INTENT(out) :: ca2
      Real*8, INTENT(IN) :: Cofcos
      Real*8, INTENT(IN) :: Cofsin
      Real*8, INTENT(IN) :: BB
      Real*8, INTENT(IN) :: UBUF
      dimension BB(10),base1(5),UBUF(10),ccc(2,194),ccc2(2,194)
C
      if((abs(Cofsin) < epsilon(0.D0)) .and. 
     &   (abs(Cofcos) < epsilon(0.D0))) then 
C  update CRSS
      ccc2=ccc
      elseif(dabs(Cofcos).lt.0.000000001) then
      write(*,*) 'Cofcos=0. Somewhere is worong in the code'
      call terminate(stopcode_runtimeerror)
      else
c   we only need the component 1 along the imposed strain mode
      dlength2=sqrt(BB(1)*BB(1)+
     #BB(2)*BB(2)+
     #BB(3)*BB(3)+
     #BB(4)*BB(4)+
     #BB(5)*BB(5))
c  for grain-1
      base1(1)=BB(1)/dlength2
      base1(2)=BB(2)/dlength2
      base1(3)=BB(3)/dlength2
      base1(4)=BB(4)/dlength2
      base1(5)=BB(5)/dlength2
C  then calculate the stress component in grain-1
      sg1c1=UBUF(1)*base1(1)+
     #      UBUF(2)*base1(2)+
     #      UBUF(3)*base1(3)+
     #      UBUF(4)*base1(4)+
     #      UBUF(5)*base1(5)
C
C now calculate the component for grain-2
c
      dlength2=sqrt(BB(6)*BB(6)+
     #BB(7)*BB(7)+
     #BB(8)*BB(8)+
     #BB(9)*BB(9)+
     #BB(10)*BB(10))
c  for grain-2
      base1(1)= BB(6)/dlength2
      base1(2)= BB(7)/dlength2
      base1(3)= BB(8)/dlength2
      base1(4)= BB(9)/dlength2
      base1(5)=BB(10)/dlength2
c           
C  then calculate the stress component in grain-2
c
      sg2c1=UBUF(6)*base1(1)+
     #      UBUF(7)*base1(2)+
     #      UBUF(8)*base1(3)+
     #      UBUF(9)*base1(4)+
     #     UBUF(10)*base1(5)
c from here we use the new method to update the CRSS 
      zeta=sg1c1/sg2c1
C  check if it is negative
      if(zeta.lt.0.D0) then
      write(*,*) 'Zeta is negative, somewhere is wrong'
      call terminate(stopcode_runtimeerror)
      endif
      enta1=sqrt(1.D0/zeta)
      enta2=sqrt(zeta)
c     enta1=2.0/(1.0+zeta)
c     enta2=2.0*zeta/(1.0+zeta)
      Crssg1=Cofcos*Cofcos*enta1+Cofsin*Cofsin
      Crssg2=Cofcos*Cofcos*enta2+Cofsin*Cofsin
      ca1=Crssg1
      ca2=Crssg2  
C  update the CRSS for grain-1
      Do i=1,M11,1
      Do j=1,2,1
         CCC2(j,i)=Crssg1*CCC(j,i)
      enddo
      enddo
c   update the CRSS for grain-2
      Do i=1,M11,1
      Do j=1,2,1
         CCC2(j,i+M11)=Crssg2*CCC(j,i+M11)
      enddo
      enddo
      endif
      return
      end subroutine

      end module
