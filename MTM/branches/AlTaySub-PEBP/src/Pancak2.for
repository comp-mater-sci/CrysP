#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
C MODIFICATIONS AUG 2010
C THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
C WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY
C
      Subroutine Pancak2(KOST,NGL,B,DI1,DG,TDC,spanv,WR,
     1 SWRLX,BBVM,XX,IPR,Ftot,GEWF)
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif      
      USE MICROSTR
      use altayHard
#ifdef PEBP_ENABLED
      use KOST1xState
#endif
      implicit double precision (a-h,o-z)
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1
      COMMON /LAMEL/ laml,fi10b(2),phi0b(2),fi20b(2),TRFb(3,3,2),
     1 gewfb(2),GMMAb(2),Fb(3,3,2),GAXESb(3,2),GEULRb(3,2),
     2 CIJb(3,3,2),TGb(3,3,2),RHOSSb(3,3,2),
     3 fi1b(2),phib(2),fi2b(2),
     4 fk1b(2,96,2),NGR,NRL,ENTA,ITFMAS
      COMMON /IGLIJS/ FK1(2,96),M11,CC(2,96)
      COMMON /DOUBLE/ A8(5,96),BB8(5),DELTAT,RHO(5),B5(5)
      common /extra/ A1(10,194),UU(10,10)
      COMMON /RHO/ RHOS(5),RHOA(5)
      common /CEIGEN/ IOR,ISTP,NBLOC
      COMMON /ACTIVE/ NACTIV,INDACT(8),NLP,INDLP(8),SLIPLP(8),
     1 TLXX,TAURLP(8)
      dimension buftrf(3,3),C1(3,3),C2(3,3),
     1 DG(3,3),TDC(3,3),TDCb(3,3,2),TRCb(3,3,2),
     2 B(5,5),BBVM2(2),relax(3,3,3),buftg(3,3),DACC(10),
     3 rls(3,3,3,2),rla(3,3),rlm(3,3,3),C3(3,3),TRP(10),APRIME(10),
     4 B3(10,3),PLUMIN(2,3),Ftot(3,3),Grpar(3,3),TGRb(3,3),CUst(10)
C     first index op PLUMIN = nr. of grain
C     second index = nr. of relaxation
      dimension spanv(5),XX(194),STRSS(10),BB(10)
      dimension CCC(2,194),DTAU(194),DTAU1(194),TAUR(194),TAUR1(194)
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
      double precision :: ddt = 1.D0
#endif
      SAVE

	if (laml.ne.1.and.laml.ne.2) then
#ifndef ALTAY_SUBROUTINE
	write(*,*) 'laml=', laml
	stop
#else
      RCM_RAISE(1,'Pancak2','Wrong selection of lamels',RCM_RTN)
#endif      
      endif
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if (IOR.eq.1) IGrElm=0 
      TWOSQ3=sqrt(2.0/3.0)
      TAU=1.0
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
      call MATPROD(GRPAR,FTot,TmatGr(1,1,IGrElm),3,3,3)
      if (IPR.gt.1) then
	    if(NLIST.eq.1) then
          write (IMP,409) IGrElm
 409      format (' IGrElm = ',i5) 
          do i=1,3 
             write (IMP,407) (TmatGr(j,i,IGrElm),j=1,3)
          enddo
 407      format (' TmatGr ',3d15.7)
          do i=1,3 
             write (IMP,408) (GRPAR(j,i),j=1,3)
          enddo
 408      format (' GRPAR  ',3d15.7)
      endif 
	end if 
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  QGX
      If (NGR.eq.2) then
      call cluster1(TDC,GRPAR,GEWF,TGRB,alfa,
     1  WINT,Tprinc,IPR)
	else
	call cluster1(TDC,GRPAR,qq,TGRB,alfa,
     1  WINT,Tprinc,IPR)
	end if
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
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
      CALL MATPROD(C1,DG,buftrf,3,3,3)
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
      RLS(i,j,IRL,IL)=(C3(I,J)+C3(J,I))*0.5
      RLA(i,j)=(C3(I,J)-C3(J,I))*0.5
  83  continue
      B3(L1+1,IRL)=PLUMIN(IL,IRL)*RLA(2,3)/sqr2
      B3(L1+2,IRL)=PLUMIN(IL,IRL)*RLA(3,1)/sqr2
      B3(L1+3,IRL)=PLUMIN(IL,IRL)*RLA(1,2)/sqr2
      call STR5(B5,RLS(1,1,IRL,IL))
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
      TDCb(I,J,IL)=(C2(I,J)+C2(J,I))*0.5
  81  TRCb(I,J,IL)=(C2(I,J)-C2(J,I))*0.5
  80  CONTINUE                                                          
      call STR5(B5,TDCb(1,1,IL))
      deltat=0.0
      do 30 i=1,5
      deltat=deltat+B5(i)**2
      j=i+L1
      BB(j)=B5(i)
  30  continue
      deltat=SQRT(2.0D0*deltat/3.0D0)
C
C     Calculation of time increment by dividing von Mises equivalent
C     strain by von Mises equivalent strain rate
C
      do 44 j=1,5
      B5(j)=B5(j)/deltat
      B8(j,IL)=B5(j)
  44  continue
      BBVM2(IL)=deltat
      K1=M11*(IL-1)
C      TLCOST=TLC0
      if (KOST.eq.1) then
         GMMA=GMMAb(IL)
C         TLCOST=TLCOST*TAU
      endif
      TAU=FTAU(GMMA,KOST)
      DO 92 I=1,M11
      j=I+K1
      do jsgn=1,2
         CCC(jsgn,j)=1.0
      enddo
#ifdef PEBP_ENABLED
      select case(KOST)
      case(1)
            do jsgn=1,2
                  CCC(jsgn,j)=FK1b(jsgn,I,IL)*TAU
            enddo
      case(11)
            ! Note: PEBP can work only for bcc (24 slip systems)
            call KS_getCRSS(IOR,CCC(:,1:24),info)
      end select
#else
      if (KOST.EQ.1) then
                       do jsgn=1,2
                         CCC(jsgn,j)=FK1b(jsgn,I,IL)*TAU
                       enddo
      endif
#endif
C     set Tau_crit for antitwinning direction equal to
C     GETAL times Tau_crit for twinning direction 
      if (I.gt.NGL) then
          CCC(2,j)=CCC(1,j)*GETAL
      endif
  92  continue
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
C MAS-AL part  QGX
C the CRSS of the two pseudo slip systems are CrssP1 and CrssP2
C cos1 and cos2 are related with the cosine between the imposed strain rate and 
C the symmetry part of the relaxations
C W1 and W2 are the rate of plastic work by Taylor for grain1 and grain2
      IF(ITFMAS.eq.1) then
      W1=0.0
	w2=0.0
	Do imas=1,5,1
c
	w1=w1+UBUF(imas)*BB(imas)*DELTAT
	W2=w2+UBUF(imas+5)*BB(imas+5)*DELTAT
	enddo
      do iee=1,5,1
	rhos(iee)=a1(iee,M2+1)
	enddo
	cos1=0.0
	do iee=1,5,1
	cos1=cos1+rhos(iee)*BB(iee)*sqrt(2.0/3.0)
	enddo
	do iee=1,5,1
	rhos(iee)=a1(iee,M2+2)
	enddo
	cos2=0.0
	do iee=1,5,1
	cos2=cos2+rhos(iee)*BB(iee)*sqrt(2.0/3.0)
	enddo
	CrssP1=dabs((w1-w2)*cos1*sqrt(2.0/3.0)/DELTAT)*ENTA
	CrssP2=dabs((w1-w2)*cos2*sqrt(2.0/3.0)/DELTAT)*ENTA
	endif
c

      if (IPR.lt.4) goto 220
#ifndef ALTAY_SUBROUTINE
	if(NLIST.eq.1) then
      write (IMP,221) IPR,IOR,ISTP,NBLOC
	end if
      write (*,221) IPR,IOR,ISTP,NBLOC
 221  format (' Pancak2 ',
     1 ' IPR IOR, ISTP, NBLOC=',4I5)
      if (IPR.ge.4) stop
#else
      RCM_RAISE(1,'Pancak2','IPR must be < 4',RCM_RTN)
#endif
  220    DTAU1=DTAU 
         TAUR1=TAUR  
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011	     
C      if (IROT.eq.0) goto 89
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      if (NRL.eq.0) then
                      UU=UU2
                      DI=DI2
                      STRSS=UBUF
                      goto 89
                    endif
      IF(ITFMAS.eq.1) then
	  if (swrlx(1)) then  
        CCC(1,M2+1)=CrssP1
        CCC(2,M2+1)=CrssP1
	  endif
	  if (swrlx(2)) then  
        CCC(1,M2+2)=CrssP2
        CCC(2,M2+2)=CrssP2
	  endif
	else	
	  do 86 IRL=1,NRL
        if (.not.swrlx(IRL)) goto 86
        j=M2+IRL  
        CCC(1,j)=TAURL(IRL)
        CCC(2,j)=TAURL(IRL)
  86    continue 
	endif
C
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
          stop  
#else
          RCM_RAISE(1,'Pancak2','Problem with TBH',RCM_RTN)

#endif         
      endif
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
      DTAU1=DTAU
      TAUR1=TAUR
      UBUF=STRSS
 213  if (IPR.gt.0.and.NRL.gt.0) then
      if(NLIST.eq.1) then 
      write (IMP,780) gamr
	end if
	end if
 780  format (' RELAXATIONS: GAMMA 13, 23, 12 =',3d12.4)
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
c 
      WR=0.0
      do 304 i=1,5
      WR=WR+spanv(i)*BB(i+ii)
  304 continue
      if (IPR.EQ.2) then
	if (NLIST.eq.1) then 
	write (IMP,777) WR
	end if
	end if
  777 format (' Rate of Plastic work:',d10.4)
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
                           stop
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
                           stop
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
      END        
      
                                                             
                                                                                                                                                                           
      Subroutine CLUSTER1(TDC,GRPAR,GEWF,TGrb,alfa,
     1 WINT,Tprinc,IPR)
      implicit double precision (a-h,o-z)
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1
      dimension AXX(3,3),GRPAR(3,3),TGRB(3,3),
     1 C1(3,3),PrDir(3,3),TDC(3,3),TDCGr(3,3),
     2 vec1(3),vec2(3),Tprinc(3,3),AL(3),AA(3)
      data PrDir/8*0.0d0,1.0d0/
      SAVE
      if (IPR.gt.0) then
	if (NLIST.eq.1) then
	write (IMP,100)
	end if
	end if
 100  format (//,' CLUSTER1')
C     Calculation of volume affected by the surface
      do i=1,3
C        write (*,119) (GRPAR(i,k),k=1,3)
C 119    format (3d15.5)
        x=0.0
          do j=1,3 
            X=X+GRPAR(j,i)**2
          enddo
        AL(i)=sqrt(X)
      enddo 
C     Box product
      vec1(1)=GRPAR(2,2)*GRPAR(3,3)-GRPAR(3,2)*GRPAR(2,3)
      vec1(2)=GRPAR(3,2)*GRPAR(1,3)-GRPAR(1,2)*GRPAR(3,3)            
      vec1(3)=GRPAR(1,2)*GRPAR(2,3)-GRPAR(2,2)*GRPAR(1,3)
      u=0.0
      do i=1,3
         u=u+GRPAR(i,1)*vec1(i)
      enddo
      u=abs(u)*0.25/(AL(1)*AL(2)*AL(3))
C      write (*,120) u,AL(1),AL(2),AL(3)
C 120  format (' u=',4d15.5)
C     The factor 0.25 is there so that for equiaxed grains, GEWF below becomes 1/3;
C      for very flattened grains, it should tend to 1.
C 
C     re-order the basisvectors so that AA(1)>=AA(2)>=AA(3)
C     find out which one of these corresponds to the original AL(3)
C     Case 1: is AL(3) the longest? 
      if (AL(2).le.AL(3).and.AL(1).le.AL(3)) then
        AA(1)=AL(3)
        if(AL(2).ge.AL(1))then
           AA(2)=AL(2)
           AA(3)=AL(1)
        else
           AA(2)=AL(1)
           AA(3)=AL(2)
        endif
C             write (*,121) AA
C 121         format ('Case 1',3d15.5)
        GEWF=u*(2.0*(AA(2)-AA(3))*AA(3)**2+4.0*AA(3)**3/3.0)
      else 
C       Case 2: is AL(3) the shortest?
        if (AL(3).le.AL(1).and.AL(3).le.AL(2)) then
          AA(3)=AL(3)
          if(AL(1).ge.AL(2))then
             AA(1)=AL(1)
             AA(2)=AL(2)
          else
             AA(1)=AL(2)
             AA(2)=AL(1)
          endif
C             write (*,122) AA
C 122         format ('Case 2',3d15.5)
          GEWF=u*(4.0*(AA(1)-AA(3))*(AA(2)-AA(3))*AA(3)
     1        +2.0*(AA(2)-AA(3))*AA(3)**2+2.0*(AA(1)-AA(3))*AA(3)**2
     2        +4.0*AA(3)**3/3.0)
        else
C         Case 3: AL(3) is neither shortest nor longest      
          AA(2)=AL(3)
          if(AL(1).ge.AL(2))then
             AA(1)=AL(1)
             AA(3)=AL(2)
          else
             AA(1)=AL(2)
             AA(3)=AL(1)
          endif
C             write (*,123) AA
C 123         format ('Case 3',3d15.5)
          GEWF=u*(2.0*(AA(1)-AA(3))*AA(3)**2+4.0*AA(3)**3/3.0)
        endif
      endif
      if (IPR.gt.0) then
	if (NLIST.eq.1) then
	write (IMP,103) GEWF
	end if
	end if 
C      write (*,103) GEWF
 103  format (/,' GEWF ',3d15.7,/) 

C     Construction of orientation matrices for frames associated to the
C     interfaces
      IN=3  
      IA=1
      IB=2
        do i=1,3
           AXX(i,1)=GRPAR(i,IA)
        enddo
C       Orientation of interfaces containing axes IA and IB
C       Normal axis: (vector product)
        AXX(1,3)=GRPAR(2,IA)*GRPAR(3,IB)-GRPAR(3,IA)*GRPAR(2,IB)
        AXX(2,3)=GRPAR(3,IA)*GRPAR(1,IB)-GRPAR(1,IA)*GRPAR(3,IB)
        AXX(3,3)=GRPAR(1,IA)*GRPAR(2,IB)-GRPAR(2,IA)*GRPAR(1,IB)
C       Orientation of 2nd axis:(vector product)
        AXX(1,2)=AXX(2,3)*AXX(3,1)-AXX(3,3)*AXX(2,1)
        AXX(2,2)=AXX(3,3)*AXX(1,1)-AXX(1,3)*AXX(3,1)
        AXX(3,2)=AXX(1,3)*AXX(2,1)-AXX(2,3)*AXX(1,1)
C       Normalisation
        do j=1,3
           x=0.0d0
           do i=1,3
              x=x+AXX(i,j)**2
           enddo
           x=sqrt(x)
           do i=1,3
              AXX(i,j)=AXX(i,j)/x
           enddo
        enddo
        do i=1,3
           do j=1,3
              TGrb(i,j)=AXX(j,i)
           enddo
           if (IPR.gt.0) then
		 if(NLIST.eq.1) then 
		 write (IMP,102) (TGrb(i,j),j=1,3)
	     end if
	     end if
  102      format (' TGrb ',3d15.7)            
        enddo

C     Transform TDC to the "Grb" reference frame 
      CALL MATPROD(C1,TDC,AXX,3,3,3)
      CALL MATPROD(TDCGr,TGrb,C1,3,3,3) 
C     Calculate velocity of end tip of a vector with
C     unit length and positioned normal to the 
C     grain boundary segment at the origin of thre frame.
      do i=1,2
         vec1(i)=0.0
      enddo 
      vec1(3)=1.0
      call MATPROD(vec2,TDCGr,vec1,3,3,1)
C     Calculate angle between projection of velocity vector 
C       on Grain Boundary Segment and axis 1
      Sphi=vec2(2)
      Cphi=vec2(1)
      if (abs(Sphi).lt.1.0d-6.and.abs(Cphi).lt.1.0d-6) then
         phi=0.0
      else
         phi=ATAN2(Sphi,Cphi)
      endif  
C      write (*,105) vec2,phi*180.0/pi
C 105  format ('vec2',3d15.5,f10.1)
C     Transformation to a frame in which the projection of the 
C       deformed vector is axis 1      
      y=cos(phi)
      PrDir(1,1)=y
      x=sin(phi)
      PrDir(1,2)=-x
      PrDir(2,1)=x
      PrDir(2,2)=y
      AXX=PrDir
      AXX(1,2)=x
      AXX(2,1)=-x
C      CALL MATPROD(C1,TDCGr,PrDir,3,3,3)
C      CALL MATPROD(C2,AXX,C1,3,3,3)
C      write (*,104) ((C2(i,j),j=1,3),i=1,3)
C 104  format ('C2',3d20.8)
      CALL MATPROD(Tprinc,AXX,TGrb,3,3,3)
      if (IPR.eq.2) THEN
         do i=1,3
	      if(NLIST.eq.1) then
            write (IMP,116) (Tprinc(i,j),j=1,3)
	      end if
         enddo
      endif
 116  format (' Tprinc',3d15.8)
C     ALFA and WINT have to do with the (abandoned) Type III relaxation
      ALFA=0.5
      WINT=1.0D06
      if (IPR.gt.2) then
	if (NLIST.eq.1) then 
	write (IMP,115) WINT
	end if
	end if
 115  format (' CRSSR for Type III relaxation:',d15.5)
      RETURN
      END                                                               

