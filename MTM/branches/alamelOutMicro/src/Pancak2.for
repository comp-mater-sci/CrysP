      Subroutine Pancak2(KOST,NGL,NTW,M11,B,DI1,DG,TDC,spanv,WR,
     1 SWRLX,BBVM,CC,XX,IPR,IROT,Ftot,GEWF)
      USE MICROSTR
      implicit double precision (a-h,o-z)
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1,NDAT2
      COMMON /LAMEL/ laml,fi10b(2),phi0b(2),fi20b(2),TRFb(3,3,2),
     1 gewfb(2),GMMAb(2),Fb(3,3,2),GAXESb(3,2),GEULRb(3,2),
     2 CIJb(3,3,2),TGb(3,3,2),RHOSSb(3,3,2),
     3 fi1b(2),phib(2),fi2b(2),
     4 fk1b(96,2)
C
C     Extra arrays nodig voor lineaire programmatie op 2 korrels tegelijk
C
      common /extra/ A1(10,198),UU(11,11),FK(198),W(11)
      COMMON /DOUBLE/ A8(5,96),BB8(5),DELTAT,RHO(5),B5(5)
      COMMON/TLR3/ BB(10),V(11),MASK(199)
      COMMON /RHO/ RHOS(5),RHOA(5)
      common /CEIGEN/ IOR,ISTP,NBLOC,ALFAK,CMICRO(3,3)
      COMMON /ACTIVE/ NACTIV,INDACT(8),NLP,INDLP(8),SLIPLP(8),TLXX
      dimension TT(10),buftrf(3,3),C1(3,3),C2(3,3),
     1 DG(3,3),TDC(3,3),TDCb(3,3,2),TRCb(3,3,2),
     2 B(5,5),BBVM2(2),CC(96),relax(3,3,3),buftg(3,3),
     3 rls(3,3,3,2),rla(3,3),rlm(3,3,3),C3(3,3),
     4 B3(10,3),PLUMIN(2,3),Ftot(3,3),Grpar(3,3),TGRb(3,3)
C     first index op PLUMIN = nr. of grain
C     second index = nr. of relaxation
      dimension spanv(5),XX(198),A11(10,198)
      dimension CCC(198),FKBUF(198)
      logical SWRLX(3)
C     rlm is unit relaxation tensor in macroscopic frame
C     rls and rla in crystal frame (symmetric and anti-sym. part)
      dimension B8(5,2),UBUF(10),UU2(11,11),BB2(10)
      dimension GAMR(3),Tprinc(3,3),TAURL(3)
      integer DI1(5),DI(11),DI2(11)
      data SQR2/0.7071067811865476D+00/,B3/30*0.0D0/,TOLXX/5.0d-6/
      data TLC0/5.0d-10/,tolcnv/1.0d-4/
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
C     NRL= number of relaxations    NGR= number of grains
      data NRL/3/,NGR/2/,TAURL/3*0.0d0/
      data GETAL/1.0D6/,TOL/1.0d-6/
      data Jtera/0/ 
      SAVE
      if (IOR.eq.1) IGrElm=0 
      TWOSQ3=sqrt(2.0/3.0)
      Nitera=15
C     first estimate of first relaxation
      estgam=0.0  
      estpos=0.0
      estneg=0.0
      yvpos=0.0
      yvneg=0.0 
      TAU=1.0
C     N is number of rows of A1;   NU number of rows of UU
      TLXX=TOLXX
      N=5*NGR
      NU=N+1
      N1=N+1
      M2=NGR*M11
      M12=NGR*M11+2*NRL
      N2RL=2*NRL
      if (laml.eq.2) goto 3
C
C     Updating of microstructure
C
      IGrElm=IGrElm+1
      if (IGrElm.gt.NGrElm) IGrElm=1
      call MATPROD(GRPAR,FTot,TmatGr(1,1,IGrElm),3,3,3)
      if (IPR.gt.1) then
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
      call cluster1(TDC,GRPAR,GEWF,TGRB,alfa,
     1  WINT,Tprinc,IPR)
C     Definition of relaxation of Type III:
      relax(1,2,3)=alfa
      relax(2,1,3)=1.0-alfa
      if (M2.eq.M12) goto 32
      do 33 i=M2+1,M12
  33  CCC(i)=0.0
  32  do 31 i=1,NU
      do 31 j=1,NU
      UU(j,i)=0.0
  31  continue
      DO 53 I=1,5
      DI(I)=DI1(I)
      DI(I+5)=DI1(I)+M11
  53  CONTINUE                                                          
      do 1 IL=1,2
      L1=5*(IL-1)
C     OMREKENING DISPLACEMENT GRADIENT.
      do 45 i=1,3
      do 45 j=1,3
      buftrf(i,j)=TRFb(j,i,IL)
      buftg(i,j)=Tprinc(j,i)
  45  continue
      CALL MATPROD(C1,DG,buftrf,3,3,3)
      CALL MATPROD(C2,TRFb(1,1,IL),C1,3,3,3)
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
      A1(i1,j+NRL)=-x
  84  continue
  82  continue
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
C      deltat=deltat/DVM
      K1=M11*(IL-1)
      TLCOST=TLC0
      if (KOST.eq.1) then
         GMMA=GMMAb(IL)
         TAU=FTAU(GMMA)
         TLCOST=TLCOST*TAU
      endif
      DO 92 I=1,M11
      j=I+K1
      CCC(j)=1.0
      if (KOST.NE.1) goto 92
      CCC(j)=FK1b(I,IL)*TAU
  92  continue
      DO 15 J=1,5
      DO 15 I=1,5
      UU(I+L1,J+L1)=B(I,J)
  15  continue
   1  continue
C     Calculation of the value of the "cosine" a:
      aval=0.0
      do i=1,3 
         do j=1,3
            aval=aval+TDC(i,j)*RLM(i,j,1)
         enddo
      enddo
C     a-value must be normalised
C     (norms of TDC: 1.5) 
      aval=aval*TWOSQ3
      if (IPR.gt.0) write (IMP,600) aval
 600  format ('a-value=',d15.5)
      alasq=alfak*aval**2
      if (KOST.eq.1) then
         TAURL(3)=TAU*WINT
      else
         TAURL(3)=WINT
      endif
      DO 54 I=1,N
C     Conversion of strain to normalized strain rate
      BB(I)=BB(I)/BBVM2(1)
  54  CONTINUE
      do 85 j=M2+1,M12
      CCC(j)=GETAL
  85  continue
      UU2=UU
      DI2=DI
      BB2=BB
      A11=A1
C      if (IRELX.gt.0) return
C
      do Kitera=1,2 ! Begin of the double loop to find the first relaxation 
      
      DO 506 I=1,N                                                      
 506  TT(I)=1.
C
C     Full constraints calculation
C
C     KONTROLE OP NEGATIEVE RECHTERLEDEN
C     REKENING HOUDEND MET NIET-OMKEERBARE TWEELINGSYSTEMEN             
       DO 21 I=1,N                                                      
      X=TT(I)
      Y8=BB(I)
      IF (X.GT.0..AND.Y8.GE.0.) GOTO 62
      IF (X.LT.0..AND.Y8.LT.0.) GOTO 62
      TT(I)=-X
      DO 20 J=1,M12                                                     
  20  A1(I,J)=-A1(I,J)                                                  
  62  IF (Y8.GE.0.) GOTO 21
      BB(I)=-Y8
      DO 23 J=1,N                                                       
  23  UU(J,I)=-UU(J,I)
  21  CONTINUE                                                          
      DO 22 I=1,N                                                       
      Y8=0.
      DO 24 J=1,N
  24  Y8=Y8+UU(I,J)*BB(J)
      IF (Y8.GE.0.) GOTO 22
      L=DI(I)
      LL=NGL
      if (L.gt.M11) LL=M11+NGL
      IF (L.GT.LL) GOTO 22
      DI(I)=L+NGL+NTW
      DO 4 J=1,N
   4  UU(I,J)=-UU(I,J)
  22  CONTINUE 
C      UU2=UU
C     UITVOEREN VAN DE SIMPLEX-SUBROUTINE.
      IF (IPR.EQ.2) WRITE (IMP,218) (CCC(I),I=1,M12)
 218  FORMAT(/' COST FUNCTION',/,(2x,10F10.4))
      IF (IPR.EQ.2) WRITE (IMP,219) (BB(I),I=1,N)
 219  format (' right hand side',/,(2x,10F10.4),/)
C     First call of Simplex (full constraints)
      if (IPR.eq.2) write (IMP,400) IOR,ISTP,NBLOC
 400  format (' First call of LA01P   IOR,ISTP,NBLOC',3I5)
      CALL LA01P(M12,N,A1,BB,CCC,XX,FAKM,N,IPR,UU,V,W,DI,MASK,FK,N1,
     1 N2RL,TLCOST)
      if (IPR.lt.4) goto 220
      write (IMP,221) IPR,IOR,ISTP,NBLOC
      write (*,221) IPR,IOR,ISTP,NBLOC
 221  format (' Pancak2 ',
     1 ' IPR IOR, ISTP, NBLOC=',4I5)
      if (IPR.ge.4) stop
 220  do i=1,M12
      FKBUF(i)=FK(i)
      enddo
      if (IROT.eq.0) goto 204
      do 209 i=1,N
      UBUF(i)=UU(NU,i)
C      write (IMP,975) i,UBUF(i)
C 975  format (' UBUF',i5,d15.8)
 209  continue
C     Activation of Relaxed Constraints 
      if (.not.swrlx(1)) Nitera=1

      do Itera=1,Nitera !Begin 2nd nested loop to find first relaxation   
      
      
      if (IPR.gt.0) write (IMP,230) Itera,estgam
  230 format (/,' PANCAK2 - 2nd nested Iteration',i5,'   estgam=',d15.5)
C
C     Secant modulus for relaxation 1:
C
        TAURL(1)=abs(estgam)*alasq
        do 86 IRL=1,NRL
C        write (IMP,*) swrlx(IRL)
        if (Kitera.eq.2.and.IRL.eq.1) goto 86
        if (.not.swrlx(IRL)) goto 86
        j=M2+IRL
        CCC(j)=TAURL(IRL)
        CCC(j+NRL)=TAURL(IRL)
  86    continue 
      IF (IPR.EQ.2) WRITE (IMP,218) (CCC(I),I=1,M12)
C     Second call of Simplex (relaxed constraints)
C     The last argument = N2RL: 2 x number of relaxed contraints
C      if (IOR.eq.1967.and.ISTP.eq.11.and.NBLOC.eq.3) IPR=2
      if (IPR.eq.2) write (IMP,401)
 401  format (' Second call of LA01P')
      CALL LA01P(M12,N,A1,BB,CCC,XX,FAKM,N,IPR,UU,V,W,DI,MASK,FK,N1,
     1 N2RL,TLCOST)
C      if (IOR.eq.1967.and.ISTP.eq.11.and.NBLOC.eq.3) stop
      if (IPR.ge.4) then
         write (IMP,222) IPR,IOR,ISTP,NBLOC
         write (*,222) IPR,IOR,ISTP,NBLOC
 222     format (' Pancak2 222 - Problem with LA01P',/,
     1   ' IPR IOR, ISTP, NBLOC=',4I5)
          stop  
      endif
C     GAMR will contain the relaxed shears:
C      if (IROT.eq.0) goto 2
C      WACCCO=0.0
      do 208 IRL=1,NRL
      gamr(IRL)=XX(M2+IRL)-XX(M2+IRL+NRL)
C      WACCCO=WACCCO+XIRL(IRL)*gamr(IRL)**2
 208  continue
      if (Kitera.eq.2) goto 205
      x=gamr(1)
      arbeid=FAKM-x*alasq*estgam+alasq*x**2
      yval=x-estgam
      convcr=abs(yval*alasq)/tau
      if (IPR.gt.0) write (IMP,231) x,estgam,yval,convcr,arbeid
 231  format (/,'x,estgam,yval,convcr,arbeid:',5d12.5) 
      if (convcr.lt.tolcnv) goto 204
      x0=estgam
      if (itera.eq.1) then
         estgam=x
      else
         if (yval.lt.0.0) then
            estgm0=estpos
            yval0=yvpos
         else
            estgm0=estneg
            yval0=yvneg
         endif
         y0=yval-yval0
         estgam=0.5*(x0+estgm0)
      endif
      if (yval.lt.0.0) then
         yvneg=yval
         estneg=x0
      else
         yvpos=yval
         estpos=x0
      endif
C     Here a good estimate of 1st relaxation is 'estgam'      
      enddo !End of 2nd nested loop to find first relaxation   
C
C     Prepare Full-constraints calculation with already found 1st relaxation
C     brought to the right-hand side of the equations
C     
      IRL=1
      if (.not.swrlx(IRL)) goto 204
      UU=UU2
      DI=DI2
      A1=A11
      j=M2+IRL
      do i=1,N
        BB(i)=BB2(i)-A1(i,j)*estgam
      enddo
      do j=M2+1,M12
          CCC(j)=GETAL   ! De-activate relaxations
      enddo
      
      enddo !end of double loop te find first relaxation 
      
 205  gamr(1)=estgam
      arbeid=FAKM+alasq*estgam**2
      if (IPR.GT.0) write (IMP,235) estgam,arbeid
 235  format (/,' End of double loop 1st relaxation=',
     1  d15.5,'  arbeid=',d15.5,//)
      Itera=Nitera+1

      
 204  If (Itera.gt.2) then
         Jtera=Jtera+1
C         write (IMP,233) Jtera,Itera,yval
C 233     format ('NO. of iterations',2I8,d15.5)
      endif 
      
      
      
      DO 27 I=1,N
      IF (TT(I).GT.0.) GOTO 27
      UU(NU,I)=-UU(NU,I)
      BB(i)=-BB(i)
      UBUF(I)=-UBUF(I)
      DO 28 J=1,M12
  28  A1(I,J)=-A1(I,J)                                                  
  27  CONTINUE
C     Check whether 1 grain does not deform at all.
      j=0
      do 40 IL=1,NGR
      XXTOT=0.0
      do i=1,M11
       j=j+1
       XXTOT=XXTOT+xx(j)
      enddo
C      write (IMP,887) IL,XXTOT
C 887  format(' IL XXTOT',i5,d15.8)
      if (XXTOT.lt.TOLXX) goto 213
   40 continue
C     If all grains have a non-zero slip, do the following:
C      write (IMP,987)
C 987  format (' Hier geweest')
      do i=1,M2
      FKBUF(i)=FK(i)
      enddo
      do I=1,N
      UBUF(I)=UU(NU,I)
      enddo
 213  if (IPR.gt.0) write (IMP,780) gamr
 780  format (' RELAXATIONS: GAMMA 13, 23, 12 =',3d12.4)
   2  continue
C
C     Hier moet de output komen van de data voor IL=laml
C
   3  BBVM=BBVM2(laml)
      jj=M11*(laml-1)
      do 203 j=1,M11
      CC(j)=CCC(j+jj)
 203  continue
C     Op 23/3/2002 werden volgende 5 lijnen gedesactiveerd:
C      do 200 j=1,3
C      do 200 i=1,3
C      TDC(i,j)=TDCb(i,j,laml)
C      TRC(i,j)=TRCb(i,j,laml)
C 200  continue
      ii=5*(laml-1)
      do 201 i=1,5
C     If one grain does not deform, then UBUF comes from the full
C     constraints solution.
      spanv(i)=UBUF(i+ii)
      B5(i)=B8(i,laml)
C      write (IMP,776) laml,B5(i),spanv(i),i+ii
C 776  format (' B5  ',i5,e15.8,   'spanv  ',d15.8,' i+ii',i5)
      x8=0.0
      y8=0.0
      do 210 IRL=1,NRL
      x8=x8+A1(i+ii,M2+IRL)*gamr(IRL)
      y8=y8+B3(i+ii,IRL)*gamr(IRL)
 210  continue
      BB8(i)=B8(i,laml)-x8
      RHOS(i)=-x8
      RHOA(i)=-y8
 201  continue
      WR=0.0
      do 304 i=1,5
      WR=WR+spanv(i)*BB(i+ii)
  304 continue
      if (IPR.EQ.2) write (IMP,777) WR
  777 format (' Rate of Plastic work:',d10.4)
C
C     New section (october 2000)
C
C     (Modification June 2001: if one of the grains does not deform at all,
C     then the stress and the active slip systems of the full constraints
C     solution are used.
C
C      do i=1,M11
C        j=i+jj
C        write (IMP,308) i,FK(j),XX(j)
C      enddo
      NACTIV=0
      do 305 i=1,M11
      j=i+jj
C     If one grain does not deform, then FKBUF comes from the full
C     constraints solution.
      if (ABS(FKBUF(j)).gt.TOL) goto 305
 308  format ('i,FK',i5,2d12.4)
      NACTIV=NACTIV+1
      if (NACTIV.le.8) THEN
                           INDACT(NACTIV)=i
                        ELSE
                           write (IMP,306)
                           write (*,306)
                           stop
                        endif
 306  format (' PANCAK2 - 306 - TOO MANY ACTIVE SLIP SYSTEMS')
 305  continue
      NLP=0
      do 310 i=1,N
      j=DI(i)
      i1=j-jj
      if (i1.lt.1.or.i1.gt.M11) goto 310
      NLP=NLP+1
      if (NLP.le.8) then
                      INDLP(NLP)=i1
                      SLIPLP(NLP)=XX(j)
                    else
                      write (IMP,311)
                      write (*,311)
                      stop
                    endif
 311  format (' PANCAK2 - 311 - TOO MANY active slip systems')
 310  continue
      if (NACTIV.eq.0.and.NLP.eq.0) then
                           write (IMP,307)
                           write (*,307)
                           stop
                       endif
 307  format (' PANCAK2 - 307 - No active slip systems found')
      RETURN
      END        
      
                                                             
                                                                                                                                                                           
      Subroutine CLUSTER1(TDC,GRPAR,GEWF,TGrb,alfa,
     1 WINT,Tprinc,IPR)
C 13/3/02 New routine specially developed for the ALAM-model
C          (first version was developed for the BISG-model)
      implicit double precision (a-h,o-z)
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1,NDAT2
      dimension AXX(3,3),GRPAR(3,3),TGRB(3,3),
     1 C1(3,3),PrDir(3,3),C2(3,3),TDC(3,3),TDCGr(3,3),
     2 vec1(3),vec2(3),Tprinc(3,3),AL(3),AA(3)
      data PrDir/8*0.0d0,1.0d0/
      data pi/ 0.3141592741012573D+01/
C      data tole/1.0d-8/ 
      SAVE
      if (IPR.gt.0) write (IMP,100)
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
      if (IPR.gt.0) write (IMP,103) GEWF 
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
           if (IPR.gt.0) write (IMP,102) (TGrb(i,j),j=1,3)
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
            write (IMP,116) (Tprinc(i,j),j=1,3)
         enddo
      endif
 116  format (' Tprinc',3d15.8)
C     ALFA and WINT have to do with the (abandoned) Type III relaxation
      ALFA=0.5
      WINT=1.0D06
      if (IPR.gt.2) write (IMP,115) WINT
 115  format (' CRSSR for Type III relaxation:',d15.5)
      RETURN
      END                                                               

