#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif

      module altaySliprate
      use altayMiscutils, only: terminate, stopcode_runtimeerror

      contains
      
      Subroutine SLIPRAT(M11,IDIMXX,XX,IOR,IPR,sgnn,MacroDefRate)
      use altayIOConfig,IIPR=>IPR !Rename the global IPR to avoid conflict
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif      
      use altayMacroKinematic
      IMPLICIT double precision (A-H,O-Z)
      type(DeformationRate),intent(in) :: MacroDefRate      
!     September 2000
!     To find the slip rates assuming that
!     - the stress, strain rate and the active slip systems are known,
!       previously obtained by PANCAK2;
!     - (under the above restrictions) the sum of the squares of the slip
!       rates must be minimal. 
!
!     Modified in Aug 2010
!
      COMMON /DOUBLE/ A1(5,96),BB8(5),RHO(5),B5(5)
      COMMON /ACTIVE/ NACTIV,INDACT(8),NLP,INDLP(8),SLIPLP(8),           &
       TLXX,TAURLP(8) 
      dimension SGNN(IDIMXX)
      dimension SLPR(8),IND(8),XX(IDIMXX),ISTOR(0:8,48),SLSTOR(0:8,48)
      data NSTOR/48/
      do j=1,M11
         XX(j)=0.0
      enddo
      ITR=0
      NLP=NACTIV
      NN=NACTIV
      NOPL=0
!     check whether solution is totally zero
      x=0.0
      do i=1,NLP 
          x=x+abs(SLIPLP(i))
          j=INDACT(i)
          sgnn(j)=1.D0
          if (TAURLP(i).lt.0.0d0) sgnn(j)=-1.D0
!      write (IMP,911)  i,j,INDACT(i),indlp(i),SLIPLP(i),TAURLP(i)
! 911  format (' SLIPRAT i=',I5,'  j=',I5,'   INDACT(i)=',i5,'  INDLP='
!     1,I5,/,'              SLIPLP(i)=',D12.4,' TAURLP(i)=',D12.4)
      enddo
      if (x.lt.TLXX) goto 6
!     end of check
      do 1 i=1,NN
      IND(i)=INDACT(i)
  1   continue
  3   call MINSQU(NN,IND,SLPR,ineg,sumsq,sgnn,IDIMXX)
      if (ineg.eq.0) then
           call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
#ifdef ALTAY_SUBROUTINE
           RCM_GUARD
#endif
           if (NN.le.5) goto 2
      endif
      if (NN.le.5) goto 6
!
!     Let us take all combinations of NN out of NACTIV
!
!     "Levels" in the combination search:
!     (first level:  if NACTIV=8, find all combinations of 7 sl. syst.
!      second level: find all combinations of 6 - etc.)
!
      N0=NACTIV
!
!     First level
!
       N1=N0-1
       ITR=1
       if (N1.lt.5) goto 6
       NN=N1
       do I1=1,N0
          call MINSQU(N1,IND,SLPR,ineg,sumsq,sgnn,IDIMXX)
          if (ineg.eq.0) then
             call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
#ifdef ALTAY_SUBROUTINE
             RCM_GUARD
#endif 
          endif
!          write (IMP,110) (IND(i),i=1,N1)
 110   format (10i5)
          J=N0-I1
          if (J.gt.0) IND(J)=INDACT(J+1)
       enddo
!
!     Level 2
!
      N2=N1-1
      ITR=2
      if (N2.lt.5) goto 2
      NN=N2
      do I1=2,N0
         J1=I1-1
         do I2=1,J1
            j=1
            do 5 i=1,N0
            if (i.eq.I1.or.i.eq.I2) goto 5
            IND(j)=INDACT(i)
            j=j+1
   5        continue
            call MINSQU(N2,IND,SLPR,ineg,sumsq,sgnn,IDIMXX)
            if (ineg.eq.0) then
                call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
#ifdef ALTAY_SUBROUTINE
                RCM_GUARD
#endif
            endif
!            write (IMP,110) (IND(i),i=1,N2)
         enddo
      enddo
!
!     Level 3
!
      N3=N2-1
      ITR=3
      if (N3.lt.5) goto 2
      NN=N3
      do I1=3,N0
         J1=I1-1
         do I2=2,J1
            J2=I2-1
            do I3=1,J2
              j=1
              do 7 i=1,N0
              if (i.eq.I1.or.i.eq.I2.or.i.eq.I3) goto 7
              IND(j)=INDACT(i)
              j=j+1
   7          continue
              call MINSQU(N3,IND,SLPR,ineg,sumsq,sgnn,IDIMXX)
!              if (ineg.eq.0) goto 2
              if (ineg.eq.0) then
                  call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
#ifdef ALTAY_SUBROUTINE
                  RCM_GUARD
#endif
              endif 
!              write (IMP,110) (IND(i),i=1,N3)
           enddo
         enddo
      enddo
   2  if (IPR.eq.2) then
      if (NLIST.eq.1) then
      write (IMP,100)
      end if
      end if
 100  format (' Results SLIPRAT')
!      NREDU=NACTIV-NN
      if (NOPL.eq.0) goto 6
      IOPL=0
      X=1.0d10
      do i=1,NOPL
         Y=SLSTOR(0,i)
!
!
!  Y is de te minimaliseren waarde van oplossing i
!  Uitprinten!
!
!
         if (Y.lt.X) then
                       X=Y
                       IOPL=i
         endif
      enddo
      NN=ISTOR(0,IOPL)
      sumsq=X
      do i=1,NN
         IND(i)=ISTOR(i,IOPL)
         SLPR(i)=SLSTOR(i,IOPL)
      enddo
      if (IPR.eq.2) then
      if (NLIST.eq.1) then
        write (IMP,104) IOR,NACTIV,NN,NOPL
      end if
      end if
 104  format (I5,' Reduction of NACTIV from',I5,'   to',i5,' NOPL=',i5)
      if (IPR.eq.2) then
      if (NLIST.eq.1) then
      write (IMP,106) IOR,sumsq,(IND(i),i=1,NN)
      end if
      end if
 106  format (I5,d12.3,8i5)
      x=0.0
      k=0
      do i=1,NN
         j=IND(i)
         Y=SLPR(i)
         YY=Y*sgnn(j)
         XX(j)=YY*MacroDefRate%vMeqStrainRate
         if (IPR.eq.2) then
         if (NLIST.eq.1) then
         write (IMP,101) i,IND(i),YY
         end if
         end if
         if (x.gt.Y) then
                       x=Y
                       k=k+1
                     endif
      enddo
       if (X.lt.0.0d0) then
       if (NLIST.eq.1) then 
       write (IMP,102) IOR,k,X
       end if
       end if
 102  format (' NEG. SL. RATE DETECTED',2I5,d15.6)
 101  format (2i5,5x,d15.6)
      return
  6   ITR=-1
      NN=NLP
      do 11 i=1,NN
      IND(i)=INDLP(i)
 11   continue
      if (IPR.eq.2) then
      if (NLIST.eq.1) then
      write (IMP,100)
      end if
      end if
      if (IPR.eq.2) then
      if (NLIST.eq.1) then
      write (IMP,108) IOR,NN
      end if
      end if
 108  format (' IOR=',I5,' Linear programming solution retained ',       &
      ' NN=',i5)
      x=0.0
      k=0
      do i=1,NN
           Y=SLIPLP(i)
           j=IND(i)
           XX(j)=Y*MacroDefRate%vMeqStrainRate
           if (IPR.eq.2) then
           if (NLIST.eq.1) then
             write (IMP,101) i,IND(i),Y
           end if
           end if
           Y=abs(Y)
           if (x.gt.Y) then
                         x=Y
                         k=k+1
                       endif
      enddo
      if (X.lt.0.0d0) then
      if (NLIST.eq.1) then
      write (IMP,102) IOR,k,X
      end if
      end if
      return
      end subroutine
      !
      Subroutine MINSQU(NN,IND,SLPR,ineg,sumsq,sgnn,IDIMXX)
      use altayIOConfig
      use altayAlgorithms, only: KLEINKWA
      IMPLICIT double precision (A-H,O-Z)
!     December 2000
!     The  normalisation by DELTAT (now: MacroDefRate%vMeqStrainRate) of the september 2000 version has been
!     removed here. Is now done in PANCAK2.
!
!     Modified Aug 2010
!
      COMMON /DOUBLE/ A8(5,96),BB8(5),RHO(5),B5(5)
      dimension sgnn(IDIMXX)
      dimension A(13,13),B(13),SLPR(8),IND(8)
      dimension AA(13,13),BA(13),VAL(13),XV(13),YV(13)
      DATA TOl/1.0d-10/
!      write (IMP,102) NN,(IND(i),i=1,NN)
! 102  format (' MINSQU NN',i5,' IND',8i5)
      if (NN.gt.5) goto 2
      N1=5
      N2=NN
      do i=1,N2
         is=IND(i)
         do j=1,5
            A(j,i)=sgnn(is)*A8(j,is)
         enddo
      enddo
      do j=1,5
         B(j)=BB8(j)
      enddo
      goto 1
  2   N1=NN+5
      N2=N1
!     Set up system of equations
      do i=1,N1
         do j=1,N1
           A(i,j)=0.0
         enddo
      enddo
      do i=1,NN
         is=IND(i)
         A(i,i)=2.D0
         B(i)=0.0
         do j=1,5
            j1=NN+j
            x=sgnn(is)*A8(j,is)
            A(i,j1)=-x
            A(j1,i)=x
         enddo
      enddo
      do j=1,5
         B(NN+j)=BB8(j)
      enddo
!     Solve by least-squares method followed by singular value decomposition
   1  call Kleinkwa(N1,N2,13,13,A,B,AA,BA,VAL,XV,YV,TOL,RES)
!      write (IMP,100) RES
! 100  format(' MINSQU - RES',d15.6)
      do i=1,NN
         SLPR(i)=BA(i)
      enddo
      sumsq=0.0d0
      x=0.0d0
      ineg=0
      do i=1,NN
         is=IND(i)
         Y=SLPR(i) 
         sumsq=sumsq+Y**2
         if (x.gt.Y) then
                       x=Y
                       ineg=i
                     endif
      enddo
!      write (IMP,101) (SLPR(i),i=1,NN)
! 101  format (10F8.5)
      if (RES.gt.(10000.0*TOL)) ineg=-1
!      write (IMP,103) INEG,RES,sumsq
! 103  format (' MINSQU INEG',i5,'  RES',d15.6,'  sumsq',d15.6)
!      write (IMP,915) (SLPR(i),i=1,NN)
! 915  format (6D15.3)
      return
      end subroutine
      !
      subroutine STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
      use altayIOConfig
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif      
      IMPLICIT double precision (A-H,O-Z)
      dimension SLPR(8),IND(8),ISTOR(0:8,48),SLSTOR(0:8,48)
      NOPL=NOPL+1
      if (NOPL.gt.NSTOR) then
#ifndef ALTAY_SUBROUTINE
         if (NLIST.eq.1) then
         write (IMP,100)
         end if
         write (*,100)
         call terminate(stopcode_runtimeerror)
#else
         RCM_RAISE(1,'STORE',                                            &
         'Too small dimension NSTOR in SLIPRAT',RCM_RTN)
#endif
      endif
 100  format (' STORE - increase dimension NSTOR in SLIPRAT,STORE')
      ISTOR(0,NOPL)=NN
      SLSTOR(0,NOPL)=SUMSQ
      do i=1,NN
         ISTOR(i,NOPL)=IND(i)
         SLSTOR(i,NOPL)=SLPR(i)
      enddo
      return
      end subroutine

      end module
      