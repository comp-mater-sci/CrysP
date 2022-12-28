#include "altayRCM.fpp"

module altaySliprate
    use altay_definitions
    use altayMiscutils
    use altay_log

    implicit none

    character(len=*), parameter :: MODULE_NAME = "altaySliprate"

    contains

    subroutine SLIPRAT(M11,IDIMXX,XX,IOR,IPR,sgnn,MacroDefRate)
        use altayIOConfig,IIPR=>IPR !Rename the global IPR to avoid conflict
        use altayRCM
        use altayMacroKinematic
        type(DeformationRate),intent(in) :: MacroDefRate
        !     To find the slip rates assuming that
        !     - the stress, strain rate and the active slip systems are known,
        !       previously obtained by PANCAK2;
        !     - (under the above resrtrictions) the sum of the squares of the slip
        !       rates must be minimal.
        !
        integer, intent(in) :: M11,IDIMXX,IPR,IOR
        real(dp) :: XX(IDIMXX), SGNN(IDIMXX)

        real(dp) :: A1,BB8,RHO,B5
        COMMON /DOUBLE/ A1(5,96),BB8(5),RHO(5),B5(5)

        real(dp) :: SLIPLP,TLXX,TAURLP
        integer:: NACTIV,INDACT,NLP,INDLP
        COMMON /ACTIVE/ NACTIV,INDACT(8),NLP,INDLP(8),SLIPLP(8),TLXX,TAURLP(8)

        integer :: IND(8),ISTOR(0:8,48)
        real(dp) :: SLPR(8),SLSTOR(0:8,48),x,y,yy,sumsq
        integer, parameter :: NSTOR=48
        integer :: i,j,k,i1,i2,i3,N0,N1,N2,N3,NN,NOPL,INEG,IOPL

        do j=1,M11
           XX(j)=0.0
        enddo
        NLP=NACTIV
        NN=NACTIV
        NOPL=0
        ! check whether solution is totally zero
        x=0.0
        do i=1,NLP
            x=x+abs(SLIPLP(i))
            j=INDACT(i)
            sgnn(j)=1.D0
            if (TAURLP(i).lt.0.0d0) sgnn(j)=-1.D0
        enddo
        if (x.lt.TLXX) goto 6
        ! end of check
        do i=1,NN
          IND(i)=INDACT(i)
        enddo
        call MINSQU(NN,IND,SLPR,ineg,sumsq,sgnn,IDIMXX)
        if (ineg.eq.0) then
             call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
             RCM_GUARD
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
       if (N1.lt.5) goto 6
       NN=N1
       do I1=1,N0
            call MINSQU(N1,IND,SLPR,ineg,sumsq,sgnn,IDIMXX)
            if (ineg.eq.0) then
               call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
               RCM_GUARD
            endif
 110   format (10i5)
            J=N0-I1
            if (J.gt.0) IND(J)=INDACT(J+1)
       enddo
       !
       ! Level 2
       !
        N2=N1-1
        if (N2.lt.5) goto 2
        NN=N2
        do I1=2,N0
            do I2=1,I1-1
                j=1
                do i=1,N0
                    if (i.eq.I1.or.i.eq.I2) cycle
                    IND(j)=INDACT(i)
                    j=j+1
                enddo
                call MINSQU(N2,IND,SLPR,ineg,sumsq,sgnn,IDIMXX)
                if (ineg.eq.0) then
                    call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
                    RCM_GUARD
                endif
            enddo
        enddo
        !
        ! Level 3
        !
        N3=N2-1
        if (N3.lt.5) goto 2
        NN=N3
        do I1=3,N0
            do I2=2,I1-1
                do I3=1,I2-1
                  j=1
                  do i=1,N0
                      if (i.eq.I1.or.i.eq.I2.or.i.eq.I3) cycle
                      IND(j)=INDACT(i)
                      j=j+1
                  enddo
                  call MINSQU(N3,IND,SLPR,ineg,sumsq,sgnn,IDIMXX)
                  if (ineg.eq.0) then
                      call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
                      RCM_GUARD
                  endif
                enddo
            enddo
        enddo
   2    if (IPR.eq.2 .and. NLIST.eq.1) write (IMP,100)
 100    format (' Results SLIPRAT')
        if (NOPL.eq.0) goto 6
        IOPL=0
        X=1.0d10
        do i=1,NOPL
            Y=SLSTOR(0,i)
            !
            !  Y is de te minimaliseren waarde van oplossing i
            !  Uitprinten!
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
        if (IPR.eq.2 .and. NLIST.eq.1) then
            write (IMP,104) IOR,NACTIV,NN,NOPL
            write (IMP,106) IOR,sumsq,(IND(i),i=1,NN)
 104        format (I5,' Reduction of NACTIV from',I5,'   to',i5,' NOPL=',i5)
 106        format (I5,d12.3,8i5)
        endif
        x=0.0
        k=0
        do i=1,NN
            j=IND(i)
            Y=SLPR(i)
            YY=Y*sgnn(j)
            XX(j)=YY*MacroDefRate%vMeqStrainRate
            if (IPR.eq.2 .and. NLIST.eq.1) write (IMP,101) i,IND(i),YY
            if (x.gt.Y) then
                x=Y
                k=k+1
            endif
        enddo
        if (X.lt.0.0d0 .and. NLIST.eq.1) write (IMP,102) IOR,k,X
 102    format (' NEG. SL. RATE DETECTED',2I5,d15.6)
 101    format (2i5,5x,d15.6)
        return
  6     continue
        NN=NLP
        do i=1,NN
            IND(i)=INDLP(i)
        enddo
        if (IPR.eq.2 .and. NLIST.eq.1) then
            write (IMP,100)
            write (IMP,108) IOR,NN
        end if
 108    format (' IOR=',I5,' Linear programming solution retained ',' NN=',i5)
        x=0.0
        k=0
        do i=1,NN
             Y=SLIPLP(i)
             j=IND(i)
             XX(j)=Y*MacroDefRate%vMeqStrainRate
             if (IPR.eq.2 .and. NLIST.eq.1) write (IMP,101) i,IND(i),Y
             Y=abs(Y)
             if (x.gt.Y) then
                 x=Y
                 k=k+1
             endif
        enddo
        if (X.lt.0.0d0 .and. NLIST.eq.1) write (IMP,102) IOR,k,X
    end subroutine


    subroutine MINSQU(NN,IND,SLPR,ineg,sumsq,sgnn,IDIMXX)
        use altayIOConfig
        use altayAlgorithms, only: KLEINKWA

        integer, intent(in) :: IND(8), NN,IDIMXX
        integer, intent(out) :: ineg
        real(dp), intent(out) :: SLPR(8),sumsq


        real(dp) :: A8,BB8,RHO,B5
        COMMON /DOUBLE/ A8(5,96),BB8(5),RHO(5),B5(5)

        real(dp) :: sgnn(IDIMXX)
        real(dp) :: A(13,13),B(13),RES,x,Y
        real(dp) :: AA(13,13),BA(13),VAL(13),XV(13),YV(13)
        real(dp), parameter :: TOL=1.0e-10_dp
        integer :: i,j,N1,N2

        if (.not. (NN.gt.5)) then
            N1=5
            N2=NN
            do i=1,N2
               do j=1,5
                  A(j,i)=sgnn(IND(i))*A8(j,IND(i))
               enddo
            enddo
            B(1:5)=BB8(1:5)
        else
            N1=NN+5
            N2=N1
            ! Set up system of equations
            A(1:N1,1:N1)=0.0_dp
            do i=1,NN
               A(i,i)=2.0_dp
               B(i)=0.0
               do j=1,5
                  x=sgnn(IND(i))*A8(j,IND(i))
                  A(i,NN+j)=-x
                  A(NN+j,i)=x
               enddo
            enddo
            B(1+NN:5+NN)=BB8(1:5)
        endif

        ! Solve by least-squares method followed by singular value decomposition
        call Kleinkwa(N1,N2,13,13,A,B,BA,RES)

        SLPR(1:NN)=BA(1:NN)
        sumsq=0.0_dp
        x=0.0d0
        ineg=0
        do i=1,NN
           Y=SLPR(i)
           sumsq=sumsq+Y**2
           if (x.gt.Y) then
               x=Y
               ineg=i
           endif
        enddo
        if (RES.gt.(10000.0*TOL)) then
            ineg=-1
            call vef_trace(MODULE_NAME,'MINSQU', 'RES too large')
        end if
    end subroutine

    subroutine STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
        use altayIOConfig
        use altayRCM

        integer, intent(in)::NN, IND(8),NSTOR
        integer, intent(inout)::NOPL,ISTOR(0:8,48)
        real(dp), intent(inout)::SLSTOR(0:8,48)
        real(dp), intent(in) :: SLPR(8), sumsq

        NOPL=NOPL+1
        if (NOPL.gt.NSTOR) then
           RCM_RAISE(1,'STORE','Too small dimension NSTOR in SLIPRAT',RCM_RTN)
        endif
 100    format (' STORE - increase dimension NSTOR in SLIPRAT,STORE')
        ISTOR(0,NOPL)=NN
        ISTOR(1:NN,NOPL)=IND(1:NN)
        SLSTOR(0,NOPL)=SUMSQ
        SLSTOR(1:NN,NOPL)=SLPR(1:NN)

    end subroutine

end module
