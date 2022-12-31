#include "altayRCM.fpp"

module altaySliprate
    use altay_definitions
    use altayMiscutils
    use altay_log
    use altayIOConfig,IIPR=>IPR !Rename the global IPR to avoid conflict
    use altayMacroKinematic
    use altayRCM
    use altayAlgorithms

    implicit none
    private
    character(len=*), parameter :: MODULE_NAME = "altaySliprate"

    public :: SLIPRAT
    contains

    subroutine SLIPRAT(M11,IDIMXX,XX,IOR,IPR,sgnn,MacroDefRate,NACTIV, &
                       SLIPLP,TLXX,TAURLP,INDACT,NLP,INDLP)
        type(DeformationRate),intent(in) :: MacroDefRate
        !     To find the slip rates assuming that
        !     - the stress, strain rate and the active slip systems are known,
        !       previously obtained by PANCAK2;
        !     - (under the above resrtrictions) the sum of the squares of the slip
        !       rates must be minimal.
        !
        integer, intent(in) :: M11,IDIMXX,IPR,IOR,NACTIV
        real(dp) :: XX(IDIMXX), SGNN(IDIMXX)
        real(dp) :: SLIPLP(8),TLXX,TAURLP(8)
        integer:: INDACT(8),NLP,INDLP(8)

        real(dp) :: A1,BB8
        COMMON /DOUBLE1/ A1(5,96)
        COMMON /DOUBLE2/ BB8(5)

        integer :: IND(8),ISTOR(0:8,48)
        real(dp) :: SLPR(8),SLSTOR(0:8,48),x,y,yy,sumsq
        integer, parameter :: NSTOR=48
        integer :: i,j,k,i1,i2,i3,N0,N1,N2,N3,NN,NOPL,INEG,IOPL

        XX(1:M11)=0.0
        NLP=NACTIV
        NN=NACTIV
        NOPL=0
        ! check whether solution is totally zero
        x=0.0
        do i=1,NLP
            x=x+abs(SLIPLP(i))
            j=INDACT(i)
            sgnn(j)=sign(1.0_dp,TAURLP(i))
        enddo
        if (x>=TLXX) then
            ! end of check
            IND(1:NN)=INDACT(1:NN)
            call MINSQU(NN,IND,SLPR,ineg,sumsq,sgnn,IDIMXX,BB8,A1)
            if (ineg==0) then
                 call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
                 RCM_GUARD
                 if (NN <= 5) goto 2
            endif
            if (NN>5) then
                !
                !     Let us take all combinations of NN out of NACTIV
                !
                !     "Levels" in the combination search:
                !     (first level:  if NACTIV=8, find all combinations of 7 sl. syst.
                !      second level: find all combinations of 6 - etc.)
                !
                N0=NACTIV
                ! Level 1
                N1=N0-1
                if (N1>=5) then
                    NN=N1
                    do I1=1,N0
                         call MINSQU(N1,IND,SLPR,ineg,sumsq,sgnn,IDIMXX,BB8,A1)
                         if (ineg==0) then
                            call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
                            RCM_GUARD
                         endif
                         J=N0-I1
                         if (J>0) IND(J)=INDACT(J+1)
                    enddo
                    ! Level 2
                    N2=N1-1
                    if (N2>=5) then
                        NN=N2
                        do I1=2,N0
                            do I2=1,I1-1
                                call fill(IND,INDACT,[I1,I2],N0)
                                call MINSQU(N2,IND,SLPR,ineg,sumsq,sgnn,IDIMXX,BB8,A1)
                                if (ineg==0) then
                                    call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
                                    RCM_GUARD
                                endif
                            enddo
                        enddo
                        ! Level 3
                        N3=N2-1
                        if (N3>=5) then
                            NN=N3
                            do I1=3,N0
                                do I2=2,I1-1
                                    do I3=1,I2-1
                                      call fill(IND,INDACT,[I1,I2,I3],N0)
                                      call MINSQU(N3,IND,SLPR,ineg,sumsq,sgnn,IDIMXX,BB8,A1)
                                      if (ineg==0) then
                                          call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
                                          RCM_GUARD
                                      endif
                                    enddo
                                enddo
                            enddo
                        endif
                    endif
   2                if (IPR==2 .and. NLIST==1) write (IMP,100)
 100                format (' Results SLIPRAT')
                    if (NOPL==0) goto 6
                    IOPL=0
                    X=1.0d10
                    do i=1,NOPL
                        Y=SLSTOR(0,i)
                        !  Y is de te minimaliseren waarde van oplossing i
                        !  Uitprinten!
                        if (Y<X) then
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
                    if (IPR==2 .and. NLIST==1) then
                        write (IMP,104) IOR,NACTIV,NN,NOPL
                        write (IMP,106) IOR,sumsq,(IND(i),i=1,NN)
 104                    format (I5,' Reduction of NACTIV from',I5,'   to',i5,' NOPL=',i5)
 106                    format (I5,d12.3,8i5)
                    endif
                    x=0.0_dp
                    k=0
                    do i=1,NN
                        j=IND(i)
                        Y=SLPR(i)
                        YY=Y*sgnn(j)
                        XX(j)=YY*MacroDefRate%vMeqStrainRate
                        if (IPR==2 .and. NLIST==1) write (IMP,101) i,IND(i),YY
                        if (x>Y) then
                            x=Y
                            k=k+1
                        endif
                    enddo
                    if (X<0.0d0 .and. NLIST==1) write (IMP,102) IOR,k,X
 102                format (' NEG. SL. RATE DETECTED',2I5,d15.6)
 101                format (2i5,5x,d15.6)
                    return
                endif
            endif
        endif
  6     continue
        NN=NLP
        do i=1,NN
            IND(i)=INDLP(i)
        enddo
        if (IPR==2 .and. NLIST==1) then
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
             if (IPR==2 .and. NLIST==1) write (IMP,101) i,IND(i),Y
             Y=abs(Y)
             if (x>Y) then
                 x=Y
                 k=k+1
             endif
        enddo
        if (X<0.0d0 .and. NLIST==1) write (IMP,102) IOR,k,X
    end subroutine

    subroutine fill(IND_,INDACT_,skip,N_max)
        integer, intent(inout) :: IND_(8)
        integer, intent(in) :: INDACT_(8), N_max
        integer, dimension(:), intent(in) :: skip
        integer :: i,j

        j=1
        do i=1,N_max
            if (any(i==skip)) cycle
            IND_(j)=INDACT_(i)
            j=j+1
        enddo

    end subroutine

    subroutine MINSQU(NN,IND,SLPR,ineg,sumsq,sgnn,IDIMXX,BB8,A8)

        integer, intent(in) :: IND(8), NN,IDIMXX
        real(dp),intent(in) :: A8(5,96),BB8(5),sgnn(IDIMXX)
        integer, intent(out) :: ineg
        real(dp), intent(out) :: SLPR(8),sumsq

        real(dp) :: A(13,13),B(13),RES,x,Y,BA(13)
        real(dp), parameter :: TOL=1.0e-6_dp
        integer :: i,j,N1,N2

        if (NN<=5) then
            N1=5
            N2=NN
            do i=1,N2
                 A(1:5,i)=sgnn(IND(i))*A8(1:5,IND(i))
            enddo
            B(1:5)=BB8
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
            B(1+NN:5+NN)=BB8
        endif

        ! Solve by least-squares method followed by singular value decomposition
        call Kleinkwa(N1,N2,13,13,A,B,BA,RES)

        SLPR(1:NN)=BA(1:NN)
        sumsq=0.0_dp
        x=0.0_dp
        ineg=0
        do i=1,NN
           Y=SLPR(i)
           sumsq=sumsq+Y**2
           if (x > Y) then
               x=Y
               ineg=i
           endif
        enddo
        if (RES > TOL) then
            ineg=-1
            call vef_trace(MODULE_NAME,'MINSQU', 'RES too large')
        end if
    end subroutine

    subroutine STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)

        integer, intent(in)::NN, IND(8),NSTOR
        integer, intent(inout)::NOPL,ISTOR(0:8,48)
        real(dp), intent(inout)::SLSTOR(0:8,48)
        real(dp), intent(in) :: SLPR(8), sumsq

        NOPL=NOPL+1
        if (NOPL>NSTOR) then
            RCM_RAISE(1,'STORE','Too small dimension NSTOR in SLIPRAT',RCM_RTN)
        endif
 100    format (' STORE - increase dimension NSTOR in SLIPRAT,STORE')
        ISTOR(0,NOPL)=NN
        ISTOR(1:NN,NOPL)=IND(1:NN)
        SLSTOR(0,NOPL)=SUMSQ
        SLSTOR(1:NN,NOPL)=SLPR(1:NN)

    end subroutine

end module
