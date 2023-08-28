module altaySliprate
    use utils
    use logging
    use altayMacroKinematic
    use altayAlgorithms

    implicit none
    private
    character(len=*), parameter :: MOD_NAME = "altaySliprate"

    public :: SLIPRAT
    contains

    subroutine SLIPRAT(IDIMXX,XX,sgnn,MacroDefRate,NACTIV,SLIPLP,TLXX,TAURLP,INDACT,INDLP,BB8,A1)
        type(DeformationRate),intent(in) :: MacroDefRate
        !     To find the slip rates assuming that
        !     - the stress, strain rate and the active slip systems are known,
        !       previously obtained by PANCAK2;
        !     - (under the above restrictions) the sum of the squares of the slip
        !       rates must be minimal.
        !
        integer, intent(in) :: IDIMXX,NACTIV,INDLP(8)
        real(DP), intent(in) :: TLXX,A1(:,:),TAURLP(8),BB8(5),SLIPLP(8)
        integer, intent(inout) :: INDACT(8)
        real(DP), intent(inout) :: SGNN(IDIMXX),XX(IDIMXX)

        integer :: IND(8),ISTOR(0:8,48)
        real(DP) :: SLPR(8),SLSTOR(0:8,48),sumsq
        integer, parameter :: NSTOR=48
        integer :: j,i1,i2,i3,N0,N1,N2,N3,NN,NOPL,INEG,IOPL

        XX(1:IDIMXX)=0.0_DP
        NN=NACTIV
        NOPL=0
        sgnn(INDACT(1:NACTIV))=sign(1.0_dp,TAURLP(1:NACTIV))

        ! check whether solution is totally zero
        if (sum(abs(SLIPLP(1:NACTIV))) >= TLXX) then
            IND(1:NN)=INDACT(1:NN)
            call MINSQU(NN,IND,SLPR,ineg,sumsq,sgnn,IDIMXX,BB8,A1)
            if (ineg==0) then
                 call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
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
                         if (ineg==0) call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
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
                                if (ineg==0) call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
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
                                      if (ineg==0) call STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)
                                    enddo
                                enddo
                            enddo
                        endif
                    endif
   2                if (NOPL/=0) then
                        IOPL=minloc(SLSTOR(0,1:NOPL),1)
                        NN=ISTOR(0,IOPL)
                        sumsq=SLSTOR(0,IOPL)
                        IND(1:NN)=ISTOR(1:NN,IOPL)
                        SLPR(1:NN)=SLSTOR(1:NN,IOPL)
                        XX(IND(1:NN))=SLPR(1:NN)*sgnn(IND(1:NN))*MacroDefRate%vMeqStrainRate
                        return
                    endif
                endif
            endif
        endif
        NN=NACTIV
        IND(1:NN)=INDLP(1:NN)
        XX(IND(1:NN))=SLIPLP(1:NN)*MacroDefRate%vMeqStrainRate
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
        real(DP),intent(in) :: A8(:,:),BB8(5),sgnn(IDIMXX)
        integer, intent(out) :: ineg
        real(DP), intent(out) :: SLPR(8),sumsq

        real(DP) :: A(13,13),B(13),RES,x,BA(13)
        real(DP), parameter :: TOL=1.0e-6_dp
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
               do j=1,5
                  A(NN+j,i)=sgnn(IND(i))*A8(j,IND(i))
                  A(i,NN+j)=-A(NN+j,i)
               enddo
            enddo
            B(1:NN)=0.0_DP
            B(1+NN:5+NN)=BB8
        endif

        ! Solve by least-squares method followed by singular value decomposition
        call Kleinkwa(N1,N2,13,13,A,B,BA,RES)

        SLPR(1:NN)=BA(1:NN)
        sumsq=sum(SLPR(1:NN)**2)
        if (RES > TOL) then
            ineg=-1
            call log_trace(MOD_NAME,'MINSQU', 'RES too large')
        else
            x=0.0_dp
            ineg=0
            do i=1,NN
               if (x > SLPR(i)) then
                   x=SLPR(i)
                   ineg=i
               endif
            enddo
        end if
    end subroutine

    subroutine STORE(NSTOR,NOPL,NN,SLPR,IND,ISTOR,SLSTOR,SUMSQ)

        integer, intent(in)::NN, IND(8),NSTOR
        integer, intent(inout)::NOPL,ISTOR(0:8,48)
        real(DP), intent(inout)::SLSTOR(0:8,48)
        real(DP), intent(in) :: SLPR(8), sumsq

        NOPL=NOPL+1
        if (NOPL>NSTOR) call log_error(MOD_NAME, 'store', ERR_DIMS, 'Too small dimension NSTOR in SLIPRAT')
        ISTOR(0,NOPL)=NN
        ISTOR(1:NN,NOPL)=IND(1:NN)
        SLSTOR(0,NOPL)=SUMSQ
        SLSTOR(1:NN,NOPL)=SLPR(1:NN)

    end subroutine

end module
