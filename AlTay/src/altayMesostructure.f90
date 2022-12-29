!> Microstructure representation in AlTay
!> The microstructure is created by grain boundary segments.
module altayMesostructure
    use criMathUtils
    use altayAlgorithms
    use altayMiscutils
    use altay_definitions
    use altayMacroKinematic

    implicit none
    !> Transformation matrix associated to the grain boundary reference frame
    !> in the initial state.
    !> Shape is: [3,3,ngr], where ngr is the number of grains.
    real(dp), dimension(:,:,:),allocatable :: TmatGr
    integer :: NGrElm = 0             !< Number of grain boundary orientations
    character(len=40)  :: TitMic = '' !< Microstructure title

contains

    !> Reading of "microstructure" (Euler angles defining
    !> grain boundary segments) in SMT-format, allocation
    !> and assignment of the module variables.
    subroutine GRFIL(fnam,F_mic,ierr)
        use altayIOConfig

        integer,intent(out)         :: ierr
        character(len=*),intent(in) :: fnam !< Microstructure file name
        !> F_mic is a deformation gradient that conceptually
        !> 'deforms' a spherical grain into an ellipsoidal shape
        real(dp), dimension(3,3), intent(in) :: F_mic

        integer           :: IGrElm !< Counter for loop over GBs
        type(EulerAngles) :: EulGB
        real(dp), dimension(3,3) :: T

        ierr = -1
        if(NLIST == 1) write (IMP,103) fnam
    103  format (' GRFIL - Input microstructure file:' ,a)

        open (unit=NDAT2,file=fnam,status='old',iostat=ierr)
        if (ierr /= 0) return

        read (NDAT2,94) NGrElm,TitMic ! read number of GBs and title
    94  format(I5,5x,A)
#ifndef NO_STDOUT
        write (*,93) NGrElm,TitMic
#endif
        if(NLIST == 1) write (IMP,93) NGrElm,TitMic
      93  format (' Number of orientations in MICROSTRUCTURE file:' ,I5,/,' Title in file: ',A)

        allocate(TmatGr(3,3,NGrElm),STAT=ierr)
        if (ierr /= 0) then
            if(NLIST == 1) write(IMP,102)
            return
        end if
      102  format (' GRFIL - Allocation of memory failed')

        do IGrElm=1,NGrElm
            read (NDAT2,96) EulGB%fi2,EulGB%PHI,EulGB%fi1 ! read Euler angles from microstructure file in order: phi2, PHI, phi1
            !Calc. the transformation matrix T
            T = rotmat(deg2rad(EulGB))
            !TmatGr(1:3,i,IGrElm) for i=1,2 holds two non-parallel vectors
            !  within the initial GB (grain boundary) plane.
            !TmatGr(1:3,i,IGrElm) for i=3 holds a vector out of the initial
            !  GB plane (not necessarily perpendicular to the GB plane).
            TmatGr(:,:,IGrElm)=matmul(F_mic,transpose(T))
        enddo
      96 format(3F10.0)

        close(unit=NDAT2)
        ierr = 0
    end subroutine GRFIL

    !> Finalizes the module. The subroutine puts the module variables
    !> into initial state and deallocates the storage.
    subroutine MICROSTR_finalize(info)
         integer,intent(out)     :: info

         NGrElm = 0
         TitMic = ''
         if (allocated(TmatGr)) deallocate(TmatGr,stat=info)

    end subroutine

    subroutine CLUSTER1(NGR,IGrElm,MacroDefRate,MacroDefState,GEWF,Tprinc,Cofcos,Cofsin)
    !   IF both relaxations are orthogonal:
    !      Cofcos=0 and Cofsin=0 is returned
    !   ELSE:
    !      Cofcos and Cofsin are the cosine and sine of the angle for relaxation-1
    !
    !   relaxation-2 is always the orthogonal one.
    !   TDC is the normalized von-Mises equivalent strain rate
        use altayIOConfig, only: IPR,NLIST,IMP

        integer,intent(in)                     :: NGR,IGrElm
        type(DeformationRate),intent(in)       :: MacroDefRate
        type(DeformationState),intent(in)      :: MacroDefState
        real(dp),intent(inout)                 :: GEWF
        real(dp),dimension(3,3),intent(out)    :: Tprinc
        real(dp),intent(out)                   :: Cofcos, Cofsin

        real(dp) :: AXX(3,3),GRPAR(3,3), PrDir(3,3),TDCGr(3,3), vec1(3),vec2(3),AL(3),AA(3)
        real(dp) :: x, u, dlength, dot1, dot2, TGANGLE, Y
        integer :: i,j
        real(dp), parameter, dimension(3,3) :: &
            relaxI = reshape([0._dp, 0._dp, 1._dp, &
                              0._dp, 0._dp, 0._dp, &
                              1._dp, 0._dp, 0._dp],shape(relaxI)), &
            relaxII= reshape([0._dp, 0._dp, 0._dp, &
                              0._dp, 0._dp, 1._dp, &
                              0._dp, 1._dp, 0._dp],shape(relaxII))

        Cofcos = 0.D0
        Cofsin = 0.D0

        if (NGR == 1) then      ! let Tprinc be equal to the identity matrix.
            Tprinc = unitMatrix
            return
        end if

        GRPAR = matmul(MacroDefState%TotalDefGrad,TmatGr(:,:,IGrElm))
        if ((IPR > 1) .and.(NLIST == 1)) then
            write (IMP,409) IGrElm
            409 format (' IGrElm = ',i5)
            do i=1,3
                write (IMP,407) (TmatGr(j,i,IGrElm),j=1,3)
            enddo
            407 format (' TmatGr ',3d15.7)
            do i=1,3
                write (IMP,408) (GRPAR(j,i),j=1,3)
            enddo
            408 format (' GRPAR  ',3d15.7)
        endif

        if ((IPR > 0) .and. (NLIST == 1) )then
            write (IMP,100)
            100  format (//,' CLUSTER1')
        end if
        !     Calculation of volume affected by the surface
        do i=1,3
            x=0.0
            do j=1,3
                X=X+GRPAR(j,i)**2
            enddo
            AL(i)=sqrt(X)
        enddo
        ! Box product
        vec1(1)=GRPAR(2,2)*GRPAR(3,3)-GRPAR(3,2)*GRPAR(2,3)
        vec1(2)=GRPAR(3,2)*GRPAR(1,3)-GRPAR(1,2)*GRPAR(3,3)
        vec1(3)=GRPAR(1,2)*GRPAR(2,3)-GRPAR(2,2)*GRPAR(1,3)
        u=0.0D0
        do i=1,3
            u=u+GRPAR(i,1)*vec1(i)
        enddo
        ! The factor 0.25 is there so that for equiaxed grains, GEWF below becomes 1/3;
        ! for very flattened grains, it should tend to 1.
        u=abs(u)*0.25D0/(AL(1)*AL(2)*AL(3))
        !     re-order the basisvectors so that AA(1)>=AA(2)>=AA(3)
        !     find out which one of these corresponds to the original AL(3)
        if (AL(2) <= AL(3).and.AL(1) <= AL(3)) then    ! AL(3) is the longest
            AA(1)=AL(3)
            if(AL(2) >= AL(1))then
                AA(2)=AL(2)
                AA(3)=AL(1)
            else
                AA(2)=AL(1)
                AA(3)=AL(2)
            endif
            GEWF=u*(2.0D0*(AA(2)-AA(3))*AA(3)**2+4.D0*AA(3)**3/3.0D0)
        elseif (AL(3) <= AL(1).and.AL(3) <= AL(2)) then   ! AL(3) is the shortest
            AA(3)=AL(3)
            if(AL(1) >= AL(2))then
                  AA(1)=AL(1)
                  AA(2)=AL(2)
            else
                  AA(1)=AL(2)
                  AA(2)=AL(1)
            endif
            GEWF=u*(4.D0*(AA(1)-AA(3))*(AA(2)-AA(3))*AA(3)  &
                  +2.0D0*(AA(2)-AA(3))*AA(3)**2+2.0D0*(AA(1)-AA(3))*AA(3)**2 &
                  +4.D0*AA(3)**3/3.D0)
        else                                          ! AL(3) is neither shortest nor longest
            AA(2)=AL(3)
            if(AL(1) >= AL(2))then
                AA(1)=AL(1)
                AA(3)=AL(2)
            else
                AA(1)=AL(2)
                AA(3)=AL(1)
            endif
            GEWF=u*(2.D0*(AA(1)-AA(3))*AA(3)**2+4.D0*AA(3)**3/3.D0)
        endif
        if ((IPR > 0) .and. (NLIST == 1)) write (IMP,103) GEWF
        103  format (/,' GEWF ',3d15.7,/)

        ! Construction of orientation matrices for frames associated to the
        ! interfaces
        AXX(1:3,1)=GRPAR(1:3,1)
        ! Orientation of interfaces containing axes
        ! Normal axis: (vector product)
        AXX(1,3)=GRPAR(2,1)*GRPAR(3,2)-GRPAR(3,1)*GRPAR(2,2)
        AXX(2,3)=GRPAR(3,1)*GRPAR(1,2)-GRPAR(1,1)*GRPAR(3,2)
        AXX(3,3)=GRPAR(1,1)*GRPAR(2,2)-GRPAR(2,1)*GRPAR(1,2)
        !       Orientation of 2nd axis:(vector product)
        AXX(1,2)=AXX(2,3)*AXX(3,1)-AXX(3,3)*AXX(2,1)
        AXX(2,2)=AXX(3,3)*AXX(1,1)-AXX(1,3)*AXX(3,1)
        AXX(3,2)=AXX(1,3)*AXX(2,1)-AXX(2,3)*AXX(1,1)
        !       Normalisation
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
                Tprinc(i,j)=AXX(j,i)
            enddo
            if (IPR > 0 .and. NLIST == 1) write (IMP,102) (Tprinc(i,j),j=1,3)
        102 format (' TGrb ',3d15.7)
        enddo

        dlength=norm2(MacroDefRate%StrainModevM)
        !     Transform MacroDefRate%StrainModevM to the "Grb" reference frame
        TDCGr = rotateSRTensorFrom(MacroDefRate%StrainModevM,Tprinc)

        dot1=0.0
        dot2=0.0
        do i=1,3
            do j=1,3
                dot1=dot1+relaxI(i,j)*TDCGr(i,j)
                dot2=dot2+relaxII(i,j)*TDCGr(i,j)
            enddo
        enddo
        dot1=dot1/sqrt(2.0D0)/dlength
        dot2=dot2/sqrt(2.0D0)/dlength

        if(abs(dot1) < 0.000001.and.abs(dot2) < 0.000001) then
            ! both relaxations are orthogonal
            Cofcos=0.0
            Cofsin=0.0
        elseif(abs(dot1) < 0.000001) then
            if(abs(dot2-1.D0) < 0.00001) then
                  !  Need to rotate current frame (represented by Tprinc) with 90 degree to let relaxation-2 be the orthogonal one
                  !  new axe-1 be old axe-2
                  !  new axe-2 be minus old axe-1
                  vec1=AXX(1:3,2)
                  AXX(1:3,2)=-AXX(1:3,1)
                  AXX(1:3,1)=vec1
                  Tprinc = transpose(AXX)
                  Cofcos=1.D0
                  Cofsin=0.D0
            else
                 !  Need to rotate current frame (represented by Tprinc) with 90 degree to let relaxation-2 be the orthogonal one
                 !  new axe-1 be old axe-2
                 !  new axe-2 be minus old axe-1
                 !  The angle is only between relaxation-1 and D0. It is nothing related with relaxation-2.
                 vec1=AXX(1:3,2)
                 AXX(1:3,2)=-AXX(1:3,1)
                 AXX(1:3,1)=vec1
                 Tprinc = transpose(AXX)
                 ! Transform MacroDefRate%StrainModevM to the new "Grb" reference frame
                 TDCGr = rotateSRTensorFrom(MacroDefRate%StrainModevM,Tprinc)
                 ! make sure relaxation-2 is orthogonal
                 ! calculate the cosine for relaxation-1
                 dot2=0.0
                 dot1=0.0
                 do i=1,3
                     do j=1,3
                         dot2=dot2+relaxII(i,j)*TDCGr(i,j)
                         dot1=dot1+relaxI(i,j)*TDCGr(i,j)
                     enddo
                 enddo
                 !   normalize
                 dot1=dot1/sqrt(2.0D0)/dlength

                 Cofcos=dot1
                 Cofsin=sqrt(1.0D0-dot1*dot1)
            endif

        elseif(abs(dot2) < 0.000001) then
            ! Relaxation-2 is already a orthogonal one
            ! calculate the cosine for relaxation-1
            if(abs(dot1-1.0D0) < 0.00001) then
                  Cofcos=1.0D0
                  Cofsin=0.0D0
            else
                  Cofcos=dot1
                  Cofsin=sqrt(1.0D0-dot1*dot1)
            endif
        else
            ! need to rotate by a angle < 90 (this angle could be positive or negative)
            tgangle=dot2/dot1
            PrDir=0.0
            PrDir(1,1)=1.D0/sqrt(1.D0+tgangle*tgangle)
            PrDir(1,2)=tgangle/sqrt(1.D0+tgangle*tgangle)
            PrDir(2,1)=-PrDir(1,2)
            PrDir(2,2)=PrDir(1,1)
            PrDir(3,3)=1.0D0

            !   Prdir(1,) is vector-1 in the GB frame
            !   Prdir(2,) is vector-2 in the GB frame
            !   Transform these two vector in the Sample's frame
            vec1=0.0
            vec2=0.0
            do i=1,3
                do j=1,3
                    vec1(i)=vec1(i)+AXX(i,j)*PrDir(1,j)
                    vec2(i)=vec2(i)+AXX(i,j)*PrDir(2,j)
                enddo
            enddo

            AXX(1:3,1)=vec1
            AXX(1:3,2)=vec2
            Tprinc = transpose(AXX)

            ! Transform MacroDefRate%StrainModevM to the new "Grb" reference frame
            TDCGr = rotateSRTensorFrom(MacroDefRate%StrainModevM,Tprinc)
            !  make sure relaxation-2 is orthogonal
            dot2=0.0
            dot1=0.0
            do i=1,3
                do j=1,3
                    dot2=dot2+relaxII(i,j)*TDCGr(i,j)
                    dot1=dot1+relaxI(i,j)*TDCGr(i,j)
                enddo
            enddo
            ! normalize
            dot1=dot1/sqrt(2.0D0)/dlength

            Cofcos=dot1
            Cofsin=sqrt(1.0D0-dot1*dot1)
        endif

    end subroutine

end module
