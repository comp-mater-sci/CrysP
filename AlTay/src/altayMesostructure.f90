!> Microstructure representation in AlTay
!> The microstructure is created by grain boundary segments.
module altayMesostructure
    use criMathUtils
    use altayAlgorithms
    use altayMiscutils
    use definitions
    use altayMacroKinematic

    implicit none
    private

    !> Transformation matrix associated to the grain boundary reference frame
    !> in the initial state.
    !> Shape is: [3,3,ngr], where ngr is the number of grains.
    real(dp), dimension(:,:,:),allocatable :: TmatGr
    integer, public, protected :: NGrElm = 0             !< Number of grain boundary orientations
    character(len=40)  :: TitMic = '' !< Microstructure title

    public :: &
        GRFIL, &
        MICROSTR_finalize, &
        CLUSTER1

    contains

    !> Reading of "microstructure" (Euler angles defining grain boundary segments)
    !> in SMT-format, allocation and assignment of the module variables.
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

    subroutine CLUSTER1(IGrElm,MacroDefRate,MacroDefState,GEWF,Tprinc)
    !   TDC is the normalized von-Mises equivalent strain rate
        use altayIOConfig, only: IPR,NLIST,IMP

        integer,intent(in)                     :: IGrElm
        type(DeformationRate),intent(in)       :: MacroDefRate
        type(DeformationState),intent(in)      :: MacroDefState
        real(dp),intent(out)                   :: GEWF
        real(dp),intent(out)                   :: Tprinc(3,3)

        real(dp) :: GRPAR(3,3), PrDir(2,3),TDCGr(3,3), vec1(3),vec2(3),AL(3),AA(3)
        real(dp) :: x, u, dlength, dot1, dot2, TGANGLE
        integer :: i,j
        real(dp), parameter, dimension(3,3) :: &
            relaxI = reshape([0._dp, 0._dp, 1._dp, &
                              0._dp, 0._dp, 0._dp, &
                              1._dp, 0._dp, 0._dp],shape(relaxI)), &
            relaxII= reshape([0._dp, 0._dp, 0._dp, &
                              0._dp, 0._dp, 1._dp, &
                              0._dp, 1._dp, 0._dp],shape(relaxII))


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
        ! Calculation of volume affected by the surface
        AL=norm2(GRPAR,1)
        ! Box product
        vec1(1)=GRPAR(2,2)*GRPAR(3,3)-GRPAR(3,2)*GRPAR(2,3)
        vec1(2)=GRPAR(3,2)*GRPAR(1,3)-GRPAR(1,2)*GRPAR(3,3)
        vec1(3)=GRPAR(1,2)*GRPAR(2,3)-GRPAR(2,2)*GRPAR(1,3)
        ! The factor 0.25 is there so that for equiaxed grains, GEWF below becomes 1/3;
        ! for very flattened grains, it should tend to 1.
        u=abs(sum(GRPAR(:,1)*vec1))*0.25D0/product(AL)

        if (minloc(AL,1) == 3) then
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
            ! MD: equivalent except for floating point arithmetic
            ! GEWF=u*(4.D0*(AL(1)-AL(3))*(AL(2)-AL(3))*AL(3)  &
            !         +2.0D0*(AL(1)+AL(2)-2.0_dp*AL(3))*AL(3)**2 &
            !         +4.D0*AL(3)**3/3.D0)
        else
            if(AL(1) >= AL(2))then
                GEWF=u*(2.D0*(AL(1)-AL(2))*AL(2)**2+4.D0*AL(2)**3/3.D0)
            else
                GEWF=u*(2.D0*(AL(2)-AL(1))*AL(1)**2+4.D0*AL(1)**3/3.D0)
            endif
        endif
        if ((IPR > 0) .and. (NLIST == 1)) write (IMP,103) GEWF
        103  format (/,' GEWF ',3d15.7,/)

        ! Construction of orientation matrices for frames associated to the interfaces
        Tprinc(1,1:3)=GRPAR(1:3,1)
        ! Orientation of interfaces containing axes
        ! Normal axis: (vector product)
        Tprinc(3,1)=GRPAR(2,1)*GRPAR(3,2)-GRPAR(3,1)*GRPAR(2,2)
        Tprinc(3,2)=GRPAR(3,1)*GRPAR(1,2)-GRPAR(1,1)*GRPAR(3,2)
        Tprinc(3,3)=GRPAR(1,1)*GRPAR(2,2)-GRPAR(2,1)*GRPAR(1,2)
        !       Orientation of 2nd axis:(vector product)
        Tprinc(2,1)=Tprinc(3,2)*Tprinc(1,3)-Tprinc(3,3)*Tprinc(1,2)
        Tprinc(2,2)=Tprinc(3,3)*Tprinc(1,1)-Tprinc(3,1)*Tprinc(1,3)
        Tprinc(2,3)=Tprinc(3,1)*Tprinc(1,2)-Tprinc(3,2)*Tprinc(1,1)
        !       Normalisation
        do j=1,3
            x=norm2(Tprinc(j,:))
            Tprinc(j,:)=Tprinc(j,:)/x
            if (IPR > 0 .and. NLIST == 1) write (IMP,102) (TPrinc(i,j),i=1,3)
        102 format (' TGrb ',3d15.7)
        enddo

        dlength=norm2(MacroDefRate%StrainModevM)
        !     Transform MacroDefRate%StrainModevM to the "Grb" reference frame
        TDCGr = rotateSRTensorFrom(MacroDefRate%StrainModevM,Tprinc)

        dot1=sum(relaxI*TDCGr)/sqrt(2.0D0)/dlength
        dot2=sum(relaxII*TDCGr)/sqrt(2.0D0)/dlength

        if(abs(dot1) < 0.000001_DP .and. abs(dot2) >= 0.000001_DP) then
            !  Need to rotate current frame (represented by Tprinc) with 90 degree to let relaxation-2 be the orthogonal one
            vec1=Tprinc(2,1:3)
            Tprinc(2,1:3)=-Tprinc(1,1:3)
            Tprinc(2,1:3)=vec1
        elseif(abs(dot1) >= 0.000001_DP .and. abs(dot2) >= 0.000001_DP) then
            ! need to rotate by a angle < 90 (this angle could be positive or negative)
            tgangle=dot2/dot1
            PrDir=0.0_DP
            PrDir(1,1)=1.D0/sqrt(1.D0+tgangle**2)
            PrDir(1,2)=tgangle/sqrt(1.D0+tgangle**2)
            PrDir(2,1)=-PrDir(1,2)
            PrDir(2,2)=PrDir(1,1)
            ! Prdir(n,:) is vector-n in the GB frame
            ! Transform these two vector in the Sample's frame
            vec1=0.0_DP
            vec2=0.0_DP
            do i=1,3
                vec1(i)=vec1(i)+sum(Tprinc(:,i)*PrDir(1,:))
                vec2(i)=vec2(i)+sum(Tprinc(:,i)*PrDir(2,:))
            enddo
            Tprinc(1,1:3)=vec1
            Tprinc(2,1:3)=vec2
        endif

    end subroutine

end module
