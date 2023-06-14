module altaySimul
    use definitions
    use hardening_types
    use altayMacroKinematic
    use hardening_model_dsh
    use altayCurAccess
    use altayDYNFIL
    use hardening
    use altayTaylor
    use altayAlgorithms
    use altayConfig
    use altayIOConfig
    use altayMiscutils
    use logging

    implicit none
    private

    real(dp), private :: HGAMTOT,& !< homogenized slip accumulated over calls
        XM(5,96)
    integer, private :: M11,NFILE1,NFILTW
    integer, allocatable :: seed(:)

    character(*), parameter :: MOD_NAME = 'Simul'

    public :: SIMUL0, SIMUL1
    contains

    ! initialization call
    subroutine SIMUL0()

        integer :: NGR,&         !< number of grains
                   NRL        !< number of relaxations

        character(len=40) :: TITEL
        integer :: info,seedsize
        real(dp), parameter :: rad2deg=0.5729577951308232e+02_dp
        character(*), parameter :: PROC_NAME = 'SIMUL0'


        if(.not. allocated(seed)) then
            call random_seed(size=seedsize)
            allocate(seed(seedsize),source=20191102) ! low entropy, but at least deterministic
            call random_seed(put=seed)
        endif
        NGR    = acnf%simul_init%NGR

        NFILE1 = acnf%output_config%NFILE   ! control "CUR"
        NFILTW = acnf%output_config%NFILTW  ! control "TWN"
        NRES   = acnf%output_config%NRES    ! control "RES" and "RPT"
        NMSS   = acnf%output_config%NMSS    ! control "MSS"
        HGAMTOT=0.D0

        if (NGR < 1.or.NGR > 2) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Incorrect value of NGR')

 140    format (' NGR can only take the values 1 or 2 but was',I5)
        ! Check if number of crystals is right for the model
        if (modulo(size(DFIL), NGR) /= 0) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Number of grains must be even.')
!       Number of relaxations: 0 for Taylor and 2 for ALAMEL:
        NRL=(NGR-1)*2
        TITEL  = acnf%jobtitle
        if (NRES > 0) write (IMP2,98) TITEL
  97    format (' Title of the new simulation: ',A)
        ! Only if CUR file is requested
        if (NFILE1 == 1) call CURwriteTitle(IMP1,TITEL,info)
  98    format (A)
!       read the parameters of the work hardening model
        call TAYLOR1(M11,XM) ! read slip system file
    end subroutine


    subroutine SIMUL1(NFILE0,MacroDefRate)
        ! TO ORGANIZE SIMULATIONS OF DEFORMATION TEXTURES USING THE ALAMEL MODEL
        type(DeformationRate),intent(in) :: MacroDefRate !inout
        integer, intent(in) :: NFILE0

        real(dp) :: TRFb(3,3,2),GMMAb(2)
        integer :: NGR,&         !< number of grains
                   NRL,&         !< number of relaxations
                   laml
        integer :: IOR,ISTP,NPOINT, info, NFILE, NMSSx,NSTP,i,j,l,LAML1,ifil4,ITW
        real(dp), save :: gewfb(2),Fb(3,3,2),GAXESb(3,2),GEULRb(3,2),SHsam(3,3),Ssam(3,3),RHOSsa(3,3), &
                    CIJb(3,3,2),TGb(3,3,2),RHOSSb(3,3,2),fi1,PHI,fi2,C2(3,3),GEWF, &
                    GAXES(3),                                  &    ! half axes a,b,c, of the grain shape ellipsoid
                    GEULR(3),TG(3,3),CIJ(3,3),RHOST(3,3),RHOSm(3,3),FS(3,3)=1.0_dp,SeqAvg,tau,qgx,gmm1,gmm0,ssqgx,CC(2,96)
        real(DP) :: TOTGEW, fi1b(2),phib(2),fi2b(2), TRF(3,3), STOT(3,3)
        type(DeformationState), save :: MacroDefState
        ! HGAM: homogenized slip per step
        ! HGAMCALL: homogenized slip per call
        real(dp), save :: HGAM=0.D0,HGAMCALL=0.D0
        ! Macroscopically imposed vM equivalent strain per call.
        real(dp), save :: MEPSCALL=0.D0
        real(dp), save :: GMMdot !Total slip rate in current grain
        real(dp), save :: Mgrain !Taylor factor of the current grain
        real(dp), save :: Mavg   !Volume-averaged Taylor factor
        real(dp), save :: srh !Strain Rate Heterogeneity in polycrystal
        real(dp), save :: SeqGrain=0.D0 ! Equivalent stress in crystal, defined as..
                                      !  plastic work rate in crystal normalized by..
                                      !  (macro) von Mises equivalent strain rate
        real(dp), save :: WorkRate ! Rate of plastic work per unit
                                     ! volume in the crystal
        real(dp) :: Wtot ! Total plastic work per unit volume in crystal
        real(dp), parameter :: rad2deg=0.5729577951308232e+02_dp


        NPOINT = size(DFIL)
        ! Per-call selection of the model: NGR & NRL must be set
        NGR = acnf%simul_init%NGR
        ! Number of relaxations: 0 for Taylor and 2 for ALAMEL:
        NRL=(NGR-1)*2
        NFILE=NFILE0*NFILE1
        NMSSx= NFILE0*NMSS    ! control "MSS" (effective value)

        NSTP     = astate%simulCalls(astate%this)%input%nsteps
        ! Output the current texture
        if (NFILE == 1) call CURwriteBlock(IMP1,info)
        HGAMCALL = 0.D0
        MEPSCALL = 0.D0
        steploop: DO ISTP=1,NSTP

            TOTGEW=0.0
            STOT = 0.D0
            RHOST = 0.D0
            SeqAvg=0.
            Mavg=0.
            srh=0.
            HGAM=0.D0

            call dynfil2(nrstep,MacroDefState%TotalDefGrad,GAXES,GEULR,CIJ,TG)
#ifndef NO_STDOUT
            write (*,96) ISTP,GAXES
#endif
            if (.not. (nfile == 0.or.ISTP > 1)) then
                if (NRES > 0) write (IMP2,404) nrstep+1,NPOINT
 404                format (' Def. Step ',i5,'  Number of orientations',i5,/,T3,'ior'  &
                 ,T7,'EquivStress',T23,'WorkRate',T37,'tau_ref',T56,'M',T64,       &
                 'ratlon',T109,'RHO-SYMMETRIC',T172,'RHO-ROTATIONAL',T239,'STRESS',/,1x,278('*'))
            endif

            nrstep=nrstep+1

            call Update_DeformationState(MacroDefRate,MacroDefState,info)
            call UPDATC(CIJ,MacroDefState%IncrDefGrad_inverse)
            call GETANG(CIJ,GAXES,GEULR,TG)

            if (.not.astate%simulCalls(astate%this)%input%keep_texture) &
                  call DYNFIL3(nrstep,MacroDefState%TotalDefGrad,GAXES,GEULR,CIJ,TG)
!
!         Added for lamel model:
!         Organisation reading temporary texture file,
!         in such way that the program TAYLOR can process the crystals
!         by sets of 2.
!         Taylor must therefore have "advance knowledge" of the
!         orientation to come at the moment that it starts such
!         computation.
!         See also the comment before the calling of subroutine TAYLOR.
!
            laml=1
            laml1=NGR
            ifil4=0
            clusterloop: do IOR=1,NPOINT
                Mgrain=0.0
                GMMdot=0.0
                WorkRate = 0.D0
                SeqGrain = 0.D0
                Wtot = 0.0

                do L=laml,laml1
                    if (ifil4 == NPOINT) exit
                    ifil4=ifil4+1
                    call DYNFIL4(ifil4,fi1b(L),PHIb(L),fi2b(L),TRFb(1:3,1:3,L),GEWFb(L),GMMAb(L),Fb(1:3,1:3,L),GAXESb(1:3,L), &
                                 GEULRb(1:3,L),CIJb(1:3,1:3,L),TGb(1:3,1:3,L),RHOSSb(1:3,1:3,L))
                    fi1b(L)=fi1b(L)*rad2deg
                    PHIb(L)=PHIb(L)*rad2deg
                    fi2b(L)=fi2b(L)*rad2deg
                end do
                laml1=laml1+1
                if (laml1 > NGR) laml1=1
                laml=laml1
                GMM0=GMMAb(laml)
                call getTau(GMM0,TAU,info)
                fi1=fi1b(laml)
                PHI=PHIb(laml)
                fi2=fi2b(laml)
                !
                TRF = TRFb(:,:,laml)
                TG = TGb(:,:,laml)
                RHOSSa = RHOSSb(:,:,laml)
                if(laml == 1) then
                    qgx=GEWFb(laml)
                    GEWF=qgx
                else
                    GEWF=qgx
                end if
                !  In case of NGR=2:
                !     LAML=1: TAYLOR
                !             - has the present and the next orientation available
                !             - must perform the computation of a set of 2 crystals
                !             - has to output the result for the first crystal
                !     LAML=2: TAYLOR
                !             - should not perform any computation
                !             - has to output the result of the second crystal found
                !               during the previous computation.
                call TAYLOR3(Ssam,RHOSsa,TRF,GEWF,IOR,TRFb,GMMab,NGR,NRL,laml,CC,M11,MacroDefRate,MacroDefState)

                if(laml == 1) then
                    ssqgx=GEWF
                else
                    GEWF=ssqgx
                end if
                TOTGEW=TOTGEW+GEWF

                if (astate%simulCalls(astate%this)%input%full_model) then
                      call TAYLOR4(ISTP,IOR,NRES,TAU,GMMdot,SeqGrain,WorkRate, MacroDefRate,CC,M11,SSam,RHOSsa,fi1,PHI,fi2, &
                                   TRF,C2,ITW,XM)
                endif
                if (NFILTW == 1) write (IMP3,398) ITW
 398            format (I3)

                STOT = STOT + Ssam*GEWF
                RHOST = RHOST + RHOSsa*GEWF
                !
                SeqAvg = SeqAvg + SeqGrain*GEWF
                Mgrain = GMMdot /  MacroDefRate%vMeqStrainRate
                Mavg = Mavg + Mgrain*GEWF
                ! norm2(RHOSsa)=||RHOSsa||=(||d-D||)/MacroDefRate%vMeqStrainRate
                srh = srh + norm2(RHOSsa)*GEWF
                HGAM = HGAM + GMMdot*GEWF !Step time here implicitly assumed to be 1.0s
                GMM1 = GMM0 + GMMdot !Step time here implicitly assumed to be 1.0s
                Wtot = Wtot + WorkRate !Step time here implicitly assumed to be 1.0s
                ! We can choose not to update the texture state
                if (.not.astate%simulCalls(astate%this)%input%keep_texture) &
                      call DYNFIL5(IOR,fi1,PHI,fi2,C2,GEWF,GMM1,MacroDefState%TotalDefGrad,GAXES,GEULR,CIJ,TG,RHOSsa)
            enddo clusterloop

            SHsam = STOT / TOTGEW
            RHOSm = RHOST / TOTGEW

            do i=1,2
                do j=i+1,3
                    SHsam(i,j)=SHsam(i,j)*FS(i,j)
                    SHsam(j,i)=SHsam(i,j)
                    RHOSm(i,j)=RHOSm(i,j)*FS(i,j)
                    RHOSm(j,i)=RHOSm(i,j)
                end do
            end do
            Mavg=Mavg/TOTGEW
            ! DEFINITION: srh = (||d-D||) / ||D||
            srh=sqrt(2.D0/3.D0)*srh/TOTGEW
            SeqAvg=SeqAvg/TOTGEW

            MEPSCALL= MacroDefState%IncrvMeqStrain * (ISTP-1)

            if (NMSSx /= 0) &
                call writeMSSRecord(IMP5,MEPSCALL,MacroDefState%AccumvMeqStrain_ToStartOfInc, HGAMCALL,HGAMTOT,SHsam,Mavg,srh,info)
            ! Get the homogenized quantities:
            associate (callout => astate%simulCalls(astate%this)%output)
                callout%stress_tensor= SHsam
                callout%taylor_factor= Mavg
                callout%strain_rate_heterogeneity = srh
                callout%equivalent_stress= SeqAvg
                callout%effective_stress = sqrt(3.D0/2.D0)*norm2(SHsam)
                callout%homogenised_slip = HGAMCALL
                callout%homogenised_slip_tot = HGAMTOT
                callout%effective_macro_strain = MEPSCALL
                callout%effective_macro_strain_tot = MacroDefState%AccumvMeqStrain_ToStartOfInc
                callout%effective_macro_strain_tot_end = MacroDefState%AccumvMeqStrain_ToEndOfInc
            end associate

            HGAM = HGAM / TOTGEW
            HGAMCALL = HGAMCALL + HGAM
            if (.not.astate%simulCalls(astate%this)%input%keep_state) HGAMTOT = HGAMTOT + HGAM
        enddo steploop
    end subroutine

end module
