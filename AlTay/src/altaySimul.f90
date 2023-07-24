module altaySimul
    use definitions
    use altayMacroKinematic
    use hardening_model_dsh
    use altayCurAccess
    use altayDYNFIL
    use hardening
    use taylor
    use altayAlgorithms
    use altayConfig
    use logging

    implicit none
    private

    real(DP), private, allocatable :: HGAMTOT,& !< homogenized slip accumulated over calls
        XM(:,:)
    integer, private :: M11,NFILE1
    integer, allocatable :: seed(:)

    character(*), parameter :: MOD_NAME = 'Simul'

    public :: SIMUL0, SIMUL1
    contains

    ! initialization call
    subroutine SIMUL0()

        integer :: NGR         !< number of grains

        character(len=40) :: TITEL
        integer :: info,seedsize
        character(*), parameter :: PROC_NAME = 'SIMUL0'


        if(.not. allocated(seed)) then
            call random_seed(size=seedsize)
            allocate(seed(seedsize),source=20191102) ! low entropy, but at least deterministic
            call random_seed(put=seed)
        endif
        NGR    = acnf%simul_init%NGR

        NFILE1 = acnf%output_config%NFILE   ! control "CUR"
        HGAMTOT=0.D0

        if (NGR < 1.or.NGR > 2) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Incorrect value of NGR')

        ! Check if number of crystals is right for the model
        if (modulo(size(DFIL), NGR) /= 0) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Number of grains must be even.')
        TITEL  = acnf%jobtitle
        ! Only if CUR file is requested
        if (NFILE1 == 1) call CURwriteTitle(IMP1,TITEL,info)
  98    format (A)
!       read the parameters of the work hardening model
        call taylor_init(acnf%deformation_mechanism, M11,XM)
    end subroutine


    subroutine SIMUL1(NFILE0,MacroDefRate)
        ! TO ORGANIZE SIMULATIONS OF DEFORMATION TEXTURES USING THE ALAMEL MODEL
        type(DeformationRate),intent(in) :: MacroDefRate !inout
        integer, intent(in) :: NFILE0

        real(DP) :: TRFb(3,3,2),GMMAb(2)
        integer :: NGR,&         !< number of grains
                   NRL,&         !< number of relaxations
                   laml,laml1, &
                   IOR,ISTP,NPOINT, info, NFILE, i,j,l,ifil4
        real(DP) :: gewfb(2), Ssam(3,3), TG(3,3), CIJ(3,3), &
                    GEWF, RHOSSb(3,3,2),RHOSsa(3,3),TGb(3,3,2),gmm1, &
                    GAXES(3)                                        ! half axes a,b,c, of the grain shape ellipsoid
        real(DP), save :: C2(3,3),qgx,ssqgx,CC(2,96)
        real(DP) :: TOTGEW, TRF(3,3),SHsam(3,3),RHOSm(3,3)
        type(DeformationState), save :: MacroDefState
        ! HGAM: homogenized slip per step
        real(DP) :: HGAM
        ! Macroscopically imposed vM equivalent strain per call.
        real(DP), save :: GMMdot !Total slip rate in current grain
        real(DP) :: Mgrain !Taylor factor of the current grain
        real(DP) :: Mavg   !Volume-averaged Taylor factor
        real(DP) :: WorkRate ! Rate of plastic work per unit
                                     ! volume in the crystal
        real(DP) :: Wtot ! Total plastic work per unit volume in crystal


        NPOINT = size(DFIL)
        ! Per-call selection of the model: NGR & NRL must be set
        NGR = acnf%simul_init%NGR
        ! Number of relaxations: 0 for Taylor and 2 for ALAMEL:
        NRL=(NGR-1)*2
        NFILE=NFILE0*NFILE1

        ! Output the current texture
        if (NFILE == 1) call CURwriteBlock(IMP1,info)
        steploop: DO ISTP=1,astate%simulCalls(astate%this)%input%nsteps

            TOTGEW=0.0_DP
            SHsam = 0._DP
            RHOSm = 0._DP
            Mavg=0._DP
            HGAM=0.0_DP

            call dynfil_getGlobal(MacroDefState%TotalDefGrad,CIJ)

            nrstep=nrstep+1

            call Update_DeformationState(MacroDefRate,MacroDefState,info)
            call UPDATC(CIJ,MacroDefState%IncrDefGrad_inverse)
            call GETANG(CIJ,GAXES,TG)

            if (.not.astate%simulCalls(astate%this)%input%keep_texture) &
                  call DYNFIL_setGlobal(MacroDefState%TotalDefGrad,GAXES,CIJ,TG)
!
!         Added for lamel model:
!         Organisation reading temporary texture file,
!         in such way that the program TAYLOR can process the crystals
!         by sets of 2.
!         Taylor must therefore have "advance knowledge" of the
!         orientation to come at the moment that it starts such
!         computation.
            laml=1
            laml1=NGR
            ifil4=0
            clusterloop: do IOR=1,NPOINT
                Mgrain=0.0_DP
                GMMdot=0.0_DP
                WorkRate = 0.0_DP
                Wtot = 0.0_DP

                do L=laml,laml1
                    if (ifil4 == NPOINT) exit
                    ifil4=ifil4+1
                    call DYNFIL_getGrain(ifil4,TRFb(1:3,1:3,L),GEWFb(L),GMMAb(L),TGb(1:3,1:3,L),RHOSSb(1:3,1:3,L))
                end do
                laml1 = mod(laml1,NGR)+1
                laml=laml1
                !
                TRF = TRFb(:,:,laml)
                TG = TGb(:,:,laml)
                RHOSSa = RHOSSb(:,:,laml)
                if(laml == 1) qgx=GEWFb(laml)
                GEWF=qgx
                !  In case of NGR=2:
                !     LAML=1: TAYLOR3
                !             - has the present and the next orientation available
                !             - must perform the computation of a set of 2 crystals
                !             - has to output the result for the first crystal
                !     LAML=2: TAYLOR3
                !             - should not perform any computation
                !             - has to output the result of the second crystal found
                !               during the previous computation.
                call taylor_solve(Ssam,RHOSsa,TRF,GEWF,IOR,TRFb,GMMab,NGR,NRL,laml,CC,M11,MacroDefRate,MacroDefState)

                if(laml == 1) then
                    ssqgx=GEWF
                else
                    GEWF=ssqgx
                end if
                TOTGEW=TOTGEW+GEWF

                if (astate%simulCalls(astate%this)%input%full_model) &
                      call taylor_update_state(IOR,GMMdot,WorkRate,MacroDefRate,CC,M11,TRF,C2,XM)

                SHsam = SHsam + Ssam*GEWF
                RHOSm = RHOSm + RHOSsa*GEWF
                !
                Mgrain = GMMdot /  MacroDefRate%vMeqStrainRate
                Mavg = Mavg + Mgrain*GEWF
                ! norm2(RHOSsa)=||RHOSsa||=(||d-D||)/MacroDefRate%vMeqStrainRate
                HGAM = HGAM + GMMdot*GEWF !Step time here implicitly assumed to be 1.0s
                GMM1 = GMMab(laml) + GMMdot !Step time here implicitly assumed to be 1.0s
                Wtot = Wtot + WorkRate !Step time here implicitly assumed to be 1.0s
                ! We can choose not to update the texture state
                if (.not.astate%simulCalls(astate%this)%input%keep_texture) &
                      call DYNFIL_setGrain(IOR,C2,GEWF,GMM1,TG,RHOSsa)
            enddo clusterloop

            SHsam = SHsam / TOTGEW
            RHOSm = RHOSm / TOTGEW

            do i=1,2
                do j=i+1,3
                    SHsam(j,i)=SHsam(i,j)
                    RHOSm(j,i)=RHOSm(i,j)
                end do
            end do
            Mavg=Mavg/TOTGEW

            ! Get the homogenized quantities:
            associate (callout => astate%simulCalls(astate%this)%output)
                callout%stress_tensor= SHsam
                callout%taylor_factor= Mavg
                callout%effective_stress = sqrt(3.D0/2.D0)*norm2(SHsam)
                callout%homogenised_slip_tot = HGAMTOT
                callout%effective_macro_strain_tot = MacroDefState%AccumvMeqStrain_ToStartOfInc
                callout%effective_macro_strain_tot_end = MacroDefState%AccumvMeqStrain_ToEndOfInc
            end associate

            HGAM = HGAM / TOTGEW
            if (.not.astate%simulCalls(astate%this)%input%keep_state) HGAMTOT = HGAMTOT + HGAM
        enddo steploop
    end subroutine

end module
