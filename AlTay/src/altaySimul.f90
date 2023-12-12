module altaySimul
    use utils
    use hardening_model_dsh
    use altayCurAccess
    use altayDYNFIL
    use hardening
    use taylor
    use altayConfig
    use logging


    implicit none
    private

    real(DP), allocatable:: HGAMTOT, & !< homogenized slip accumulated over calls
        XM(:,:)
    integer:: M11, NFILE1

    real(DP):: deformation_gradient(3, 3), &
               von_mises_strain

    character(*), parameter:: MOD_NAME = 'Simul'

    public:: SIMUL0, SIMUL1
    contains

    ! initialization call
    subroutine SIMUL0()
        integer:: NGR         !< number of grains

        character(len = 40):: TITEL
        integer:: info
        character(*), parameter:: PROC_NAME = 'SIMUL0'


        NGR    = acnf%simul_init%NGR

        NFILE1 = acnf%output_config%NFILE   ! control "CUR"
        HGAMTOT = 0.D0

        if (NGR < 1 .or. NGR > 2) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Incorrect value of NGR')

        ! Check if number of crystals is right for the model
        if (modulo(size(DFIL), NGR) /= 0) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Number of grains must be even.')
        TITEL  = acnf%jobtitle
        ! Only if CUR file is requested
        if (NFILE1 == 1) call CURwriteTitle(IMP1, TITEL, info)
  98    format (A)
!       read the parameters of the work hardening model
        call taylor_init(acnf%deformation_mechanism, M11, XM, NGR)

        deformation_gradient = UNIT_MATRIX_3X3
        von_mises_strain = 0._DP
    end subroutine


    subroutine SIMUL1(NFILE0, velocity_gradient)
        ! TO ORGANIZE SIMULATIONS OF DEFORMATION TEXTURES USING THE ALAMEL MODEL
        real(DP), dimension(3, 3), intent(in):: velocity_gradient
        integer, intent(in):: NFILE0

        real(DP):: TRF(3, 3, 2), GMMAb(2)
        integer:: NGR, &         !< number of grains
                   laml, laml1, &
                   IOR, ISTP, NPOINT, info, NFILE, i, j, l, ifil4
        real(DP):: Ssam(3, 3), & 
                   grain_shape(3, 3), & !Coefficient matrix describing grain shape as ellipsoid in quadratic notation ((X^T)*C*X = 1)
                   grain_axis_half_lengths(3), &
                   grain_axis_orientations(3, 3), &
                   GEWF, RHOSS(3, 3, 2), gmm1
        real(DP), save:: C2(3, 3), CC(2, 96)
        real(DP):: TOTGEW, SHsam(3, 3), RHOSm(3, 3), ssqgx
        ! HGAM: homogenized slip per step
        real(DP):: HGAM
        ! Macroscopically imposed vM equivalent strain per call.
        real(DP), save:: GMMdot  ! Total slip rate in current grain
        real(DP):: Mgrain  ! Taylor factor of the current grain
        real(DP):: Mavg   !Volume-averaged Taylor factor
        real(DP):: WorkRate  ! Rate of plastic work per unit
                                     ! volume in the crystal
        real(DP):: Wtot  ! Total plastic work per unit volume in crystal
        real(DP):: deformation_gradient_increment(3, 3), &
                   deformation_gradient_increment_inverse(3, 3), &
                   strain_rate(3, 3), &
                   spin(3, 3), &
                   von_mises_strain_mode(3, 3), &
                   von_mises_strain_rate

        NPOINT = size(DFIL)
        ! Per-call selection of the model: NGR must be set
        NGR = acnf%simul_init%NGR
        ! Number of relaxations: 0 for Taylor and 2 for ALAMEL:
        NFILE = NFILE0*NFILE1


        strain_rate = symmetric_part(velocity_gradient)
        spin = antisymmetric_part(velocity_gradient)
        von_mises_strain_rate = SQR0P67*norm2(strain_rate)
        von_mises_strain_mode = strain_rate/von_mises_strain_rate
        deformation_gradient_increment = matrix_exponential_small_norm(velocity_gradient)
        deformation_gradient_increment_inverse = invert(deformation_gradient_increment) 

        ! Output the current texture
        if (NFILE == 1) call CURwriteBlock(IMP1, info)
        steploop: DO ISTP = 1, astate%simulCalls(astate%this)%input%nsteps

            TOTGEW = 0.0_DP
            SHsam = 0._DP
            RHOSm = 0._DP
            Mavg = 0._DP
            HGAM = 0.0_DP

            call dynfil_getGlobal(deformation_gradient, grain_shape)

            nrstep = nrstep+1

            deformation_gradient = matmul(deformation_gradient_increment, deformation_gradient) 

            !Update grain weights and cluster reference frame orientations for ALAMEL simulations
            if (NGR == 2) then 
                do i = 1, size(DFIL), 2
                    call update_cluster_state(DFIL(i), deformation_gradient, von_mises_strain_mode)
                end do
                
                !Updating of ellipsoidal representation of grain shape
                !See Van Houtte et. al., 1999: QUANTITATIVE PREDICTION OF COLD ROLLING TEXTURES IN LOW-CARBON STEEL BY MEANS OF THE LAMEL MODEL
                !
                !Update coefficient matrix of ellipsoid representing grain shape
                grain_shape = matmul(matmul(transpose(deformation_gradient_increment_inverse), grain_shape), deformation_gradient_increment_inverse)
                !Eigenvalue decomposition of quadratic notation of ellipsoid determines axis orientations and half-lengths
                call eigenvalue_decomposition_3x3(grain_shape, grain_axis_half_lengths, grain_axis_orientations)
                grain_axis_half_lengths = 1._dp/sqrt(grain_axis_half_lengths)  ! Eigenvalues are squared inverse of axis half-lengths
            end if

            
            if (.not.astate%simulCalls(astate%this)%input%keep_texture) &
                  call DYNFIL_setGlobal(deformation_gradient, grain_axis_half_lengths, grain_shape, grain_axis_orientations)
!
!         Added for lamel model:
!         Organisation reading temporary texture file, 
!         in such way that the program TAYLOR can process the crystals
!         by sets of 2.
!         Taylor must therefore have "advance knowledge" of the
!         orientation to come at the moment that it starts such
!         computation.
            laml = 1
            laml1 = NGR
            ifil4 = 0
            clusterloop: do IOR = 1, NPOINT
                Mgrain = 0.0_DP
                GMMdot = 0.0_DP
                WorkRate = 0.0_DP
                Wtot = 0.0_DP

                do L = laml, laml1
                    if (ifil4 == NPOINT) exit
                    ifil4 = ifil4+1
                    call DYNFIL_getGrain(ifil4, TRF(1:3, 1:3, L), GEWF, GMMAb(L), grain_axis_orientations, RHOSS(1:3, 1:3, L))
                end do
                laml1 = mod(laml1, NGR)+1
                laml = laml1
                !
                !  In case of NGR = 2:
                !     LAML = 1: TAYLOR3
                !             - has the present and the next orientation available
                !             - must perform the computation of a set of 2 crystals
                !             - has to output the result for the first crystal
                !     LAML = 2: TAYLOR3
                !             - should not perform any computation
                !             - has to output the result of the second crystal found
                !               during the previous computation.
                call taylor_solve(Ssam, RHOSs(:,:,laml), TRF, IOR, GMMab, NGR, laml, CC, M11, velocity_gradient, von_mises_strain_rate)

                if (astate%simulCalls(astate%this)%input%full_model) &
                      call taylor_update_state(IOR, GMMdot, WorkRate, spin, von_mises_strain_rate, CC(1:2, 1:M11), TRF(:,:,laml), C2, XM)

                GEWF = DFIL(IOR)%tgew
                if(laml == 1) then
                    ssqgx = GEWF
                else
                    GEWF = ssqgx
                end if
                TOTGEW = TOTGEW+GEWF


                SHsam = SHsam+Ssam*GEWF
                RHOSm = RHOSm+RHOSs(:,:,laml)*GEWF
                !
                Mgrain = GMMdot /  von_mises_strain_rate
                Mavg = Mavg+Mgrain*GEWF
                HGAM = HGAM+GMMdot*GEWF  ! Step time here implicitly assumed to be 1.0s
                GMM1 = GMMab(laml) + GMMdot  ! Step time here implicitly assumed to be 1.0s
                Wtot = Wtot+WorkRate  ! Step time here implicitly assumed to be 1.0s
                ! We can choose not to update the texture state
                if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
                      call DYNFIL_setGrain(IOR, C2, GMM1, grain_axis_orientations, RHOSs(:,:,laml))
                end if
            enddo clusterloop

            SHsam = SHsam/TOTGEW
            RHOSm = RHOSm/TOTGEW

            do i = 1, 2
                do j = i+1, 3
                    SHsam(j, i)=SHsam(i, j)
                    RHOSm(j, i)=RHOSm(i, j)
                end do
            end do
            Mavg = Mavg/TOTGEW

            ! Get the homogenized quantities:
            associate (callout => astate%simulCalls(astate%this)%output)
                callout%stress_tensor = SHsam
                callout%taylor_factor = Mavg
                callout%effective_stress = sqrt(3.D0/2.D0)*norm2(SHsam)
                callout%homogenised_slip_tot = HGAMTOT
                callout%effective_macro_strain_tot = von_mises_strain
                callout%effective_macro_strain_tot_end = von_mises_strain+von_mises_strain_rate
            end associate

            HGAM = HGAM/TOTGEW
            if (.not.astate%simulCalls(astate%this)%input%keep_state) HGAMTOT = HGAMTOT+HGAM

            von_mises_strain = von_mises_strain+von_mises_strain_rate
        enddo steploop
    end subroutine
end module
