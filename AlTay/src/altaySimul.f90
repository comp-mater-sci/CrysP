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

    real(DP):: von_mises_strain

    character(*), parameter:: MOD_NAME = 'Simul'

    public:: SIMUL0, SIMUL1
    contains

    ! initialization call
    subroutine SIMUL0()
        integer:: NGR         !< number of grains

        character(len = 40):: TITEL
        integer:: info
        character(*), parameter:: PROC_NAME = 'SIMUL0'
        integer:: i


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

        real(DP):: TRF(3, 3, 2), strain_ab(2)
        integer:: NGR, &         !< number of grains
                  laml, laml1, &
                  IOR, ISTP, NPOINT, info, NFILE, i, j, l, ifil4
        real(DP):: stress_cluster(3, 3), & 
                   grain_shape(3, 3), & !Coefficient matrix describing grain shape as ellipsoid in quadratic notation ((X^T)*C*X = 1)
                   grain_axis_half_lengths(3), &
                   grain_axis_orientations(3, 3), &
                   cluster_weight, RHOSS(3, 3, 2), strain
        real(DP):: C2(3, 3), CC(2, 96)
        real(DP):: total_weight, homogenized_stress(3, 3), ssqgx
        ! HGAM: homogenized slip per step
        real(DP):: HGAM
        ! Macroscopically imposed vM equivalent strain per call.
        real(DP):: GMMdot  ! Total slip rate in current grain
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
                   von_mises_strain_rate, &
                   def_grad(3, 3)

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

            total_weight = 0._DP
            homogenized_stress = 0._DP
            Mavg = 0._DP
            HGAM = 0._DP

            nrstep = nrstep+1

            def_grad = matmul(deformation_gradient_increment, deformation_gradient) 

            if (.not.astate%simulCalls(astate%this)%input%keep_texture) deformation_gradient = def_grad 

            !Added for lamel model:
            !Organisation reading temporary texture file, 
            !in such way that the program TAYLOR can process the crystals
            !by sets of 2.
            !Taylor must therefore have "advance knowledge" of the
            !orientation to come at the moment that it starts such
            !computation.
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
                    call DYNFIL_getGrain(ifil4, TRF(1:3, 1:3, L), cluster_weight, strain_ab(L))
                end do
                laml1 = mod(laml1, NGR)+1
                laml = laml1
                
                
                call get_stress_state(stress_cluster, RHOSs(:,:,laml), TRF, IOR, strain_ab, NGR, laml, CC, M11, velocity_gradient, def_grad, cluster_weight)

                if (astate%simulCalls(astate%this)%input%full_model) then
                    call apply_deformation_step(IOR, GMMdot, WorkRate, spin, CC(1:2, 1:M11), TRF(:,:,laml), C2, XM)
                end if
               
                strain = strain_ab(laml) + GMMdot  ! Step time here implicitly assumed to be 1.0s
                ! We can choose not to update the texture state
                if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
                      call DYNFIL_setGrain(IOR, C2, strain)
                end if
                if(laml == 1) then
                    ssqgx = cluster_weight
                else
                    cluster_weight = ssqgx
                end if
                total_weight = total_weight+cluster_weight


                homogenized_stress = homogenized_stress+stress_cluster*cluster_weight
                !
                Mgrain = GMMdot /  von_mises_strain_rate
                Mavg = Mavg+Mgrain*cluster_weight
                HGAM = HGAM+GMMdot*cluster_weight  ! Step time here implicitly assumed to be 1.0s
                Wtot = Wtot+WorkRate  ! Step time here implicitly assumed to be 1.0s
            enddo clusterloop

            homogenized_stress = homogenized_stress/total_weight

            do i = 1, 2
                do j = i+1, 3
                    homogenized_stress(j, i)=homogenized_stress(i, j)
                end do
            end do
            Mavg = Mavg/total_weight

            ! Get the homogenized quantities:
            associate (callout => astate%simulCalls(astate%this)%output)
                callout%stress_tensor = homogenized_stress
                callout%taylor_factor = Mavg
                callout%effective_stress = sqrt(3.D0/2.D0)*norm2(homogenized_stress)
                callout%homogenised_slip_tot = HGAMTOT
                callout%effective_macro_strain_tot = von_mises_strain
                callout%effective_macro_strain_tot_end = von_mises_strain+von_mises_strain_rate
            end associate

            HGAM = HGAM/total_weight
            if (.not.astate%simulCalls(astate%this)%input%keep_state) HGAMTOT = HGAMTOT+HGAM

            von_mises_strain = von_mises_strain+von_mises_strain_rate
        enddo steploop
    end subroutine
end module
