module simulation
    use utils
    use hardening_model_dsh
    use altayCurAccess
    use grain_module
    use hardening
    use taylor
    use altayConfig
    use logging

    implicit none
    private

    real(DP), allocatable:: homogenized_total_slipTOT, & !< homogenized slip accumulated over calls
        taylor_coeffs(:,:)
    integer:: n_slip_systems_grain, NFILE1

    real(DP):: von_mises_strain

    character(*), parameter:: MOD_NAME = 'Simul'

    public:: simulation_init, &
             simulation_run
    contains

    ! initialization call
    subroutine simulation_init()
        integer:: cluster_size         !< number of grains

        character(len = 40):: TITEL
        integer:: info
        character(*), parameter:: PROC_NAME = 'SIMUL0'


        cluster_size    = acnf%simul_init%NGR

        NFILE1 = acnf%output_config%NFILE   ! control "CUR"
        homogenized_total_slipTOT = 0.D0

        if (cluster_size < 1 .or. cluster_size > 2) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Incorrect value of cluster_size')

        ! Check if number of crystals is right for the model
        if (modulo(size(grains), cluster_size) /= 0) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Number of grains must be even.')
        TITEL  = acnf%jobtitle
        ! Only if CUR file is requested
        if (NFILE1 == 1) call CURwriteTitle(IMP1, TITEL, info)
  98    format (A)
!       read the parameters of the work hardening model
        call taylor_init(acnf%deformation_mechanism, n_slip_systems_grain, taylor_coeffs, cluster_size)

        deformation_gradient = UNIT_MATRIX_3X3
        von_mises_strain = 0._DP
    end subroutine

    subroutine simulation_run(NFILE0, velocity_gradient)
        real(DP), intent(in):: velocity_gradient(3, 3)
        integer, intent(in):: NFILE0
        integer:: cluster_size, &
                  index_in_cluster, &
                  index_grain, &
                  step, &
                  n_grains, &
                  info, &
                  NFILE, &
                  i, &
                  j, &
                  l
        real(DP):: total_weight, &
                   homogenized_stress(3, 3), &
                   ssqgx, &
                   homogenized_total_slip, & ! homogenized_total_slip: homogenized slip per step
                   total_slip_rate, &  ! Total slip rate in current grain
                   taylor_factor, &  ! Taylor factor of the current grain
                   homogenized_taylor_factor, &   !Volume-averaged Taylor factor
                   WorkRate, &  ! Rate of plastic work per unit volume in the crystal
                   homogenized_work, &  ! Total plastic work per unit volume in crystal
                   deformation_gradient_increment(3, 3), &
                   deformation_gradient_increment_inverse(3, 3), &
                   strain_rate(3, 3), &
                   spin(3, 3), &
                   von_mises_strain_mode(3, 3), &
                   von_mises_strain_rate, &
                   next_deformation_gradient(3, 3)
        type(Grain), dimension(:), pointer:: cluster
        type(Grain), pointer:: grain_ptr

        n_grains = size(grains)
        ! Per-call selection of the model: cluster_size must be set
        cluster_size = acnf%simul_init%NGR
        NFILE = NFILE0*NFILE1

        strain_rate = symmetric_part(velocity_gradient)
        spin = antisymmetric_part(velocity_gradient)
        von_mises_strain_rate = SQR0P67*norm2(strain_rate)
        von_mises_strain_mode = strain_rate/von_mises_strain_rate
        deformation_gradient_increment = matrix_exponential_small_norm(velocity_gradient)
        deformation_gradient_increment_inverse = invert(deformation_gradient_increment)

        ! Output the current texture
        if (NFILE == 1) call CURwriteBlock(IMP1, info)
        steploop: DO step = 1, astate%simulCalls(astate%this)%input%nsteps

            total_weight = 0._DP
            homogenized_stress = 0._DP
            homogenized_taylor_factor = 0._DP
            homogenized_total_slip = 0._DP

            nrstep = nrstep+1

            next_deformation_gradient = matmul(deformation_gradient_increment, deformation_gradient)


            !Added for lamel model:
            !Organisation reading temporary texture file,
            !in such way that the program TAYLOR can process the crystals
            !by sets of 2.
            !Taylor must therefore have "advance knowledge" of the
            !orientation to come at the moment that it starts such
            !computation.
            index_in_cluster = 0
            clusterloop: do index_grain = 1, n_grains
                taylor_factor = 0.0_DP
                total_slip_rate = 0.0_DP
                WorkRate = 0.0_DP
                homogenized_work = 0.0_DP

                index_in_cluster = mod(index_in_cluster, cluster_size)+1

                if (index_in_cluster == 1) cluster => grains(index_grain:index_grain+cluster_size-1)

                call get_stress_state(cluster, index_grain, index_in_cluster, n_slip_systems_grain, velocity_gradient, next_deformation_gradient)

                if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
                    deformation_gradient = next_deformation_gradient
                    grain_ptr => cluster(index_in_cluster)
                    call apply_deformation_step(grain_ptr, index_grain, total_slip_rate, WorkRate, spin, taylor_coeffs, n_slip_systems_grain, index_in_cluster)
                end if

                total_weight = total_weight+cluster(1)%weight
                taylor_factor = total_slip_rate /  von_mises_strain_rate

                homogenized_stress          = homogenized_stress+cluster(index_in_cluster)%stress*cluster(1)%weight
                homogenized_taylor_factor   = homogenized_taylor_factor+taylor_factor*cluster(1)%weight
                homogenized_total_slip = homogenized_total_slip+total_slip_rate*cluster(1)%weight  !Step time here implicitly assumed to be 1.0s
                homogenized_work       = homogenized_work+WorkRate                              !Step time here implicitly assumed to be 1.0s
            enddo clusterloop

            homogenized_stress = homogenized_stress/total_weight

            do i = 1, 2
                do j = i+1, 3
                    homogenized_stress(j, i)=homogenized_stress(i, j)
                end do
            end do
            homogenized_taylor_factor = homogenized_taylor_factor/total_weight

            ! Get the homogenized quantities:
            associate (callout => astate%simulCalls(astate%this)%output)
                callout%stress_tensor = homogenized_stress
                callout%taylor_factor = homogenized_taylor_factor
                callout%effective_stress = sqrt(3.D0/2.D0)*norm2(homogenized_stress)
                callout%homogenised_slip_tot = homogenized_total_slipTOT
                callout%effective_macro_strain_tot = von_mises_strain
                callout%effective_macro_strain_tot_end = von_mises_strain+von_mises_strain_rate
            end associate

            homogenized_total_slip = homogenized_total_slip/total_weight
            if (.not.astate%simulCalls(astate%this)%input%keep_state) homogenized_total_slipTOT = homogenized_total_slipTOT+homogenized_total_slip

            von_mises_strain = von_mises_strain+von_mises_strain_rate
        enddo steploop
    end subroutine
end module
