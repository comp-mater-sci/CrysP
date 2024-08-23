module simulation
    use utils
    use hardening_model_dsh
    use grain_module
    use hardening
    use taylor
    use altayConfig
    use logging
    use cluster_module

    implicit none
    private

    real(DP), allocatable:: homogenized_total_slipTOT !< homogenized slip accumulated over calls
    integer:: n_slip_systems_grain, NFILE1

    real(DP):: von_mises_strain
    real(DP), dimension(3, 3):: deformation_gradient
    type(Cluster), dimension(:), allocatable, target:: clusters

    character(*), parameter:: MOD_NAME = 'Simul'


    public:: simulation_init, &
             simulation_run, &
             deformation_gradient
    contains

    ! initialization call
    subroutine simulation_init()
        integer:: cluster_size         !< number of grains

        character(len = 40):: TITEL
        integer:: info
        character(*), parameter:: PROC_NAME = 'SIMUL0'


        cluster_size    = acnf%simul_init%NGR

        homogenized_total_slipTOT = 0.D0

        if (cluster_size < 1 .or. cluster_size > 2) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Incorrect value of cluster_size')

        ! Check if number of crystals is right for the model
        if (modulo(size(grains), cluster_size) /= 0) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Number of grains must be even.')
!       read the parameters of the work hardening model
        clusters = taylor_init(acnf%deformation_mechanism, n_slip_systems_grain, cluster_size, acnf%micros_fname, acnf%simul_init%FMicro)

        deformation_gradient = UNIT_MATRIX_3X3
        von_mises_strain = 0._DP
    end subroutine

    subroutine simulation_run(NFILE0, velocity_gradient)
        real(DP), intent(in):: velocity_gradient(3, 3)
        integer, intent(in):: NFILE0
        integer:: cluster_size, &
                  index_cluster, &
                  step, &
                  n_grains, &
                  info, &
                  NFILE, &
                  i, &
                  j, &
                  l
        real(DP):: total_weight, &
                   homogenized_stress(3, 3), &
                   homogenized_total_slip, & ! homogenized_total_slip: homogenized slip per step
                   total_slip_rate, &  ! Total slip rate in current grain
                   taylor_factor, &  ! Taylor factor of the current grain
                   homogenized_taylor_factor, &   !Volume-averaged Taylor factor
                   WorkRate, &  ! Rate of plastic work per unit volume in the crystal
                   homogenized_work, &  ! Total plastic work per unit volume in crystal
                   deformation_gradient_increment(3, 3), &
                   deformation_gradient_half_increment(3, 3), &
                   deformation_gradient_increment_inverse(3, 3), &
                   strain_rate(3, 3), &
                   spin(3, 3), &
                   von_mises_strain_mode(3, 3), &
                   von_mises_strain_rate, &
                   deformation_gradient_during_time_step(3, 3), &
                   stress_cluster(3, 3)
        type(Grain), pointer:: grain_ptr
        type(Cluster), pointer:: cluster_ptr

        n_grains = size(grains)
        ! Per-call selection of the model: cluster_size must be set
        cluster_size = acnf%simul_init%NGR
        NFILE = NFILE0*NFILE1

        strain_rate = symmetric_part(velocity_gradient)
        spin = antisymmetric_part(velocity_gradient)
        von_mises_strain_rate = SQR0P67*norm2(strain_rate)
        von_mises_strain_mode = strain_rate/von_mises_strain_rate
        deformation_gradient_increment = matrix_exponential_small_norm(velocity_gradient)
        deformation_gradient_half_increment = matrix_exponential_small_norm(velocity_gradient/2._DP)
        deformation_gradient_increment_inverse = invert(deformation_gradient_increment)


        deformation_gradient_during_time_step = matmul(deformation_gradient_half_increment, deformation_gradient)

        do i = 1, size(clusters)
            cluster_ptr => clusters(i)
            call update_cluster_state(cluster_ptr, deformation_gradient_during_time_step, velocity_gradient, i)
        end do

        ! Output the current texture
        steploop: DO step = 1, astate%simulCalls(astate%this)%input%nsteps

            total_weight = 0._DP
            homogenized_stress = 0._DP
            homogenized_taylor_factor = 0._DP
            homogenized_total_slip = 0._DP
            homogenized_work = 0.0_DP

            nrstep = nrstep+1


            !Added for lamel model:
            !Organisation reading temporary texture file,
            !in such way that the program TAYLOR can process the crystals
            !by sets of 2.
            !Taylor must therefore have "advance knowledge" of the
            !orientation to come at the moment that it starts such
            !computation.

            if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
                !Deformation gradient exactly in the middle of the time step.
                deformation_gradient_during_time_step = matmul(deformation_gradient_increment, deformation_gradient_during_time_step)
            end if
            clusterloop: do index_cluster = 1, size(clusters)
                cluster_ptr => clusters(index_cluster)
                taylor_factor = 0.0_DP
                total_slip_rate = 0.0_DP
                WorkRate = 0.0_DP

                call get_stress_state(cluster_ptr, index_cluster, n_slip_systems_grain, stress_cluster)

                if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
                    call apply_deformation_step(cluster_ptr, total_slip_rate, WorkRate, spin, n_slip_systems_grain, index_cluster, &
                    deformation_gradient_during_time_step, velocity_gradient)
                end if

                total_weight = total_weight+cluster_ptr%weight
                taylor_factor = total_slip_rate /  von_mises_strain_rate

                homogenized_stress = homogenized_stress+stress_cluster*cluster_ptr%weight
                homogenized_taylor_factor  = homogenized_taylor_factor+taylor_factor*cluster_ptr%weight
                homogenized_total_slip = homogenized_total_slip+total_slip_rate*cluster_ptr%weight  !Step time here implicitly assumed to be 1.0s
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

            if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
                deformation_gradient = matmul(deformation_gradient_increment, deformation_gradient)
            end if
        enddo steploop
    end subroutine
end module
