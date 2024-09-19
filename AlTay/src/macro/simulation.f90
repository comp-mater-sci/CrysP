module simulation
    use utils
    use hardening_model_dsh
    use grain_module
    use hardening
    use taylor
    use altayConfig
    use logging
    use cluster_module
    use file_io
    use omp_lib

    implicit none
    private

    real(DP), allocatable:: homogenized_total_slipTOT !< homogenized slip accumulated over calls
    integer:: NFILE1

    real(DP):: von_mises_strain
    real(DP), dimension(3, 3):: deformation_gradient
    type(Cluster), dimension(:), pointer, contiguous:: clusters

    character(*), parameter:: MOD_NAME = 'Simul'


    public:: simulation_init, &
             simulation_run, &
             simulation_finalize, &
             deformation_gradient, &
             output_current_state
    contains

    ! initialization call
    subroutine simulation_init(orientations, boundaries)
        integer:: cluster_size         !< number of grains

        character(len = 40):: TITEL
        integer:: info
        real(DP), dimension(:,:), allocatable, intent(in):: orientations, &
                                                            boundaries

        character(*), parameter:: PROC_NAME = 'SIMUL0'


        cluster_size    = acnf%simul_init%NGR

        homogenized_total_slipTOT = 0.D0

        if (cluster_size < 1 .or. cluster_size > 2) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Incorrect value of cluster_size')

        TITEL  = acnf%jobtitle
        ! Only if CUR file is requested
        if (NFILE1 == 1) call cur_write_title(IMP1, TITEL, info)
  98    format (A)

!       read the parameters of the work hardening model
        clusters => taylor_init(acnf%deformation_mechanism, cluster_size, acnf%simul_init%FMicro, orientations, boundaries)

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
        type(Cluster), pointer:: cluster_ptr

        ! Per-call selection of the model: cluster_size must be set
        cluster_size = acnf%simul_init%NGR
        n_grains = size(clusters) * cluster_size
        NFILE = NFILE0*NFILE1

        strain_rate = symmetric_part(velocity_gradient)
        spin = antisymmetric_part(velocity_gradient)
        von_mises_strain_rate = SQR0P67*norm2(strain_rate)
        von_mises_strain_mode = strain_rate/von_mises_strain_rate
        deformation_gradient_increment = matrix_exponential_small_norm(velocity_gradient)
        deformation_gradient_half_increment = matrix_exponential_small_norm(velocity_gradient/2._DP)
        deformation_gradient_increment_inverse = invert(deformation_gradient_increment)


        deformation_gradient_during_time_step = matmul(deformation_gradient_half_increment, deformation_gradient)

        !$OMP PARALLEL SHARED(clusters, deformation_gradient_during_time_step, velocity_gradient) PRIVATE(i, cluster_ptr)
            !$OMP DO SCHEDULE(static, 1)
                do i = 1, size(clusters)
                    cluster_ptr => clusters(i)
                    call update_cluster_state(cluster_ptr, deformation_gradient_during_time_step, velocity_gradient, i)
                end do
            !$OMP END DO
        !$OMP END PARALLEL

        ! Output the current texture
        if (NFILE == 1) call cur_write_block(IMP1, clusters, deformation_gradient)

        steploop: DO step = 1, astate%simulCalls(astate%this)%input%nsteps

            total_weight = 0._DP
            homogenized_stress = 0._DP
            homogenized_taylor_factor = 0._DP
            homogenized_total_slip = 0._DP
            homogenized_work = 0.0_DP

            nrstep = nrstep+1

            if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
                !Deformation gradient exactly in the middle of the time step.
                deformation_gradient_during_time_step = matmul(deformation_gradient_increment, deformation_gradient_during_time_step)
            end if

            !$OMP PARALLEL SHARED(step, clusters, astate, spin, deformation_gradient_during_time_step, velocity_gradient) PRIVATE(index_cluster, cluster_ptr)
                !$OMP DO SCHEDULE(DYNAMIC, 1)
                    clusterloop: do index_cluster = 1, size(clusters)
                        cluster_ptr => clusters(index_cluster)
                        call get_stress_state(cluster_ptr)

                        if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
                            call apply_deformation_step(cluster_ptr, spin, index_cluster, deformation_gradient_during_time_step, velocity_gradient)
                        end if
                    enddo clusterloop
                !$OMP END DO
            !$OMP END PARALLEL

            do i = 1, size(clusters)
                do j = 1, size(clusters(i)%grains)
                    total_weight = total_weight+clusters(i)%weight
                    homogenized_stress = homogenized_stress+clusters(i)%grains(j)%stress*clusters(i)%weight
                    if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
                        taylor_factor = clusters(i)%grains(j)%sum_slip_current/von_mises_strain_rate
                        homogenized_taylor_factor = homogenized_taylor_factor+taylor_factor*clusters(i)%weight
                        homogenized_total_slip = homogenized_total_slip+clusters(i)%grains(j)%sum_slip_current*clusters(i)%weight
                        homogenized_work = homogenized_work+clusters(i)%grains(j)%get_work_rate()
                    end if
                end do
            end do


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

    subroutine output_current_state(file_handle)
        integer, intent(in):: file_handle

        call cur_write_block(file_handle, clusters, deformation_gradient)
    end subroutine

    subroutine simulation_finalize()
        deallocate(clusters)
    end subroutine

end module
