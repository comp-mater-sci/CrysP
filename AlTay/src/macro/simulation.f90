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
    real(DP), dimension(3, 3):: deformation_gradient = UNIT_MATRIX_3X3
    type(Cluster), dimension(:), pointer, contiguous:: clusters

    character(*), parameter:: MOD_NAME = 'Simul'


    public:: simulation_init, &
             simulation_run, &
             simulation_finalize, &
             output_current_state, &
             get_stress
    contains

    ! initialization call
    subroutine simulation_init(orientations, boundaries)
        real(DP), dimension(:,:), allocatable, intent(in):: orientations, &
                                                            boundaries

        character(len = 40):: TITEL
        integer:: info, &
                  cluster_size, i, j
        character(*), parameter:: PROC_NAME = 'SIMUL0'


        cluster_size    = acnf%simul_init%NGR

        homogenized_total_slipTOT = 0.D0

        if (cluster_size < 1 .or. cluster_size > 2) &
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Incorrect value of cluster_size')

        TITEL  = acnf%jobtitle
        ! Only if CUR file is requested
        if (NFILE1 == 1) call cur_write_title(IMP1, TITEL, info)
  98    format (A)

        deformation_gradient = UNIT_MATRIX_3X3
        clusters => taylor_init(acnf%deformation_mechanism, cluster_size, acnf%simul_init%FMicro, orientations, boundaries)
        von_mises_strain = 0._DP

    end subroutine

    function get_stress(velocity_gradient) result(homogenized_stress)
        real(DP), dimension(3, 3), intent(in):: velocity_gradient
        real(DP), dimension(3, 3):: homogenized_stress

        integer:: i, j, &
                  n_clusters, &
                  cluster_size
        real(DP):: total_weight

        n_clusters = size(clusters)
        cluster_size = size(clusters(1)%grains)

        !$OMP PARALLEL SHARED(clusters, velocity_gradient, n_clusters) PRIVATE(i)
            !$OMP DO SCHEDULE(DYNAMIC, 1)
                do i = 1, n_clusters
                    call get_stress_state(clusters(i), velocity_gradient)
                end do
            !$OMP END DO
        !$OMP END PARALLEL

        total_weight = 0._DP
        homogenized_stress = 0._DP
        do i = 1, n_clusters
            total_weight = total_weight+clusters(i)%weight
            do j = 1, size(clusters(i)%grains)
                homogenized_stress = homogenized_stress+clusters(i)%grains(j)%stress*clusters(i)%weight
            end do
        end do
        homogenized_stress = homogenized_stress/total_weight/cluster_size
    end function


    subroutine simulation_run(NFILE0, velocity_gradient)
        real(DP), intent(in):: velocity_gradient(3, 3)
        integer, intent(in):: NFILE0
        integer:: cluster_size, &
                  n_clusters, &
                  step, &
                  info, &
                  NFILE, &
                  i, j, l               !> Iteration variables
        real(DP):: total_weight, &
                   homogenized_stress(3, 3), &
                   homogenized_total_slip, & ! homogenized_total_slip: homogenized slip per step
                   taylor_factor, &  ! Taylor factor of the current grain
                   homogenized_taylor_factor, &   !Volume-averaged Taylor factor
                   WorkRate, &  ! Rate of plastic work per unit volume in the crystal
                   homogenized_work, &  ! Total plastic work per unit volume in crystal
                   deformation_gradient_increment(3, 3), &
                   strain_rate(3, 3), &
                   spin(3, 3), &
                   von_mises_strain_rate

        n_clusters = size(clusters)
        cluster_size = acnf%simul_init%NGR
        NFILE = NFILE0*NFILE1

        strain_rate = symmetric_part(velocity_gradient)
        spin = antisymmetric_part(velocity_gradient)
        von_mises_strain_rate = SQR0P67*norm2(strain_rate)
        deformation_gradient_increment = matrix_exponential_small_norm(velocity_gradient)
        call meso_update_model(velocity_gradient)

        steploop: DO step = 1, astate%simulCalls(astate%this)%input%nsteps
            total_weight = 0._DP
            homogenized_stress = 0._DP
            homogenized_taylor_factor = 0._DP
            homogenized_total_slip = 0._DP
            homogenized_work = 0.0_DP

            nrstep = nrstep+1

            !$OMP PARALLEL SHARED(clusters, n_clusters, spin, velocity_gradient) PRIVATE(i)
                !$OMP DO SCHEDULE(DYNAMIC, 1)
                    do i = 1, n_clusters
                        call apply_deformation_step(clusters(i), spin, i, velocity_gradient)
                    enddo
                !$OMP END DO
            !$OMP END PARALLEL

            do i = 1, n_clusters
                do j = 1, cluster_size
                    total_weight = total_weight+clusters(i)%weight
                    homogenized_stress = homogenized_stress+clusters(i)%grains(j)%stress*clusters(i)%weight
                    taylor_factor = clusters(i)%grains(j)%sum_slip_current/von_mises_strain_rate
                    homogenized_taylor_factor = homogenized_taylor_factor+taylor_factor*clusters(i)%weight
                    homogenized_total_slip = homogenized_total_slip+clusters(i)%grains(j)%sum_slip_current*clusters(i)%weight
                    homogenized_work = homogenized_work+clusters(i)%grains(j)%get_work_rate()
                end do
            end do

            homogenized_stress = homogenized_stress/total_weight
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
            homogenized_total_slipTOT = homogenized_total_slipTOT+homogenized_total_slip

            von_mises_strain = von_mises_strain+von_mises_strain_rate
            deformation_gradient = matmul(deformation_gradient_increment, deformation_gradient)
            call meso_update_model()
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
