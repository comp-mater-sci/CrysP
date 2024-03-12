module simulation
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

    real(DP), allocatable:: homogenized_total_slip_rateTOT, & !< homogenized slip accumulated over calls
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
        homogenized_total_slip_rateTOT = 0.D0

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
                  index_in_cluster1, &
                  index_grain, &
                  step, &
                  n_grains, &
                  info, &
                  NFILE, &
                  i, &
                  j, &
                  l, &
                  ifil4
        real(DP):: orientation(3, 3, 2), &
                   strain_ab(2), &
                   stress(3, 3), & 
                   cluster_weight, &
                   crss(2, 96), &
                   total_weight, &
                   homogenized_stress(3, 3), &
                   ssqgx, &
                   homogenized_total_slip_rate, & ! homogenized_total_slip_rate: homogenized slip per step
                   total_slip_rate, &  ! Total slip rate in current grain
                   taylor_factor, &  ! Taylor factor of the current grain
                   homogenized_taylor_factor, &   !Volume-averaged Taylor factor
                   WorkRate, &  ! Rate of plastic work per unit volume in the crystal
                   homogenized_work_rate, &  ! Total plastic work per unit volume in crystal
                   deformation_gradient_increment(3, 3), &
                   deformation_gradient_increment_inverse(3, 3), &
                   strain_rate(3, 3), &
                   spin(3, 3), &
                   von_mises_strain_mode(3, 3), &
                   von_mises_strain_rate, &
                   next_deformation_gradient(3, 3)

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
            homogenized_total_slip_rate = 0._DP

            nrstep = nrstep+1

            next_deformation_gradient = matmul(deformation_gradient_increment, deformation_gradient) 


            !Added for lamel model:
            !Organisation reading temporary texture file, 
            !in such way that the program TAYLOR can process the crystals
            !by sets of 2.
            !Taylor must therefore have "advance knowledge" of the
            !orientation to come at the moment that it starts such
            !computation.
            index_in_cluster = 1
            index_in_cluster1 = cluster_size
            ifil4 = 0
            clusterloop: do index_grain = 1, n_grains
                taylor_factor = 0.0_DP
                total_slip_rate = 0.0_DP
                WorkRate = 0.0_DP
                homogenized_work_rate = 0.0_DP

                do L = index_in_cluster, index_in_cluster1
                    if (ifil4 == n_grains) exit
                    ifil4 = ifil4+1
                    call DYNFIL_getGrain(ifil4, orientation(1:3, 1:3, L), cluster_weight, strain_ab(L))
                end do
                index_in_cluster1 = mod(index_in_cluster1, cluster_size)+1
                index_in_cluster = index_in_cluster1
                
                call get_stress_state(stress, orientation, index_grain, strain_ab, cluster_size, index_in_cluster, crss, n_slip_systems_grain, velocity_gradient, next_deformation_gradient, cluster_weight)

                if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
                    deformation_gradient = next_deformation_gradient 
                    call apply_deformation_step(index_grain, total_slip_rate, WorkRate, spin, crss(1:2, 1:n_slip_systems_grain), orientation(:,:,index_in_cluster), taylor_coeffs)
                    call DYNFIL_setGrain(index_grain, orientation(:,:,index_in_cluster), strain_ab(index_in_cluster) + total_slip_rate)  ! Step time here implicitly assumed to be 1.0s
                end if
                if(index_in_cluster == 1) then
                    ssqgx = cluster_weight
                else
                    cluster_weight = ssqgx
                end if

                total_weight = total_weight+cluster_weight
                taylor_factor = total_slip_rate /  von_mises_strain_rate

                homogenized_stress          = homogenized_stress+stress*cluster_weight
                homogenized_taylor_factor   = homogenized_taylor_factor+taylor_factor*cluster_weight
                homogenized_total_slip_rate = homogenized_total_slip_rate+total_slip_rate*cluster_weight  !Step time here implicitly assumed to be 1.0s
                homogenized_work_rate       = homogenized_work_rate+WorkRate                              !Step time here implicitly assumed to be 1.0s
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
                callout%homogenised_slip_tot = homogenized_total_slip_rateTOT
                callout%effective_macro_strain_tot = von_mises_strain
                callout%effective_macro_strain_tot_end = von_mises_strain+von_mises_strain_rate
            end associate

            homogenized_total_slip_rate = homogenized_total_slip_rate/total_weight
            if (.not.astate%simulCalls(astate%this)%input%keep_state) homogenized_total_slip_rateTOT = homogenized_total_slip_rateTOT+homogenized_total_slip_rate

            von_mises_strain = von_mises_strain+von_mises_strain_rate
        enddo steploop
    end subroutine
end module
