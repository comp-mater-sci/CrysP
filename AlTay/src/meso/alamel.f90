module alamel
    use utils
    use grain_module
    use relaxation_module
    use cluster_module
    use logging
    use taylor_ambiguity
    use hardening
    use simplex

    implicit none

    private
    public:: alamel_init, &
             alamel_update, &
             alamel_prepare_deformation, &
             alamel_get_stress, &
             alamel_deform

    character(*), parameter:: MOD_NAME = 'alamel'
    integer, dimension(5), parameter::   INITIAL_BASIS_SYSTEMS_FCC = [2, 5, 6, 7, 8], &
                                         INITIAL_BASIS_SYSTEMS_BCC = [1, 2, 4, 5, 7]

    !> Cluster used by the ALAMEL model.
    type, extends(Cluster):: AlamelCluster
        integer, dimension(10):: ind_basis_systems                  !> Indices of the currently active slip systems
        real(DP), dimension(3, 3):: initial_boundary_orientation    !> Rotation matrix representing the initial boundary orientation
        real(DP), dimension(10, 10):: inverse_basis = 0._DP         !> Inverse of the matrix formed by selecting the active slip
                                                                    !> systems. Buffering this quantity greatly improves the performance of the internally used simplex routine.
        type(Relaxation), dimension(2):: relaxations                !> ALAMEL clusters contain 2 relaxations which act as slip
                                                                    !> systems with 0 critical resolved shear stress.
    end type

    real(DP), dimension(3, 3):: imposed_spin_rate
    real(DP), dimension(3, 3):: velocity_gradient
    real(DP), dimension(3, 3):: deformation_gradient
    real(DP), dimension(3, 3):: deformation_gradient_during_time_step
    real(DP), dimension(3, 3):: next_deformation_gradient
    real(DP), dimension(3, 3):: deformation_gradient_increment

contains

    !>@Brief See meso_model_init
    subroutine alamel_init(orientations, deformation_mechanism, boundaries, clusters)
        real(DP), dimension(:,:), intent(in):: orientations
        integer, dimension(:,:,:), intent(in):: deformation_mechanism
        real(DP), dimension(:,:), intent(in):: boundaries
        class(Cluster), dimension(:), allocatable, intent(out):: clusters

        integer:: i, j, k, &
                  ind_basis_systems_grain(5), &
                  n_systems_grain

        n_systems_grain = size(deformation_mechanism, 3)
        ind_basis_systems_grain = merge(INITIAL_BASIS_SYSTEMS_FCC, INITIAL_BASIS_SYSTEMS_BCC, n_systems_grain == 12)

        deformation_gradient = UNIT_MATRIX_3X3

        allocate(AlamelCluster:: clusters(size(orientations, 2)/2))
        select type(clusters)
        type is (AlamelCluster)
            j = 1
            do i = 1, size(clusters)
                allocate(clusters(i)%grains(2))  ! ALAMEL clusters always have 2 grains
                clusters(i)%initial_boundary_orientation = matmul(deformation_gradient, transpose(from_euler_angles(boundaries(:,j))))
                clusters(i)%ind_basis_systems(1:5) = ind_basis_systems_grain
                clusters(i)%ind_basis_systems(6:10) = ind_basis_systems_grain+n_systems_grain
                do k = 1, 2
                    call clusters(i)%grains(k)%init(deformation_mechanism, orientations(:,2*(i-1)+k))
                    call clusters(i)%grains(k)%set_crss(hardening_get_crss((i-1)*2+k, 0._DP))
                    call clusters(i)%relaxations(k)%init(k)
                end do
                clusters(i)%inverse_basis = invert(get_basis(clusters(i)))
                call update_relaxations(clusters(i), deformation_gradient)
                clusters(i)%weight = cluster_weight(clusters(i), deformation_gradient)
                j = merge(1, j+1, j == size(boundaries, 2))
            end do
        end select
    end subroutine

    !>@Brief Update the model after a time step has elapsed.
    !>@Details See meso_update_model
    subroutine alamel_update()
        deformation_gradient = next_deformation_gradient
        deformation_gradient_during_time_step = matmul(deformation_gradient_increment, deformation_gradient)
        next_deformation_gradient = matmul(deformation_gradient_increment, deformation_gradient_during_time_step)
    end subroutine

    !>@Brief Calculate the imposed strain rate vector corresponding to a certain velocity gradient for a cluster
    !>@Details Projects the velocity gradient onto the crystal frames of the grains comprising the cluster.
    function calc_imposed_strain_rate(alamel_cluster, v_grad) result(imposed_strain_rate)
        type(AlamelCluster), intent(in):: alamel_cluster
        real(DP), dimension(3, 3), intent(in):: v_grad !> Velocity gradient
        real(DP), dimension(10):: imposed_strain_rate
        integer:: i

        do i = 1, 2
            imposed_strain_rate(5*(i-1)+1:5*i) = convert_stress_strain_space(v_grad .toframe. alamel_cluster%grains(i)%orientation)
        end do
    end function

    pure function homogenize_stress_state(alamel_cluster, stress_cluster) result(homogenized_stress)
        type(AlamelCluster), intent(in):: alamel_cluster
        real(DP), dimension(10), intent(in):: stress_cluster
        real(DP), dimension(3, 3):: homogenized_stress

        homogenized_stress = ((convert_stress_strain_space(stress_cluster(1:5)) .fromframe. alamel_cluster%grains(1)%orientation) &
                             + (convert_stress_strain_space(stress_cluster(6:10)) .fromframe. alamel_cluster%grains(2)%orientation)) &
                             / 2._DP
    end function

    pure function get_taylor_coeffs(alamel_cluster) result(coeffs)
        type(AlamelCluster), intent(in):: alamel_cluster
        real(DP), dimension(10, size(alamel_cluster%grains(1)%slip_systems)+size(alamel_cluster%grains(2)%slip_systems)+2):: coeffs

        integer:: i, &
                  n_systems(2)

        do i = 1, 2
            n_systems(i) = size(alamel_cluster%grains(i)%slip_systems)
        end do

        do i = 1, n_systems(1)
            coeffs(1:5, i) = alamel_cluster%grains(1)%slip_systems(i)%taylor_coeffs
            coeffs(6:10, i) = 0._DP
        end do
        do i = n_systems(1)+1, sum(n_systems)
            coeffs(1:5, i) = 0._DP
            coeffs(6:10, i) = alamel_cluster%grains(2)%slip_systems(i-n_systems(1))%taylor_coeffs
        end do
        do i = 1, 2
            coeffs(:,sum(n_systems)+i) = alamel_cluster%relaxations(i)%taylor_coeffs
        end do
    end function

    pure function get_crss(alamel_cluster) result(crss)
        type(AlamelCluster), intent(in):: alamel_cluster
        real(DP), dimension(2, size(alamel_cluster%grains(1)%slip_systems)+size(alamel_cluster%grains(2)%slip_systems)+2):: crss

        integer:: i, j

        do i = 1, 2
            do j = 1, size(alamel_cluster%grains(i)%slip_systems)
                crss(:,(i-1)*size(alamel_cluster%grains(1)%slip_systems)+j) = alamel_cluster%grains(i)%slip_systems(j)%crss
            end do
        end do
        crss(:,size(crss, 2)-1) = 0._DP
        crss(:,size(crss, 2)) = 0._DP
    end function

    !>@Brief Get stress state for a cluster
    !>@details Calculate the homogenized stress over the cluster in the global frame
    function alamel_get_stress(cluster_, v_grad) result(stress)
        class(Cluster), intent(inout):: cluster_ !> Intent(inout) because simplex modifies inverse basis
        real(DP), dimension(3, 3), intent(in):: v_grad !> Imposed velocity gradient
        real(DP), dimension(3, 3):: stress

        real(DP), dimension(size(cluster_%grains(1)%slip_systems)+size(cluster_%grains(2)%slip_systems)+2):: slip_rates, &
                                                                                                                   rss
        real(DP), dimension(10):: stress_cluster
        integer:: i


        select type (cluster_)
            type is (AlamelCluster)
                                call simplex_solve(get_taylor_coeffs(cluster_), &
                                   calc_imposed_strain_rate(cluster_, v_grad), &
                                   get_crss(cluster_), &
                                   cluster_%inverse_basis, &
                                   cluster_%ind_basis_systems, &
                                   slip_rates, &
                                   stress_cluster, &
                                   rss)

                stress = homogenize_stress_state(cluster_, stress_cluster)
            end select
    end function

    !>@Brief Prepares the ALAMEL model for a deformation.
    !>@Details see meso_prepare_deformation
    subroutine alamel_prepare_deformation(v_grad)
        real(DP), dimension(3, 3), intent(in):: v_grad

        velocity_gradient = v_grad
        imposed_spin_rate = antisymmetric_part(velocity_gradient)
        deformation_gradient_increment =  matrix_exponential_small_norm(velocity_gradient/2._DP)
        deformation_gradient_during_time_step = matmul(deformation_gradient_increment, deformation_gradient)
        next_deformation_gradient = matmul(deformation_gradient_increment, deformation_gradient_during_time_step)
    end subroutine

    subroutine alamel_deform(cluster_, index_cluster, stress, slip)
        class(Cluster), target, intent(inout):: cluster_
        integer, intent(in)::       index_cluster
        real(DP), dimension(3, 3), intent(out):: stress                 !> Homogenized stress over the cluster
        real(DP), intent(out):: slip                                    !> Total slip in the cluster for this time step
        real(DP)::                  orientation_increment(3, 3), &
                                    strain_grain(5), &
                                    strain_relaxations(5), &
                                    slip_grain, &
                                    spin_coeffs_relaxations(3, 2), &
                                    taylor_coeffs(10, size(cluster_%grains(1)%slip_systems)+size(cluster_%grains(2)%slip_systems)+2)
        integer::                   i, j, &
                                    n_overstressed_slip_systems, &
                                    n_systems_grain, &
                                    n_active_simplex, &
                                    offset_grain, &
                                    offset_systems, &
                                    offset_relaxations
        integer, dimension(:), allocatable:: ind_overstressed_slip_systems
        real(DP), dimension(size(cluster_%grains(1)%slip_systems)+size(cluster_%grains(2)%slip_systems)+2):: slip_rates, &
                                                                                                             rss
        real(DP), dimension(10):: stress_cluster, &
                                  imposed_strain_rate

        type(Grain), pointer::      grain_ptr
        character(*), parameter::   PROC_NAME = 'apply_deformation_step'

        select type (cluster_)
            type is (AlamelCluster)
                n_systems_grain = size(cluster_%grains(1)%slip_systems)
                offset_relaxations = 2*n_systems_grain

                call update_relaxations(cluster_, deformation_gradient_during_time_step)
                imposed_strain_rate = calc_imposed_strain_rate(cluster_, velocity_gradient)

                taylor_coeffs = get_taylor_coeffs(cluster_)

                call simplex_solve(taylor_coeffs, &
                                   imposed_strain_rate, &
                                   get_crss(cluster_), &
                                   cluster_%inverse_basis, &
                                   cluster_%ind_basis_systems, &
                                   slip_rates, &
                                   stress_cluster, &
                                   rss)

                stress = homogenize_stress_state(cluster_, stress_cluster)

                slip = 0._DP
                do j = 1, 2
                    grain_ptr => cluster_%grains(j)
                    offset_grain = (j-1)*5
                    offset_systems = (j-1)*n_systems_grain

                    associate (slip_rates_grain=>slip_rates(offset_systems+1:offset_systems+n_systems_grain), &
                               slip_rates_relaxations=>slip_rates(offset_relaxations+1:), &
                               taylor_coeffs_relaxations=>taylor_coeffs(offset_grain+1:offset_grain+5, offset_relaxations+1:))

                        call  assess_slip_system_activity(cluster_%grains(j), &
                                                          rss(offset_systems+1:offset_systems+n_systems_grain), &
                                                          slip_rates_grain, &
                                                          n_active_simplex, &
                                                          ind_overstressed_slip_systems)

                        if (allocated(ind_overstressed_slip_systems)) then
                            !Determine strain absorbed by slip systems (imposed strain-relaxations)
                            strain_relaxations = matmul(taylor_coeffs_relaxations, slip_rates_relaxations)
                            strain_grain = imposed_strain_rate(offset_grain+1:offset_grain+5)-strain_relaxations

                            slip_rates_grain = resolve_taylor_ambiguity(ind_overstressed_slip_systems, &
                                rss(ind_overstressed_slip_systems+offset_systems), &
                                strain_grain, &
                                grain_ptr%get_taylor_coeffs(), &
                                n_active_simplex)
                        end if

                        slip_grain = sum(abs(slip_rates_grain))
                        grain_ptr%sum_slip = grain_ptr%sum_slip+slip_grain
                        slip = slip+slip_grain

                        !Update hardening model state
                        call hardening_update_state((index_cluster-1)*2+j, 1._DP, slip_rates_grain)
                        call grain_ptr%set_crss(hardening_get_crss((index_cluster-1)*2+j, grain_ptr%sum_slip))

                        !Get spin coefficients of the relaxations corresponding to the current grain
                        do i = 1, 2
                            spin_coeffs_relaxations(:,i) = cluster_%relaxations(i)%spin_coeffs((j-1)*3+1:j*3)
                        end do

                        orientation_increment = UNIT_MATRIX_3X3 &
                                                +(imposed_spin_rate .toframe. grain_ptr%orientation) &                !>Change of reference frame
                                                -convert_spin(matmul(grain_ptr%get_spin_coeffs(), slip_rates_grain)) &   !>Spin induced by activation of slip systems
                                                -convert_spin(matmul(spin_coeffs_relaxations, slip_rates_relaxations))
                        grain_ptr%orientation = matmul(orientation_increment, grain_ptr%orientation)


                    end associate
                end do

                !Update cluster state to be consistent with the end of the time step.
                call update_relaxations(cluster_, next_deformation_gradient)
                cluster_%weight = cluster_weight(cluster_, next_deformation_gradient)
        end select
    end subroutine

    real(DP) function cluster_weight(alamel_cluster, def_grad) result(weight)
        type(AlamelCluster), intent(in):: alamel_cluster
        real(DP), intent(in):: def_grad(3, 3)
        real(DP):: grain_axes(3, 3), &
                   axis_lengths(3), &
                   alignment_factor

        !Applying deformation gradient to initial grain boundary orientation yields deformed grain axes
        grain_axes = matmul(def_grad, alamel_cluster%initial_boundary_orientation)

        axis_lengths = norm2(grain_axes, 1)
        !Alignment factor equals sin(axes 2 and 3) * cos(axis 1 and normal to plane defined by axes 2 and 3)
        !The more the axes are orthogonal, the more alignment factor tends to 1.
        alignment_factor = abs(grain_axes(:,1) .dot. (grain_axes(:,2) .cross. grain_axes(:,3))) / product(axis_lengths)

        !See Van Houtte et. al., 2004: Appendix A
        select case(minloc(axis_lengths, 1))
            case(1)
                weight = alignment_factor * (2._DP*(axis_lengths(2)-axis_lengths(1))*axis_lengths(1)**2+4.D0*axis_lengths(1)**3/3._DP)
            case(2)
                weight = alignment_factor * (2._DP*(axis_lengths(1)-axis_lengths(2))*axis_lengths(2)**2+4.D0*axis_lengths(2)**3/3._DP)
            case (3)
                weight = alignment_factor * (4._DP*(axis_lengths(1)-axis_lengths(3))*(axis_lengths(2)-axis_lengths(3))*axis_lengths(3) + &
                2._DP*(axis_lengths(1)+axis_lengths(2) - 2._DP*axis_lengths(3))*axis_lengths(3)**2+4._DP*axis_lengths(3)**3/3._DP)
        end select
    end function

    function get_basis(alamel_cluster) result(basis)
        type(AlamelCluster), intent(in):: alamel_cluster
        real(DP), dimension(10, 10):: basis

        integer:: i, &
                  n_systems_grain, &
                  ind_basis_system

        n_systems_grain = size(alamel_cluster%grains(1)%slip_systems)

        do i = 1, 10
            ind_basis_system = alamel_cluster%ind_basis_systems(i)
            if (ind_basis_system <= n_systems_grain) then
                basis(1:5, i) = alamel_cluster%grains(1)%slip_systems(ind_basis_system)%taylor_coeffs
                basis(6:10, i) = 0._DP
            else if (ind_basis_system <= 2*n_systems_grain) then
                basis(1:5, i) = 0._DP
                basis(6:10, i) = alamel_cluster%grains(2)%slip_systems(ind_basis_system-n_systems_grain)%taylor_coeffs
            else
                basis(:,i) = alamel_cluster%relaxations(ind_basis_system-2*n_systems_grain)%taylor_coeffs
            end if
        end do
    end function

    subroutine update_relaxations(alamel_cluster, def_grad)
        type(AlamelCluster), intent(inout):: alamel_cluster
        real(DP), dimension(3, 3), intent(in):: def_grad

        real(DP):: boundary_to_crystal(3, 3, 2), &
                   new_vec(10), &
                   dummy(10), &
                   new_boundary_frame(3, 3), &
                   basis(10, 10)
        integer:: i, &
                  n_systems_grains

        !Calculate current boundary reference frame
        new_boundary_frame = matmul(def_grad, alamel_cluster%initial_boundary_orientation)
        new_boundary_frame(:,3) = new_boundary_frame(:,1) .cross. new_boundary_frame(:,2)
        new_boundary_frame(:,2) = new_boundary_frame(:,3) .cross. new_boundary_frame(:,1)
        new_boundary_frame = normalize(new_boundary_frame)

        !Transform relaxation from boundary frame to crystal frame
        !Composed of rotation from boundary to global frame and then from global to crystal frame.
        do i = 1, 2
            boundary_to_crystal(:,:,i) = matmul(alamel_cluster%grains(i)%orientation, new_boundary_frame)
        end do

        !For ALAMEL we may assume that the relaxations are part of the basis and they change with every time step. Therefore we
        !must always recalculate the inverse basis.
        do i = 1, 2
            call alamel_cluster%relaxations(i)%update(boundary_to_crystal)
        end do

        basis = get_basis(alamel_cluster)
        n_systems_grains = size(alamel_cluster%grains(1)%slip_systems)+size(alamel_cluster%grains(2)%slip_systems)
        do i = 1, 10
            if (alamel_cluster%ind_basis_systems(i)> n_systems_grains)then
                new_vec = matmul(alamel_cluster%inverse_basis, basis(:,i))
                call update_inverse_basis(alamel_cluster%inverse_basis, new_vec, i, dummy)
            end if
        end do
    end subroutine
end module
