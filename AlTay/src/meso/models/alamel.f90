!> Implementation of the ALAMEL model developed at KU Leuven.
!>
!> See "Deformation texture prediction: from the Taylor model to the advanced Lamel model"
!> by Van Houtte et. al. published in the International Journal of Plasticity 21 for details.

module alamel
    use utils
    use grain_module
    use relaxation_module
    use cluster_module
    use logging
    use taylor_ambiguity
    use micro
    use simplex
    use meso_model

    implicit none

    private
    public:: AlamelModel

    character(*), parameter:: MOD_NAME = 'alamel'   !! Module name for easy logging.

    !> Cluster used by the ALAMEL model.
    type, extends(Cluster):: AlamelCluster
        integer, dimension(10):: ind_basis_systems                  !! Indices of the currently active slip systems
        real(DP), dimension(3, 3):: initial_boundary_orientation    !! Rotation matrix representing the initial boundary orientation
        real(DP), dimension(10, 10):: inverse_basis = 0._DP         !! Inverse of the matrix formed by selecting the active slip
                                                                    !! systems. Buffering this quantity greatly improves the performance.
        type(Relaxation), dimension(2):: relaxations                !! ALAMEL clusters contain 2 relaxations which act as slip
                                                                    !! systems with 0 critical resolved shear stress.
    end type

    !> Implementation of the ALAMEL model
    type, extends(MesoModel):: AlamelModel
        real(DP), dimension(3, 3):: deformation_gradient                    !! Description of the current grain shape (at the beginning of the current time step). Considered identical for all grains.
        real(DP), dimension(3, 3):: deformation_gradient_during_time_step   !! Grain shape in the middle of the time step being simulated
        real(DP), dimension(3, 3):: next_deformation_gradient               !! Grain shape at the end of the time step.
        real(DP), dimension(3, 3):: deformation_gradient_increment          !! Increment of the deformation gradient over *half* a time step.
    contains
        procedure, nopass:: get_parameters => alamel_get_parameters         !! Inherited from [[MesoModel]]
        procedure:: init                   => alamel_init                   !! Inherited from [[MesoModel]]
        procedure:: get_stress             => alamel_get_stress             !! Inherited from [[MesoModel]]
        procedure:: apply_step             => alamel_deform                 !! Inherited from [[MesoModel]]
        procedure:: update                 => alamel_update                 !! Inherited from [[MesoModel]]
        procedure:: prepare_deformation    => alamel_prepare_deformation    !! Inherited from [[MesoModel]]
    end type

contains

    !> Convert the type of a provided generic cluster to AlamelCluster
    !>
    !> This is the closest Fortran can get to proper type casting.
    !> Useful for accessing AlamelCluster-specific fields without the boilerplate of type selection and error handling in each calling procedure.
    !> If the input cluster is not an AlamelCluster, the routine crashes the program.
    !> Otherwise, a pointer to this cluster of type AlamelCluster is returned.
    function to_alamel_cluster(cluster_) result(ptr)
        class(Cluster), target, intent(in):: cluster_   !! Generic input cluster
        type(AlamelCluster), pointer:: ptr              !! Pointer to input cluster of type AlamelCluster

        select type (cluster_)
            type is (AlamelCluster)
                ptr => cluster_
            class default
                call log_error(MOD_NAME, 'to_alamel_cluster', ERR_TYPE)
        end select
    end function

    !> See [[MesoModel:get_parameters]]
    function alamel_get_parameters() result(params)
        type(Parameter), dimension(:), allocatable:: params !! - **Boundaries**: List of Euler angles in Bunge convention
                                                            !! denoting the orientation of the grain boundary plane normals.

        allocate(params(1))
        params(1) = parameter_init("Boundaries", TYPE_ANGLES_LIST)
    end function

    !> See [[MesoModel:init]]
    subroutine alamel_init(this, grains, params, clusters)
        class(AlamelModel), intent(inout):: this
        type(Grain), dimension(:), intent(in):: grains
        type(Parameter), dimension(:), intent(in):: params
        class(Cluster), dimension(:), allocatable, intent(out):: clusters

        integer:: i, j, k, &
                  ind_basis_systems_grain(5), &
                  n_systems_first_grain
        real(DP), dimension(:,:), allocatable:: boundaries


        allocate(boundaries(3, parameter_size(params(1))))
        boundaries = params(1)

        this%deformation_gradient = UNIT_MATRIX_3X3

        allocate(AlamelCluster:: clusters(size(grains)/2))

        !Must check type even though we just allocated due to Fortran semantics.
        select type(clusters)
        type is (AlamelCluster)
            j = 1
            do i = 1, size(clusters)
                clusters(i)%grains = grains(2*(i-1)+1:2*i)
                clusters(i)%initial_boundary_orientation = matmul(this%deformation_gradient, transpose(euler_to_tensor(boundaries(:,j))))
                n_systems_first_grain = size(clusters(i)%grains(1)%model%taylor_coeffs, 2)
                do k = 1, 2
                    ind_basis_systems_grain = clusters(i)%grains(k)%model%basis
                    clusters(i)%ind_basis_systems((k-1)*5+1:k*5) = ind_basis_systems_grain + (k-1)*n_systems_first_grain
                    call clusters(i)%relaxations(k)%init(k)
                end do
                clusters(i)%inverse_basis = invert(get_basis(clusters(i)))
                call update_relaxations(clusters(i), this%deformation_gradient)
                clusters(i)%weight = cluster_weight(clusters(i), this%deformation_gradient)
                j = merge(1, j+1, j == size(boundaries, 2))
            end do
        end select
    end subroutine

    !> See [[MesoModel:update]]
    subroutine alamel_update(this)
        class(AlamelModel), intent(inout):: this

        this%deformation_gradient = this%next_deformation_gradient
        this%deformation_gradient_during_time_step = matmul(this%deformation_gradient_increment, this%deformation_gradient)
        this%next_deformation_gradient = matmul(this%deformation_gradient_increment, this%deformation_gradient_during_time_step)
    end subroutine

    !> Get the number of slip systems of each grain in the cluster.
    !>
    !> Returns an array with the following structure: [number of systems for first grain, number of systems for second grain].
    pure function get_n_systems(cluster_) result(n_systems)
        class(Cluster), intent(in):: cluster_
        integer, dimension(2):: n_systems

        integer:: i

        do i = 1, size(cluster_%grains)
            n_systems(i) = size(cluster_%grains(i)%state%crss, 2)
        end do
    end function

    !> Get the total number of slip systems in the cluster.
    !>
    !> Simply the sum of the slip systems of the first and second grain and the 2 relaxations.
    pure function total_systems(cluster_) result(n_systems)
        class(Cluster), intent(in):: cluster_
        integer:: n_systems

        n_systems = size(cluster_%grains(1)%model%taylor_coeffs, 2) + size(cluster_%grains(2)%model%taylor_coeffs, 2) + 2
    end function


    !> Calculate the imposed strain rate vector corresponding to a certain velocity gradient for a cluster
    !>
    !> Projects the velocity gradient onto the crystal frames of the grains comprising the cluster.
    function calc_imposed_strain_rate(alamel_cluster, v_grad) result(imposed_strain_rate)
        type(AlamelCluster), intent(in):: alamel_cluster    !! The cluster.
        real(DP), dimension(3, 3), intent(in):: v_grad      !! Velocity gradient to project onto the cluster.
        real(DP), dimension(10):: imposed_strain_rate

        integer:: i

        do i = 1, 2
            imposed_strain_rate(5*(i-1)+1:5*i) = tensor_to_deviatoric(v_grad .toframe. alamel_cluster%grains(i)%orientation)
        end do
    end function

    !> Homogenize the stress state of the cluster.
    !>
    !> Weighted average of the stress states of the individual grains converted to the global frame.
    pure function homogenize_stress_state(alamel_cluster, stress_cluster) result(homogenized_stress)
        type(AlamelCluster), intent(in):: alamel_cluster
        real(DP), dimension(10), intent(in):: stress_cluster    !! Stress state as calculated by simplex.
                                                                !! I.e. a 10D vector representing the stress state of both grains in their respective reference frames.
        real(DP), dimension(3, 3):: homogenized_stress

        homogenized_stress = ((deviatoric_to_tensor(stress_cluster(1:5)) .fromframe. alamel_cluster%grains(1)%orientation) &
                             + (deviatoric_to_tensor(stress_cluster(6:10)) .fromframe. alamel_cluster%grains(2)%orientation)) &
                             / 2._DP
    end function

    !> Assemble the Taylor coefficients of the individual grains and relaxations into a single matrix.
    pure function get_taylor_coeffs(alamel_cluster) result(coeffs)
        type(AlamelCluster), intent(in):: alamel_cluster
        real(DP), dimension(10, total_systems(alamel_cluster)):: coeffs

        integer:: i, &
                  n_systems(2)

        n_systems = get_n_systems(alamel_cluster)

        coeffs(1:5, 1:n_systems(1))                 = alamel_cluster%grains(1)%model%taylor_coeffs
        coeffs(6:10, 1:n_systems(1))                = 0._DP
        coeffs(1:5, n_systems(1)+1:sum(n_systems))  = 0._DP
        coeffs(6:10, n_systems(1)+1:sum(n_systems)) = alamel_cluster%grains(2)%model%taylor_coeffs
        coeffs(:,sum(n_systems)+1)                  = alamel_cluster%relaxations(1)%taylor_coeffs
        coeffs(:,sum(n_systems)+2)                  = alamel_cluster%relaxations(2)%taylor_coeffs
    end function

    !> Assemble the CRSS of the individual grains and relaxations into a single matrix.
    pure function get_crss(alamel_cluster) result(crss)
        type(AlamelCluster), intent(in):: alamel_cluster
        real(DP), dimension(2, total_systems(alamel_cluster)):: crss

        integer:: n_systems(2)

        n_systems = get_n_systems(alamel_cluster)

        crss(:,1:n_systems(1)) = alamel_cluster%grains(1)%state%crss
        crss(:, n_systems(1)+1:sum(n_systems)) = alamel_cluster%grains(2)%state%crss
        crss(:, sum(n_systems)+1:) = 0._DP
    end function

    !> See [[MesoModel:get_stress]]
    function alamel_get_stress(this, cluster_, v_grad) result(stress)
        class(AlamelModel), intent(in):: this
        class(Cluster), target, intent(inout):: cluster_
        real(DP), dimension(3, 3), intent(in):: v_grad
        real(DP), dimension(3, 3):: stress

        real(DP), dimension(total_systems(cluster_)):: slip_rates, &
                                                       rss
        real(DP), dimension(10):: stress_cluster
        type(AlamelCluster), pointer:: cluster_ptr

        !Convert to type AlamelCluster to gain access to model-specific fields
        cluster_ptr => to_alamel_cluster(cluster_)

        call simplex_solve(get_taylor_coeffs(cluster_ptr), &
                           calc_imposed_strain_rate(cluster_ptr, v_grad), &
                           get_crss(cluster_ptr), &
                           cluster_ptr%inverse_basis, &
                           cluster_ptr%ind_basis_systems, &
                           slip_rates, &
                           stress_cluster, &
                           rss)

        stress = homogenize_stress_state(cluster_ptr, stress_cluster)
    end function

    !> See [[MesoModel:prepare_deformation]]
    subroutine alamel_prepare_deformation(this, v_grad)
        class(AlamelModel), intent(inout):: this
        real(DP), dimension(3, 3), intent(in):: v_grad

        this%velocity_gradient = v_grad
        this%imposed_spin_rate = spin_to_tensor(tensor_to_spin(v_grad)) !Strip symmetric component
        this%deformation_gradient_increment =  matrix_exponential_small_norm(this%velocity_gradient/2._DP)
        this%deformation_gradient_during_time_step = matmul(this%deformation_gradient_increment, this%deformation_gradient)
        this%next_deformation_gradient = matmul(this%deformation_gradient_increment, this%deformation_gradient_during_time_step)
    end subroutine

    !> See [[MesoModel:apply_step]]
    subroutine alamel_deform(this, cluster_, stress, slip)
        class(AlamelModel), intent(in):: this
        class(Cluster), target, intent(inout):: cluster_
        real(DP), dimension(3, 3), intent(out):: stress
        real(DP), intent(out):: slip
        real(DP)::                  orientation_increment(3, 3), &
                                    strain_grain(5), &
                                    strain_relaxations(5), &
                                    slip_grain, &
                                    spin_coeffs_relaxations(3, 2), &
                                    taylor_coeffs(10, total_systems(cluster_))
        integer::                   i, j, &
                                    n_systems(2), &
                                    n_active_simplex, &
                                    offset_grain, &
                                    offset_systems, &
                                    offset_relaxations
        integer, dimension(:), allocatable:: ind_overstressed_slip_systems
        real(DP), dimension(total_systems(cluster_)):: slip_rates, &
                                                       rss
        real(DP), dimension(10):: stress_cluster, &
                                  imposed_strain_rate
        type(AlamelCluster), pointer:: cluster_ptr

        cluster_ptr => to_alamel_cluster(cluster_)

        n_systems = get_n_systems(cluster_)
        offset_relaxations = sum(n_systems)

        call update_relaxations(cluster_ptr, this%deformation_gradient_during_time_step)
        imposed_strain_rate = calc_imposed_strain_rate(cluster_ptr, this%velocity_gradient)

        taylor_coeffs = get_taylor_coeffs(cluster_ptr)

        call simplex_solve(taylor_coeffs, &
                           imposed_strain_rate, &
                           get_crss(cluster_ptr), &
                           cluster_ptr%inverse_basis, &
                           cluster_ptr%ind_basis_systems, &
                           slip_rates, &
                           stress_cluster, &
                           rss)

        stress = homogenize_stress_state(cluster_ptr, stress_cluster)

        slip = 0._DP
        do j = 1, 2
            offset_grain = (j-1)*5
            offset_systems = (j-1)*n_systems(1)

            associate (grain_ => cluster_ptr%grains(j), &
                       slip_rates_grain=>slip_rates(offset_systems+1:offset_systems+n_systems(j)), &
                       slip_rates_relaxations=>slip_rates(offset_relaxations+1:), &
                       taylor_coeffs_relaxations=>taylor_coeffs(offset_grain+1:offset_grain+5, offset_relaxations+1:))

                call  assess_slip_system_activity(cluster_ptr%grains(j), &
                                                  rss(offset_systems+1:offset_systems+n_systems(j)), &
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
                        grain_%model%taylor_coeffs, &
                        n_active_simplex)
                end if

                slip_grain = sum(abs(slip_rates_grain))
                slip = slip+slip_grain

                !Update hardening model state
                call micro_deform(grain_, 1._DP, slip_rates_grain)

                !Get spin coefficients of the relaxations corresponding to the current grain
                do i = 1, 2
                    spin_coeffs_relaxations(:,i) = cluster_ptr%relaxations(i)%spin_coeffs((j-1)*3+1:j*3)
                end do

                orientation_increment = UNIT_MATRIX_3X3 &
                                        -(this%imposed_spin_rate .toframe. grain_%orientation) &                !Change of reference frame
                                        +spin_to_tensor(matmul(grain_%model%spin_coeffs, slip_rates_grain)) &     !Spin induced by activation of slip systems
                                        +spin_to_tensor(matmul(spin_coeffs_relaxations, slip_rates_relaxations))
                grain_%orientation = matmul(orientation_increment, grain_%orientation)
            end associate
        end do

        !Update cluster state to be consistent with the end of the time step.
        call update_relaxations(cluster_ptr, this%next_deformation_gradient)
        cluster_ptr%weight = cluster_weight(cluster_ptr, this%next_deformation_gradient)
    end subroutine

    !> Determine the weight of a cluster
    !>
    !> Rough estimation based on the boundary plane orientation relative to the grain shape.
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

    !> Get the basis matrix of a cluster.
    function get_basis(alamel_cluster) result(basis)
        type(AlamelCluster), intent(in):: alamel_cluster
        real(DP), dimension(10, 10):: basis

        integer:: i, &
                  n_systems(2), &
                  ind_basis_system

        n_systems = get_n_systems(alamel_cluster)

        do i = 1, 10
            ind_basis_system = alamel_cluster%ind_basis_systems(i)
            if (ind_basis_system <= n_systems(1)) then
                basis(1:5, i) = alamel_cluster%grains(1)%model%taylor_coeffs(:,ind_basis_system)
                basis(6:10, i) = 0._DP
            else if (ind_basis_system <= sum(n_systems)) then
                basis(1:5, i) = 0._DP
                basis(6:10, i) = alamel_cluster%grains(2)%model%taylor_coeffs(:,ind_basis_system-n_systems(1))
            else
                basis(:,i) = alamel_cluster%relaxations(ind_basis_system-sum(n_systems))%taylor_coeffs
            end if
        end do
    end function

    !> Update the orientation of the relaxation after deformation.
    subroutine update_relaxations(alamel_cluster, def_grad)
        type(AlamelCluster), intent(inout):: alamel_cluster
        real(DP), dimension(3, 3), intent(in):: def_grad

        real(DP):: boundary_to_crystal(3, 3, 2), &
                   new_vec(10), &
                   dummy(10), &
                   new_boundary_frame(3, 3), &
                   basis(10, 10)
        integer:: i

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
        do i = 1, 10
            if (alamel_cluster%ind_basis_systems(i)> sum(get_n_systems(alamel_cluster)))then
                new_vec = matmul(alamel_cluster%inverse_basis, basis(:,i))
                call update_inverse_basis(alamel_cluster%inverse_basis, new_vec, i, dummy)
            end if
        end do
    end subroutine
end module
