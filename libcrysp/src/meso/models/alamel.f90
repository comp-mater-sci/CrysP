!> Implementation of the ALAMEL model developed at KU Leuven.
!>
!> See "Deformation texture prediction: from the Taylor model to the advanced Lamel model"
!> by Van Houtte et. al. published in the International Journal of Plasticity 21 for details.

module alamel
    use base_defs
    use conversions
    use crysp_grain
    use relaxation_module
    use crysp_cluster
    use logging
    use taylor_ambiguity
    use micro
    use simplex
    use crysp_meso_model
    use crysp_serialization
    use crysp_input

    implicit none

    private
    public:: AlamelModel

    character(*), parameter:: MOD_NAME = 'alamel'   !! Module name for easy logging.

    !> Cluster used by the ALAMEL model.
    type, extends(ClusterState):: AlamelClusterState
        integer, dimension(10):: ind_basis_systems                  !! Indices of the currently active slip systems
        real(DP), dimension(3):: initial_boundary_normal            !! Inital direction of the normal to the boundary plane.
        real(DP), dimension(10, 10):: inverse_basis = 0._DP         !! Inverse of the matrix formed by selecting the active slip
                                                                    !! systems. Buffering this quantity greatly improves the performance.
        type(Relaxation), dimension(2):: relaxations                !! ALAMEL clusters contain 2 relaxations which act as slip
                                                                    !! systems with 0 critical resolved shear stress.
    contains
        procedure:: serialize => alamel_cluster_serialize
        procedure:: deserialize => alamel_cluster_deserialize
    end type

    !> Implementation of the ALAMEL model
    type, extends(MesoModel):: AlamelModel
        real(DP), dimension(3, 3):: deformation_gradient                    !! Description of the current grain shape (at the beginning of the current time step). Considered identical for all grains.
    contains
        procedure, nopass:: get_name       => alamel_get_name
        procedure, nopass:: get_description => alamel_get_description
        procedure, nopass:: get_signature  => alamel_get_signature
        procedure, nopass:: get_input => alamel_get_input         !! Inherited from [[MesoModel]]
        procedure, nopass:: get_cluster_state => alamel_get_state
        procedure:: init                   => alamel_init                   !! Inherited from [[MesoModel]]
        procedure:: get_stress             => alamel_get_stress             !! Inherited from [[MesoModel]]
        procedure:: apply_step             => alamel_deform                 !! Inherited from [[MesoModel]]
        procedure:: update                 => alamel_update                 !! Inherited from [[MesoModel]]
        procedure:: serialize => alamel_serialize
        procedure:: deserialize => alamel_deserialize
    end type

contains

    pure function alamel_get_name() result(name)
        character(:), allocatable:: name

        name = "ALAMEL"
    end function

    pure function alamel_get_description() result(description)
        character(:), allocatable:: description

        description = "Applies the global velocity gradient to clusters of 2 grains. 2 of the shear components " // &
                      "along their boundary plane are relaxed such that the grains may deform independently, " // &
                      "but the cluster as a whole still follows the imposed deformation. " // &
                      "This implements partial stress equilibrium at the grain boundaries. " // &
                      "The boundary orientation evolution is tracked during deformation."
    end function

    pure function alamel_get_signature() result(signature)
        integer, dimension(:), allocatable:: signature

        signature = [INPUT_ANGLES_LIST]
    end function

    !> See [[MesoModel:get_parameters]]
    pure function alamel_get_input() result(inputs)
        type(Input), dimension(:), allocatable:: inputs !! - **Boundaries**: List of Euler angles in Bunge convention
                                                            !! denoting the orientation of the grain boundary plane normals.

        inputs = [Input(to_c_string("Boundaries",NAME_LEN), INPUT_ANGLES_LIST)]
    end function

    pure function alamel_get_state() result(state)
        class(ClusterState), allocatable:: state

        allocate(AlamelClusterState):: state
    end function

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

        boundaries = params(1)

        this%deformation_gradient = UNIT_MATRIX_3X3

        allocate(AlamelCluster:: clusters(size(grains)/2))

        !Must check type even though we just allocated due to Fortran semantics.
        select type(clusters)
        type is (AlamelCluster)
            j = 1
            do i = 1, size(clusters)
                clusters(i)%grains = grains(2*(i-1)+1:2*i)
                clusters(i)%initial_boundary_normal = spherical_to_cartesian(boundaries(:,j))
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

    !> See [[MesoModel:update]]
    subroutine alamel_update(this, velocity_gradient, time)
        class(AlamelModel), intent(inout):: this
        real(DP), dimension(3,3), intent(in):: velocity_gradient
        real(DP), intent(in):: time

        !F(t+1) = e^(Ldt) * F(t)
        this%deformation_gradient = matmul(matrix_exponential(time*velocity_gradient), this%deformation_gradient)
    end subroutine

    !> See [[MesoModel:apply_step]]
    subroutine alamel_deform(this, cluster_, velocity_gradient, time, stress, slip)
        class(AlamelModel), intent(in):: this
        class(Cluster), target,   intent(inout):: cluster_
        real(DP), dimension(3,3), intent(in):: velocity_gradient
        real(DP),                 intent(in):: time
        real(DP), dimension(3,3), intent(out):: stress
        real(DP),                 intent(out):: slip

        real(DP):: strain_grain(5), &
                   strain_relaxations(5), &
                   slip_grain, &
                   spin_coeffs_relaxations(3, 2), &
                   taylor_coeffs_relaxations(5,2), &
                   taylor_coeffs(10, total_systems(cluster_)), &
                   stress_cluster(10), &
                   imposed_strain_rate(10), &
                   spin_relax(3), &
                   deformation_relax(5), &
                   v_grad_relax(3,3), &
                   v_grad_grain(3,3), &
                   stress_grain(3,3)
        integer::  i, j, &
                   n_systems(2), &
                   n_active_simplex, &
                   offset_grain, &
                   offset_systems, &
                   offset_relaxations
        integer, dimension(:), allocatable:: ind_overstressed_slip_systems
        real(DP), dimension(total_systems(cluster_)):: slip_rates, &
                                                       rss
        type(AlamelCluster), pointer:: cluster_ptr

        cluster_ptr => to_alamel_cluster(cluster_)

        n_systems = get_n_systems(cluster_)
        offset_relaxations = sum(n_systems)

        imposed_strain_rate = calc_imposed_strain_rate(cluster_ptr, velocity_gradient)

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
                slip = slip+slip_grain / 2._DP


                !Get spin coefficients of the relaxations corresponding to the current grain
                do i = 1, 2
                    spin_coeffs_relaxations(:,i) = cluster_ptr%relaxations(i)%spin_coeffs((j-1)*3+1:j*3)
                end do

                deformation_relax = matmul(taylor_coeffs_relaxations, slip_rates_relaxations)
                spin_relax = matmul(spin_coeffs_relaxations, slip_rates_relaxations)

                v_grad_relax = (deviatoric_to_tensor(deformation_relax) + spin_to_tensor(spin_relax)) .fromframe. grain_%orientation
                v_grad_grain = velocity_gradient - v_grad_relax
                stress_grain = deviatoric_to_tensor(stress_cluster(offset_grain+1:offset_grain+5)) .fromframe. grain_%orientation

                call grain_%deform(v_grad_grain, time, slip_rates_grain, stress_grain)
            end associate
        end do

        !Update cluster state to be consistent with the end of the time step.
        call update_relaxations(cluster_ptr, this%deformation_gradient)
        cluster_ptr%weight = cluster_weight(cluster_ptr, this%deformation_gradient)
    end subroutine


    !> Get the direction of a surface normal after application of a deformation gradient.
    !>
    !> Based on Nanson's formula.
    !> Input and output are NOT normalized.
    !> Deformation is assumed isochoric so det(F) == 1
    function deform_normal_direction(normal, deformation_gradient) result(new_normal)
        real(DP), dimension(3), intent(in):: normal                 !! Surface normal in the reference configuration
        real(DP), dimension(3,3), intent(in):: deformation_gradient !! Maps reference to deformed configuration
        real(DP), dimension(3):: new_normal                         !! Surface normal in the deformed configuration

        new_normal = matmul(invert(transpose(deformation_gradient)), normal)
    end function

    !> Determine the weight of a cluster
    !>
    !> The boundary influence zone is assumed to be proportional to the surface area of the boundary.
    !> Using Nanson's formula we can calculate the area of a deformed boundary.
    !> The area of the boundary in the undeformed (equiaxed) configuration is considered 1.
    !> Replaces the approach from Van Houtte et. al., 2004: Appendix A
    function cluster_weight(alamel_cluster, def_grad) result(weight)
        type(AlamelCluster), intent(in):: alamel_cluster
        real(DP), intent(in):: def_grad(3, 3)
        real(DP):: weight

        real(DP):: boundary_normal(3), &
                   oriented_area(3)

        oriented_area = det(def_grad) * deform_normal_direction(alamel_cluster%initial_boundary_normal, def_grad)
        weight = norm2(oriented_area)
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
                   basis(10, 10), &
                   vec(3)
        integer:: i

        !Construct current boundary frame from the initial normal and deformation gradient:

        !We can use nanson's formula to map the reference boundary normal to the current one:
        new_boundary_frame(:,3) = deform_normal_direction(alamel_cluster%initial_boundary_normal, def_grad)
        !The second vector is orthogonal to the normal, so take cross product with arbitrary vector
        !Make sure the arbitrary vector is not pointing in the same direction as the reference vector.
        vec = 0.0
        vec(minloc(abs(new_boundary_frame(:,3)))) = 1.0
        new_boundary_frame(:,2) = new_boundary_frame(:,3) .cross. vec
        !Final vector must be orthogonal to both existing vectors:
        new_boundary_frame(:,1) = new_boundary_frame(:,2) .cross. new_boundary_frame(:,3)
        !Normalize everything at once
        new_boundary_frame = normalize(new_boundary_frame)

        !Transform relaxation from boundary frame to crystal frame
        !Composed of rotation from boundary to global frame and then from global to crystal frame.
        do i = 1, 2
            boundary_to_crystal(:,:,i) = transformation_matrix(new_boundary_frame, alamel_cluster%grains(i)%orientation)
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

    pure function alamel_serialize(this) result(params)
        class(AlamelModel), intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        params = [serialize(this%deformation_gradient)]
    end function

    function alamel_deserialize(this, params) result(params_)
        class(AlamelModel), intent(out):: this
        type(Parameter), dimension(:), intent(in):: params
        type(Parameter), dimension(:), allocatable:: params_

        this%deformation_gradient = params(1)
        params_ = params .pop. 1
    end function

    pure function alamel_cluster_serialize(this) result(params)
        class(AlamelClusterState), intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        integer:: i

        params = this%ClusterState%serialize()
        params .add. [serialize(this%ind_basis_systems), &
                      serialize(this%initial_boundary_normal), &
                      serialize(this%inverse_basis)]
        do i=1,2
            params = params .add. this%relaxations(i)%serialize()
        end do
    end function

    function alamel_cluster_deserialize(this, params) result(params_)
        class(AlamelClusterState), intent(out):: this
        type(Parameter), dimension(:), intent(in):: params
        type(Parameter), dimension(:), allocatable:: params_

        integer:: i

        params_ = this%ClusterState%deserialize(params)
        this%ind_basis_systems = params_(1)
        this%initial_boundary_normal = params_(2)
        this%inverse_basis = params_(3)

        do i=1,2
            params_ = this%relaxations(i)%deserialize(params_)
        end do
    end function
end module
