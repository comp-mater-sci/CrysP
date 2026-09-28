!> Traditional full constraints Taylor theory.
!>
!> Assumes all grains deform identically.

module full_constraints_taylor
    use base_defs
    use math_utils
    use conversions
    use crysp_cluster
    use crysp_status
    use taylor_ambiguity
    use micro
    use simplex
    use crysp_meso_model
    use crysp_model
    use crysp_parameters
    use crysp_input

    implicit none

    private
    public:: TaylorModel

    character(*), parameter:: MOD_NAME = "full_constraints_taylor"

    !> Cluster-specific state needed for full constraints Taylor simulations.
    !>
    !> A Taylor cluster only holds 1 grain.
    type, extends(Cluster):: TaylorCluster
        integer, dimension(5):: ind_basis_systems
        real(DP), dimension(5, 5):: inverse_basis
    contains
        procedure:: serialize => taylor_cluster_serialize
        procedure:: deserialize => taylor_cluster_deserialize
    end type


    !> Model for full constraints Taylor simulations.
    type, extends(MesoModel):: TaylorModel
    contains
        procedure:: describe => fctaylor_describe
        procedure, nopass:: make_cluster => fctaylor_make_cluster
        procedure:: init       => fctaylor_init       !! Inherited from [[MesoModel]]
        procedure:: get_stress => fctaylor_get_stress !! Inherited from [[MesoModel]]
        procedure:: apply_step => fctaylor_deform     !! Inherited from [[MesoModel]]
        procedure:: update     => fctaylor_update     !! Inherited from [[MesoModel]]
    end type

contains

    !> The model takes no inputs and carries no state of its own, so it only names itself.
    function fctaylor_describe(this) result(desc)
        class(TaylorModel), intent(in):: this
        type(ModelDescriptor):: desc

        desc = this%MesoModel%Model%describe()
        desc%name = "Full-Constraints Taylor"
        desc%description = "Each grain is forced to deform exactly like the material as a whole."
    end function

    function fctaylor_make_cluster() result(clstr)
        class(Cluster), allocatable:: clstr

        allocate(TaylorCluster:: clstr)
    end function

    !> Convert the type of a provided generic cluster to TaylorCluster
    !>
    !> This is the closest Fortran can get to proper type casting.
    !> Useful for accessing TaylorCluster-specific fields without the boilerplate of type selection and error handling in
    !> each calling procedure.
    !> A cluster of any other dynamic type means the model and the cluster list disagree, which is a defect in libCrysP rather than
    !> caller error. It is still reported rather than fatal, so that an embedding host survives it.
    function to_taylor_cluster(cluster_, ptr) result(stat)
        class(Cluster), target, intent(in):: cluster_   !! Generic input cluster
        type(TaylorCluster), pointer, intent(out):: ptr !! Pointer to input cluster of type TaylorCluster
        type(Status):: stat                             !! `CRYSP_OK`, or `CRYSP_ERR_TYPE` if the cluster is not a `TaylorCluster`.

        select type (cluster_)
            type is (TaylorCluster)
                ptr => cluster_
            class default
                ptr => null()
                stat = fail(MOD_NAME, 'to_taylor_cluster', CRYSP_ERR_TYPE, 'Cluster is not a TaylorCluster')
        end select
    end function

    !> See [[MesoModel:Init]]
    subroutine fctaylor_init(this, grains, params, clusters)
        class(TaylorModel), intent(inout):: this
        type(Grain), dimension(:), intent(in):: grains
        type(Parameter), dimension(:), intent(in):: params
        class(Cluster), dimension(:), allocatable, intent(out):: clusters

        real(DP), dimension(:,:), allocatable:: taylor_coeffs
        integer:: i

        allocate(TaylorCluster:: clusters(size(grains)))

        !Must check type even though we just allocated due to Fortran semantics.
        select type (clusters)
            type is (TaylorCluster)
                do i = 1, size(clusters)
                    clusters(i)%grains = [grains(i)]
                    clusters(i)%weight = 1._DP
                    !Per grain: phases may differ in slip system set, so neither the basis nor the number of systems is shared.
                    clusters(i)%ind_basis_systems = grains(i)%phase%model%basis
                    taylor_coeffs = grains(i)%phase%model%taylor_coeffs
                    clusters(i)%inverse_basis = invert(taylor_coeffs(:,clusters(i)%ind_basis_systems))
                end do
        end select
    end subroutine

    !> See [[MesoModel:get_stress]]
    function fctaylor_get_stress(this, cluster_, v_grad, stress) result(stat)
        class(TaylorModel), intent(in):: this
        class(Cluster), target, intent(inout):: cluster_
        real(DP), dimension(3, 3), intent(in):: v_grad
        real(DP), dimension(3, 3), intent(out):: stress
        type(Status):: stat

        real(DP), dimension(size(cluster_%grains(1)%phase%model%taylor_coeffs, 2)):: slip_rates, &
                                                                     rss
        real(DP), dimension(5):: stress_cluster
        type(TaylorCluster), pointer:: cluster_ptr

        stress = 0._DP

        stat = to_taylor_cluster(cluster_, cluster_ptr)
        if (failed(stat)) return

        associate (grain_ => cluster_ptr%grains(1))
            stat = simplex_solve(grain_%phase%model%taylor_coeffs, &
                                 tensor_to_deviatoric(v_grad .toframe. grain_%orientation), &
                                 grain_%hardening_state%crss, &
                                 cluster_ptr%inverse_basis, &
                                 cluster_ptr%ind_basis_systems, &
                                 slip_rates, &
                                 stress_cluster, &
                                 rss)
            if (failed(stat)) return

            stress = deviatoric_to_tensor(stress_cluster) .fromframe. grain_%orientation
        end associate
    end function

    !> See [[MesoModel:apply_step]]
    function fctaylor_deform(this, cluster_, velocity_gradient, time, stress, slip) result(stat)
        class(TaylorModel), intent(in):: this
        class(Cluster), target, intent(inout):: cluster_
        real(DP), dimension(3,3), intent(in):: velocity_gradient
        real(DP), intent(in):: time
        real(DP), dimension(3, 3), intent(out):: stress
        real(DP), intent(out):: slip
        type(Status):: stat

        integer::                   n_systems, &
                                    n_active_simplex
        real(DP), dimension(size(cluster_%grains(1)%phase%model%taylor_coeffs, 2)):: slip_rates, &
                                                                               rss
        real(DP):: stress_cluster(5), &
                   imposed_strain_rate(5), &
                   lattice_spin(3,3), &
                   v_grad_grain(3,3)
        integer, dimension(:), allocatable:: ind_active_slip_systems
        type(TaylorCluster), pointer:: cluster_ptr

        stress = 0._DP
        slip = 0._DP

        stat = to_taylor_cluster(cluster_, cluster_ptr)
        if (failed(stat)) return

        associate(grain_=>cluster_ptr%grains(1))

            n_systems = size(grain_%phase%model%taylor_coeffs, 2)

            imposed_strain_rate = tensor_to_deviatoric(velocity_gradient .toframe. grain_%orientation)

            stat = simplex_solve(grain_%phase%model%taylor_coeffs, &
                                 imposed_strain_rate, &
                                 grain_%hardening_state%crss, &
                                 cluster_ptr%inverse_basis, &
                                 cluster_ptr%ind_basis_systems, &
                                 slip_rates, &
                                 stress_cluster, &
                                 rss)
            if (failed(stat)) return

            stress = deviatoric_to_tensor(stress_cluster) .fromframe. grain_%orientation

            stat = assess_slip_system_activity(grain_, rss, slip_rates, n_active_simplex, ind_active_slip_systems)
            if (failed(stat)) return

            if (allocated(ind_active_slip_systems)) then
                stat = resolve_taylor_ambiguity(ind_active_slip_systems, &
                    rss(ind_active_slip_systems), &
                    imposed_strain_rate, &
                    grain_%phase%model%taylor_coeffs, &
                    n_active_simplex, &
                    slip_rates)
                if (failed(stat)) return
            end if

            slip = sum(abs(slip_rates))

            stat = grain_%deform(velocity_gradient, time, slip_rates, stress)
        end associate
    end function

    !> Update the model state
    !>
    !> Nothing to do for this model. Must implement procedure anyway because the base class defining it is abstract.
    subroutine fctaylor_update(this, velocity_gradient, time)
        class(TaylorModel), intent(inout):: this  !! Model instance
        real(DP), dimension(3,3), intent(in):: velocity_gradient !! Velocity gradient during the time step. Assumed constant.
        real(DP), intent(in):: time                              !! Duration of the time step.
    end subroutine

    function taylor_cluster_serialize(this, phases) result(params)
        class(TaylorCluster), target, intent(in):: this
        type(Phase), dimension(:), target, intent(in):: phases
        type(Parameter), dimension(:), allocatable:: params

        integer:: base_size
        type(Parameter), dimension(:), allocatable:: base

        allocate(params(this%size()))

        base = this%Cluster%serialize(phases)
        base_size = size(base)
        params(:base_size) = base
        params(base_size+1) = this%ind_basis_systems
        params(base_size+2) = this%inverse_basis
    end function

    subroutine taylor_cluster_deserialize(this, params, phases)
        class(TaylorCluster), target, intent(out):: this
        type(Parameter), dimension(:), intent(in):: params
        type(Phase), dimension(:), target, intent(in):: phases

        integer:: base_size

        call this%Cluster%deserialize(params, phases)

        base_size = this%Cluster%size()
        this%ind_basis_systems = params(base_size+1)
        this%inverse_basis = params(base_size+2)
    end subroutine
end module
